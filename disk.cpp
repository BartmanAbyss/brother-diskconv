#include "stdafx.h"

uint32_t SectorInfo::ComputeContentHash() const {
	uint32_t hash = mbMFM;

	hash += mAddressMark;
	hash += mSectorSize;
	hash += mComputedCRC;
	hash += mRecordedCRC;
	hash += mSectorSize;

	for(uint32_t i=0; i<mSectorSize; i+=4) {
		hash += *(const uint32_t *)&mData[i];
		hash = (hash >> 1) + (hash << 31);
	}

	return hash;
}

bool SectorInfo::HasSameContents(const SectorInfo& other) const {
	if (mbMFM != other.mbMFM)
		return false;

	if (mAddressMark != other.mAddressMark)
		return false;

	if (mSectorSize != other.mSectorSize)
		return false;

	if (mComputedCRC != other.mComputedCRC)
		return false;

	if (mRecordedCRC != other.mRecordedCRC)
		return false;

	if (memcmp(mData, other.mData, mSectorSize))
		return false;

	return true;
}

void sift_sectors(TrackInfo& track_info, int track_num, std::vector<SectorInfo *>& secptrs) {
	std::vector<SectorInfo *> newsecptrs;

	// gather sectors from track
	secptrs.clear();
	for(auto it = track_info.mSectors.begin(), itEnd = track_info.mSectors.end();
		it != itEnd;
		++it)
	{
		secptrs.push_back(&*it);
	};

	// sort sectors by index
	std::sort(secptrs.begin(), secptrs.end(),
		[](const SectorInfo *x, const SectorInfo *y) -> bool {
			return x->mIndex < y->mIndex;
		}
	);

	// extract out one group at a time
	std::vector<SectorInfo *> secgroup;

	while(!secptrs.empty()) {
		const int sector = secptrs.front()->mIndex;

		secgroup.clear();
		for(size_t idx = 0; idx < secptrs.size(); ) {
			if (secptrs[idx]->mIndex == sector) {
				secgroup.push_back(secptrs[idx]);
				secptrs[idx] = secptrs.back();
				secptrs.pop_back();
			} else {
				++idx;
			}
		}

		// sort sectors in group by angular position
		std::sort(secgroup.begin(), secgroup.end(),
			[](const SectorInfo *x, const SectorInfo *y) -> bool {
				return x->mPosition < y->mPosition;
			}
		);

		// fish out subgroups from remaining sectors
		auto it1 = secgroup.begin();
		int sector_count = 0;

		while(it1 != secgroup.end()) {
			float position0 = (*it1)->mPosition;
			float posend0 = (*it1)->mEndingPosition;

			float poserr_sum = 0;
			float posenderr_sum = 0;
			auto it2 = it1 + 1;

			bool mismatch = false;
			std::vector<SectorInfo *> subgroup(1, *it1);
			while(it2 != secgroup.end()) {
				// stop if sector angle is more than 5% off
				float poserr = (*it2)->mPosition - position0;

				if (poserr > 0.5f)
					poserr -= 1.0f;

				if (fabsf(poserr) > 0.03f)
					break;

				poserr_sum += poserr;

				float posenderr = (*it2)->mEndingPosition - posend0;
				if (posenderr > 0.5f)
					posenderr -= 1.0f;

				posenderr_sum += posenderr;

				subgroup.push_back(*it2);

				if (!(*it1)->HasSameContents(**it2))
					mismatch = true;

				++it2;
			}

			// compute average angular position (even with sectors that didn't entirely read cleanly)
			position0 += poserr_sum / (float)subgroup.size();
			position0 -= floorf(position0);

			posend0 += posenderr_sum / (float)subgroup.size();
			posend0 -= floorf(posend0);

			// check if we have any sectors which passed CRC check; if so, remove all that didn't
			bool crcOK = true;

			int n1 = subgroup.size();

			if (std::find_if(subgroup.begin(), subgroup.end(), [](const SectorInfo *x) { return x->mRecordedCRC == x->mComputedCRC; }) != subgroup.end()) {
				subgroup.erase(std::remove_if(subgroup.begin(), subgroup.end(),
					[](const SectorInfo *x) { return x->mRecordedCRC != x->mComputedCRC; }), subgroup.end());
			} else {
				crcOK = false;
			}

			int n2 = subgroup.size();

			if (n1 != n2)
				printf("WARNING: Track %2d, sector %2d: %u/%u bad sector reads discarded at position %.2f.\n", track_num, sector, n1-n2, n1, position0);

			SectorInfo *best_sector = subgroup.front();

			// check if we had more than one sector in this position that we kept
			bool clean_sift = true;

			if (subgroup.size() > 1 && mismatch) {
				// Alright, we have multiple sectors in the same place with different contents. Let's see
				// if we can narrow this down. Compute hashes of all the sectors, then find the most
				// popular sector.

				clean_sift = false;

				struct HashedSectorRef {
					SectorInfo *mpSector;
					uint32_t mHash;
				};

				struct HashedSectorPred {
					bool operator()(const HashedSectorRef& x, const HashedSectorRef& y) const {
						return x.mHash == y.mHash && x.mpSector->HasSameContents(*y.mpSector);
					}

					size_t operator()(const HashedSectorRef& x) const {
						return x.mHash;
					}
				};

				std::unordered_map<HashedSectorRef, uint32_t, HashedSectorPred, HashedSectorPred> hashedSectors;

				for(auto it = subgroup.begin(); it != subgroup.end(); ++it) {
					HashedSectorRef hsref = { *it, (*it)->ComputeContentHash() };
					++hashedSectors[hsref];
				}

				// check if we now only have one sector left; this can happen if the first read was bad
				// and the rest were good
				if (hashedSectors.size() == 1) {
					clean_sift = true;
				} else {
					// find the most popular; count is small, so we'll just do this linearly
					auto best_ref = hashedSectors.begin();
					for(auto it = hashedSectors.begin(), itEnd = hashedSectors.end(); it != itEnd; ++it) {
						if (it->second > best_ref->second)
							best_ref = it;
					}

					best_sector = best_ref->first.mpSector;

					// check if we had a sector duplicated more than once
					if (best_ref->second > 1) {
						printf(
							"WARNING: Track %2d, sector %2d: %d different sectors found at the same position\n"
							"         %.2f but different %s data. Keeping the most popular one.\n",
							track_num,
							sector,
							n2,
							position0,
							crcOK ? "good" : "bad");
					} else {
						if (crcOK) {
							printf(
								"WARNING: Track %2d, sector %2d: %d different sectors found at the same\n"
								"         position %.2f but different good data. Keeping one of them.\n",
								track_num,
								sector,
								n2,
								position0);
						} else {
							// compute how much of the sector is in common
							uint32_t max_match = best_sector->mSectorSize;

							for(auto it = it1; it != it2; ++it) {
								if ((*it) == best_sector)
									continue;

								for(uint32_t i=0; i<max_match; ++i) {
									if ((*it)->mData[i] != best_sector->mData[i]) {
										max_match = i;
										break;
									}
								}
							}

							best_sector->mWeakOffset = max_match;

							printf(
								"WARNING: Track %2d, sector %2d: Multiple sectors found at the same position\n"
								"         %.2f but different bad data. Encoding weak sector at offset %d.\n",
								track_num,
								sector,
								position0,
								max_match);
						}
					}
				}
			}

			if (clean_sift && !crcOK) {
				// It's highly unlikely that a weak sector would have stable data, but it's possible
				// for an ATX image source.
				if (best_sector->mWeakOffset >= 0)
					printf("WARNING: Weak sector detected for track %d, sector %d at position %.2f, offset %d.\n", track_num, sector, position0, best_sector->mWeakOffset);
				else
					printf("WARNING: Track %2d, sector %2d: Stable CRC error detected at position %.2f.\n", track_num, sector, position0);
			}

			// adjust sector position to center
			best_sector->mPosition = position0;
			best_sector->mEndingPosition = posend0;

			newsecptrs.push_back(best_sector);
			++sector_count;

			it1 = it2;
		}

		if (sector_count > 1)
			printf("WARNING: Track %2d, sector %2d: %u phantom sector%s found.\n", track_num, sector, sector_count - 1, sector_count > 2 ? "s" : "");
	}

	secptrs.swap(newsecptrs);

	// resort sectors by angular position
	std::sort(secptrs.begin(), secptrs.end(),
		[](const SectorInfo *x, const SectorInfo *y) -> bool {
			return x->mPosition < y->mPosition;
		}
	);
}
