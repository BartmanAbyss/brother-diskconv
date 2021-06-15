// brotherdisk - raw disk conversion utility for Brother LW-30
// Copyright (C) 2017-2018 Bartman/Abyss
//
// based on 
//
// a8rawconv - A8 raw disk conversion utility
// Copyright (C) 2014-2015 Avery Lee
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General License for more details.
//
// You should have received a copy of the GNU General License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

// STATUS
//
// [TODO] Kryoflux->IPF
// [TODO] ->FAT (currently dummy implementation for Tetris disk)
// [TODO] FAT->IMG
// [TODO] write_fat / write_img extract common code?
//
// [DONE] Kryoflux->IMG
// [DONE] IMG->IPF
// [DONE] IPF loads twice as slow as original disk; check layout/gaps with reference disk

// args
// -vv -if fat test_data\fat fat.img
// -vv -of fat tetris.img test_data\fat_out


#include "stdafx.h"

// CAPS just used for validating IPF output (_DEBUG only)
#ifdef _DEBUG
	//#define CAPSLIB
#endif
#ifdef CAPSLIB
	#include "caps/CapsLibAll.h"
	#pragma comment(lib, "caps/CAPSImg.lib")
#endif

template<class T>
class for_each_iterator {
public:
	for_each_iterator(T& c) : mContainer(c) {}

	template<class Fn>
	void operator>>=(const Fn& fn) {
		std::for_each(mContainer.begin(), mContainer.end(), fn);
	}

private:
	T& mContainer;
};

template<class T>
for_each_iterator<T> get_for_each(T& container) {
	return for_each_iterator<T>(container);
}

#define FOR_EACH(var, container) get_for_each((container)) >>= [&](decltype(*container.begin())& var)

void fatal(const char *msg) {
	puts(msg);
	exit(10);
}

void fatalf(const char *msg, ...) {
	va_list val;
	va_start(val, msg);
	vprintf(msg, val);
	va_end(val);
	exit(10);
}

void fatal_read() {
	fatalf("Unable to read from input file: %s.\n", g_inputPath.c_str());
}

DiskInfo g_disk;

int g_inputPathCountPos;
int g_inputPathCountWidth;
std::string g_inputPath;

int g_verbosity;
std::string g_outputPath;
bool g_showLayout;
float g_clockPeriodAdjust = 1.0f;
int g_trackSelect = -1;
int g_trackCount = 81;

enum class Format {
	Auto,
	KryoFluxStream,
	IPF,
	IMG,
	FAT
};

Format g_inputFormat = Format::Auto;
Format g_outputFormat = Format::Auto;

///////////////////////////////////////////////////////////////////////////

void process_track_brother(const RawTrack& rawTrack);

namespace brother {
	static constexpr std::array<uint16_t, 80> sync_table{
		0xDAEF, 0xB7AD, 0xFBBE, 0xEADF, 0xBFFA, 0xAEB6, 0xF5D7, 0xDBEE, 0xBAAB, 0xFDBD,
		0xEBDE, 0xD5F7, 0xAFB5, 0xF6D6, 0xDDED, 0xBBAA, 0xEDBB, 0xD6DD, 0xB5F6, 0xF7AF,
		0xDED5, 0xBDEB, 0xABFD, 0xEEBA, 0xD7DB, 0xB6F5, 0xFAAE, 0xDFBF, 0xBEEA, 0xADFB,
		0xEFB7, 0xDADA, 0xB7EF, 0xFBAD, 0xEABE, 0xBFDF, 0xAEFA, 0xF5B6, 0xDBD7, 0xBAEE,
		0xFDAB, 0xEBBD, 0xD5DE, 0xAFF7, 0xF6B5, 0xDDD6, 0xBBED, 0xAADD, 0xEDF6, 0xD6AF,
		0xB5D5, 0xF7EB, 0xDEFD, 0xBDBA, 0xABDB, 0xEEF5, 0xD7AE, 0xB6BF, 0xFAEA, 0xDFFB,
		0xBEB7, 0xADDA, 0xEFEF, 0xDAAD, 0xB7BE, 0xFBDF, 0xEAFA, 0xBFB6, 0xAED7, 0xF5EE,
		0xDBAB, 0xBABD, 0xFDDE, 0xEBF7, 0xD5B5, 0xAFD6, 0xF6ED, 0xDDAA, 0xD6BB, 0xB5DD
	};

	static constexpr uint8_t gcr_table[]{
		0xAA, 0xAB, 0xAD, 0xAE, 0xAF, 0xB5, 0xB6, 0xB7,
		0xBA, 0xBB, 0xBD, 0xBE, 0xBF, 0xD5, 0xD6, 0xD7,
		0xDA, 0xDB, 0xDD, 0xDE, 0xDF, 0xEA, 0xEB, 0xED,
		0xEE, 0xEF, 0xF5, 0xF6, 0xF7, 0xFA, 0xFB, 0xFD,
		0xFE, 0xFF // reserved
	};

	static constexpr uint8_t gcr5decoder[256] = {
	#define IL 255
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,

		// $A0
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL, 0, 1,IL, 2, 3, 4,
		// $B0
		IL,IL,IL,IL,IL, 5, 6, 7,IL,IL, 8, 9,IL,10,11,12,
		// $C0
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,
		// $D0
		IL,IL,IL,IL,IL,13,14,15,IL,IL,16,17,IL,18,19,20,
		// $E0
		IL,IL,IL,IL,IL,IL,IL,IL,IL,IL,21,22,IL,23,24,25,
		// $F0
		IL,IL,IL,IL,IL,26,27,28,IL,IL,29,30,IL,31,IL,IL,
	#undef IL
	};

	static constexpr uint8_t sector_interleave1[]{ // 0-based
		0, 5, 10, 3, 8, 1, 6, 11, 4, 9, 2, 7
	};

	// format
	static constexpr uint8_t sector_prefix[]{ // 8 bytes
		0xBF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xAB
	};
	static constexpr uint8_t sector_predata[]{ // 19 bytes
		0xDD, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA,
		0xAA, 0xAA, 0xBF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE,
		0xED
	};
	static constexpr uint8_t sector_postdata[]{ // 13 bytes
		0xAD, 0xEE, 0xF5, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD,
		0xDD, 0xDD, 0xDD, 0xDD, 0xDD
	};

	// write
	static constexpr uint8_t sector_header[]{ // 15 bytes
		0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xAA, 0xBF,
		0xFF, 0xFF, 0xFF, 0xFF, /*0xFF,*/ 0xFE, 0xED
	};
	static constexpr uint8_t sector_footer[]{
		0xF5, 0xDD, 0x00, 0x00, 0x00, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD, 0xDD
	};

	// format track: 0xaa (2), 0xaa (48), 12*sector
	// format sector: sector_prefix (8), track_sync (2), sector_sync (2), predata (19), payload=0xaa (414), postdata (13), 0xaa (??)
	// write sector: (after sector_sync, 0xdd) sector_header (2+14), payload (416), sector_footer (11)

	// from write_format, write_sector_data_header_data_footer
	constexpr static auto sector_length = sizeof(sector_prefix) + 2/*track_sync*/ + 2/*sector_sync*/ + 1/*0xdd*/ + sizeof(sector_header) + 416/*payload*/ + sizeof(sector_footer);
	constexpr static auto track_length = 2/*0xaa*/ + 48/*0xaa*/ + 12 * sector_length;

	constexpr static uint32_t bytes_per_sector = 256;
	constexpr static uint32_t sectors_per_track = 12;
	constexpr static uint32_t tracks_per_disk = 78;

	void gcr_decode_8_to_5(const uint8_t* input, uint8_t* output) {
		// output:
		// 76543210
		// --------
		// 00000111 0
		// 11222223 1
		// 33334444 2
		// 45555566 3
		// 66677777 4

		uint8_t decode[8];
		for(int i = 0; i < 8; i++) {
			decode[i] = gcr5decoder[input[i]];
			//if(decode[i] == 255)
			//	printf("illegal GCR\n");
		}

		output[0] = (decode[0] << 3) | (decode[1] >> 2);
		output[1] = (decode[1] << 6) | (decode[2] << 1) | (decode[3] >> 4);
		output[2] = (decode[3] << 4) | (decode[4] >> 1);
		output[3] = (decode[4] << 7) | (decode[5] << 2) | (decode[6] >> 3);
		output[4] = (decode[6] << 5) | decode[7];
	}

	void gcr_encode_5_to_8(const uint8_t* input, uint8_t* output) {
		// input:
		// 76543210
		// --------
		// 00000111 0
		// 11222223 1
		// 33334444 2
		// 45555566 3
		// 66677777 4

		output[0] = gcr_table[(input[0] >> 3) & 0x1f];
		output[1] = gcr_table[((input[0] << 2) & 0x1f) | ((input[1] >> 6) & 0b11)];
		output[2] = gcr_table[(input[1] >> 1) & 0x1f];
		output[3] = gcr_table[((input[1] << 4) & 0x1f) | ((input[2] >> 4) & 0b1111)];
		output[4] = gcr_table[((input[2] << 1) & 0x1f) | ((input[3] >> 7) & 0b1)];
		output[5] = gcr_table[(input[3] >> 2) & 0x1f];
		output[6] = gcr_table[((input[3] << 3) & 0x1f) | ((input[4] >> 5) & 0b111)];
		output[7] = gcr_table[input[4] & 0x1f];
	}

	std::array<uint8_t, 3> checksum_256_bytes(const uint8_t* input) {
		size_t i = 0;
		uint8_t a = 0;
		uint8_t c = input[i++];
		uint8_t d = input[i++];
		uint8_t e = input[i++];
		for(size_t b = 0; b < 253; b++) {
			a = d;
			if(c & 0b10000000)
				a ^= 1;
			d = c;
			c = a;
			a = (d << 1) ^ e;
			e = d;
			d = a;
			e ^= input[i++];
		}

		return{ c, d, e };
	}

	std::array<uint8_t, 416> gcr_encode_and_checksum(const uint8_t* input /* 256 bytes */) {
		std::array<uint8_t, 416> output;
		for(int i = 0; i < 51; i++)
			gcr_encode_5_to_8(&input[i * 5], &output[i * 8]);

		auto checksum = checksum_256_bytes(input);
		std::array<uint8_t, 5> end_and_checksum{ input[255], checksum[0], checksum[1], checksum[2], 0x58 };
		gcr_encode_5_to_8(&end_and_checksum[0], &output[408]);

		return output;
	}
} // namespace brother

void process_track_brother(const RawTrack& rawTrack) {
	// 1 transition 4µs
	// 300 revolutions per minute => 0.2s per revolution
	// 50,000 transitions per revolution

	// => 95.877 samples per transition
	//printf("samples per transition: %f\n", rawTrack.mSamplesPerRev / 50000.f);
	printf("track: %d\n", rawTrack.mTrack);

	// copy from a2gcr
	double rpm = 300.0;

	const double cells_per_rev = 250000.0 / (rpm / 60.0);
	double scks_per_cell = rawTrack.mSamplesPerRev / cells_per_rev * g_clockPeriodAdjust;

	printf("%.2f samples per cell\n", scks_per_cell);

	auto& decTrack = g_disk.mTracks[rawTrack.mTrack];

	if(rawTrack.mTransitions.size() < 2)
		return;

	const uint32_t *samp = rawTrack.mTransitions.data();
	size_t samps_left = rawTrack.mTransitions.size() - 1;
	int time_left = 0;
	int time_basis = 0;

	int cell_len = (int)(scks_per_cell + 0.5);
	int cell_range = cell_len / 3;
	int cell_timer = 0;

	uint8_t shifter = 0;

	int bit_state = 0;
	int byte_state = 0;

	int sector_headers = 0;
	int data_sectors = 0;
	int good_sectors = 0;

	int track_index = -1;
	int sector_index = -1;

	float sector_position = 0;
	uint32_t raw_start = 0;
	uint32_t rot_start = 0;
	uint32_t rot_end = 0;

	constexpr auto payload_size = 416;
	uint8_t buf[payload_size];

	uint16_t track_sync = 0, sector_sync = 0;

	for(;;) {
		while(time_left <= 0) {
			if(!samps_left)
				goto done;

			//printf("duration: %d\n", samp[1] - samp[0]);
			time_left += samp[1] - samp[0];
			time_basis = samp[1];
			++samp;
			--samps_left;
		}

		// if the shift register is empty, restart shift timing at next transition
		if(!shifter) {
			time_left = 0;
			cell_timer = cell_len;
			shifter = 1;
			bit_state = 0;
		} else {
			// compare time to next transition against cell length
			int trans_delta = time_left - cell_timer;
			shifter += shifter;

			if(trans_delta <= cell_range) {
				cell_timer = cell_len - trans_delta / 3;
				time_left = 0;

				shifter++;
			} else {
				// we don't have a transition in range -- clock in a 0 bit
				time_left -= cell_timer;
				cell_timer = cell_len;
			}

			// advance bit machine state
			if(bit_state == 9 && shifter == 0xab) {
				// sync to next 0xAB (sector header)
				if(g_verbosity >= 3)
					printf("sync to %02X\n", shifter);
				shifter = 0;
				bit_state = 0;
				byte_state = 4;
			}
			if(bit_state == 9 && shifter == 0xed) {
				// sync to next 0xED (sector data)
				if(g_verbosity >= 3)
					printf("sync to %02X\n", shifter);
				shifter = 0;
				bit_state = 0;
				byte_state = 1000;
			}

			if(bit_state == 0) {
				if(shifter & 0x80) {
					bit_state = 1;

					decTrack.mGCRData.push_back(shifter);

					if(g_verbosity >= 3)
						printf("byte_state: %4u  shifter: %02X\n", byte_state, shifter);

					// okay, we have a byte... advance the byte state machine.
					if(byte_state == 0) {			// waiting for FF[0]
						raw_start = time_basis - time_left;

						if(shifter == 0xFF)
							byte_state = 1;
					} else if(byte_state == 1) {	// waiting for FF[1]
						if(shifter == 0xFF)
							byte_state = 2;
						else
							byte_state = 0;
					} else if(byte_state == 2) {	// waiting for FF[2]
						if(shifter == 0xFF) {
							bit_state = 9;
							// wait for resync
						} else
							byte_state = 0;
					} else if(byte_state == 4) {	// track_sync[0]
						track_sync = shifter;
						byte_state = 5;
					} else if(byte_state == 5) {	// track_sync[1]
						track_sync |= shifter << 8;
						byte_state = 6;
					} else if(byte_state == 6) {	// sector_sync[0]
						sector_sync = shifter;
						byte_state = 7;
					} else if(byte_state == 7) {	// sector_sync[1]
						sector_sync |= shifter << 8;
						byte_state = 8;
					} else if(byte_state == 8) {	// 0xDD
						if(shifter == 0xDD) {
							auto track_it = std::find(brother::sync_table.begin(), brother::sync_table.end(), track_sync);
							auto sector_it = std::find(brother::sync_table.begin(), brother::sync_table.end(), sector_sync);
							if(g_verbosity >= 2)
								printf("track_sync: %04X, sector_sync: %04X\n", track_sync, sector_sync);
							if(track_it == brother::sync_table.end()) {
								if(g_verbosity >= 2)
									printf("illegal track sync %04X\n", track_sync);
								track_index = sector_index = -1;
								byte_state = 0;
								continue;
							}
							if(sector_it == brother::sync_table.end()) {
								if(g_verbosity >= 2)
									printf("illegal sector sync %04X\n", sector_sync);
								byte_state = 0;
								continue;
							}
							track_index = std::distance(brother::sync_table.begin(), track_it);
							sector_index = std::distance(brother::sync_table.begin(), sector_it);

							// find the nearest index mark
							int vsn_time = time_basis - time_left;
							auto it_index = std::upper_bound(rawTrack.mIndexTimes.begin(), rawTrack.mIndexTimes.end(), (uint32_t)vsn_time + 1);

							if(it_index == rawTrack.mIndexTimes.begin()) {
								if(g_verbosity >= 2)
									printf("Skipping track %d, sector %d before first index mark\n", track_index, sector_index);

								track_index = sector_index = -1;
								byte_state = 0;
								continue;
							}

							if(it_index == rawTrack.mIndexTimes.end()) {
								if(g_verbosity >= 2)
									printf("Skipping track %d, sector %d after last index mark\n", track_index, sector_index);

								track_index = sector_index = -1;
								byte_state = 0;
								continue;
							}

							int vsn_offset = vsn_time - *--it_index;

							rot_start = it_index[0];
							rot_end = it_index[1];

							sector_position = (float)vsn_offset / (float)(it_index[1] - it_index[0]);

							if(sector_position >= 1.0f)
								sector_position -= 1.0f;

							if(g_verbosity >= 2)
								printf("track %d sector %d @ %d bits\n", track_index, sector_index, static_cast<int>(sector_position * 50000));

							++sector_headers;
							byte_state = 0;
						}
					} else if(byte_state >= 1000) {
						if(track_index == -1 || sector_index == -1) {
							byte_state = 0;
							continue;
						}
						int data_offset = byte_state - 1000;
						if(data_offset < payload_size)
							buf[data_offset] = shifter;

						if(++byte_state == 1000 + payload_size) {
							int vsn_time = time_basis - time_left;
							uint8_t chksum = 0;
							uint32_t invalid = 0;

							for(auto d : buf) {
								const uint8_t z0 = brother::gcr5decoder[d];
								if(z0 == 255)
									invalid++;
							}

							if(invalid)
								printf("%u invalid GCR bytes encountered\n", invalid);

							++data_sectors;

							auto& secs = g_disk.mTracks[track_index].mSectors;
							auto& sector = secs.emplace_back(SectorInfo());

							sector.mbMFM = false;
							sector.mAddressMark = 0;
							sector.mSectorSize = brother::bytes_per_sector;
							sector.mWeakOffset = -1;
							sector.mIndex = sector_index;
							sector.mRawStart = raw_start;
							sector.mRawEnd = vsn_time;
							sector.mPosition = sector_position;
							sector.mEndingPosition = (float)(vsn_time - rot_start) / (float)(rot_end - rot_start);
							sector.mEndingPosition -= floorf(sector.mEndingPosition);

							// Decode the sector data.
							//
							// Brother LW-30 sector data uses 5-to-8 encoding to encode 253 data bytes + 3 checksum bytes + 1 padding byte as 416 GCR bytes.

							for(int i = 0; i < 51; i++)
								brother::gcr_decode_8_to_5(&buf[i * 8], &sector.mData[i * 5]);

							std::array<uint8_t, 5> end_and_checksum;
							brother::gcr_decode_8_to_5(&buf[408], &end_and_checksum[0]);
							sector.mData[255] = end_and_checksum[0];

							auto checksum = brother::checksum_256_bytes(sector.mData);

							sector.mComputedCRC = ((uint32_t)checksum[0] << 16) | ((uint32_t)checksum[1] << 8) | (uint32_t)checksum[2];
							sector.mRecordedCRC = ((uint32_t)end_and_checksum[1] << 16) | ((uint32_t)end_and_checksum[2] << 8) | (uint32_t)end_and_checksum[3];

							bool checksumOK = memcmp(&checksum[0], &end_and_checksum[1], 3) == 0;

							if(!checksumOK && g_verbosity >= 1) {
								printf("warning: track %d, sector %d checksum mismatch! expected %06X, got %06X\n", track_index, sector_index, sector.mRecordedCRC, sector.mComputedCRC);
							}

							if(checksumOK)
								++good_sectors;

							byte_state = 0;
							track_index = sector_index = -1;
						}
					}
				}
			} else if(bit_state < 8) {
				++bit_state;

				if(bit_state == 8)
					bit_state = 0;
			}
		}
	}

done:

	if(g_verbosity > 0) {
		printf("%d sector headers decoded\n", sector_headers);
		printf("%d data sectors decoded\n", data_sectors);
		printf("%d good sectors decoded\n", good_sectors);
	}
}

void img_write(const char *path, DiskInfo& disk, int track) {
	printf("Writing IMG disk image: %s\n", path);

	FILE *fo = fopen(path, "wb");
	if(!fo)
		fatalf("Unable to open output file: %s.\n", path);

	// write tracks
	char secbuf[brother::bytes_per_sector];
	bool seenMissingWarning = false;
	uint32_t missingSectors = 0;
	uint32_t badSectors = 0;

	for(int i = 0; i < brother::tracks_per_disk; ++i) {
		TrackInfo& track_info = disk.mTracks[i];

		// sort and sift out usable sectors from track
		std::vector<SectorInfo *> secptrs;
		sift_sectors(track_info, i, secptrs);

		// iterate over sectors
		const SectorInfo *secptrs2[brother::sectors_per_track] = { nullptr };

		for(const auto& secptr : secptrs) {
			if(secptr->mIndex >= 0 && secptr->mIndex < (int)brother::sectors_per_track) {
				if(secptrs2[secptr->mIndex])
					printf("WARNING: Discarding duplicate physical sector for track %d, sector %d.\n", i, secptr->mIndex);
				else
					secptrs2[secptr->mIndex] = secptr;
			}
		};

		// write out sectors
		uint32_t missingSectorMask = 0;

		for(uint32_t j = 0; j < brother::sectors_per_track; ++j) {
			int physec = j;
			const SectorInfo *sec = secptrs2[physec];

			if(!sec) {
				if(track < 0 || track == i) {
					++missingSectors;
					missingSectorMask |= (1 << physec);
				}

				memset(secbuf, 0, sizeof secbuf);
			} else {
				memcpy(secbuf, sec->mData, brother::bytes_per_sector);

				if(sec->mSectorSize != brother::bytes_per_sector) {
					printf("WARNING: Variable sector size not supported by IMG format. Writing out truncated data for track %d, sector %d.\n", i, physec + 1);
					++badSectors;
				} else if(sec->mRecordedCRC != sec->mComputedCRC) {
					printf("WARNING: CRC error encoding not supported by IMG format. Ignoring CRC error for track %d, sector %d.\n", i, physec + 1);
					++badSectors;
				} else if(sec->mWeakOffset >= 0) {
					printf("WARNING: Weak sector encoding not supported by IMG format. Ignoring error for track %d, sector %d.\n", i, physec + 1);
					++badSectors;
				}
			}

			fwrite(secbuf, brother::bytes_per_sector, 1, fo);
		}

		if(missingSectorMask) {
			if(!seenMissingWarning) {
				seenMissingWarning = true;
				printf("WARNING: Missing sectors not supported by IMG format. Writing out null data.\n");
			}

			if(missingSectorMask == (1 << brother::sectors_per_track) - 1)
				printf("WARNING: No sectors found on track %u.\n", i);
			else {
				printf("WARNING: Track %u: missing sectors:", i);

				for(uint32_t j = 0; j < brother::sectors_per_track; ++j) {
					if(missingSectorMask & (1 << j))
						printf(" %u", j + 1);
				}

				printf("\n");
			}
		}
	}

	fclose(fo);

	printf("%d missing sector%s, %d sector%s with errors\n"
		, missingSectors, missingSectors == 1 ? "" : "s"
		, badSectors, badSectors == 1 ? "" : "s");
}

// Static CRC table
static constexpr uint32_t crc32_tab[256] = {
	0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3, 0x0EDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988, 0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
	0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7, 0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
	0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172,	0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,	0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940,	0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
	0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116,	0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,	0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924,	0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,

	0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A,	0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,	0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818,	0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
	0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E,	0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,	0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C,	0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
	0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2,	0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,	0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0,	0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
	0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086,	0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,	0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4,	0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,

	0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A,	0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,	0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8,	0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
	0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE,	0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,	0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC,	0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
	0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252,	0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,	0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60,	0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
	0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236,	0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,	0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04,	0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,

	0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A,	0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,	0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38,	0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
	0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E,	0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,	0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C,	0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
	0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2,	0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,	0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0,	0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
	0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6,	0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,	0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94,	0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D,
};

uint32_t crc32(const void* buf, size_t len, uint32_t crc = 0) {
	const char* b = (char*)buf;
	crc = ~crc;
	for(unsigned int i = 0; i < len; i++)
		crc = crc32_tab[(crc ^ *b++) & 0xff] ^ (crc >> 8);
	return ~crc;
}

namespace ipf {
	enum class MediaType : uint32_t { Unknown, Floppy_Disk };
	enum class EncoderType : uint32_t { Unknown, CAPS, SPS };
	enum class Platform : uint32_t {
		Unknown, Amiga, Atari_ST, PC, Amstrad_CPC, Spectrum, Sam_Coupe, Archimedes, C64, Atari_8bit
	};
	enum class Density : uint32_t {
		Unknown, Noise, Auto, Copylock_Amiga, Copylock_Amiga_New, Copylock_ST, Speedlock_Amiga,
		Speedlock_Amiga_Old, Adam_Brierley_Amiga, Adam_Brierley_Key_Amiga
	};
	enum class SignalType : uint32_t { Unknown, cell_2us };
	enum class BlockEncoderType : uint32_t { Unknown, MFM, RAW };
	enum class BlockFlags : uint32_t {
		None = 0x00,
		FwGap = 0x01,
		BwGap = 0x02,
		DataInBit = 0x04
	};
	enum class DataType : uint8_t { Unknown, Sync, Data, IGap, Raw, Fuzzy };
	enum class GapType : uint32_t { Unknown, Forward, Backward };
	enum class GapElemType { Unknown, GapLength, SampleLength };

	struct RecordHeader {
		uint32_t type = 0;
		uint32_t length = 0;
		uint32_t crc = 0;
	};

	struct CapsRecord : RecordHeader {
		CapsRecord() {
			type = 'CAPS';
			length = sizeof(*this);
		}
	};

	struct InfoRecord : RecordHeader {
		InfoRecord() {
			type = 'INFO';
			length = sizeof(*this);
			time_t t = time(nullptr);
			auto tm = *localtime(&t);
			creationDate = (tm.tm_year + 1900) * 10000 + (tm.tm_mon + 1) * 100 + tm.tm_mday;
			creationTime = tm.tm_hour * 10000000 + tm.tm_min * 100000 + tm.tm_sec * 1000;
		}
		MediaType mediaType = MediaType::Floppy_Disk;
		EncoderType encoderType = EncoderType::SPS;
		uint32_t encoderRev = 1;
		uint32_t fileKey = 0;
		uint32_t fileRev = 1;
		uint32_t origin = 0;
		uint32_t minTrack = 3;
		uint32_t maxTrack = 80;
		uint32_t minSide = 0;
		uint32_t maxSide = 0;
		uint32_t creationDate;
		uint32_t creationTime;
		Platform platforms[4]{ Platform::Unknown, Platform::Unknown, Platform::Unknown, Platform::Unknown };
		uint32_t diskNumber = 0;
		uint32_t creatorId = 0xBAFF00;
		uint32_t reserved[3]{};
	};

	struct ImageRecord : RecordHeader {
		ImageRecord() {
			type = 'IMGE';
			length = sizeof(*this);
		}
		uint32_t track = 0;
		uint32_t side = 0;
		Density density = Density::Auto;
		SignalType signalType = SignalType::cell_2us;
		uint32_t trackBytes = 0;
		uint32_t startBytePos = 0;
		uint32_t startBitPos = 0;
		uint32_t dataBits = 0;
		uint32_t gapBits = 0;
		uint32_t trackBits = 0;
		uint32_t blockCount = 0;
		uint32_t encoder = 0;
		uint32_t trackFlags = 0;
		uint32_t dataKey = 0;
		uint32_t reserved[3]{};
	};

	struct DataRecord : RecordHeader {
		DataRecord() {
			type = 'DATA';
			RecordHeader::length = sizeof(*this);
		}
		uint32_t length = 0;
		uint32_t bitSize = 0;
		uint32_t crc = 0;
		uint32_t key = 0;
	};

	struct BlockDescriptor {
		uint32_t dataBits = 0;
		uint32_t gapBits = 0;
		struct {
			uint32_t gapOffset = 0;
			SignalType cellType = SignalType::cell_2us;
		} SPS;
		BlockEncoderType encoderType = BlockEncoderType::RAW;
		BlockFlags blockFlags = BlockFlags::None;
		uint32_t gapDefaultValue = 0;
		uint32_t dataOffset = 0;
	};

	struct CteiRecord {
		uint32_t releaseCrc;
		uint32_t analyzerRev;
		uint32_t reserved[14];
	};
	
	struct CtexRecord {
		uint32_t track;
		uint32_t side;
		Density density;
		uint32_t formatId;
		uint32_t fix;
		uint32_t trackSize;
		uint32_t reserved[2];
	};

	void write(RecordHeader& header, FILE* fo) {
		auto length = header.length;
		header.crc = 0;
		auto* data = reinterpret_cast<uint32_t*>(&header);
		// byteswap before calculating CRC
		for(uint32_t i = 0; i < length / sizeof(uint32_t); i++) {
			data[i] = _byteswap_ulong(data[i]);
		}
		header.crc = _byteswap_ulong(crc32(&header, length, 0));
		fwrite(&header, 1, length, fo);
	}

	void write(DataRecord& header, BlockDescriptor* blocks, size_t count, void* stream, size_t streamLen, FILE* fo) {
		header.length = count * sizeof(BlockDescriptor) + streamLen;
		header.bitSize = header.length * 8;

		auto* data = reinterpret_cast<uint32_t*>(blocks);
		// byteswap before calculating CRC
		for(uint32_t i = 0; i < count * sizeof(BlockDescriptor) / sizeof(uint32_t); i++) {
			data[i] = _byteswap_ulong(data[i]);
		}

		header.DataRecord::crc = crc32(blocks, count * sizeof(BlockDescriptor));
		header.DataRecord::crc = crc32(stream, streamLen, header.crc);
		write(header, fo);
		fwrite(blocks, 1, count * sizeof(BlockDescriptor), fo);
		fwrite(stream, 1, streamLen, fo);
	}
}

void ipf_write(const char *path, DiskInfo& disk, int singletrack) {
	// currently only working from IMG input, not KryoFlux RAW!
	printf("Writing IPF disk image: %s\n", path);

	FILE *fo = fopen(path, "wb");
	if(!fo)
		fatalf("Unable to open output file: %s.\n", path);

	ipf::CapsRecord caps{};
	ipf::write(caps, fo);

	ipf::InfoRecord info{};
	ipf::write(info, fo);

	std::array<std::vector<uint8_t>, brother::tracks_per_disk> tracks;

	// format track: 0xaa (2), 0xaa (48), 12*sector
	// format sector: sector_prefix (8), track_sync (2), sector_sync (2), predata (19), payload=0xaa (414), postdata (13), 0xaa (42), should come out to ~4,000 bits
	// write sector: (after sector_sync, 0xdd) sector_header (2+14), payload (416), sector_footer (11)

	// 1 transition 4µs
	// 300 revolutions per minute => 0.2s per revolution
	// 50,000 transitions per revolution
	constexpr auto bitsPerTrack = 50000;

	// Tetris disk sector distances: 
	// 4,081 bits
	// 4,083 bits
	// 4,100 bits
	// 4,106 bits
	// 4,112 bits
	// 4,115 bits
	// 4,802 bits
	// 5,129 bits

	// create track data
	for(size_t track = 0; track < brother::tracks_per_disk; track++) {
		auto& track_info = disk.mTracks[track];
		auto& data = tracks[track];
		for(int x = 0; x < 2 + 48; x++)
			data.push_back(0xaa);
		auto interleave_offset = (track % 4) * 4;
		for(size_t s = interleave_offset; s < interleave_offset + brother::sectors_per_track; s++) {
			auto sector = brother::sector_interleave1[s % brother::sectors_per_track];
			for(const auto& d : brother::sector_prefix) // 16 bytes
				data.push_back(d);
			data.push_back(brother::sync_table[track] & 0xff);
			data.push_back(brother::sync_table[track] >> 8);
			data.push_back(brother::sync_table[sector] & 0xff);
			data.push_back(brother::sync_table[sector] >> 8);
			data.push_back(0xdd);
			for(const auto& d : brother::sector_header) // 16 bytes
				data.push_back(d);
			auto payload = brother::gcr_encode_and_checksum(track_info.mSectors[sector].mData); // 256 -> 416 bytes
			for(const auto& d : payload)
				data.push_back(d);
			for(const auto& d : brother::sector_footer) // 11 bytes
				data.push_back(d);
			for(int i = 0; i < 42; i++)
				data.push_back(0xaa);
		}
		//if(g_verbosity >= 2)
		//	printf("%zd bits per track\n", data.size() * 8);
	}

	// IMGE records
	for(int t = 0; t < brother::tracks_per_disk; ++t) {
		auto& track_info = disk.mTracks[t];
		auto& data = tracks[t];
		ipf::ImageRecord image{};
		image.track = 3 + t;
		image.side = 0;
		image.trackBytes = data.size();
		image.startBytePos = 0;
		image.startBitPos = 0;
		image.dataBits = image.trackBytes * 8;
		image.gapBits = bitsPerTrack - image.dataBits;
		image.trackBits = image.dataBits + image.gapBits;
		image.blockCount = 1;
 		image.dataKey = image.track;
		ipf::write(image, fo);
	}
	// DATA records
	for(int t = 0; t < brother::tracks_per_disk; ++t) {
		std::vector<uint8_t> stream;
		auto& track = tracks[t];
		stream.push_back((2 << 5) | (uint8_t)ipf::DataType::Raw); // 2 bytes for data length
		stream.push_back((track.size() >> 8) & 0xff);
		stream.push_back(track.size() & 0xff);
		for(auto d : track)
			stream.push_back(d);
		stream.push_back(0x00); // end of stream
		ipf::DataRecord data{};
		data.key = 3 + t;
		ipf::BlockDescriptor block;
		block.dataBits = track.size() * 8;
		block.gapBits = bitsPerTrack - block.dataBits;
		block.gapDefaultValue = 0xaa;
		block.dataOffset = 1 * sizeof(block);
		ipf::write(data, &block, 1, stream.data(), stream.size(), fo);
	}

	fclose(fo);

#ifdef CAPSLIB
	// validate
	auto image = CAPSAddImage(); if(image < 0) fatalf("error in CAPSAddImage\n");
	if(auto err = CAPSLockImage(image, (PCHAR)path); err != imgeOk) fatalf("error in CAPSLockImage\n");
	CapsImageInfo capsInfo;
	if(auto err = CAPSGetImageInfo(&capsInfo, image); err != imgeOk) fatalf("error in CAPSGetImageInfo\n");
	CapsTrackInfo trackInfo;
	for(auto t = capsInfo.mincylinder; t <= capsInfo.maxcylinder; t++)
		if(auto err = CAPSLockTrack(&trackInfo, image, t, 0, 0); err != imgeOk) fatalf("error in CAPSLockTrack %d\n", err);
	CAPSUnlockImage(image);
	CAPSRemImage(image);
#endif
}

#pragma pack(push, 1)
struct fat_entry {
	char filename[8];
	char extension[3];
	uint8_t attributes;
	uint8_t reserved;
	uint8_t created_time_refinement;
	uint32_t created_date_time;
	uint16_t last_access_date;
	uint16_t first_cluster_high;
	uint32_t modified_date_time;
	uint16_t first_cluster_low;
	uint32_t file_size;
};
#pragma pack(pop)

void fat_write(const char *path, DiskInfo& disk) {
	printf("Extracting FAT file format: %s\n", path);

	auto img = std::make_unique<uint8_t[]>(brother::bytes_per_sector * brother::sectors_per_track * brother::tracks_per_disk);

	// write tracks
	char secbuf[brother::bytes_per_sector];
	bool seenMissingWarning = false;
	uint32_t missingSectors = 0;
	uint32_t badSectors = 0;

	for(int track = 0; track < brother::tracks_per_disk; ++track) {
		TrackInfo& track_info = disk.mTracks[track];

		// sort and sift out usable sectors from track
		std::vector<SectorInfo *> secptrs;
		sift_sectors(track_info, track, secptrs);

		// iterate over sectors
		const SectorInfo *secptrs2[brother::sectors_per_track] = { nullptr };

		for(const auto& secptr : secptrs) {
			if(secptr->mIndex >= 0 && secptr->mIndex < (int)brother::sectors_per_track) {
				if(secptrs2[secptr->mIndex])
					printf("WARNING: Discarding duplicate physical sector for track %d, sector %d.\n", track, secptr->mIndex);
				else
					secptrs2[secptr->mIndex] = secptr;
			}
		};

		// write out sectors
		uint32_t missingSectorMask = 0;

		for(uint32_t sector = 0; sector < brother::sectors_per_track; ++sector) {
			int physec = sector;
			const SectorInfo *sec = secptrs2[physec];

			if(!sec) {
				++missingSectors;
				missingSectorMask |= (1 << physec);

				memset(secbuf, 0, sizeof secbuf);
			} else {
				memcpy(secbuf, sec->mData, brother::bytes_per_sector);

				if(sec->mSectorSize != brother::bytes_per_sector) {
					printf("WARNING: Variable sector size not supported by IMG format. Writing out truncated data for track %d, sector %d.\n", track, physec + 1);
					++badSectors;
				} else if(sec->mRecordedCRC != sec->mComputedCRC) {
					printf("WARNING: CRC error encoding not supported by IMG format. Ignoring CRC error for track %d, sector %d.\n", track, physec + 1);
					++badSectors;
				} else if(sec->mWeakOffset >= 0) {
					printf("WARNING: Weak sector encoding not supported by IMG format. Ignoring error for track %d, sector %d.\n", track, physec + 1);
					++badSectors;
				}
			}

			memcpy(img.get() + (track * brother::sectors_per_track + sector) * brother::bytes_per_sector, secbuf, brother::bytes_per_sector);
		}

		if(missingSectorMask) {
			if(!seenMissingWarning) {
				seenMissingWarning = true;
				printf("WARNING: Missing sectors not supported by IMG format. Writing out null data.\n");
			}

			if(missingSectorMask == (1 << brother::sectors_per_track) - 1)
				printf("WARNING: No sectors found on track %u.\n", track);
			else {
				printf("WARNING: Track %u: missing sectors:", track);

				for(uint32_t j = 0; j < brother::sectors_per_track; ++j) {
					if(missingSectorMask & (1 << j))
						printf(" %u", j + 1);
				}

				printf("\n");
			}
		}
	}

	printf("%d missing sector%s, %d sector%s with errors\n"
		, missingSectors, missingSectors == 1 ? "" : "s"
		, badSectors, badSectors == 1 ? "" : "s");

	// TODO: validation!
	auto fat = reinterpret_cast<fat_entry*>(img.get() + 0x420);
	for(int i = 0; i < 64; i++) {
		auto& entry = fat[i];
		if(entry.filename[0] == '\0')
			break;
		auto cluster = static_cast<uint32_t>(entry.first_cluster_low) | (static_cast<uint32_t>(entry.first_cluster_high) << 16);
		auto filename = std::string(entry.filename, sizeof(entry.filename)) + "." + std::string(entry.extension, sizeof(entry.extension));
		auto filesize = entry.file_size;
		filename.erase(std::remove_if(filename.begin(), filename.end(), [](auto& c) { return c == ' '; }), filename.end());
		printf("Extracting %s (%d bytes)\n", filename.c_str(), entry.file_size);
		auto fout = fopen((std::filesystem::path(path) / filename).string().c_str(), "wb");
		printf("  clusters: ");
		while(filesize != 0 && cluster != 0 && cluster < 0xff0) {
			printf("%d ", cluster);
			auto offset = 0x800 + cluster * 0x200;
			auto slice = std::min(filesize, 0x200u);
			fwrite(img.get() + offset, 1, slice, fout);
			filesize -= slice;
			// next cluster
			if(cluster & 1)
				cluster = ((img[0x100 + (cluster * 3) / 2    ] & 0xf0) >> 4) | (img[0x100 + (cluster * 3) / 2 + 1] << 4);
			else
				cluster = ((img[0x100 + (cluster * 3) / 2 + 1] & 0x0f) << 8) |  img[0x100 + (cluster * 3) / 2];
		}
		printf("\n");

		fclose(fout);
	}
}

///////////////////////////////////////////////////////////////////////////

void banner() {
	puts("brotherdisk - raw disk conversion utility for Brother LW-30");
	puts("Copyright (C) 2017 Bartman/Abyss");
	puts("based on");
	puts("A8 raw disk conversion utility v0.92");
	puts("Copyright (C) 2014-2016 Avery Lee, All Rights Reserved.");
	puts("Licensed under GNU General License, version 2 or later.");
	puts("");
}

void exit_usage() {
	puts("Usage: brotherdisk [options] input output");
	puts("");
	puts("Options:");
	puts("  -if   Set input format:");
	puts("          auto       Determine by input name extension");
	puts("          kryoflux   Read as KryoFlux raw stream (specify track00.0.raw)");
	puts("          img        Use IMG disk image format");
	puts("  -l    Show track/sector layout map");
	puts("  -of   Set output format:");
	puts("          auto       Determine by output name extension");
	puts("          img        Use IMG disk image format (for emulation)");
	puts("          ipf        Use IPF disk image format (for write to disk)");
	puts("          fat        Extract FAT file system");
	puts("  -p    Adjust clock period by percentage (50-200)");
	puts("          -p 50      Use 50% of normal period (2x clock)");
	puts("  -t    Restrict processing to single track");
	puts("          -t 4       Process only track 4");
	puts("  -v    Verbose output");
	puts("  -vv   Very verbose output: sector-level debugging");
	puts("  -vvv  Extremely verbose output: bit level debugging");
	puts("  -vvvv Ridiculously verbose output: flux level debugging");
	puts("");

	exit(1);
}

void exit_argerr() {
	puts("Use -h or -? for help.");
	exit(1);
}

void parse_args(int argc, char **argv) {
	bool allow_switches = true;

	banner();

	if (argc) {
		--argc;
		++argv;
	}

	if (!argc) {
		exit_usage();
	}

	bool autoDecoder = true;

	while(argc--) {
		const char *arg = *argv++;

		if (allow_switches && *arg == '-') {
			const char *sw = arg+1;

			if (!strcmp(sw, "-")) {
				allow_switches = false;
			} else if (!strcmp(sw, "h") || !strcmp(sw, "?")) {
				exit_usage();
			} else if (!strcmp(sw, "l")) {
				g_showLayout = true;
			} else if (!strcmp(sw, "if")) {
				if (!argc--) {
					printf("Missing argument for -if switch.\n");
					exit_argerr();
				}

				arg = *argv++;
				if (!strcmp(arg, "auto"))
					g_inputFormat = Format::Auto;
				else if (!strcmp(arg, "kryoflux"))
					g_inputFormat = Format::KryoFluxStream;
				else if(!strcmp(arg, "img"))
					g_inputFormat = Format::IMG;
				else if(!strcmp(arg, "fat"))
					g_inputFormat = Format::FAT;
				else {
					printf("Unsupported input format type: %s.\n", arg);
					exit_argerr();
				}
			} else if (!strcmp(sw, "of")) {
				if (!argc--) {
					printf("Missing argument for -of switch.\n");
					exit_argerr();
				}

				arg = *argv++;
				if (!strcmp(arg, "auto"))
					g_outputFormat = Format::Auto;
				else if (!strcmp(arg, "img"))
					g_outputFormat = Format::IMG;
				else if(!strcmp(arg, "ipf"))
					g_outputFormat = Format::IPF;
				else if(!strcmp(arg, "fat"))
					g_outputFormat = Format::FAT;
				else {
					printf("Unsupported output format type: %s.\n", arg);
					exit_argerr();
				}
			} else if (!strcmp(sw, "p")) {
				if (!argc--) {
					printf("Missing argument for -p switch.\n");
					exit_argerr();
				}

				arg = *argv++;

				char dummy;
				float period;
				if (1 != sscanf(arg, "%g%c", &period, &dummy) || !(period >= 50.0f && period <= 200.0f))
				{
					printf("Invalid period adjustment: %s\n", arg);
					exit_argerr();
				}

				g_clockPeriodAdjust = period / 100.0f;
			} else if (!strcmp(sw, "v")) {
				g_verbosity = 1;
			} else if (!strcmp(sw, "vv")) {
				g_verbosity = 2;
			} else if (!strcmp(sw, "vvv")) {
				g_verbosity = 3;
			} else if (!strcmp(sw, "vvvv")) {
				g_verbosity = 4;
			} else if (!strcmp(sw, "t")) {
				if (!argc--) {
					printf("Missing argument for -t switch.\n");
					exit_argerr();
				}

				arg = *argv++;

				char dummy;
				unsigned track;
				if (1 != sscanf(arg, "%u%c", &track, &dummy) || track > 79) {
					printf("Invalid track number: %s\n", arg);
					exit_argerr();
				}

				g_trackSelect = track;
			} else {
				printf("Unknown switch: %s\n", arg);
				exit_argerr();
			}
		} else {
			if (g_inputPath.empty()) {
				if (!*arg) {
					printf("Invalid input path.\n");
					exit_argerr();
				}

				g_inputPath = arg;
			} else if (g_outputPath.empty()) {
				if (!*arg) {
					printf("Invalid output path.\n");
					exit_argerr();
				}

				g_outputPath = arg;
			} else {
				printf("Extraneous argument: %s\n", arg);
				exit_argerr();
			}
		}
	}

	if (g_inputPath.empty()) {
		printf("Missing input path.\n");
		exit_argerr();
	}

	if (g_outputPath.empty()) {
		printf("Missing output path.\n");
		exit_argerr();
	}

	if (g_inputFormat == Format::Auto) {
		const char *extptr = strrchr(g_inputPath.c_str(), '.');

		if (extptr) {
			std::string ext(extptr+1);

			std::transform(ext.begin(), ext.end(), ext.begin(), [](char c) { return tolower((unsigned char)c); });

			if (ext == "raw")
				g_inputFormat = Format::KryoFluxStream;
			if(ext == "img")
				g_inputFormat = Format::IMG;
		}

		if (g_inputFormat == Format::Auto) {
			printf("Unable to determine input format from input path: %s. Use -if to override input format.\n", g_inputPath.c_str());
			exit_usage();
		}
	}

	if (g_inputFormat == Format::KryoFluxStream) {
		// attempt to identify track counter in KryoFlux stream filename
		const char *fn = g_inputPath.c_str();
		const char *s = strrchr(fn, '.');

		if (s) {
			while(s != fn && s[-1] >= '0' && s[-1] <= '9')
				--s;

			if (s != fn && s[-1] == '.') {
				--s;

				g_inputPathCountWidth = 0;
				while(s != fn && s[-1] == '0') {
					++g_inputPathCountWidth;
					--s;
				}

				if (s != fn && (s[-1] < '0' || s[-1] > '9'))
					g_inputPathCountPos = s - fn;
			}
		}

		if (!g_inputPathCountPos || !g_inputPathCountWidth || g_inputPathCountWidth > 10) {
			printf("Unable to determine filename pattern for KryoFlux raw track streams. Expected pattern: track00.0.raw.\n");
			exit_usage();
		}
	}

	if (g_outputFormat == Format::Auto) {
		const char *extptr = strrchr(g_outputPath.c_str(), '.');

		if (extptr) {
			std::string ext(extptr+1);

			std::transform(ext.begin(), ext.end(), ext.begin(), [](char c) { return tolower((unsigned char)c); });

			if(ext == "img")
				g_outputFormat = Format::IMG;
			if(ext == "ipf")
				g_outputFormat = Format::IPF;
		}

		if (g_outputFormat == Format::Auto) {
			printf("Unable to determine output format from output path: %s. Use -of to override output format.\n", g_outputPath.c_str());
			exit_usage();
		}
	}
}

void show_layout() {
	char trackbuf[73];

	DiskInfo tempDisk(g_disk);

	// write tracks
	for(int i=0; i<g_trackCount; ++i) {
		if (g_trackSelect >= 0 && g_trackSelect != i)
			continue;

		TrackInfo& track_info = tempDisk.mTracks[i];
		const int num_raw_secs = (int)track_info.mSectors.size();

		// sort sectors by angular position
		std::vector<SectorInfo *> secptrs(num_raw_secs);
		for(int j=0; j<num_raw_secs; ++j)
			secptrs[j] = &track_info.mSectors[j];

		sift_sectors(track_info, i, secptrs);

		memset(trackbuf, ' ', 68);
		memset(trackbuf+68, 0, 5);

		FOR_EACH(sec_ptr, secptrs) {
			int xpos = (unsigned)(sec_ptr->mPosition * 68);
			if (sec_ptr->mIndex >= 10)
				trackbuf[xpos++] = '0' + sec_ptr->mIndex / 10;

			trackbuf[xpos++] = '0' + sec_ptr->mIndex % 10;
		};

		printf("%2d (%2d) | %s\n", i, (int)secptrs.size(), trackbuf);
	}
}

void img_read(RawDisk& raw_disk, const char *path, int trackselect) {
	printf("Reading IMG disk image: %s\n", path);

	FILE *fo = fopen(path, "rb");
	if(!fo)
		fatalf("Unable to open input file: %s.\n", path);

	for(int track = 0; track < brother::tracks_per_disk; ++track) {
		if(trackselect >= 0 && trackselect != track) {
			fseek(fo, brother::bytes_per_sector * brother::sectors_per_track, SEEK_CUR);
			continue;
		}
		auto& secs = g_disk.mTracks[track].mSectors;
		for(int s = 0; s < brother::sectors_per_track; s++) {
			auto& sector = secs.emplace_back(SectorInfo());
			sector.mbMFM = false;
			sector.mAddressMark = 0;
			sector.mSectorSize = brother::bytes_per_sector;
			sector.mWeakOffset = -1;
			sector.mIndex = s;
			//sector.mRawStart = raw_start;
			//sector.mRawEnd = vsn_time;
			//sector.mPosition = sector_position;
			//sector.mEndingPosition = (float)(vsn_time - rot_start) / (float)(rot_end - rot_start);
			//sector.mEndingPosition -= floorf(sector.mEndingPosition);

			if(fread(sector.mData, 1, brother::bytes_per_sector, fo) != brother::bytes_per_sector)
				fatalf("Could not read %d bytes for track %d, sector %d.\n", brother::bytes_per_sector, track + 1, s + 1);

			auto checksum = brother::checksum_256_bytes(sector.mData);
			sector.mComputedCRC = sector.mRecordedCRC = ((uint32_t)checksum[0] << 16) | ((uint32_t)checksum[1] << 8) | (uint32_t)checksum[2];
		}
	}
}

/*
#pragma pack(push, 1)
struct fat_entry {
	char filename[8];
	char extension[3];
	uint8_t attributes;
	uint8_t reserved;
	uint8_t created_time_refinement;
	uint32_t created_date_time;
	uint16_t last_access_date;
	uint16_t first_cluster_high;
	uint32_t modified_date_time;
	uint16_t first_cluster_low;
	uint32_t file_size;
};
#pragma pack(pop)
*/

void fat_read(RawDisk& raw_disk, const char *path) {
	printf("Extracting FAT file format: %s\n", path);

	auto img = std::make_unique<uint8_t[]>(brother::bytes_per_sector * brother::sectors_per_track * brother::tracks_per_disk);
	static constexpr uint8_t sector00[]{
		0xEB, 0xFE, 0x90, // JMP instruction
		'B', 'D', 'O', 'S', 'V', '2', '5', '0', // OEM ID
		0x00, 0x01, // Bytes per sector
		0x02, // Sectors per cluster
		0x01, 0x00, // Reserved sectors
		0x01, // Number of FATs
		0x40, 0x00, // Root entries
		0xA8, 0x03, // Total sectors
		0x58, // Media descriptor
		0x03, 0x00, // Sectors per FAT
		0x0C, 0x00, // Sectors per track
		0x01, // Number of heads
		0x00, 0x00, 0x00, 0xC9, 0x00
	};
	static constexpr uint8_t sector01[]{ // FAT
		0x58, 0xFF, 0xFF
	};
	static constexpr uint8_t sector04[]{
		'W', 'P', '1', 'A', ' ', 'F', 'L', 'O', 'P', 'P', 'Y', 0x08
	};
	memcpy(&img[0 * brother::bytes_per_sector], sector00, sizeof(sector00));
	memcpy(&img[1 * brother::bytes_per_sector], sector01, sizeof(sector01));
	memcpy(&img[4 * brother::bytes_per_sector], sector04, sizeof(sector04));

	int32_t cluster = 2;
	int32_t next_entry = 0;
	auto fat = reinterpret_cast<fat_entry*>(img.get() + 0x420);
	for(auto& p : std::filesystem::directory_iterator(path)) {
		if(!p.is_regular_file())
			continue;
		auto filename = p.path().stem().string();
		auto extension = p.path().extension().string();
		if(!extension.empty()) extension = extension.substr(1); // remove leading '.'
		auto filesize = static_cast<size_t>(p.file_size());
		std::cout << p.path().filename().string() << '\n';
		auto data = std::make_unique<uint8_t[]>(filesize);
		auto file = fopen(p.path().string().c_str(), "rb");
		if(!file)
			fatalf("Unable to open input file: %s.\n", p.path().string().c_str());
		if(fread(data.get(), 1, filesize, file) != filesize)
			fatalf("Unable to read from input file: %s.\n", p.path().string().c_str());
		fclose(file);

		// fat entry
		auto& entry = fat[next_entry];
		if(filesize != 0) {
			entry.first_cluster_low = cluster & 0xffff;
			entry.first_cluster_high = cluster >> 16;
		} else {
			entry.first_cluster_low = entry.first_cluster_high = 0;
		}
		entry.attributes = 0x20; // archive
		entry.file_size = static_cast<uint32_t>(filesize);
		memset(&entry.filename, ' ', sizeof(entry.filename));
		memset(&entry.extension, ' ', sizeof(entry.extension));
		for(size_t i = 0; i < std::min(filename.length(), sizeof(entry.filename)); i++)
			entry.filename[i] = filename[i];
		for(size_t i = 0; i < std::min(extension.length(), sizeof(entry.extension)); i++)
			entry.extension[i] = extension[i];

		// file data
		if(0x800 + cluster * 0x200 + filesize >= brother::tracks_per_disk * brother::sectors_per_track * brother::bytes_per_sector)
			fatalf("out of disk space.\n");

		printf("  clusters: ");
		size_t fileoff = 0;
		while(filesize != 0) {
			printf("%d ", cluster);
			auto offset = 0x800 + cluster * 0x200;
			auto slice = std::min(filesize, 0x200u);
			memcpy(&img[0x800 + cluster * 0x200], data.get() + fileoff, slice);
			fileoff += slice;
			filesize -= slice;
			auto next_cluster = filesize ? (cluster + 1) : 0xfff;
			// next cluster
			if(cluster & 1) {
				img[0x100 + (cluster * 3) / 2]     |= (next_cluster & 0x0f) << 4;
				img[0x100 + (cluster * 3) / 2 + 1]  = (next_cluster >> 4) & 0xff;
			} else {
				img[0x100 + (cluster * 3) / 2]     |=  next_cluster & 0xff;
				img[0x100 + (cluster * 3) / 2 + 1]  = (next_cluster >> 8) & 0x0f;
			}
			cluster++;
		}
		printf("\n");
		next_entry++;
	}

	for(int track = 0; track < brother::tracks_per_disk; ++track) {
		auto& secs = g_disk.mTracks[track].mSectors;
		for(int s = 0; s < brother::sectors_per_track; s++) {
			auto& sector = secs.emplace_back(SectorInfo());
			sector.mbMFM = false;
			sector.mAddressMark = 0;
			sector.mSectorSize = brother::bytes_per_sector;
			sector.mWeakOffset = -1;
			sector.mIndex = s;

			memcpy(sector.mData, &img[(track * brother::sectors_per_track + s) * brother::bytes_per_sector], sector.mSectorSize);
			auto checksum = brother::checksum_256_bytes(sector.mData);
			sector.mComputedCRC = sector.mRecordedCRC = ((uint32_t)checksum[0] << 16) | ((uint32_t)checksum[1] << 8) | (uint32_t)checksum[2];
		}
	}
}

int main(int argc, char **argv) {
	parse_args(argc, argv);

	bool src_raw = false;
	bool dst_raw = false;

	RawDisk raw_disk;

	switch(g_inputFormat) {
	case Format::KryoFluxStream:
		kf_read(raw_disk, g_trackCount, 1, g_inputPath.c_str(), g_inputPathCountPos, g_inputPathCountWidth, g_trackSelect);
		src_raw = true;
		break;
	case Format::IMG:
		img_read(raw_disk, g_inputPath.c_str(), g_trackSelect);
		break;
	case Format::FAT:
		fat_read(raw_disk, g_inputPath.c_str());
		break;
	default:
		ASSERT(!"unsupported input format\n");
		break;
	}

	// Check if we are going from raw or decoded source.
	if (src_raw) {
		// Raw -- if the destination is decoded then we need
		// to decode tracks. If the destination is raw but requires splice points,
		// then we may need to decode the tracks if we didn't already have splice
		// points.
		if (!dst_raw) {
			for(int i=0; i<g_trackCount; ++i) {
				if (g_trackSelect >= 0 && g_trackSelect != i)
					continue;

				RawTrack& raw_track = raw_disk.mTracks[i];

				// if we just need splice points and we already have them, skip this
				// track
				if (dst_raw && raw_track.mSpliceStart >= 0)
					continue;

				process_track_brother(raw_track);
			}
		}
	} else if (!src_raw) {
		// Decoded -- if the destination is raw, then we need to encode tracks.
		//if (dst_raw)
		//	encode_disk(raw_disk, g_disk, g_clockPeriodAdjust, g_trackSelect);
	}

	if (g_showLayout && (!src_raw || !dst_raw))
		show_layout();

	switch(g_outputFormat) {
	case Format::IMG:
		img_write(g_outputPath.c_str(), g_disk, g_trackSelect);
		break;
	case Format::IPF:
		ipf_write(g_outputPath.c_str(), g_disk, g_trackSelect);
		break;
	case Format::FAT:
		fat_write(g_outputPath.c_str(), g_disk);
		break;
	default:
		ASSERT(!"unsupported output format\n");
		break;
	}

	return 0;
}
