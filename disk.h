#ifndef f_DISK_H
#define f_DISK_H

struct RawTrack {
	int mTrack;
	float mSamplesPerRev;

	int32_t mSpliceStart;
	int32_t mSpliceEnd;

	std::vector<uint32_t> mTransitions;
	std::vector<uint32_t> mIndexTimes;
};

struct RawDisk {
	RawTrack mTracks[81];
};

struct SectorInfo {
	uint32_t mRawStart;
	uint32_t mRawEnd;
	float mPosition;
	float mEndingPosition;
	int mIndex;
	int mWeakOffset;
	uint32_t mSectorSize;
	bool mbMFM;
	uint8_t mAddressMark;
	uint32_t mRecordedCRC;
	uint32_t mComputedCRC;
	uint8_t mData[1024];

	uint32_t ComputeContentHash() const;
	bool HasSameContents(const SectorInfo& other) const;
};

struct TrackInfo {
	std::vector<SectorInfo> mSectors;
	std::vector<uint8_t> mGCRData;
};

struct DiskInfo {
	TrackInfo mTracks[81];
};

void sift_sectors(TrackInfo& track_info, int track_num, std::vector<SectorInfo *>& secptrs);

#endif
