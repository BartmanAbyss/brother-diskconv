#ifndef f_DISKIO_H
#define f_DISKIO_H

void kf_read(RawDisk& raw_disk, int trackcount, int trackstep, const char *basepath, int countpos, int countwidth, int trackselect);

#endif
