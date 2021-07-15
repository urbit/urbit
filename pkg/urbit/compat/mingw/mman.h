#ifndef _SYS_MMAN_H
#define _SYS_MMAN_H

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
int munmap(void *addr, size_t length);
int msync(void *addr, size_t length, int flags);
int mprotect(void *addr, size_t len, int prot);

#define PROT_NONE       0x00   /* No access.  */
#define PROT_READ       0x01   /* Pages can be read.  */
#define PROT_WRITE      0x02   /* Pages can be written.  */
#define PROT_EXEC       0x04   /* Pages can be executed.  */

#define MAP_FILE        0x0001 /* Mapped from a file or device.  */
#define MAP_ANON        0x0002 /* Allocated from anonymous virtual memory.  */
#define MAP_TYPE        0x000f /* Mask for type field.  */
#define MAP_SHARED      0x0010 /* Share changes.  */
#define MAP_PRIVATE     0x0000 /* Changes private; copy pages on write.  */
#define MAP_FIXED       0x0100 /* Map address must be exactly as requested. */
#define MAP_FAILED      ((void *) -1)

#define MS_ASYNC        1      /* Sync memory asynchronously.  */
#define MS_SYNC         0      /* Synchronous memory sync.  */
#define MS_INVALIDATE   2      /* Invalidate the caches.  */

#endif//_SYS_MMAN_H
