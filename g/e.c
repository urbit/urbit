/* g/m.c
**
** This file is in the public domain.
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <ctype.h>

#include "all.h"

/* u3_ce_fault(): handle a memory event with libsigsegv protocol.
*/
c3_i
u3_ce_fault(void* adr_v, c3_i ser_i)
{
  if ( ser_i ) {
    c3_w*    adr_w = (c3_w*) adr_v;

    if ( (adr_w < u3_Loom) || (adr_w > (u3_Loom + u3_cc_pages)) ) {
      fprintf(stderr, "address %p out of loom!\r\n", adr_v);
      return 0;
    }
    else { 
      c3_w off_w = (adr_w - u3_Loom);
      c3_w pag_w = off_w >> u3_cc_page;
      c3_w blk_w = (pag_w >> 5);
      c3_w bit_w = (pag_w & 31);

      c3_assert(0 == (u3P.dit_w[blk_w] & bit_w));
      u3P.dit_w[blk_w] |= (1 << bit_w);
    
      if ( -1 == mprotect((void *)(u3_Loom + (pag_w << u3_cc_page)),
                          (1 << (u3_cc_page + 2)),
                          (PROT_READ | PROT_WRITE)) )
      {
        perror("mprotect");
        return 0;
      }
    }
  }
  return 1;
}

/* _ce_image_open(): open or create image.  yes if it already exists.
*/
static c3_o
_ce_image_open(u3_cs_image* img_u)
{
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s.bin", u3P.cpu_c, img_u->nam_c);
  if ( -1 != (img_u->fid_i = open(ful_c, O_RDWR)) ) {
    struct stat buf_u;

    if ( -1 == fstat(img_u->fid_i, &buf_u) ) {
      perror(ful_c);
      c3_assert(0);
      return u3_no;
    }
    else { 
      c3_d siz_d = buf_u.st_size;
      c3_d pgs_d = (siz_d + (c3_d)((1 << (u3_cc_page + 2)) - 1)) >> 
                   (c3_d)(u3_cc_page + 2);
 
      if ( siz_d != (pgs_d << (c3_d)(u3_cc_page + 2)) ) {
        fprintf(stderr, "%s: corrupt size %llx\r\n", ful_c, siz_d);
        c3_assert(0);
        return u3_no;
      }
      img_u->pgs_w = (c3_w) pgs_d;
      c3_assert(pgs_d == (c3_d)img_u->pgs_w);

      return u3_yes;
    }
  }
  else {
    if ( -1 == (img_u->fid_i = open(ful_c, O_RDWR | O_CREAT)) ) {
      perror(ful_c);
      c3_assert(0);
    }
    else {
      img_u->pgs_w = 0;
      return u3_no;
    }
  }
}

/* _ce_patch_write_control(): write control block file.
*/
static void
_ce_patch_write_control(u3_cs_patch* pat_u)
{
  c3_w len_w = sizeof(u3_cs_control) + 
               (pat_u->con_u->pgs_w * sizeof(u3_cs_line));

  if ( len_w != write(pat_u->ctl_i, pat_u->con_u, len_w) ) {
    c3_assert(0);
  }
}

/* _ce_patch_read_control(): read control block file.
*/
static c3_o
_ce_patch_read_control(u3_cs_patch* pat_u)
{
  c3_w len_w;

  c3_assert(0 == pat_u->con_u);
  {
    struct stat buf_u;

    if ( -1 == fstat(pat_u->ctl_i, &buf_u) ) {
      c3_assert(0);
      return u3_no;
    }
    len_w = (c3_w) buf_u.st_size;
  }

  pat_u->con_u = malloc(len_w);
  if ( (len_w != read(pat_u->ctl_i, pat_u->con_u, len_w)) ||
        (len_w != sizeof(u3_cs_control) +
                  (pat_u->con_u->pgs_w * sizeof(u3_cs_line))) )
  {
    free(pat_u->con_u);
    pat_u->con_u = 0;
    return u3_no;
  }
  return u3_yes;
}

/* _ce_patch_create(): create patch files.
*/
static void
_ce_patch_create(u3_cs_patch* pat_u)
{
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/control.bin", u3P.cpu_c);
  if ( -1 == (pat_u->ctl_i = open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0666)) ) {
    c3_assert(0);
  }

  snprintf(ful_c, 8192, "%s/.urb/memory.bin", u3P.cpu_c);
  if ( -1 == (pat_u->mem_i = open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0666)) ) {
    c3_assert(0);
  }
}

/* _ce_patch_delete(): delete a patch.
*/
static void
_ce_patch_delete(void)
{
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s/.urb/control.bin", u3P.cpu_c);
  unlink(ful_c);

  snprintf(ful_c, 8192, "%s/.urb/memory.bin", u3P.cpu_c);
  unlink(ful_c);
}

/* _ce_patch_verify(): check patch data mug.
*/
static c3_o
_ce_patch_verify(u3_cs_patch* pat_u)
{
  c3_w i_w;

  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w mug_w = pat_u->con_u->mem_u[i_w].mug_w;
    c3_w mem_w[u3_cc_page];

    if ( -1 == lseek(pat_u->mem_i, (i_w << (u3_cc_page + 2)), SEEK_SET) ) {
      c3_assert(0);
      return u3_no;
    }
    if ( -1 == read(pat_u->mem_i, mem_w, (1 << (u3_cc_page + 2))) ) {
      c3_assert(0);
      return u3_no;
    }
    {
      c3_w nug_w = u3_cr_mug_words(mem_w, u3_cc_page);

      if ( mug_w != nug_w ) {
        printf("_ce_patch_verify: mug mismatch (%x, %x)\r\n", mug_w, nug_w);
        return u3_no;
      }
    }
  }
  return u3_yes;
}

/* _ce_patch_free(): free a patch.
*/
static void
_ce_patch_free(u3_cs_patch* pat_u)
{
  free(pat_u->con_u);
  close(pat_u->ctl_i);
  close(pat_u->mem_i);
  free(pat_u);
}

/* _ce_patch_open(): open patch, if any.
*/
static u3_cs_patch*
_ce_patch_open(void)
{
  u3_cs_patch* pat_u;
  c3_c ful_c[8193];
  c3_i ctl_i, mem_i;

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/control.bin", u3P.cpu_c);
  if ( -1 == (ctl_i = open(ful_c, O_RDWR)) ) {
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/memory.bin", u3P.cpu_c);
  if ( -1 == (mem_i = open(ful_c, O_RDWR)) ) {
    close(ctl_i);

    _ce_patch_delete();
    return 0;
  }
  pat_u = malloc(sizeof(u3_cs_patch));
  pat_u->ctl_i = ctl_i;
  pat_u->mem_i = mem_i;
  pat_u->con_u = 0;

  if ( u3_no == _ce_patch_read_control(pat_u) ) {
    close(pat_u->ctl_i);
    close(pat_u->mem_i);
    free(pat_u);

    _ce_patch_delete();
    return 0;
  }
  if ( u3_no == _ce_patch_verify(pat_u) ) {
    _ce_patch_free(pat_u);
    _ce_patch_delete();
    return 0;
  }
  return pat_u;
}

/* _ce_patch_write_page(): write a page of patch memory.
*/
static void
_ce_patch_write_page(u3_cs_patch* pat_u, 
                     c3_w         pgc_w,
                     c3_w*        mem_w)
{
  if ( -1 == lseek(pat_u->mem_i, (pgc_w << (u3_cc_page + 2)), SEEK_SET) ) {
    c3_assert(0);
  }
  if ( (1 << (u3_cc_page + 2)) != 
       write(pat_u->mem_i, mem_w, (1 << (u3_cc_page + 2))) )
  {
    c3_assert(0);
  }
}

/* _ce_patch_count_page(): count a page, producing new counter.
*/
static c3_w
_ce_patch_count_page(c3_w pag_w,
                     c3_w pgc_w)
{
  c3_w blk_w = (pag_w >> 5);
  c3_w bit_w = (pag_w & 31);

  if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
    pgc_w += 1;
  }
  return pgc_w;
}

/* _ce_patch_save_page(): save a page, producing new page counter.
*/
static c3_w
_ce_patch_save_page(u3_cs_patch* pat_u,
                    c3_w         pag_w,
                    c3_w         pgc_w)
{
  c3_w blk_w = (pag_w >> 5);
  c3_w bit_w = (pag_w & 31);

  if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
    c3_w* mem_w = u3_Loom + (pag_w << u3_cc_page);

    pat_u->con_u->mem_u[pgc_w].pag_w = pag_w;
    pat_u->con_u->mem_u[pgc_w].mug_w = u3_cr_mug_words(mem_w, u3_cc_page);

    _ce_patch_write_page(pat_u, pgc_w, mem_w);

    if ( -1 == mprotect(u3_Loom + (pag_w << u3_cc_page),
                        (1 << (u3_cc_page + 2)),
                        PROT_READ) ) 
    {
      c3_assert(0);
    }
                    
    u3P.dit_w[blk_w] &= ~(1 << bit_w); 
    pgc_w += 1;
  }
  return pgc_w;
}

/* _ce_patch_junk_page(): mark a page as junk.
*/
static void
_ce_patch_junk_page(u3_cs_patch* pat_u,
                    c3_w         pag_w)
{
  if ( -1 == mprotect(u3_Loom + (pag_w << u3_cc_page),
                      (1 << (u3_cc_page + 2)),
                      PROT_READ) ) 
  {
    c3_assert(0);
  }
}

/* _ce_patch_compose(): make and write current patch.
*/
static u3_cs_patch*
_ce_patch_compose(void)
{
  c3_w pgs_w = 0;
  c3_w nor_w = 0;
  c3_w sou_w = 0;

  /* Calculate number of saved pages, north and south.
  */
  {
    c3_w nwr_w, swu_w;

    u3_cm_water(&nwr_w, &swu_w);

    nor_w = (nwr_w + ((1 << u3_cc_page) - 1)) >> u3_cc_page;
    sou_w = (swu_w + ((1 << u3_cc_page) - 1)) >> u3_cc_page;
  }

  /* Count dirty pages.
  */
  {
    c3_w i_w;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgs_w = _ce_patch_count_page(i_w, pgs_w);
    }
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgs_w = _ce_patch_count_page((u3_cc_pages - (i_w + 1)), pgs_w);
    }
  }

  if ( !pgs_w ) {
    return 0;
  }
  else {
    u3_cs_patch* pat_u = malloc(sizeof(u3_cs_patch));
    c3_w i_w, pgc_w;

    _ce_patch_create(pat_u);
    pat_u->con_u = malloc(sizeof(u3_cs_control) + (pgs_w + sizeof(u3_cs_line)));
    pgc_w = 0;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, i_w, pgc_w);
    } 
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, (u3_cc_pages - (i_w + 1)), pgs_w);
    }
    for ( i_w = nor_w; i_w < (u3_cc_pages - sou_w); i_w++ ) {
      _ce_patch_junk_page(pat_u, i_w);
    }
  
    _ce_patch_write_control(pat_u);
    return pat_u;
  }
}

/* _ce_sync(): sync a file descriptor.
*/
static void
_ce_sync(c3_i fid_i)
{
#if defined(U2_OS_linux)
  fdatasync(fid_i);
#elif defined(U2_OS_osx)
  fcntl(fid_i, F_FULLFSYNC);
#elif defined(U2_OS_bsd)
  fsync(fid_i);
#else
# error "port: datasync"
#endif
}

/* _ce_patch_sync(): make sure patch is synced to disk.
*/
static void
_ce_patch_sync(u3_cs_patch* pat_u)
{
  _ce_sync(pat_u->ctl_i);
  _ce_sync(pat_u->mem_i);
}

/* _ce_image_sync(): make sure image is synced to disk.
*/
static void
_ce_image_sync(u3_cs_image* img_u)
{
  _ce_sync(img_u->fid_i);
}

/* _ce_patch_apply(): apply patch to image.
*/
static void
_ce_patch_apply(u3_cs_patch* pat_u)
{
  c3_w i_w;

  u3P.nor_u.pgs_w = pat_u->con_u->nor_w;
  ftruncate(u3P.nor_u.fid_i, u3P.nor_u.pgs_w << (u3_cc_page + 2));
  
  u3P.sou_u.pgs_w = pat_u->con_u->sou_w;
  ftruncate(u3P.sou_u.fid_i, u3P.sou_u.pgs_w << (u3_cc_page + 2));
 
  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mem_w[u3_cc_page];
    c3_i fid_i;
    c3_w off_w;

    if ( pag_w < pat_u->con_u->nor_w ) {
      fid_i = u3P.nor_u.fid_i;
      off_w = pag_w;
    } 
    else {
      fid_i = u3P.sou_u.fid_i;
      off_w = (u3_cc_pages - (pag_w + 1));
    }
 
    if ( -1 == read(pat_u->mem_i, mem_w, (1 << (u3_cc_page + 2))) ) {
      c3_assert(0);
    }
    else {
      if ( -1 == write(fid_i, mem_w, (1 << (u3_cc_page + 2))) ) {
        c3_assert(0);
      }
    }
  }
}

/* _ce_image_blit(): apply image to memory.
*/
static void
_ce_image_blit(u3_cs_image* img_u,
               c3_w*        ptr_w,
               c3_ws        stp_ws)
{
  c3_w i_w;

  lseek(img_u->fid_i, 0, SEEK_SET);
  for ( i_w=0; i_w < img_u->pgs_w; i_w++ ) {
    if ( -1 == read(img_u->fid_i, ptr_w, (1 << (u3_cc_page + 2))) ) {
      c3_assert(0);
    }
    ptr_w += stp_ws;
  }
}

/* u3_ce_save(): save current changes.
*/
void
u3_ce_save(void)
{
  u3_cs_patch* pat_u;

  //  Write all dirty pages to disk; clear protection and dirty bits.
  //
  //  This has to block the main thread.  All further processing can happen
  //  in a separate thread, though we can't save again till this completes.
  //
  printf("_ce_patch_compose\r\n");
  pat_u = _ce_patch_compose();

  //  Sync the patch files.
  //
  printf("_ce_patch_sync\r\n");
  _ce_patch_sync(pat_u);

  //  Copy the patch files into the image file.
  //
  printf("_ce_patch_apply\r\n");
  _ce_patch_apply(pat_u);

  //  Sync the image file.
  //
  printf("_ce_image_sync\r\n");
  _ce_image_sync(&u3P.nor_u);
  _ce_image_sync(&u3P.sou_u);

  //  Delete the patchfile and free it.
  //
  printf("_ce_patch_delete\r\n");
  _ce_patch_delete();
  printf("_ce_patch_free\r\n");
  _ce_patch_free(pat_u);
}

/* _ce_limits(): set up file and stack limits.
*/
static void
_ce_limits(void)
{
  struct rlimit rlm;
  c3_i          ret_i;

#define LOOM_STACK (65536 << 10)
  ret_i = getrlimit(RLIMIT_STACK, &rlm);
  c3_assert(0 == ret_i);
  rlm.rlim_cur = rlm.rlim_max > LOOM_STACK ? LOOM_STACK : rlm.rlim_max;
  if ( 0 != setrlimit(RLIMIT_STACK, &rlm) ) {
    perror("stack");
    exit(1);
  }
#undef LOOM_STACK

  ret_i = getrlimit(RLIMIT_NOFILE, &rlm);
  c3_assert(0 == ret_i);
  rlm.rlim_cur = 4096;
  if ( 0 != setrlimit(RLIMIT_NOFILE, &rlm) ) {
    perror("file limit");
    //  no exit, not a critical limit
  }

  getrlimit(RLIMIT_CORE, &rlm);
  rlm.rlim_cur = RLIM_INFINITY;
  if ( 0 != setrlimit(RLIMIT_CORE, &rlm) ) {
    perror("core limit");
    //  no exit, not a critical limit
  }
}

#if 0
/* _ce_signals(): set up interrupts, etc.
*/
static void
_ce_signals(void)
{
  if ( 0 != sigsegv_install_handler(u3_ce_fault) ) {
    fprintf(stderr, "sigsegv install failed\n");
    exit(1);
  }
  signal(SIGINT, _loom_stop);
}
#endif

/* u3_ce_boot(): start the memory system.
*/
void 
u3_ce_boot(c3_c* cpu_c)
{
  _ce_limits();

  /* Map at fixed address.
  */
  u3_Loom = (void *)U2_OS_LoomBase;
  {
    c3_w len_w = (1 << (u3_cc_bits + 2));
    void* map_v;

    map_v = mmap((void *)u3_Loom,
                 len_w,
                 // PROT_READ,
                 PROT_READ | PROT_WRITE,
                 (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
                 -1, 0);

    if ( -1 == (c3_ps)map_v ) {
      map_v = mmap((void *)0,
                   len_w,
                   PROT_READ | PROT_WRITE,
                   MAP_ANON | MAP_PRIVATE,
                   -1, 0);

      if ( -1 == (c3_ps)map_v ) {
        fprintf(stderr, "map failed twice\n");
      } else {
        fprintf(stderr, "map failed - try U2_OS_LoomBase %p\n", map_v);
      }
      exit(1);
    }
    printf("loom: mapped %dMB\n", len_w >> 20);
  }

  /* Open and apply any patches.
  */
  {
    u3_cs_patch* pat_u;

    if ( 0 != (pat_u = _ce_patch_open()) ) {
      printf("_ce_patch_apply\r\n");
      _ce_patch_apply(pat_u);

      printf("_ce_image_sync\r\n");
      _ce_image_sync(&u3P.nor_u);
      _ce_image_sync(&u3P.sou_u);

      printf("_ce_patch_delete\r\n");
      _ce_patch_delete();
      printf("_ce_patch_free\r\n");
      _ce_patch_free(pat_u);
    }
  }

  /* Open and load, or create, image files.
  */
  {
    u3P.cpu_c = cpu_c;
    u3P.nor_u.nam_c = "north";
    u3P.sou_u.nam_c = "south";

    if ( u3_yes == _ce_image_open(&u3P.nor_u) ) {
      _ce_image_blit(&u3P.nor_u, u3_Loom, (1 << u3_cc_page));
    }
    if ( u3_yes == _ce_image_open(&u3P.sou_u) ) {
      _ce_image_blit(&u3P.sou_u, 
                     (u3_Loom + (1 << u3_cc_bits) - (1 << u3_cc_page)),
                     -(1 << u3_cc_page));
    }
  }
}
