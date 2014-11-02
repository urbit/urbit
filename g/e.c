/* g/e.c
**
** This file is in the public domain.
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sigsegv.h>
#include <pmmintrin.h>
#include <xmmintrin.h>

#include "all.h"

#if 0
/* Image check.
*/
struct {
  c3_w nor_w;
  c3_w sou_w;
  c3_w mug_w[u3_cc_pages];
} u3K;

/* _ce_check_page(): checksum page.
*/
static c3_w
_ce_check_page(c3_w pag_w)
{
  c3_w* mem_w = u3_Loom + (pag_w << u3_cc_page);
  c3_w  mug_w = u3_cr_mug_words(mem_w, (1 << u3_cc_page));

  return mug_w;
}

/* u3_ce_check(): compute a checksum on all memory within the watermarks.
*/
void
u3_ce_check(c3_c* cap_c)
{
  c3_w nor_w = 0;
  c3_w sou_w = 0;

  {
    c3_w nwr_w, swu_w;

    u3_cm_water(&nwr_w, &swu_w);

    nor_w = (nwr_w + ((1 << u3_cc_page) - 1)) >> u3_cc_page;
    sou_w = (swu_w + ((1 << u3_cc_page) - 1)) >> u3_cc_page;
  }

  /* Count dirty pages.
  */
  {
    c3_w i_w, sum_w, mug_w;

    sum_w = 0;
    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      mug_w = _ce_check_page(i_w);
      if ( strcmp(cap_c, "boot") ) {
        c3_assert(mug_w == u3K.mug_w[i_w]);
      }
      sum_w += mug_w;
    }
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      mug_w = _ce_check_page((u3_cc_pages - (i_w + 1)));
      if ( strcmp(cap_c, "boot") ) {
        c3_assert(mug_w == u3K.mug_w[(u3_cc_pages - (i_w + 1))]);
      }
      sum_w += mug_w;
    }
    printf("%s: sum %x (%x, %x)\r\n", cap_c, sum_w, nor_w, sou_w);
  }
}
#endif

/* u3_ce_fault(): handle a memory event with libsigsegv protocol.
*/
c3_i
u3_ce_fault(void* adr_v, c3_i ser_i)
{
  if ( ser_i ) {
    c3_w* adr_w = (c3_w*) adr_v;

    if ( (adr_w < u3_Loom) || (adr_w > (u3_Loom + u3_cc_words)) ) {
      fprintf(stderr, "address %p out of loom!\r\n", adr_v);
      c3_assert(0);
      return 0;
    }
    else { 
      c3_w off_w = (adr_w - u3_Loom);
      c3_w pag_w = off_w >> u3_cc_page;
      c3_w blk_w = (pag_w >> 5);
      c3_w bit_w = (pag_w & 31);

      // printf("dirty page %d\r\n", pag_w);
      c3_assert(0 == (u3P.dit_w[blk_w] & (1 << bit_w)));
      u3P.dit_w[blk_w] |= (1 << bit_w);
    
      if ( -1 == mprotect((void *)(u3_Loom + (pag_w << u3_cc_page)),
                          (1 << (u3_cc_page + 2)),
                          (PROT_READ | PROT_WRITE)) )
      {
        perror("mprotect");
        c3_assert(0);
        return 0;
      }
    }
  }
  return 1;
}

/* _ce_image_open(): open or create image.
*/
static c3_o
_ce_image_open(u3_cs_image* img_u, c3_o nuu_o)
{
  c3_i mod_i = u3_so(nuu_o) ? (O_RDWR | O_CREAT) : O_RDWR;
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s.bin", u3P.cpu_c, img_u->nam_c);
  if ( -1 == (img_u->fid_i = open(ful_c, mod_i, 0666)) ) {
    perror(ful_c);
    return u3_no;
  }
  else {
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

      if ( u3_yes == nuu_o ) {
        if ( siz_d ) { 
          c3_assert(0);
          return u3_no;
        }
        return u3_yes;
      }
      else { 
        if ( siz_d != (pgs_d << (c3_d)(u3_cc_page + 2)) ) {
          fprintf(stderr, "%s: corrupt size %llx\r\n", ful_c, siz_d);
          return u3_no;
        }
        img_u->pgs_w = (c3_w) pgs_d;
        c3_assert(pgs_d == (c3_d)img_u->pgs_w);

        return u3_yes;
      }
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

  snprintf(ful_c, 8192, "%s/.urb/chk/control.bin", u3P.cpu_c);
  if ( -1 == (pat_u->ctl_i = open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0666)) ) {
    c3_assert(0);
  }

  snprintf(ful_c, 8192, "%s/.urb/chk/memory.bin", u3P.cpu_c);
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

  snprintf(ful_c, 8192, "%s/.urb/chk/control.bin", u3P.cpu_c);
  unlink(ful_c);

  snprintf(ful_c, 8192, "%s/.urb/chk/memory.bin", u3P.cpu_c);
  unlink(ful_c);
}

/* _ce_patch_verify(): check patch data mug.
*/
static c3_o
_ce_patch_verify(u3_cs_patch* pat_u)
{
  c3_w i_w;

  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mug_w = pat_u->con_u->mem_u[i_w].mug_w;
    c3_w mem_w[1 << u3_cc_page];

    if ( -1 == lseek(pat_u->mem_i, (i_w << (u3_cc_page + 2)), SEEK_SET) ) {
      perror("seek");
      c3_assert(0);
      return u3_no;
    }
    if ( -1 == read(pat_u->mem_i, mem_w, (1 << (u3_cc_page + 2))) ) {
      perror("read");
      c3_assert(0);
      return u3_no;
    }
    {
      c3_w nug_w = u3_cr_mug_words(mem_w, (1 << u3_cc_page));

      if ( mug_w != nug_w ) {
        printf("_ce_patch_verify: mug mismatch %d/%d; (%x, %x)\r\n", 
            pag_w, i_w, mug_w, nug_w);
        c3_assert(0);
        return u3_no;
      }
#if 0
      else {
        printf("verify: patch %d/%d, %x\r\n", pag_w, i_w, mug_w);
      }
#endif
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

  snprintf(ful_c, 8192, "%s/.urb/chk/control.bin", u3P.cpu_c);
  if ( -1 == (ctl_i = open(ful_c, O_RDWR)) ) {
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/chk/memory.bin", u3P.cpu_c);
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
    pat_u->con_u->mem_u[pgc_w].mug_w = u3_cr_mug_words(mem_w, 
                                                       (1 << u3_cc_page));

#if 0
    u3K.mug_w[pag_w] = pat_u->con_u->mem_u[pgc_w].mug_w;
      printf("save: page %d, mug %x\r\n", 
              pag_w, u3_cr_mug_words(mem_w, (1 << u3_cc_page)));
#endif
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
  c3_w blk_w = (pag_w >> 5);
  c3_w bit_w = (pag_w & 31);

  if ( -1 == mprotect(u3_Loom + (pag_w << u3_cc_page),
                      (1 << (u3_cc_page + 2)),
                      PROT_READ) ) 
  {
    c3_assert(0);
  }
  u3P.dit_w[blk_w] &= ~(1 << bit_w); 
}

/* u3_ce_dirty(): count dirty pages.
*/
c3_w
u3_ce_dirty(void)
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
  //  u3K.nor_w = nor_w;
  //  u3K.sou_w = sou_w;

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
  return pgs_w;
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
  //  u3K.nor_w = nor_w;
  //  u3K.sou_w = sou_w;

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
    fprintf(stderr, "no dirty pages\r\n");
    return 0;
  }
  else {
    u3_cs_patch* pat_u = malloc(sizeof(u3_cs_patch));
    c3_w i_w, pgc_w;

    _ce_patch_create(pat_u);
    pat_u->con_u = malloc(sizeof(u3_cs_control) + (pgs_w * sizeof(u3_cs_line)));
    pgc_w = 0;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, i_w, pgc_w);
    } 
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, (u3_cc_pages - (i_w + 1)), pgc_w);
    }
    for ( i_w = nor_w; i_w < (u3_cc_pages - sou_w); i_w++ ) {
      _ce_patch_junk_page(pat_u, i_w);
    }

    pat_u->con_u->nor_w = nor_w;
    pat_u->con_u->sou_w = sou_w;
    pat_u->con_u->pgs_w = pgc_w;
  
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

  //printf("image: nor_w %d, new %d\r\n", u3P.nor_u.pgs_w, pat_u->con_u->nor_w);
  //printf("image: sou_w %d, new %d\r\n", u3P.sou_u.pgs_w, pat_u->con_u->sou_w);

  if ( u3P.nor_u.pgs_w > pat_u->con_u->nor_w ) {
    ftruncate(u3P.nor_u.fid_i, u3P.nor_u.pgs_w << (u3_cc_page + 2));
  }
  u3P.nor_u.pgs_w = pat_u->con_u->nor_w;

  if ( u3P.sou_u.pgs_w > pat_u->con_u->sou_w ) {
    ftruncate(u3P.sou_u.fid_i, u3P.sou_u.pgs_w << (u3_cc_page + 2));
  }
  u3P.sou_u.pgs_w = pat_u->con_u->sou_w;
 
  if ( (-1 == lseek(pat_u->mem_i, 0, SEEK_SET)) ||
       (-1 == lseek(u3P.nor_u.fid_i, 0, SEEK_SET)) ||
       (-1 == lseek(u3P.sou_u.fid_i, 0, SEEK_SET)) )
  {
    perror("apply: seek");
    c3_assert(0);
  }

  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mem_w[1 << u3_cc_page];
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
      perror("apply: read");
      c3_assert(0);
    }
    else {
      if ( -1 == lseek(fid_i, (off_w << (u3_cc_page + 2)), SEEK_SET) ) {
        perror("apply: lseek");
        c3_assert(0);
      }
      if ( -1 == write(fid_i, mem_w, (1 << (u3_cc_page + 2))) ) {
        perror("apply: write");
        c3_assert(0);
      }
    }
#if 0
    printf("apply: %d, %x\n", pag_w, u3_cr_mug_words(mem_w, (1 << u3_cc_page)));
#endif
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
      perror("read");
      c3_assert(0);
    }
#if 0
    {
      c3_w off_w = (ptr_w - u3_Loom);
      c3_w pag_w = (off_w >> u3_cc_page);

      printf("blit: page %d, mug %x\r\n", pag_w, 
          u3_cr_mug_words(ptr_w, (1 << u3_cc_page)));
    }
#endif
    ptr_w += stp_ws;
  }
}

#if 0
/* _ce_image_fine(): compare image to memory.
*/
static void
_ce_image_fine(u3_cs_image* img_u,
               c3_w*        ptr_w,
               c3_ws        stp_ws)
{
  c3_w i_w;
  c3_w buf_w[1 << u3_cc_page];

  lseek(img_u->fid_i, 0, SEEK_SET);
  for ( i_w=0; i_w < img_u->pgs_w; i_w++ ) {
    c3_w mem_w, fil_w;

    if ( -1 == read(img_u->fid_i, buf_w, (1 << (u3_cc_page + 2))) ) {
      perror("read");
      c3_assert(0);
    }
    mem_w = u3_cr_mug_words(ptr_w, (1 << u3_cc_page));
    fil_w = u3_cr_mug_words(buf_w, (1 << u3_cc_page));

    if ( mem_w != fil_w ) {
      c3_w pag_w = (ptr_w - u3_Loom) >> u3_cc_page;

      fprintf(stderr, "mismatch: page %d, mem_w %x, fil_w %x, K %x\r\n", 
                       pag_w, 
                       mem_w, 
                       fil_w,
                       u3K.mug_w[pag_w]);
      abort();
    }
    ptr_w += stp_ws;
  }
}
#endif

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

  if ( !(pat_u = _ce_patch_compose()) ) {
    return;
  }
 
  //  Sync the patch files.
  //
  // u3_ca_print_memory("sync: save", 4096 * pat_u->con_u->pgs_w);
  _ce_patch_sync(pat_u);

  //  Verify the patch - because why not?
  //
  // printf("_ce_patch_verify\r\n");
  _ce_patch_verify(pat_u);

  //  Write the patch data into the image file.  Idempotent.
  //
  // printf("_ce_patch_apply\r\n");
  _ce_patch_apply(pat_u);

#if 0
  {
    _ce_image_fine(&u3P.nor_u, 
                   u3_Loom, 
                   (1 << u3_cc_page));

    _ce_image_fine(&u3P.sou_u, 
                   (u3_Loom + (1 << u3_cc_bits) - (1 << u3_cc_page)),
                   -(1 << u3_cc_page));

    c3_assert(u3P.nor_u.pgs_w == u3K.nor_w);
    c3_assert(u3P.sou_u.pgs_w == u3K.sou_w);
  }
#endif

  //  Sync the image file.
  //
  // printf("_ce_image_sync\r\n");
  _ce_image_sync(&u3P.nor_u);
  _ce_image_sync(&u3P.sou_u);

  //  Delete the patchfile and free it.
  //
  // printf("_ce_patch_delete\r\n");
  _ce_patch_delete();
  // printf("_ce_patch_free\r\n");
  _ce_patch_free(pat_u);
}

/* _ce_limits(): set up global modes and limits.
*/
static void
_ce_limits(void)
{
  struct rlimit rlm;
  c3_i          ret_i;

  /* Set compatible floating-point modes.
  */
  {
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);
  }

  /* Moar stack.
  */
  {
    ret_i = getrlimit(RLIMIT_STACK, &rlm);
    c3_assert(0 == ret_i);
    rlm.rlim_cur = (rlm.rlim_max > (65536 << 10)) 
                          ? (65536 << 10)
                          : rlm.rlim_max;
    if ( 0 != setrlimit(RLIMIT_STACK, &rlm) ) {
      perror("stack");
      exit(1);
    }
  }

  /* Moar filez.
  */
  {
    ret_i = getrlimit(RLIMIT_NOFILE, &rlm);
    c3_assert(0 == ret_i);
    rlm.rlim_cur = 4096;
    if ( 0 != setrlimit(RLIMIT_NOFILE, &rlm) ) {
      perror("file limit");
      //  no exit, not a critical limit
    }
  }

  /* Moar core.
  */
  {
    getrlimit(RLIMIT_CORE, &rlm);
    rlm.rlim_cur = RLIM_INFINITY;
    if ( 0 != setrlimit(RLIMIT_CORE, &rlm) ) {
      perror("core limit");
      //  no exit, not a critical limit
    }
  }
}

/* _ce_signals(): set up interrupts, etc.
*/
static void
_ce_signals(void)
{
  if ( 0 != sigsegv_install_handler(u3_ce_fault) ) {
    fprintf(stderr, "sigsegv install failed\n");
    exit(1);
  }
  // signal(SIGINT, _loom_stop);
}

/* u3_ce_init(): start the environment, with/without checkpointing.
*/
void
u3_ce_init(c3_o chk_o)
{
  _ce_limits();
  _ce_signals();

  /* Map at fixed address.
  */
  {
    c3_w  len_w = u3_cc_bytes;
    void* map_v;

    map_v = mmap((void *)u3_Loom,
                 len_w,
                 u3_so(chk_o) ? PROT_READ : (PROT_READ | PROT_WRITE),
                 (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
                 -1, 0);

    if ( -1 == (c3_ps)map_v ) {
      map_v = mmap((void *)0,
                   len_w,
                   PROT_READ,
                   MAP_ANON | MAP_PRIVATE,
                   -1, 0);

      if ( -1 == (c3_ps)map_v ) {
        fprintf(stderr, "boot: map failed twice\r\n");
      } else {
        fprintf(stderr, "boot: map failed - try U2_OS_LoomBase %p\r\n", map_v);
      }
      exit(1);
    }
    printf("loom: mapped %dMB\r\n", len_w >> 20);
  }
}

/* u3_ce_grab(): garbage-collect the world, plus extra roots, then 
*/
void
u3_ce_grab(c3_c* cap_c, u3_noun som, ...)   // terminate with u3_none
{
  // u3_ch_free(u3R->cax.har_u);
  // u3R->cax.har_u = u3_ch_new();

  u3_cv_mark();
  u3_cm_mark();
  {
    va_list vap;
    u3_noun tur;

    va_start(vap, som);

    if ( som != u3_none ) {
      u3_ca_mark_noun(som);

      while ( u3_none != (tur = va_arg(vap, u3_noun)) ) {
        u3_ca_mark_noun(tur);
      }
    }
    va_end(vap);
  }
  u3_ca_sweep(cap_c);
}

/* u3_ce_boot(): start the u3 system.
*/
void
u3_ce_boot(c3_o nuu_o, c3_o bug_o, c3_c* cpu_c)
{
  u3_ce_init(nuu_o);

  u3P.cpu_c = cpu_c;
  u3P.nor_u.nam_c = "north";
  u3P.sou_u.nam_c = "south";

  /* Open and apply any patches.
  */
  if ( u3_so(nuu_o) ) {
    if ( (u3_no == _ce_image_open(&u3P.nor_u, u3_yes)) ||
         (u3_no == _ce_image_open(&u3P.sou_u, u3_yes)) )
    {
      printf("boot: image failed\r\n");
      exit(1);
    }
  }
  else {
    u3_cs_patch* pat_u;

    /* Open image files.
    */
    {
      if ( (u3_no == _ce_image_open(&u3P.nor_u, u3_no)) ||
           (u3_no == _ce_image_open(&u3P.sou_u, u3_no)) ) 
      {
        fprintf(stderr, "boot: no image\r\n");
        exit(1);
      }
    }

    /* Load any patch files; apply them to images.
    */
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

    /* Write image files to memory; reinstate protection.
    */
    {
      _ce_image_blit(&u3P.nor_u, 
                     u3_Loom, 
                     (1 << u3_cc_page));

      _ce_image_blit(&u3P.sou_u, 
                     (u3_Loom + (1 << u3_cc_bits) - (1 << u3_cc_page)),
                     -(1 << u3_cc_page));

      if ( 0 != mprotect((void *)u3_Loom, u3_cc_bytes, PROT_READ) ) {
        perror("protect");
        c3_assert(0);
      }
      printf("protected loom\r\n");
    }

    /* If the images were empty, we are logically booting.
    */
    if ( (0 == u3P.nor_u.pgs_w) && (0 == u3P.sou_u.pgs_w) ) {
      printf("logical boot\r\n");
      nuu_o = u3_yes;
    }
  }

  /* Construct or activate the allocator.
  */
  u3_cm_boot(nuu_o, bug_o);

  /* Initialize the jet system.
  */
  u3_cj_boot();

  /* Install the kernel.
  */
  if ( u3_so(nuu_o) ) {
    c3_c pas_c[2049];
    struct stat buf_u;

    snprintf(pas_c, 2048, "%s/.urb/urbit.pill", cpu_c);
    if ( -1 == stat(pas_c, &buf_u) ) {
      snprintf(pas_c, 2048, "%s/urbit.pill", U2_LIB);
    }
    printf("boot: loading %s\r\n", pas_c);
    u3_cv_make(pas_c);

    u3_cv_jack();
  }
  else {
    u3_cv_hose();
    u3_cj_ream();
  }
}
