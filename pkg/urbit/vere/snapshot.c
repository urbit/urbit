//! @file snapshot.c

#include "all.h"
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>


//==============================================================================
// Data structures
//==============================================================================

//! Control line.
typedef struct {
  c3_w pag_w;
  c3_w mug_w;
} _line;

//! Memory change, control file.
typedef struct {
  c3_w  ver_w;     //!< version number
  c3_w  nor_w;     //!< new page count north
  c3_w  sou_w;     //!< new page count south
  c3_w  pgs_w;     //!< number of changed pages
  _line mem_u[0];  //!< per page
} _control;

//! Memory change, top level.
typedef struct {
  c3_i      ctl_i;
  c3_i      mem_i;
  _control* con_u;
} _patch;

//! Memory segment, open file.
typedef struct {
  c3_c* nam_c;  //!< segment name
  c3_i  fid_i;  //!< open file, or 0
  c3_w  pgs_w;  //!< length in pages
} _image;

//! Entire memory system.
typedef struct {
  c3_c*  dir_c;                  //!< path to
  c3_w   dit_w[u3a_pages >> 5];  //!< touched since last save
  _image nor_u;                  //!< north segment
  _image sou_u;                  //!< south segment
} _pool;


//==============================================================================
// Constants
//==============================================================================

//! Snapshot version number.
static const c3_w ver_w = 1;


//==============================================================================
// Static globals
//==============================================================================

//! Global memory control.
static _pool pol_u;

#ifdef U3_SNAPSHOT_VALIDATION
//! Image check
static struct {
  c3_w nor_w;
  c3_w sou_w;
  c3_w mug_w[u3a_pages];
} chk_u;
#endif


//==============================================================================
// Globals
//==============================================================================

// Add globals here.


//==============================================================================
// Static functions
//==============================================================================

//! Open the image file at <path to pier>/.urb/chk/<segment name>.bin. If the
//! file does not already exist, then create it.
//!
//! @param img_u[in,out]  memory segment struct containing the segment name.
//!                       File descriptor and page length fields are filled in
//!                       by this function.
//!
//! @return c3y  image file was successfully opened (or created).
//! @return c3n  image file could not be opened (or created).
static c3_o
_image_open(_image* img_u)
{
  c3_i mod_i = O_RDWR | O_CREAT;
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s", pol_u.dir_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", pol_u.dir_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk", pol_u.dir_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s.bin", pol_u.dir_c, img_u->nam_c);
  if ( -1 == (img_u->fid_i = open(ful_c, mod_i, 0666)) ) {
    fprintf(stderr, "loom: open %s: %s\r\n", ful_c, strerror(errno));
    return c3n;
  }
  else {
    struct stat buf_u;

    if ( -1 == fstat(img_u->fid_i, &buf_u) ) {
      fprintf(stderr, "loom: stat %s: %s\r\n", ful_c, strerror(errno));
      c3_assert(0);
      return c3n;
    }
    else {
      c3_d siz_d = buf_u.st_size;
      c3_d pgs_d = (siz_d + (c3_d)((1 << (u3a_page + 2)) - 1)) >>
                   (c3_d)(u3a_page + 2);

      if ( !siz_d ) {
        return c3y;
      }
      else {
        if ( siz_d != (pgs_d << (c3_d)(u3a_page + 2)) ) {
          fprintf(stderr, "%s: corrupt size %" PRIx64 "\r\n", ful_c, siz_d);
          return c3n;
        }
        img_u->pgs_w = (c3_w) pgs_d;
        c3_assert(pgs_d == (c3_d)img_u->pgs_w);

        return c3y;
      }
    }
  }
}

//! Free a patch's resources and delete its control and memory files from the
//! filesystem.
//!
//! @param[in] pat_u  patch struct to free. Can be NULL.
static void
_patch_delete(_patch* pat_u)
{
  if ( 0 != pat_u ) {
    c3_free(pat_u->con_u);
    close(pat_u->ctl_i);
    close(pat_u->mem_i);
    c3_free(pat_u);
  }

  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s/.urb/chk/control.bin", pol_u.dir_c);
  unlink(ful_c);

  snprintf(ful_c, 8192, "%s/.urb/chk/memory.bin", pol_u.dir_c);
  unlink(ful_c);
}

//! Check patch data mug.
//!
//! @param pat_u  patch struct to verify. Cannot be NULL.
//!
//! @return c3y  patch passed verification.
//! @return c3n  patch failed verification because of a version mismatch, a
//!              filesystem error, or a mug mismatch.
static c3_o
_patch_verify(_patch* pat_u)
{
  c3_w i_w;

  if ( ver_w != pat_u->con_u->ver_w ) {
    fprintf(stderr, "loom: patch version mismatch: have %u, need %u\r\n",
                    pat_u->con_u->ver_w,
                    ver_w);
    return c3n;
  }

  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mug_w = pat_u->con_u->mem_u[i_w].mug_w;
    c3_w mem_w[1 << u3a_page];

    if ( -1 == lseek(pat_u->mem_i, (i_w << (u3a_page + 2)), SEEK_SET) ) {
      fprintf(stderr, "loom: patch seek: %s\r\n", strerror(errno));
      return c3n;
    }
    if ( -1 == read(pat_u->mem_i, mem_w, (1 << (u3a_page + 2))) ) {
      fprintf(stderr, "loom: patch read: %s\r\n", strerror(errno));
      return c3n;
    }
    {
      c3_w nug_w = u3r_mug_words(mem_w, (1 << u3a_page));

      if ( mug_w != nug_w ) {
        fprintf(stderr, "loom: patch mug mismatch %d/%d; (%x, %x)\r\n",
                        pag_w, i_w, mug_w, nug_w);
        return c3n;
      }
#if 0
      else {
        u3l_log("verify: patch %d/%d, %x\r\n", pag_w, i_w, mug_w);
      }
#endif
    }
  }
  return c3y;
}

//! Open a patch by reading its control and memory files into memory.
//!
//! @return pat_u  successfully opened patch.
//! @return NULL   patch could not be opened because of a filesystem error or
//!                mug mismatch.
static _patch*
_patch_open(void)
{
  _patch* pat_u = 0;
  c3_c ful_c[8193];
  c3_i ctl_i, mem_i;

  snprintf(ful_c, 8192, "%s", pol_u.dir_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", pol_u.dir_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/control.bin", pol_u.dir_c);
  if ( -1 == (ctl_i = open(ful_c, O_RDWR)) ) {
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/chk/memory.bin", pol_u.dir_c);
  if ( -1 == (mem_i = open(ful_c, O_RDWR)) ) {
    close(ctl_i);

    _patch_delete(pat_u);
    return 0;
  }
  pat_u = c3_malloc(sizeof(_patch));
  pat_u->ctl_i = ctl_i;
  pat_u->mem_i = mem_i;
  pat_u->con_u = 0;

  // Read _control struct from patch's control file.
  {
    struct stat buf_u;
    c3_assert(-1 != fstat(pat_u->ctl_i, &buf_u));

    c3_w len_w = (c3_w)buf_u.st_size;
    pat_u->con_u = c3_malloc(len_w);

    if ( len_w != read(pat_u->ctl_i, pat_u->con_u, len_w) ||
         len_w != sizeof(_control) + (pat_u->con_u->pgs_w * sizeof(_line)) )
    {
      _patch_delete(pat_u);
      return 0;
    }
  }
  if ( c3n == _patch_verify(pat_u) ) {
    _patch_delete(pat_u);
    return 0;
  }
  return pat_u;
}

//! Save a page, producing a new page counter.
static c3_w
_patch_save_page(_patch* pat_u,
                 c3_w    pag_w,
                 c3_w    pgc_w)
{
  c3_w blk_w = (pag_w >> 5);
  c3_w bit_w = (pag_w & 31);

  if ( pol_u.dit_w[blk_w] & (1 << bit_w) ) {
    c3_w* mem_w = u3_Loom + (pag_w << u3a_page);

    pat_u->con_u->mem_u[pgc_w].pag_w = pag_w;
    pat_u->con_u->mem_u[pgc_w].mug_w = u3r_mug_words(mem_w,
                                                       (1 << u3a_page));

#if 0
    u3l_log("protect a: page %d\r\n", pag_w);
#endif
    // Write page to patch's memory file.
    {
      c3_assert(-1 != lseek(pat_u->mem_i, pgc_w << (u3a_page + 2), SEEK_SET));
      c3_w len_w = 1 << (u3a_page + 2);
      c3_assert(len_w == write(pat_u->mem_i, mem_w, len_w));
    }

    if ( -1 == mprotect(u3_Loom + (pag_w << u3a_page),
                        (1 << (u3a_page + 2)),
                        PROT_READ) )
    {
      c3_assert(0);
    }

    pol_u.dit_w[blk_w] &= ~(1 << bit_w);
    pgc_w += 1;
  }
  return pgc_w;
}

//! Make and write current patch.
static _patch*
_patch_compose(void)
{
  c3_w pgs_w = 0;
  c3_w nor_w = 0;
  c3_w sou_w = 0;

  /* Calculate number of saved pages, north and south.
  */
  {
    c3_w nwr_w, swu_w;

    u3m_water(&nwr_w, &swu_w);

    nor_w = (nwr_w + ((1 << u3a_page) - 1)) >> u3a_page;
    sou_w = (swu_w + ((1 << u3a_page) - 1)) >> u3a_page;
  }

#ifdef U3_SNAPSHOT_VALIDATION
  chk_u.nor_w = nor_w;
  chk_u.sou_w = sou_w;
#endif

  /* Count dirty pages.
  */
  {
    c3_w pag_w;
    for ( pag_w = 0; pag_w < u3a_pages; pag_w++ ) {
      if ( nor_w == pag_w ) {
        pag_w = u3a_pages - sou_w;
      }

      c3_w blk_w = pag_w >> 5;
      c3_w bit_w = pag_w & 31;

      if ( pol_u.dit_w[blk_w] & (1 << bit_w) ) {
        pgs_w++;
      }
    }
  }

  if ( !pgs_w ) {
    return 0;
  }
  else {
    _patch* pat_u = c3_malloc(sizeof(_patch));
    c3_w i_w, pgc_w;

    // Create and open the patch's control and memory files.
    {
      c3_c ful_c[8193];

      snprintf(ful_c, sizeof(ful_c)-1, "%s", pol_u.dir_c);
      mkdir(ful_c, 0700);

      snprintf(ful_c, sizeof(ful_c)-1, "%s/.urb", pol_u.dir_c);
      mkdir(ful_c, 0700);

      snprintf(ful_c, sizeof(ful_c)-1, "%s/.urb/chk/control.bin", pol_u.dir_c);
      pat_u->ctl_i = open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0600);
      if ( -1 == pat_u->ctl_i ) {
        fprintf(stderr, "loom: patch open control.bin: %s\r\n", strerror(errno));
        c3_assert(0);
      }

      snprintf(ful_c, sizeof(ful_c)-1, "%s/.urb/chk/memory.bin", pol_u.dir_c);
      pat_u->mem_i = open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0600);
      if ( -1 == pat_u->mem_i ) {
        fprintf(stderr, "loom: patch open memory.bin: %s\r\n", strerror(errno));
        c3_assert(0);
      }
    }

    pat_u->con_u = c3_malloc(sizeof(_control) + (pgs_w * sizeof(_line)));
    pat_u->con_u->ver_w = ver_w;
    pgc_w = 0;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgc_w = _patch_save_page(pat_u, i_w, pgc_w);
    }
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgc_w = _patch_save_page(pat_u, (u3a_pages - (i_w + 1)), pgc_w);
    }

    pat_u->con_u->nor_w = nor_w;
    pat_u->con_u->sou_w = sou_w;
    pat_u->con_u->pgs_w = pgc_w;

    // Write _control struct to patch's control file.
    {
      c3_w len_w = sizeof(_control) + (pat_u->con_u->pgs_w * sizeof(_line));
      c3_assert(len_w == write(pat_u->ctl_i, pat_u->con_u, len_w));
    }
    return pat_u;
  }
}

//! Make sure patch is synced to disk.
static void
_patch_sync(_patch* pat_u)
{
  if ( -1 == c3_sync(pat_u->ctl_i) ) {
    fprintf(stderr, "loom: control file sync failed: %s\r\n",
                    strerror(errno));
    c3_assert(!"loom: control sync");
  }

  if ( -1 == c3_sync(pat_u->mem_i) ) {
    fprintf(stderr, "loom: patch file sync failed: %s\r\n",
                    strerror(errno));
    c3_assert(!"loom: patch sync");
  }
}

//! Make sure image is synced to disk.
static void
_image_sync(_image* img_u)
{
  if ( -1 == c3_sync(img_u->fid_i) ) {
    fprintf(stderr, "loom: image (%s) sync failed: %s\r\n",
                    img_u->nam_c,
                    strerror(errno));
    c3_assert(!"loom: image sync");
  }
}

//! Resize image, truncating if it shrunk.
static void
_image_resize(_image* img_u, c3_w pgs_w)
{
  if ( img_u->pgs_w > pgs_w ) {
    if ( ftruncate(img_u->fid_i, pgs_w << (u3a_page + 2)) ) {
      fprintf(stderr, "loom: image truncate %s: %s\r\n",
                      img_u->nam_c,
                      strerror(errno));
      c3_assert(0);
    }
  }

  img_u->pgs_w = pgs_w;
}

//! Apply patch to images.
static void
_patch_apply(_patch* pat_u)
{
  c3_w i_w;

  //  resize images
  //
  _image_resize(&pol_u.nor_u, pat_u->con_u->nor_w);
  _image_resize(&pol_u.sou_u, pat_u->con_u->sou_w);

  //  seek to begining of patch and images
  //
  if (  (-1 == lseek(pat_u->mem_i, 0, SEEK_SET))
     || (-1 == lseek(pol_u.nor_u.fid_i, 0, SEEK_SET))
     || (-1 == lseek(pol_u.sou_u.fid_i, 0, SEEK_SET)) )
  {
    fprintf(stderr, "loom: patch apply seek 0: %s\r\n", strerror(errno));
    c3_assert(0);
  }

  //  write patch pages into the appropriate image
  //
  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mem_w[1 << u3a_page];
    c3_i fid_i;
    c3_w off_w;

    if ( pag_w < pat_u->con_u->nor_w ) {
      fid_i = pol_u.nor_u.fid_i;
      off_w = pag_w;
    }
    else {
      fid_i = pol_u.sou_u.fid_i;
      off_w = (u3a_pages - (pag_w + 1));
    }

    if ( -1 == read(pat_u->mem_i, mem_w, (1 << (u3a_page + 2))) ) {
      fprintf(stderr, "loom: patch apply read: %s\r\n", strerror(errno));
      c3_assert(0);
    }
    else {
      if ( -1 == lseek(fid_i, (off_w << (u3a_page + 2)), SEEK_SET) ) {
        fprintf(stderr, "loom: patch apply seek: %s\r\n", strerror(errno));
        c3_assert(0);
      }
      if ( -1 == write(fid_i, mem_w, (1 << (u3a_page + 2))) ) {
        fprintf(stderr, "loom: patch apply write: %s\r\n", strerror(errno));
        c3_assert(0);
      }
    }
#if 0
    u3l_log("apply: %d, %x\n", pag_w, u3r_mug_words(mem_w, (1 << u3a_page)));
#endif
  }
}

//! Apply image to memory.
static void
_image_blit(_image* img_u,
            c3_w*   ptr_w,
            c3_ws   stp_ws)
{
  if ( 0 == img_u->pgs_w ) {
    return;
  }

  c3_w i_w;
  c3_w siz_w = 1 << (u3a_page + 2);

  lseek(img_u->fid_i, 0, SEEK_SET);
  for ( i_w = 0; i_w < img_u->pgs_w; i_w++ ) {
    if ( -1 == read(img_u->fid_i, ptr_w, siz_w) ) {
      fprintf(stderr, "loom: image blit read: %s\r\n", strerror(errno));
      c3_assert(0);
    }

    if ( 0 != mprotect(ptr_w, siz_w, PROT_READ) ) {
      fprintf(stderr, "loom: live mprotect: %s\r\n", strerror(errno));
      c3_assert(0);
    }

    c3_w pag_w = u3a_outa(ptr_w) >> u3a_page;
    c3_w blk_w = pag_w >> 5;
    c3_w bit_w = pag_w & 31;
    pol_u.dit_w[blk_w] &= ~(1 << bit_w);

    ptr_w += stp_ws;
  }
}

#ifdef U3_SNAPSHOT_VALIDATION
//! Compare image to memory.
static void
_image_fine(_image* img_u,
            c3_w*   ptr_w,
            c3_ws   stp_ws)
{
  c3_w i_w;
  c3_w buf_w[1 << u3a_page];

  lseek(img_u->fid_i, 0, SEEK_SET);
  for ( i_w=0; i_w < img_u->pgs_w; i_w++ ) {
    c3_w mem_w, fil_w;

    if ( -1 == read(img_u->fid_i, buf_w, (1 << (u3a_page + 2))) ) {
      fprintf(stderr, "loom: image fine read: %s\r\n", strerror(errno));
      c3_assert(0);
    }
    mem_w = u3r_mug_words(ptr_w, (1 << u3a_page));
    fil_w = u3r_mug_words(buf_w, (1 << u3a_page));

    if ( mem_w != fil_w ) {
      c3_w pag_w = (ptr_w - u3_Loom) >> u3a_page;

      fprintf(stderr, "mismatch: page %d, mem_w %x, fil_w %x, K %x\r\n",
                     pag_w,
                     mem_w,
                     fil_w,
                     chk_u.mug_w[pag_w]);
      abort();
    }
    ptr_w += stp_ws;
  }
}
#endif

//! TODO(peter): comment once interface freezes.
static c3_o
_image_copy(_image* fom_u, _image* tou_u)
{
  c3_w i_w;

  //  resize images
  //
  _image_resize(tou_u, fom_u->pgs_w);

  //  seek to begining of patch and images
  //
  if (  (-1 == lseek(fom_u->fid_i, 0, SEEK_SET))
     || (-1 == lseek(tou_u->fid_i, 0, SEEK_SET)) )
  {
    fprintf(stderr, "loom: image copy seek 0: %s\r\n", strerror(errno));
    return c3n;
  }

  //  copy pages into destination image
  //
  for ( i_w = 0; i_w < fom_u->pgs_w; i_w++ ) {
    c3_w mem_w[1 << u3a_page];
    c3_w off_w = i_w;

    if ( -1 == read(fom_u->fid_i, mem_w, (1 << (u3a_page + 2))) ) {
      fprintf(stderr, "loom: image copy read: %s\r\n", strerror(errno));
      return c3n;
    }
    else {
      if ( -1 == lseek(tou_u->fid_i, (off_w << (u3a_page + 2)), SEEK_SET) ) {
        fprintf(stderr, "loom: image copy seek: %s\r\n", strerror(errno));
        return c3n;
      }
      if ( -1 == write(tou_u->fid_i, mem_w, (1 << (u3a_page + 2))) ) {
        fprintf(stderr, "loom: image copy write: %s\r\n", strerror(errno));
        return c3n;
      }
    }
  }

  return c3y;
}

//! TODO(peter): comment once interface freezes.
static void
_backup(void)
{
  _image nop_u = { .nam_c = "north", .pgs_w = 0 };
  _image sop_u = { .nam_c = "south", .pgs_w = 0 };
  c3_i mod_i = O_RDWR | O_CREAT;
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s/.urb/bhk", pol_u.dir_c);

  if ( mkdir(ful_c, 0700) ) {
    if ( EEXIST != errno ) {
      fprintf(stderr, "loom: image backup: %s\r\n", strerror(errno));
    }
    return;
  }

  snprintf(ful_c, 8192, "%s/.urb/bhk/%s.bin", pol_u.dir_c, nop_u.nam_c);

  if ( -1 == (nop_u.fid_i = open(ful_c, mod_i, 0666)) ) {
    fprintf(stderr, "loom: open %s: %s\r\n", ful_c, strerror(errno));
    return;
  }

  snprintf(ful_c, 8192, "%s/.urb/bhk/%s.bin", pol_u.dir_c, sop_u.nam_c);

  if ( -1 == (sop_u.fid_i = open(ful_c, mod_i, 0666)) ) {
    fprintf(stderr, "loom: open %s: %s\r\n", ful_c, strerror(errno));
    return;
  }

  if (  (c3n == _image_copy(&pol_u.nor_u, &nop_u))
     || (c3n == _image_copy(&pol_u.sou_u, &sop_u)) )
  {

    unlink(ful_c);
    snprintf(ful_c, 8192, "%s/.urb/bhk/%s.bin", pol_u.dir_c, nop_u.nam_c);
    unlink(ful_c);
    snprintf(ful_c, 8192, "%s/.urb/bhk", pol_u.dir_c);
    rmdir(ful_c);
  }

  close(nop_u.fid_i);
  close(sop_u.fid_i);
}


//==============================================================================
// Functions
//==============================================================================

//! Handle a memory event with libsigsegv protocol.
c3_i
u3_snap_fault(void* adr_v, c3_i ser_i)
{
  //  Let the stack overflow handler run.
  if ( 0 == ser_i ) {
    return 0;
  }

  //  XX u3l_log avoid here, as it can
  //  cause problems when handling errors

  c3_w* adr_w = (c3_w*) adr_v;

  if ( (adr_w < u3_Loom) || (adr_w >= (u3_Loom + u3a_words)) ) {
    fprintf(stderr, "address %p out of loom!\r\n", adr_w);
    fprintf(stderr, "loom: [%p : %p)\r\n", u3_Loom, u3_Loom + u3a_words);
    c3_assert(0);
    return 0;
  }
  else {
    c3_w off_w = u3a_outa(adr_w);
    c3_w pag_w = off_w >> u3a_page;
    c3_w blk_w = (pag_w >> 5);
    c3_w bit_w = (pag_w & 31);

#if 0
    if ( pag_w == 131041 ) {
      u3l_log("dirty page %d (at %p); unprotecting %p to %p\r\n",
              pag_w,
              adr_v,
              (u3_Loom + (pag_w << u3a_page)),
              (u3_Loom + (pag_w << u3a_page) + (1 << u3a_page)));
    }
#endif

    if ( 0 != (pol_u.dit_w[blk_w] & (1 << bit_w)) ) {
      fprintf(stderr, "strange page: %d, at %p, off %x\r\n",
              pag_w, adr_w, off_w);
      c3_assert(0);
      return 0;
    }

    pol_u.dit_w[blk_w] |= (1 << bit_w);

    if ( -1 == mprotect((void *)(u3_Loom + (pag_w << u3a_page)),
                        (1 << (u3a_page + 2)),
                        (PROT_READ | PROT_WRITE)) )
    {
      fprintf(stderr, "loom: fault mprotect: %s\r\n", strerror(errno));
      c3_assert(0);
      return 0;
    }
  }
  return 1;
}

//! Save current changes.
//!
//! If we are in dry-run mode, do nothing.
//!
//! First, call `_patch_compose` to write all dirty pages to disk and
//! clear protection and dirty bits. If there were no dirty pages to write,
//! then we're done.
//!
//! - Sync the patch files to disk.
//! - Verify the patch (because why not?)
//! - Write the patch data into the image file (This is idempotent.).
//! - Sync the image file.
//! - Delete the patchfile and free it.
//!
//! Once we've written the dirty pages to disk (and have reset their dirty bits
//! and protection flags), we *could* handle the rest of the checkpointing
//! process in a separate thread, but we'd need to wait until that finishes
//! before we try to make another snapshot.
void
u3_snap_save(void)
{
  _patch* pat_u;

  if ( u3C.wag_w & u3o_dryrun ) {
    return;
  }

  if ( !(pat_u = _patch_compose()) ) {
    return;
  }

  // u3a_print_memory(stderr, "sync: save", 4096 * pat_u->con_u->pgs_w);

  _patch_sync(pat_u);

  if ( c3n == _patch_verify(pat_u) ) {
    c3_assert(!"loom: save failed");
  }

  _patch_apply(pat_u);

#ifdef U3_SNAPSHOT_VALIDATION
  {
    _image_fine(&pol_u.nor_u,
                   u3_Loom,
                   (1 << u3a_page));

    _image_fine(&pol_u.sou_u,
                   (u3_Loom + (1 << u3a_bits) - (1 << u3a_page)),
                   -(1 << u3a_page));

    c3_assert(pol_u.nor_u.pgs_w == chk_u.nor_w);
    c3_assert(pol_u.sou_u.pgs_w == chk_u.sou_w);
  }
#endif

  _image_sync(&pol_u.nor_u);
  _image_sync(&pol_u.sou_u);
  _patch_delete(pat_u);

  _backup();
}

//! Start the checkpointing system.
c3_o
u3_snap_live(c3_o nuu_o, c3_c* dir_c)
{
  pol_u.dir_c = dir_c;
  pol_u.nor_u.nam_c = "north";
  pol_u.sou_u.nam_c = "south";

  //  XX review dryrun requirements, enable or remove
  //
#if 0
  if ( u3C.wag_w & u3o_dryrun ) {
    return c3y;
  } else
#endif
  {
    //  Open image files.
    //
    if ( (c3n == _image_open(&pol_u.nor_u)) ||
         (c3n == _image_open(&pol_u.sou_u)) )
    {
      fprintf(stderr, "boot: image failed\r\n");
      exit(1);
    }
    else {
      _patch* pat_u;

      /* Load any patch files; apply them to images.
      */
      if ( 0 != (pat_u = _patch_open()) ) {
        _patch_apply(pat_u);
        _image_sync(&pol_u.nor_u);
        _image_sync(&pol_u.sou_u);
        _patch_delete(pat_u);
      }

      //  mark all pages dirty (pages in the snapshot will be marked clean)
      //
      u3_snap_foul();

      /* Write image files to memory; reinstate protection.
      */
      {
        _image_blit(&pol_u.nor_u,
                       u3_Loom,
                       (1 << u3a_page));

        _image_blit(&pol_u.sou_u,
                       (u3_Loom + (1 << u3a_bits) - (1 << u3a_page)),
                       -(1 << u3a_page));

        u3l_log("boot: protected loom\r\n");
      }

      /* If the images were empty, we are logically booting. By default, we mark
      ** all pages as dirty, which enables us to track only home road pages by
      ** marking those as clean when they're mapped into memory from the
      ** snapshot on a future boot for which the images are not empty.
      */
      if ( (0 == pol_u.nor_u.pgs_w) && (0 == pol_u.sou_u.pgs_w) ) {
        u3l_log("live: logical boot\r\n");
        nuu_o = c3y;
      }
      else {
        u3a_print_memory(stderr, "live: loaded",
                         (pol_u.nor_u.pgs_w + pol_u.sou_u.pgs_w) << u3a_page);
      }
    }
  }
  return nuu_o;
}

//! Disable page tracking, which makes the entire loom writable.
c3_o
u3_snap_yolo(void)
{
  //    NB: u3_snap_save() will reinstate protection flags
  //
  if ( 0 != mprotect((void *)u3_Loom, u3a_bytes, (PROT_READ | PROT_WRITE)) ) {
    return c3n;
  }

  return c3y;
}

//! Dirty all pages of the loom.
void
u3_snap_foul(void)
{
  memset((void*)pol_u.dit_w, 0xff, sizeof(pol_u.dit_w));
}
