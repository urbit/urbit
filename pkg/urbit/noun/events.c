/// @file events.c
///
/// incremental, orthogonal, paginated loom snapshots
///
/// ### components
///
///   - page: 16KB chunk of the loom.
///   - north segment (u3e_image, north.bin): low contiguous loom pages,
///     (in practice, the home road heap). indexed from low to high:
///     in-order on disk.
///   - south segment (u3e_image, south.bin): high contiguous loom pages,
///     (in practice, the home road stack). indexed from high to low:
///     reversed on disk.
///   - patch memory (memory.bin): new or changed pages since the last snapshot
///   - patch control (u3e_control control.bin): patch metadata, watermarks,
///     and indices/mugs for pages in patch memory.
///
/// ### initialization (u3e_live())
///
///   - with the loom already mapped, all pages are marked dirty in a bitmap.
///   - if snapshot is missing or partial, empty segments are created.
///   - if a patch is present, it's applied (crash recovery).
///   - snapshot segments are copied onto the loom; all included pages
///     are marked clean and protected (read-only).
///
/// #### page faults (u3e_fault())
///
///   - stores into protected pages generate faults (currently SIGSEGV,
///     handled outside this module).
///   - faults are handled by dirtying the page and switching protections to
///     read/write.
///   - a guard page is initially placed in the approximate middle of the free
///     space between the heap and stack at the time of the first page fault.
///     when a fault is detected in the guard page, the guard page is recentered
///     in the free space of the current road. if the guard page cannot be
///     recentered, then memory exhaustion has occurred.
///
/// ### updates (u3e_save())
///
///   - all updates to a snapshot are made through a patch.
///   - high/low watermarks for the north/south segments are established,
///     and dirty pages below/above them are added to the patch.
///     - modifications have been caught by the fault handler.
///     - newly-used pages are automatically included (preemptively dirtied).
///     - unused, innermost pages are reclaimed (segments are truncated to the
///       high/low watermarks; the last page in each is always adjacent to the
///       contiguous free space).
///   - patch pages are written to memory.bin, metadata to control.bin.
///   - the patch is applied to the snapshot segments, in-place.
///   - patch files are deleted.
///
/// ### limitations
///
///   - loom page size is fixed (16 KB), and must be a multiple of the
///     system page size. (can the size vary at runtime give south.bin's
///     reversed order? alternately, if system page size > ours, the fault
///     handler could dirty N pages at a time.)
///   - update atomicity is suspect: patch application must either
///     completely succeed or leave on-disk segments intact. unapplied
///     patches can be discarded (triggering event replay), but once
///     patch application begins it must succeed (can fail if disk is full).
///     may require integration into the overall signal-handling regime.
///   - any errors are handled with assertions; failed/partial writes are not
///     retried.
///
/// ### enhancements
///
///   - use platform specific page fault mechanism (mach rpc, userfaultfd, &c).
///   - implement demand paging / heuristic page-out.
///   - add a guard page in the middle of the loom to reactively handle stack
///     overflow.
///   - parallelism
///

#include "all.h"
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

/// North segment image file name.
static const c3_c nor_nam_c[] = "north.bin";

/// South segment image file name.
static const c3_c sou_nam_c[] = "south.bin";

/// Control patch file name.
static const c3_c ctl_nam_c[] = "control.bin";

/// Memory patch file name.
static const c3_c mem_nam_c[] = "memory.bin";

// Base loom offset of the guard page.
static u3p(c3_w) gar_pag_p;

/// Urbit page size in 4-byte words.
static const size_t pag_wiz_i = 1 << u3a_page;

/// Urbit page size in bytes.
static const size_t pag_siz_i = sizeof(c3_w) * pag_wiz_i;

#ifdef U3_SNAPSHOT_VALIDATION
/* Image check.
*/
struct {
  c3_w nor_w;
  c3_w sou_w;
  c3_w mug_w[u3a_pages];
} u3K;

/* _ce_check_page(): checksum page.
*/
static c3_w
_ce_check_page(c3_w pag_w)
{
  c3_w* mem_w = u3_Loom + pag_w * pag_wiz_i;
  c3_w  mug_w = u3r_mug_words(mem_w, pag_wiz_i);

  return mug_w;
}

/* u3e_check(): compute a checksum on all memory within the watermarks.
*/
void
u3e_check(c3_c* cap_c)
{
  c3_w nor_w = 0;
  c3_w sou_w = 0;

  {
    c3_w nwr_w, swu_w;

    u3m_water(&nwr_w, &swu_w);

    nor_w = (nwr_w + (pag_wiz_i - 1)) / pag_wiz_i;
    sou_w = (swu_w + (pag_wiz_i - 1)) / pag_wiz_i;
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
      mug_w = _ce_check_page((u3a_pages - (i_w + 1)));
      if ( strcmp(cap_c, "boot") ) {
        c3_assert(mug_w == u3K.mug_w[(u3a_pages - (i_w + 1))]);
      }
      sum_w += mug_w;
    }
    u3l_log("%s: sum %x (%x, %x)\r\n", cap_c, sum_w, nor_w, sou_w);
  }
}

/* _ce_maplloc(): crude off-loom allocator.
 */
static void*
_ce_maplloc(c3_w len_w)
{
  void* map_v;

  map_v = mmap(0,
               len_w,
               (PROT_READ | PROT_WRITE),
               (MAP_ANON | MAP_PRIVATE),
               -1, 0);

  if ( -1 == (c3_ps)map_v ) {
    c3_assert(0);
  }
  else {
    c3_w* map_w = map_v;

    map_w[0] = len_w;

    return map_w + 1;
  }
}

/* _ce_mapfree(): crude off-loom allocator.
*/
static void
_ce_mapfree(void* map_v)
{
  c3_w* map_w = map_v;
  c3_i res_i;

  map_w -= 1;
  res_i = munmap(map_w, map_w[0]);

  c3_assert(0 == res_i);
}
#endif

#ifdef U3_GUARD_PAGE
/// Place a guard page at the (approximate) middle of the free space between
/// the heap and stack of the current road, bailing if memory has been
/// exhausted.
static c3_i
_ce_center_guard_page(void)
{
  u3p(c3_w) bot_p, top_p;
  if ( !u3R ) {
    top_p = u3a_outa(u3_Loom + u3a_words);
    bot_p = u3a_outa(u3_Loom);
  }
  else if ( c3y == u3a_is_north(u3R) ) {
    top_p = c3_rod(u3R->cap_p, pag_wiz_i);
    bot_p = c3_rop(u3R->hat_p, pag_wiz_i);
  }
  else {
    top_p = c3_rod(u3R->hat_p, pag_wiz_i);
    bot_p = c3_rop(u3R->cap_p, pag_wiz_i);
  }

  if ( top_p < bot_p + pag_wiz_i ) {
    fprintf(stderr,
            "loom: not enough memory to recenter the guard page\r\n");
    goto bail;
  }
  const u3p(c3_w) old_gar_p = gar_pag_p;
  const c3_w      mid_p     = (top_p - bot_p) / 2;
  gar_pag_p                 = bot_p + c3_rod(mid_p, pag_wiz_i);
  if ( old_gar_p == gar_pag_p ) {
    fprintf(stderr,
            "loom: can't move the guard page to the same location"
            " (base address %p)\r\n",
            u3a_into(gar_pag_p));
    goto bail;
  }

  if ( -1 == mprotect(u3a_into(gar_pag_p), pag_siz_i, PROT_NONE) ) {
    fprintf(stderr,
            "loom: failed to protect the guard page "
            "(base address %p)\r\n",
            u3a_into(gar_pag_p));
    goto fail;
  }

  return 1;

bail:
  u3m_signal(c3__meme);
fail:
  return 0;
}
#endif /* ifdef U3_GUARD_PAGE */

/* u3e_fault(): handle a memory event with libsigsegv protocol.
*/
c3_i
u3e_fault(void* adr_v, c3_i ser_i)
{
  //  Let the stack overflow handler run.
  if ( 0 == ser_i ) {
    return 0;
  }

  //  XX u3l_log avoid here, as it can
  //  cause problems when handling errors

  c3_w* adr_w = (c3_w*)adr_v;

  if ( (adr_w < u3_Loom) || (adr_w >= (u3_Loom + u3a_words)) ) {
    fprintf(stderr, "address %p out of loom!\r\n", adr_w);
    fprintf(stderr, "loom: [%p : %p)\r\n", u3_Loom, u3_Loom + u3a_words);
    c3_assert(0);
    return 0;
  }

  u3p(c3_w) adr_p  = u3a_outa(adr_w);
  c3_w      pag_w  = adr_p >> u3a_page;
  c3_w      blk_w  = (pag_w >> 5);
  c3_w      bit_w  = (pag_w & 31);

#ifdef U3_GUARD_PAGE
  // The fault happened in the guard page.
  if ( gar_pag_p <= adr_p && adr_p < gar_pag_p + pag_wiz_i ) {
    if ( 0 == _ce_center_guard_page() ) {
      return 0;
    }
  }
  else
#endif /* ifdef U3_GUARD_PAGE */
  if ( 0 != (u3P.dit_w[blk_w] & (1 << bit_w)) ) {
    fprintf(stderr, "strange page: %d, at %p, off %x\r\n", pag_w, adr_w, adr_p);
    c3_assert(0);
    return 0;
  }

  u3P.dit_w[blk_w] |= (1 << bit_w);

  if ( -1 == mprotect((void *)(u3_Loom + (pag_w << u3a_page)),
                      pag_siz_i,
                      (PROT_READ | PROT_WRITE)) )
  {
    fprintf(stderr, "loom: fault mprotect: %s\r\n", strerror(errno));
    c3_assert(0);
    return 0;
  }

  return 1;
}

/// Open/create an image.
///
/// @param[in] dir_c  Directory in which the image file resides/will
///                   reside. Must already exist.
/// @param[in] img_u  Image to open/create.
///
/// @return c3n  Image file can't be opened.
/// @return c3n  `fstat()` failed.
/// @return c3n  Image file size is not a multiple of the page size.
/// @return c3y  Otherwise.
static c3_o
_ce_image_open(const c3_c* const dir_c, u3e_image* img_u)
{
  c3_c ful_c[8193];
  snprintf(ful_c, sizeof(ful_c), "%s/%s", dir_c, img_u->nam_c);

  if ( -1 == (img_u->fid_i = open(ful_c, O_RDWR | O_CREAT, 0666)) ) {
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
      c3_d pgs_d = (siz_d + (c3_d)(pag_siz_i - 1)) / pag_siz_i;

      if ( !siz_d ) {
        return c3y;
      }
      else {
        if ( siz_d != (pgs_d * pag_siz_i) ) {
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

/* _ce_patch_write_control(): write control block file.
*/
static void
_ce_patch_write_control(u3_ce_patch* pat_u)
{
  c3_w len_w = sizeof(u3e_control) +
               (pat_u->con_u->pgs_w * sizeof(u3e_line));

  if ( len_w != write(pat_u->ctl_i, pat_u->con_u, len_w) ) {
    c3_assert(0);
  }
}

/* _ce_patch_read_control(): read control block file.
*/
static c3_o
_ce_patch_read_control(u3_ce_patch* pat_u)
{
  c3_w len_w;

  c3_assert(0 == pat_u->con_u);
  {
    struct stat buf_u;

    if ( -1 == fstat(pat_u->ctl_i, &buf_u) ) {
      c3_assert(0);
      return c3n;
    }
    len_w = (c3_w) buf_u.st_size;
  }

  pat_u->con_u = c3_malloc(len_w);
  if ( (len_w != read(pat_u->ctl_i, pat_u->con_u, len_w)) ||
        (len_w != sizeof(u3e_control) +
                  (pat_u->con_u->pgs_w * sizeof(u3e_line))) )
  {
    c3_free(pat_u->con_u);
    pat_u->con_u = 0;
    return c3n;
  }
  return c3y;
}

/* _ce_patch_create(): create patch files.
*/
static void
_ce_patch_create(u3_ce_patch* pat_u)
{
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s", u3P.dir_c);
  c3_mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.dir_c);
  c3_mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, ctl_nam_c);
  if ( -1 == (pat_u->ctl_i = c3_open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0600)) ) {
    fprintf(stderr, "loom: patch open %s: %s\r\n", ctl_nam_c, strerror(errno));
    c3_assert(0);
  }

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, mem_nam_c);
  if ( -1 == (pat_u->mem_i = c3_open(ful_c, O_RDWR | O_CREAT | O_EXCL, 0600)) ) {
    fprintf(stderr, "loom: patch open %s: %s\r\n", mem_nam_c, strerror(errno));
    c3_assert(0);
  }
}

/* _ce_patch_delete(): delete a patch.
 */
static void
_ce_patch_delete(void)
{
  c3_c ful_c[8193];

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, ctl_nam_c);
  c3_unlink(ful_c);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, mem_nam_c);
  c3_unlink(ful_c);
}

/* _ce_patch_verify(): check patch data mug.
 */
static c3_o
_ce_patch_verify(u3_ce_patch* pat_u)
{
  c3_w i_w;

  if ( u3e_version != pat_u->con_u->ver_y ) {
    fprintf(stderr, "loom: patch version mismatch: have %u, need %u\r\n",
                    pat_u->con_u->ver_y,
                    u3e_version);
    return c3n;
  }

  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mug_w = pat_u->con_u->mem_u[i_w].mug_w;
    c3_w mem_w[pag_wiz_i];

    if ( -1 == lseek(pat_u->mem_i, i_w * pag_siz_i, SEEK_SET) ) {
      fprintf(stderr, "loom: patch seek: %s\r\n", strerror(errno));
      return c3n;
    }
    if ( -1 == read(pat_u->mem_i, mem_w, pag_siz_i) ) {
      fprintf(stderr, "loom: patch read: %s\r\n", strerror(errno));
      return c3n;
    }
    {
      c3_w nug_w = u3r_mug_words(mem_w, pag_wiz_i);
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

/* _ce_patch_free(): free a patch.
*/
static void
_ce_patch_free(u3_ce_patch* pat_u)
{
  c3_free(pat_u->con_u);
  close(pat_u->ctl_i);
  close(pat_u->mem_i);
  c3_free(pat_u);
}

/* _ce_patch_open(): open patch, if any.
*/
static u3_ce_patch*
_ce_patch_open(void)
{
  u3_ce_patch* pat_u;
  c3_c ful_c[8193];
  c3_i ctl_i, mem_i;

  snprintf(ful_c, 8192, "%s", u3P.dir_c);
  c3_mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.dir_c);
  c3_mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, ctl_nam_c);
  if ( -1 == (ctl_i = c3_open(ful_c, O_RDWR)) ) {
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/chk/%s", u3P.dir_c, mem_nam_c);
  if ( -1 == (mem_i = c3_open(ful_c, O_RDWR)) ) {
    close(ctl_i);

    _ce_patch_delete();
    return 0;
  }
  pat_u = c3_malloc(sizeof(u3_ce_patch));
  pat_u->ctl_i = ctl_i;
  pat_u->mem_i = mem_i;
  pat_u->con_u = 0;

  if ( c3n == _ce_patch_read_control(pat_u) ) {
    close(pat_u->ctl_i);
    close(pat_u->mem_i);
    c3_free(pat_u);

    _ce_patch_delete();
    return 0;
  }
  if ( c3n == _ce_patch_verify(pat_u) ) {
    _ce_patch_free(pat_u);
    _ce_patch_delete();
    return 0;
  }
  return pat_u;
}

/* _ce_patch_write_page(): write a page of patch memory.
 */
static void
_ce_patch_write_page(u3_ce_patch* pat_u,
                     c3_w         pgc_w,
                     c3_w*        mem_w)
{
  c3_assert(-1 != lseek(pat_u->mem_i, pgc_w * pag_siz_i, SEEK_SET));
  c3_assert(pag_siz_i == write(pat_u->mem_i, mem_w, pag_siz_i));
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
_ce_patch_save_page(u3_ce_patch* pat_u,
                    c3_w         pag_w,
                    c3_w         pgc_w)
{
  c3_w blk_w = (pag_w >> 5);
  c3_w bit_w = (pag_w & 31);

  if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
    c3_w* mem_w = u3_Loom + (pag_w * pag_wiz_i);

    pat_u->con_u->mem_u[pgc_w].pag_w = pag_w;
    pat_u->con_u->mem_u[pgc_w].mug_w = u3r_mug_words(mem_w, pag_wiz_i);

#if 0
    u3l_log("protect a: page %d\r\n", pag_w);
#endif
    _ce_patch_write_page(pat_u, pgc_w, mem_w);

    if ( -1 == mprotect(u3_Loom + (pag_w * pag_wiz_i),
                        pag_siz_i,
                        PROT_READ) )
    {
      c3_assert(0);
    }

    u3P.dit_w[blk_w] &= ~(1 << bit_w);
    pgc_w += 1;
  }
  return pgc_w;
}

/* _ce_patch_compose(): make and write current patch.
 */
static u3_ce_patch*
_ce_patch_compose(void)
{
  c3_w pgs_w = 0;
  c3_w nor_w = 0;
  c3_w sou_w = 0;

  /* Calculate number of saved pages, north and south.
  */
  {
    c3_w nwr_w, swu_w;

    u3m_water(&nwr_w, &swu_w);

    nor_w = (nwr_w + (pag_wiz_i - 1)) / pag_wiz_i;
    sou_w = (swu_w + (pag_wiz_i - 1)) / pag_wiz_i;
  }

#ifdef U3_SNAPSHOT_VALIDATION
  u3K.nor_w = nor_w;
  u3K.sou_w = sou_w;
#endif

  /* Count dirty pages.
  */
  {
    c3_w i_w;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgs_w = _ce_patch_count_page(i_w, pgs_w);
    }
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgs_w = _ce_patch_count_page((u3a_pages - (i_w + 1)), pgs_w);
    }
  }

  if ( !pgs_w ) {
    return 0;
  }
  else {
    u3_ce_patch* pat_u = c3_malloc(sizeof(u3_ce_patch));
    c3_w i_w, pgc_w;

    _ce_patch_create(pat_u);
    pat_u->con_u = c3_malloc(sizeof(u3e_control) + (pgs_w * sizeof(u3e_line)));
    pat_u->con_u->ver_y = u3e_version;
    pgc_w = 0;

    for ( i_w = 0; i_w < nor_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, i_w, pgc_w);
    }
    for ( i_w = 0; i_w < sou_w; i_w++ ) {
      pgc_w = _ce_patch_save_page(pat_u, (u3a_pages - (i_w + 1)), pgc_w);
    }

    pat_u->con_u->nor_w = nor_w;
    pat_u->con_u->sou_w = sou_w;
    pat_u->con_u->pgs_w = pgc_w;

    _ce_patch_write_control(pat_u);
    return pat_u;
  }
}

/* _ce_patch_sync(): make sure patch is synced to disk.
*/
static void
_ce_patch_sync(u3_ce_patch* pat_u)
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

/* _ce_image_sync(): make sure image is synced to disk.
*/
static void
_ce_image_sync(u3e_image* img_u)
{
  if ( -1 == c3_sync(img_u->fid_i) ) {
    fprintf(stderr, "loom: image (%s) sync failed: %s\r\n",
                    img_u->nam_c,
                    strerror(errno));
    c3_assert(!"loom: image sync");
  }
}

/// Resize an image.
///
/// Truncates an image if it shrunk.
///
/// If the image shrunk and was mapped into memory, then the truncated portion
/// is replaced with an anonymous, private (copy-on-write) mapping:
///
/// ```
///  <high address>
/// +==================+ <- previous end of image in memory
/// |                  | <-+
/// |                  |   |
/// |------------------+   |
/// |                  |   +- file-backed mapping replaced with anonymous
/// |                  |   |  mapping
/// |------------------    |
/// |                  |   |
/// |                  | <-+
/// +==================+ <- new end of image in memory
///  <low address>
/// ```
///
/// If the image grew and was mapped into memory, then a new file-backed private
/// (copy-on-write) mapping is established that is backed by the image:
///
/// ```
///  <high address>
/// +==================+ <- new end of image in memory
/// |                  | <-+
/// |                  |   |
/// |------------------+   |
/// |                  |   +- anonymous mapping replaced with file-backed
/// |                  |   |  mapping
/// |------------------|   |
/// |                  |   |
/// |                  | <-+
/// +==================+ <- previous end of image in memory
/// |                  |
/// |                  |
/// |      .....       |
/// |                  |
/// |                  |
/// +==================+ <- base of image in memory
///  <low address>
/// ```
///
/// It's unclear whether anonymous mappings that lie within the previous bounds
/// of the image (identified by `.....` in the diagram above) should be replaced
/// with file-backed mappings when the image is resized. If the
/// (Linux/macOS/Windows) kernel is smart enough to simply replace the
/// anonymous mapping in the page cache with the file-backed mapping, then the
/// anonymous mappings should be replaced with file-backed mappings. If, on the
/// other hand, the kernel treats the pages of the two mappings as separate
/// entities, then the cost of a cache miss may make replacing the anonymous
/// mapping too expensive. For now, we remain conservative and don't replace the
/// mappings and accept the potential cost of the accumulation of anonymous
/// mappings (and the use of swap space that likely accompanies those mappings).
///
/// @param[in] img_u  Image.
/// @param[in] pgs_w  New size of the image.
/// @param[in] bas_y  Base address of the image in memory. Used to establish a
///                   new mapping in memory. Should be NULL if no new mappings
///                   should be created.
static void
_ce_image_resize(u3e_image* img_u, c3_w pgs_w, c3_y* bas_y)
{
  // The image is mapped into memory at base address `bas_y`.
  if ( bas_y ) {
    c3_y* ptr_y  = bas_y + c3_min(img_u->pgs_w, pgs_w) * pag_siz_i;
    c3_ws dif_ws = (img_u->pgs_w - pgs_w) * pag_siz_i;

    c3_i fla_i, fid_i, pro_i;
    size_t off_i;
    // The image shrunk.
    if ( dif_ws > 0 ) {
      fla_i = MAP_ANONYMOUS | MAP_FIXED | MAP_PRIVATE;
      fid_i = -1;
      pro_i = PROT_READ | PROT_WRITE;
      off_i = 0;
    }
    // The image grew.
    else if ( dif_ws < 0 ) {
      fla_i = MAP_FIXED | MAP_PRIVATE;
      fid_i = img_u->fid_i;
      pro_i = PROT_READ;
      off_i = (size_t)(ptr_y - bas_y);
    }

    if ( dif_ws != 0 && -1 == (c3_ps)mmap(ptr_y,
                                          c3_abs(dif_ws),
                                          pro_i,
                                          fla_i,
                                          fid_i,
                                          off_i) )
    {
      fprintf(stderr,
              "loom: failed to establish new mapping "
              "for %s at %p after resizing: %s\r\n",
              img_u->nam_c,
              ptr_y,
              strerror(errno));
      exit(1);
    }
  }

  if ( img_u->pgs_w > pgs_w ) {
    if ( ftruncate(img_u->fid_i, pgs_w * pag_siz_i) ) {
      fprintf(stderr, "loom: image truncate %s: %s\r\n",
                      img_u->nam_c,
                      strerror(errno));
      c3_assert(0);
    }
  }

  img_u->pgs_w = pgs_w;
}

/* _ce_patch_apply(): apply patch to images.
*/
static void
_ce_patch_apply(u3_ce_patch* pat_u)
{
  c3_w i_w;

  //  resize images
  //
  {
    c3_y* bas_y = ( u3C.wag_w & u3o_map_snapshot ) ? (c3_y*)u3_Loom : NULL;
    _ce_image_resize(&u3P.nor_u, pat_u->con_u->nor_w, bas_y);
    // Don't map the south (stack) image because it's almost always dirty and a
    // single page.
    _ce_image_resize(&u3P.sou_u, pat_u->con_u->sou_w, NULL);
  }

  //  seek to begining of patch and images
  //
  if (  (-1 == lseek(pat_u->mem_i, 0, SEEK_SET))
     || (-1 == lseek(u3P.nor_u.fid_i, 0, SEEK_SET))
     || (-1 == lseek(u3P.sou_u.fid_i, 0, SEEK_SET)) )
  {
    fprintf(stderr, "loom: patch apply seek 0: %s\r\n", strerror(errno));
    c3_assert(0);
  }

  //  write patch pages into the appropriate image
  //
  for ( i_w = 0; i_w < pat_u->con_u->pgs_w; i_w++ ) {
    c3_w pag_w = pat_u->con_u->mem_u[i_w].pag_w;
    c3_w mem_w[pag_wiz_i];
    c3_i fid_i;
    c3_w off_w;

    if ( pag_w < pat_u->con_u->nor_w ) {
      fid_i = u3P.nor_u.fid_i;
      off_w = pag_w;
    }
    else {
      fid_i = u3P.sou_u.fid_i;
      off_w = (u3a_pages - (pag_w + 1));
    }

    if ( -1 == read(pat_u->mem_i, mem_w, pag_siz_i) ) {
      fprintf(stderr, "loom: patch apply read: %s\r\n", strerror(errno));
      c3_assert(0);
    }
    else {
      if ( -1 == lseek(fid_i, off_w * pag_siz_i, SEEK_SET) ) {
        fprintf(stderr, "loom: patch apply seek: %s\r\n", strerror(errno));
        c3_assert(0);
      }
      if ( -1 == write(fid_i, mem_w, pag_siz_i) ) {
        fprintf(stderr, "loom: patch apply write: %s\r\n", strerror(errno));
        c3_assert(0);
      }
    }
#if 0
    u3l_log("apply: %d, %x\n", pag_w, u3r_mug_words(mem_w, pag_wiz_i));
#endif
  }
}

/// Apply north and south images to memory.
///
/// Maps the entire north (i.e. heap) image at the bottom of the loom and the
/// entire south (i.e. stack) image at the top of the loom. Both are private
/// (copy-on-write) mappings so that the kernel can swap clean pages out of
/// memory without resorting to writing to swap space.
///
/// @param[in] nor_u  North (heap) image.
/// @param[in] sou_u  South (stack) image.
/// @param[in] pro_o  Write-protect the memory to which image is applied if
///                   `c3y`.
static void
_ce_image_apply(u3e_image* nor_u, u3e_image* sou_u, c3_o pro_o)
{
#define mark_page_clean(address)                                               \
  do {                                                                         \
    c3_w pag_w = u3a_outa(address) >> u3a_page;                                \
    c3_w blk_w = pag_w >> 5;                                                   \
    c3_w bit_w = pag_w & 31;                                                   \
    u3P.dit_w[blk_w] &= ~(1 << bit_w);                                         \
  } while ( 0 )

#define read_image(image, base_address, should_protect, step_direction)        \
  do {                                                                         \
    lseek((image)->fid_i, 0, SEEK_SET);                                        \
    c3_y* ptr_y = (c3_y*)(base_address);                                       \
    for ( c3_w idx_w = 0; idx_w < (image)->pgs_w; idx_w++ ) {                  \
      c3_assert(-1 != read((image)->fid_i, ptr_y, pag_siz_i));                 \
      if ( c3y == (should_protect) ) {                                         \
        c3_assert(0 == mprotect(ptr_y, pag_siz_i, PROT_READ));                 \
      }                                                                        \
      mark_page_clean(ptr_y);                                                  \
      ptr_y += (step_direction) * pag_siz_i;                                   \
    }                                                                          \
  } while ( 0 )

#define map_image(image, base_address, page_protections)                       \
  do {                                                                         \
    if ( -1 == (c3_ps)mmap(base_address,                                       \
                    (image)->pgs_w * pag_siz_i,                                \
                    page_protections,                                          \
                    MAP_FIXED | MAP_PRIVATE,                                   \
                    (image)->fid_i,                                            \
                    0) )                                                       \
    {                                                                          \
      fprintf(stderr,                                                          \
              "loom: failed to map %s snapshot "                               \
              "image at base address %p: %s\r\n",                              \
              (image)->nam_c,                                                  \
              base_address,                                                    \
              strerror(errno));                                                \
      exit(1);                                                                 \
    }                                                                          \
                                                                               \
    c3_y* ptr_y = base_address;                                                \
    for ( c3_w idx_w = 0; idx_w < (image)->pgs_w; idx_w++ ) {                  \
      mark_page_clean(ptr_y);                                                  \
      ptr_y += pag_siz_i;                                                      \
    }                                                                          \
  } while ( 0 )                                                                \


  if ( sou_u && sou_u->pgs_w > 0 ) {
    c3_y* bas_y = ((c3_y*)u3_Loom + u3a_bytes) - pag_siz_i;
    read_image(sou_u, bas_y, pro_o, -1);
  }

  if ( nor_u && nor_u->pgs_w > 0 ) {
    if ( u3C.wag_w & u3o_map_snapshot ) {
      const c3_i pro_i = ( c3y == pro_o ) ? PROT_READ : PROT_READ | PROT_WRITE;
      map_image(nor_u, (c3_y*)u3_Loom, pro_i);
    }
    else {
      read_image(nor_u, (c3_y*)u3_Loom, pro_o, 1);
    }
  }
#undef mark_page_clean
#undef read_image
#undef map_image
}

#ifdef U3_SNAPSHOT_VALIDATION
/* _ce_image_fine(): compare image to memory.
 */
static void
_ce_image_fine(u3e_image* img_u,
               c3_w*        ptr_w,
               c3_ws        stp_ws)
{
  c3_w i_w;
  c3_w buf_w[pag_wiz_i];

  lseek(img_u->fid_i, 0, SEEK_SET);
  for ( i_w=0; i_w < img_u->pgs_w; i_w++ ) {
    c3_w mem_w, fil_w;

    if ( -1 == read(img_u->fid_i, buf_w, pag_siz_i) ) {
      fprintf(stderr, "loom: image fine read: %s\r\n", strerror(errno));
      c3_assert(0);
    }
    mem_w = u3r_mug_words(ptr_w, pag_wiz_i);
    fil_w = u3r_mug_words(buf_w, pag_wiz_i);

    if ( mem_w != fil_w ) {
      c3_w pag_w = (ptr_w - u3_Loom) / pag_wiz_i;

      fprintf(stderr, "mismatch: %s page %d, mem_w %x, fil_w %x, K %x\r\n",
                     img_u->nam_c,
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

/* _ce_image_copy():
*/
static c3_o
_ce_image_copy(u3e_image* fom_u, u3e_image* tou_u)
{
  c3_w i_w;

  //  resize images
  //
  _ce_image_resize(tou_u, fom_u->pgs_w, NULL);

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
    c3_w mem_w[pag_wiz_i];
    c3_w off_w = i_w;

    if ( -1 == read(fom_u->fid_i, mem_w, pag_siz_i) ) {
      fprintf(stderr, "loom: image copy read: %s\r\n", strerror(errno));
      return c3n;
    }
    if ( -1 == lseek(tou_u->fid_i, off_w * pag_siz_i, SEEK_SET) ) {
      fprintf(stderr, "loom: image copy seek: %s\r\n", strerror(errno));
      return c3n;
    }

    if ( -1 == write(tou_u->fid_i, mem_w, pag_siz_i) ) {
      fprintf(stderr, "loom: image copy write: %s\r\n", strerror(errno));
      return c3n;
    }
  }
  _ce_image_sync(tou_u);

  return c3y;
}

/*
  u3e_save(): save current changes.

  If we are in dry-run mode, do nothing.

  First, call `_ce_patch_compose` to write all dirty pages to disk and
  clear protection and dirty bits. If there were no dirty pages to write,
  then we're done.

  - Sync the patch files to disk.
  - Verify the patch (because why not?)
  - Write the patch data into the image file (This is idempotent.).
  - Sync the image file.
  - Delete the patchfile and free it.

  Once we've written the dirty pages to disk (and have reset their dirty bits
  and protection flags), we *could* handle the rest of the checkpointing
  process in a separate thread, but we'd need to wait until that finishes
  before we try to make another snapshot.
*/
void
u3e_save(void)
{
  u3_ce_patch* pat_u;

  if ( u3C.wag_w & u3o_dryrun ) {
    return;
  }

  if ( !(pat_u = _ce_patch_compose()) ) {
    return;
  }

  // u3a_print_memory(stderr, "sync: save", 4096 * pat_u->con_u->pgs_w);

  _ce_patch_sync(pat_u);

  if ( c3n == _ce_patch_verify(pat_u) ) {
    c3_assert(!"loom: save failed");
  }

  _ce_patch_apply(pat_u);

#ifdef U3_SNAPSHOT_VALIDATION
  {
    _ce_image_fine(&u3P.nor_u, u3_Loom, pag_wiz_i);

    _ce_image_fine(&u3P.sou_u,
                   (u3_Loom + (1 << u3a_bits) - pag_wiz_i),
                   -(ssize_t)pag_wiz_i);

    c3_assert(u3P.nor_u.pgs_w == u3K.nor_w);
    c3_assert(u3P.sou_u.pgs_w == u3K.sou_w);
  }
#endif

  _ce_image_sync(&u3P.nor_u);
  _ce_image_sync(&u3P.sou_u);
  _ce_patch_free(pat_u);
  _ce_patch_delete();

  c3_c pax_c[8193];
  snprintf(pax_c, sizeof(pax_c), "%s/.urb/bhk", u3P.dir_c);
  if ( c3n == u3e_copy(pax_c) ) {
    fprintf(stderr, "loom: failed to copy snapshot to %s\r\n", pax_c);
  }
}

c3_o
u3e_copy(const c3_c* const dir_c)
{
  c3_o                suc_o = c3n;
  static c3_i         fla_i = O_RDWR | O_CREAT;
  static const mode_t mod_u = 0666;

  // Attempt to create `dir_c`.
  if ( 0 != mkdir(dir_c, 0700) && EEXIST != errno ) {
    fprintf(stderr,
            "loom: failed to create %s: %s\r\n",
            dir_c,
            strerror(errno));
    goto exit;
  }

  // Attempt to create north image file in `dir_c`.
  u3e_image nop_u = {.nam_c = nor_nam_c, .pgs_w = 0};
  c3_c      pan_c[8193];
  snprintf(pan_c, sizeof(pan_c), "%s/%s", dir_c, nop_u.nam_c);
  if ( -1 == (nop_u.fid_i = open(pan_c, fla_i, mod_u)) ) {
    fprintf(stderr, "loom: failed to open %s: %s\r\n", pan_c, strerror(errno));
    goto exit;
  }

  // Attempt to create south image file in `dir_c`.
  u3e_image sop_u = {.nam_c = sou_nam_c, .pgs_w = 0};
  c3_c      pas_c[8193];
  snprintf(pas_c, sizeof(pas_c), "%s/%s", dir_c, sop_u.nam_c);
  if ( -1 == (sop_u.fid_i = open(pas_c, fla_i, mod_u)) ) {
    fprintf(stderr, "loom: failed to open %s: %s\r\n", pas_c, strerror(errno));
    goto close_north;
  }

  // Copy north and south image files to `dir_c` from `u3P.dir_c`.
  if ( (c3y == _ce_image_copy(&u3P.nor_u, &nop_u))
       && (c3y == _ce_image_copy(&u3P.sou_u, &sop_u)) )
  {
    suc_o = c3y;
  }

close_south:
  close(sop_u.fid_i);
  if ( c3n == suc_o ) {
    c3_unlink(pas_c);
  }
close_north:
  close(nop_u.fid_i);
  if ( c3n == suc_o ) {
    c3_unlink(pan_c);
  }
exit:
  return suc_o;
}

void
u3e_load(const c3_c* dir_c)
{
  c3_assert(dir_c);

  u3e_image nor_u = {.nam_c = nor_nam_c};
  u3e_image sou_u = {.nam_c = sou_nam_c};

  if ( (c3n == _ce_image_open(dir_c, &nor_u))
       || (c3n == _ce_image_open(dir_c, &sou_u)) )
  {
    fprintf(stderr, "boot: failed to load snapshot in %s\r\n", dir_c);
    exit(1);
  }

  _ce_image_apply(&nor_u, &sou_u, c3n);

  u3e_foul();
}

c3_o
u3e_live(const c3_c* dir_c)
{
  c3_assert(dir_c);

  c3_o nuu_o = c3n;

  // require that our page size is a multiple of the system page size.
  //
  c3_assert(0 == pag_siz_i % sysconf(_SC_PAGESIZE));

  u3P.dir_c       = dir_c;
  u3P.nor_u.nam_c = nor_nam_c;
  u3P.sou_u.nam_c = sou_nam_c;

  // XX review dryrun requirements, enable or remove
  //
#if 0
  if ( u3C.wag_w & u3o_dryrun ) {
    return c3y;
  } else
#endif
  {
    // Open image files.
    //
    mkdir(u3P.dir_c, 0700);

    c3_c dir_c[8193];

    snprintf(dir_c, sizeof(dir_c), "%s/.urb", u3P.dir_c);
    mkdir(dir_c, 0700);

    snprintf(dir_c, sizeof(dir_c), "%s/.urb/chk", u3P.dir_c);
    mkdir(dir_c, 0700);

    if ( (c3n == _ce_image_open(dir_c, &u3P.nor_u))
         || (c3n == _ce_image_open(dir_c, &u3P.sou_u)) )
    {
      fprintf(stderr, "boot: image failed\r\n");
      exit(1);
    }
    else {
      u3_ce_patch* pat_u;

      /* Load any patch files; apply them to images.
      */
      if ( 0 != (pat_u = _ce_patch_open()) ) {
        _ce_patch_apply(pat_u);
        _ce_image_sync(&u3P.nor_u);
        _ce_image_sync(&u3P.sou_u);
        _ce_patch_free(pat_u);
        _ce_patch_delete();
      }

      // mark all pages dirty (pages in the snapshot will be marked clean)
      //
      u3e_foul();

      /* Write image files to memory; reinstate protection.
       */
      _ce_image_apply(&u3P.nor_u, &u3P.sou_u, c3y);
      u3l_log("boot: protected loom\r\n");

      /* If the images were empty, we are logically booting.
       */
      if ( (0 == u3P.nor_u.pgs_w) && (0 == u3P.sou_u.pgs_w) ) {
        u3l_log("live: logical boot\r\n");
        nuu_o = c3y;
      }
      else {
        u3a_print_memory(stderr,
                         "live: loaded",
                         (u3P.nor_u.pgs_w + u3P.sou_u.pgs_w) * pag_wiz_i);
      }
    }
  }

  return nuu_o;
}

c3_o
u3e_yolo(void)
{
  // NB: u3e_save() will reinstate protection flags
  //
  if ( 0 != mprotect(u3_Loom, u3a_bytes, (PROT_READ | PROT_WRITE)) ) {
    return c3n;
  }

  return c3y;
}

void
u3e_foul(void)
{
  memset(u3P.dit_w, 0xff, sizeof(u3P.dit_w));
}

void
u3e_init(void)
{
#ifdef U3_GUARD_PAGE
  _ce_center_guard_page();
#endif
}
