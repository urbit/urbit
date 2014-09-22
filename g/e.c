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
static c3_i
u3_ce_fault(void* adr_v, c3_i ser_i)
{
  if ( ser_i ) {
    c3_w*    adr_w = (c3_w*) adr_v;

    if ( (adr_w < u3_Loom) || (adr_w > (u3_Loom + u3_cc_size)) ) {
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

/* u3_ce_sync(): write a checkpoint at the current state.
*/

    for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
      if ( (pag_w >= ceg_u->bot_w) &&
           (win_w=(pag_w - ceg_u->bot_w)) < ceg_u->len_w )
      {
        if ( win_w >= ceg_u->pgs_w ) {
          ceg_u->pgs_w = win_w + 1;
        }
        break;
      }
    }
    if ( 0 == ceg_u ) {
      fprintf(stderr, "page %d is not in a segment!\n", pag_w);
      return 1;
    }

    return 1;
  }
  return 0;
}

/* _ce_image_open(): open or create image.  yes if it already exists.
*/
static c3_o
_ce_image_open(u3_cs_image* img_u)
{
  c3_c ful_c[8193];
  c3_i fid_i;

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/chk/%s.bin", u3P.cpu_c, img_u->nam_c);
  if ( -1 != (img_u->fid_i = open(ful_c, O_RDWR)) ) {
    struct stat buf_u;

    if ( -1 == fstat(fid_i, &buf_u) )
      perror(ful_c);
      c3_assert(0);
      return u3_no;
    }
    else { 
      c3_d siz_d = buf_u.st_siz;
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

/* u3_ce_boot(): start the memory system.
*/
void 
u3_ce_boot(c3_c* cpu_c)
{
  /* Map at fixed address.
  */
  {
    void* map_v;

    map_v = mmap((void *)u3_Loom,
                 (u3_cc_size << 2),
                 PROT_READ,
                 (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
                 -1, 0);

    if ( -1 == (c3_ps)map_v ) {
      map_v = mmap((void *)0,
                   (u3_cc_size << 2),
                    PROT_READ,
                    MAP_ANON | MAP_PRIVATE,
                    -1, 0);

      if ( -1 == (c3_ps)map_v ) {
        fprintf(stderr, "map failed twice\n");
      } else {
        fprintf(stderr, "map failed - try U2_OS_LoomBase %p\n", map_v);
      }
      exit(1);
    }
    printf("loom: mapped %dMB\n", (u3_cc_size >> 18));
  }

  /* Open and load, or create, image files.
  */
  {
    u3P.cpu_c = cpu_c;
    u3P.nor_u.nam_c = "north";
    u3P.sou_u.nam_c = "south";

    if ( u3_yes == _ce_image_open(&u3P.nor_u) ) {
      _ce_image_blit_north(&u3P.nor_u);
    }
    if ( u3_yes == _ce_image_open(&u3P.sou_u) ) {
      _ce_image_blit_south(&u3P.sou_u);
    }
  }

  /* Open and apply any patches.
  */
  {
    u3_cs_patch* pat_u;

    if ( 0 != (pat_u = _ce_patch_read()) ) {
      _ce_patch_memory(pat_u);
      _ce_image_patch(pat_u, &u3P.nor_u, &u3P.sou_u);

      _ce_image_fsync(&u3P.nor_u);
      _ce_image_fsync(&u3P.sou_u);

      _ce_patch_delete();
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
static void
_ce_patch_read_control(u3_cs_patch* pat_u)
{
  u3_cs_control* con_u;
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
}

/* _ce_patch_create(): create patch files.
*/
static c3_i
_ce_patch_create(u3_cs_patch* pat_u)
{
  c3_i fid_i;

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/control.bin", u3P.cpu_c);
  if ( -1 == (pat_u->ctl_i = open(ful_c, O_WRONLY | O_CREAT | O_EXCL, 0666)) ) {
    c3_assert(0);
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/memory.bin", u3P.cpu_c);
  if ( -1 == (pat_u->mem_i = open(ful_c, O_WRONLY | O_CREAT | O_EXCL, 0666)) ) {
    c3_assert(0);
    return 0;
  }
}

/* _ce_patch_open(): open patch, if any.
*/
static u3_cs_patch*
_ce_patch_open(void)
{
  u3_cs_patch* pat_u;
  c3_i ctl_i, mem_i;

  snprintf(ful_c, 8192, "%s", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb", u3P.cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8192, "%s/.urb/control.bin", u3P.cpu_c);
  if ( -1 == (ctl_i = open(ful_c, O_RDONLY)) ) {
    return 0;
  }

  snprintf(ful_c, 8192, "%s/.urb/memory.bin", u3P.cpu_c);
  if ( -1 == (mem_i = open(ful_c, O_RDONLY)) ) {
    close(ctl_i);
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
    return 0;
  }
  return pat_u;
}

/* _ce_patch_compose(): make current patch.
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

  /* Count dirty pages in northward (low) section.
  */
  {
    c3_w i_w;

    for ( i_w = 0; i_w < nor_w; i++ ) {
      c3_w blk_w = (i_w >> 5);
      c3_w bit_w = (i_w & 31);

      if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
        pgs_w += 1;
      }
    }
  }

  /* Count dirty pages in southward (high) section.
  */
  {
    c3_w i_w;

    for ( i_w = 0; i_w < sou_w; i++ ) {
      c3_w j_w   = (u3_cc_pages - (i_w + 1));
      c3_w blk_w = (j_w >> 5);
      c3_w bit_w = (j_w & 31);

      if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
        pgs_w += 1;
      }
    }
  }

  if ( !pgs_w ) {
    return 0;
  }
  else {
    u3_cs_patch* pat_u = malloc(sizeof u3_cs_patch);
    c3_w i_w, pgc_w;

    _ce_patch_create(pat_u);
     
    /* Build and fill control block.
    */
    {
      pat_u->con_u = malloc(sizeof(u3_cs_control) + 
                            (pgs_w + sizeof(u3_cs_line)));

      pgc_w = 0;
      for ( i_w = 0; i_w < nor_w; i_w++ ) {
        c3_w blk_w = (i_w >> 5);
        c3_w bit_w = (i_w & 31);

        if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
          pgs_w += 1;
        }
      }

      c3_assert(0 == (u3P.dit_w[blk_w] & bit_w));
      u3P.dit_w[blk_w] |= (1 << bit_w);
    

  /* Handle intermediate section.
  */

  /* 

  pgs_w = 0;
  for ( i_w = 0; i_w < (u3_cc_pages >> 5); i_w++ ) {

  }
}

/* u3_ce_save(): save current changes.
*/
void
u3_ce_save(void)
{
  u3_cs_patch* pat_u = _ce_patch_compose();

  _ce_patch_save(pat_u);
}
