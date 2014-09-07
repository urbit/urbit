/* n/m.c
**
** This file is in the public domain.
*/
#define C3_GLOBAL
#include "all.h"

/* _boot_north(): install a north road.
*/
static u3_road*
_boot_north(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u3_road));
  c3_w*    cap_w = mat_w;
  u3_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u3_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _boot_south(): install a south road.
*/
static u3_road*
_boot_south(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u3_road));
  c3_w*    cap_w = mat_w;
  u3_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u3_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* u3_cm_boot(): make u3R and u3H from nothing.
*/
void
u3_cm_boot(c3_p adr_p, c3_w len_w)
{
  void* map_v;

  map_v = mmap((void *)(c3_p)adr_p,
               (len_w << 2),
               PROT_READ | PROT_WRITE,
               (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
               -1, 0);

  if ( -1 == (c3_ps)map_v ) {
    map_v = mmap((void *)0,
                 (len_w << 2),
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
  printf("loom: mapped %dMB\n", (len_w >> 18));
  u3L = map_v;
  u3H = u3R = _boot_north(map_v, len_w);
}

/* u3_cm_clear(): clear all allocated data in road.
*/
void
u3_cm_clear(void)
{
  u3_ch_free(u3R->jed.har_u);
}

#if 0
static void
_road_sane(void)
{
  c3_w i_w;
   
  for ( i_w = 0; i_w < u3_cc_fbox_no; i_w++ ) {
    u3_cs_fbox* fre_u = u3R->all.fre_u[i_w];
    
    while ( fre_u ) {
      if ( fre_u == u3R->all.fre_u[i_w] ) {
        c3_assert(fre_u->pre_u == 0);
      }
      else {
        c3_assert(fre_u->pre_u != 0);
        c3_assert(fre_u->pre_u->nex_u == fre_u);
        if ( fre_u->nex_u != 0 ) {
          c3_assert(fre_u->nex_u->pre_u == fre_u);
        }
      }
      fre_u = fre_u->nex_u;
    }
  }
}
#endif

void
u3_cm_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u3_so(u3_co_is_north) ? u3R->hat_w - u3R->rut_w 
                                : u3R->hat_w - u3R->rut_w;

  for ( i_w = 0; i_w < u3_cc_fbox_no; i_w++ ) {
    u3_cs_fbox* fre_u = u3R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  printf("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u3R->rut_w;
    c3_w  mem_w = 0;

    while ( box_w < u3R->hat_w ) {
      u3_cs_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
#ifdef U3_MEMORY_DEBUG
        printf("live %d words, code %x\n", box_u->siz_w, box_u->cod_w);
#endif
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    printf("second count: %x\n", mem_w);
  }
}

/* u3_cm_bail(): bail out.  Does not return.
**
**  Bail motes:
**
**    %evil               ::  erroneous cryptography
**    %exit               ::  semantic failure
**    %oops               ::  assertion failure
**    %intr               ::  interrupt
**    %fail               ::  computability failure
**    %need               ::  namespace block
**    %meme               ::  out of memory
*/ 
c3_i
u3_cm_bail(c3_m how_m)
{
  c3_c str_c[5];

  str_c[0] = ((how_m >> 0) & 0xff);
  str_c[1] = ((how_m >> 8) & 0xff);
  str_c[2] = ((how_m >> 16) & 0xff);
  str_c[3] = ((how_m >> 24) & 0xff);
  str_c[4] = 0;
  printf("bail: %s\n", str_c);

  assert(0);
  if ( c3__meme == how_m ) {
    u3_cm_dump();
  }

  _longjmp(u3R->esc.buf, how_m);
  return how_m;
}

/* u3_cm_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u3_cm_error(c3_c* str_c)
{
  printf("error: %s\n", str_c);   // rong
  return u3_cm_bail(c3__exit);
}

/* u3_cm_leap(): in u3R, create a new road within the existing one.
*/
void
u3_cm_leap()
{
  u3_road* rod_u;

  if ( u3_yes == u3_co_is_north ) {
    rod_u = _boot_south(u3R->hat_w, (u3R->cap_w - u3R->hat_w));
  } 
  else {
    rod_u = _boot_north(u3R->cap_w, (u3R->hat_w - u3R->cap_w));
  }

  c3_assert(0 == u3R->kid_u);
  rod_u->par_u = u3R;
  u3R->kid_u = rod_u;
  u3R = rod_u;
}

/* u3_cm_fall(): in u3R, return an inner road to its parent.
*/
void
u3_cm_fall()
{
  c3_assert(0 != u3R->par_u);

  u3R->par_u->cap_w = u3R->hat_w;
  u3R = u3R->par_u;
}

/* u3_cm_golf(): record cap_w length for u3_flog().
*/
c3_w
u3_cm_golf(void)
{
  if ( u3_yes == u3_co_is_north ) {
    return u3R->mat_w - u3R->cap_w;
  } 
  else {
    return u3R->cap_w - u3R->mat_w;
  }
}

/* u3_cm_flog(): reset cap_w.
*/
void
u3_cm_flog(c3_w gof_w)
{
  if ( u3_yes == u3_co_is_north ) {
    u3R->cap_w = u3R->mat_w - gof_w;
  } else {
    u3R->cap_w = u3R->mat_w + gof_w;
  }
}

/* u3_cm_water(): produce watermarks.
*/
void
u3_cm_water(c3_w* low_w, c3_w* hig_w)
{
  c3_assert(u3R == u3H);

  *low_w = (u3H->hat_w - u3H->rut_w);
  *hig_w = (u3H->mat_w - u3H->cap_w) + c3_wiseof(u3_road);
}
