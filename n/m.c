/* n/m.c
**
** This file is in the public domain.
*/
#define C3_GLOBAL
#include "all.h"

/* _boot_north(): install a north road.
*/
static u2_road*
_boot_north(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u2_road));
  c3_w*    cap_w = mat_w;
  u2_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u2_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _boot_south(): install a south road.
*/
static u2_road*
_boot_south(c3_w* mem_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - c3_wiseof(u2_road));
  c3_w*    cap_w = mat_w;
  u2_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, sizeof(u2_road));

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* u2_cm_boot(): make u2R and u2H from nothing.
*/
void
u2_cm_boot(c3_p adr_p, c3_w len_w)
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
  u2L = map_v;
  u2H = u2R = _boot_north(map_v, len_w);
}

#if 0
static void
_road_sane(void)
{
  c3_w i_w;
   
  for ( i_w = 0; i_w < u2_cc_fbox_no; i_w++ ) {
    u2_cs_fbox* fre_u = u2R->all.fre_u[i_w];
    
    while ( fre_u ) {
      if ( fre_u == u2R->all.fre_u[i_w] ) {
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
u2_cm_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u2_so(u2_co_is_north) ? u2R->hat_w - u2R->rut_w 
                                : u2R->hat_w - u2R->rut_w;

  for ( i_w = 0; i_w < u2_cc_fbox_no; i_w++ ) {
    u2_cs_fbox* fre_u = u2R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  printf("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u2R->rut_w;
    c3_w  mem_w = 0;

    while ( box_w < u2R->hat_w ) {
      u2_cs_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    printf("second count: %x\n", mem_w);
  }
}

/* u2_cm_bail(): bail out.  Does not return.
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
u2_cm_bail(c3_m how_m)
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
    u2_cm_dump();
  }

  _longjmp(u2R->esc.buf, how_m);
  return how_m;
}

/* u2_cm_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u2_cm_error(c3_c* str_c)
{
  printf("error: %s\n", str_c);   // rong
  return u2_cm_bail(c3__exit);
}

/* u2_cm_leap(): in u2R, create a new road within the existing one.
*/
void
u2_cm_leap()
{
  u2_road* rod_u;

  if ( u2_yes == u2_co_is_north ) {
    rod_u = _boot_south(u2R->hat_w, (u2R->cap_w - u2R->hat_w));
  } 
  else {
    rod_u = _boot_north(u2R->cap_w, (u2R->hat_w - u2R->cap_w));
  }

  c3_assert(0 == u2R->kid_u);
  rod_u->par_u = u2R;
  u2R->kid_u = rod_u;
  u2R = rod_u;
}

/* u2_cm_fall(): in u2R, return an inner road to its parent.
*/
void
u2_cm_fall()
{
  c3_assert(0 != u2R->par_u);

  u2R->par_u->cap_w = u2R->hat_w;
  u2R = u2R->par_u;
}

/* u2_cm_golf(): record cap_w length for u2_flog().
*/
c3_w
u2_cm_golf(void)
{
  if ( u2_yes == u2_co_is_north ) {
    return u2R->mat_w - u2R->cap_w;
  } 
  else {
    return u2R->cap_w - u2R->mat_w;
  }
}

/* u2_cm_flog(): reset cap_w.
*/
void
u2_cm_flog(c3_w gof_w)
{
  if ( u2_yes == u2_co_is_north ) {
    u2R->cap_w = u2R->mat_w - gof_w;
  } else {
    u2R->cap_w = u2R->mat_w + gof_w;
  }
}

/* u2_cm_water(): produce watermarks.
*/
void
u2_cm_water(c3_w* low_w, c3_w* hig_w)
{
  c3_assert(u2R == u2H);

  *low_w = (u2H->hat_w - u2H->rut_w);
  *hig_w = (u2H->mat_w - u2H->cap_w) + c3_wiseof(u2_road);
}
