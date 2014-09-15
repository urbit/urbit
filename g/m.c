/* g/m.c
**
** This file is in the public domain.
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <ctype.h>

#include "all.h"

/* u3_cm_file(): load file, as atom, or bail.
*/
u3_noun
u3_cm_file(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u3_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3_cm_bail(c3__fail);
  }
  else {
    u3_noun pad = u3_ci_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}

/* _boot_north(): install a north road.
*/
static u3_road*
_boot_north(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - siz_w);
  c3_w*    cap_w = mat_w;
  u3_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, 4 * siz_w);

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _boot_south(): install a south road.
*/
static u3_road*
_boot_south(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - siz_w);
  c3_w*    cap_w = mat_w;
  u3_road* rod_u = (void*) mat_w;

  memset(rod_u, 0, 4 * siz_w);

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _boot_parts(): build internal tables.
*/
static void
_boot_parts(void)
{
  u3R->cax.har_u = u3_ch_new();
  u3R->jed.har_u = u3_ch_new();
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
  u3_Loom = map_v;
  u3H = (u3_cs_home *)_boot_north(map_v, c3_wiseof(u3_cs_home), len_w);
  u3R = &u3H->rod_u;

  _boot_parts();
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

/* _cm_punt(): crudely print trace.
*/
static void
_cm_punt(void)
{
  u3_noun xat;

  for ( xat = u3R->bug.tax; xat; xat = u3t(xat) ) {
    u3_cm_p("&", u3h(xat));
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

  _cm_punt();
  u3_cv_louse(how_m);

  assert(0);
  if ( c3__meme == how_m ) {
    u3_cm_dump();
  }

  _longjmp(u3R->esc.buf, how_m);
  return how_m;
}
int c3_cooked() { return u3_cm_bail(c3__oops); }

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
    rod_u = _boot_south(u3R->hat_w, 
                        c3_wiseof(u3_cs_road), 
                        (u3R->cap_w - u3R->hat_w));
  } 
  else {
    rod_u = _boot_north(u3R->cap_w, 
                        c3_wiseof(u3_cs_road),
                        (u3R->hat_w - u3R->cap_w));
  }

  c3_assert(0 == u3R->kid_u);
  rod_u->par_u = u3R;
  u3R->kid_u = rod_u;

  u3R = rod_u;
  _boot_parts();
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
  c3_assert(u3R == &u3H->rod_u);

  *low_w = (u3H->rod_u.hat_w - u3H->rod_u.rut_w);
  *hig_w = (u3H->rod_u.mat_w - u3H->rod_u.cap_w) + c3_wiseof(u3_cs_home);
}

/* u3_cm_soft(): system soft wrapper.  unifies unix and nock errors.
**
**  Produces [%$ result] or [%error (list tank)].
*/
u3_noun
u3_cm_soft(c3_w sec_w, u3_funk fun_f, u3_noun arg)
{
  u3_noun why_a;
  u3_noun ton;

  u3_cm_leap();

  if ( u3_no == (why_a = u3_cm_trap()) ) {
    if ( 0 != u3R->net.nyd ) {
      c3_assert(0);   //  XX actually, convert to error
    } 
    else {
      ton = u3nc(why_a, u3R->bug.tax);
    }
    u3_cm_fall();
    ton = u3_ca_gain(ton);
    u3_cm_flog(0);
  }
  else {
    u3_noun pro = fun_f(arg);

    u3_cm_fall();
    ton = u3nc(0, u3_ca_gain(pro));
    u3_cm_flog(0);
  }
  return ton;
}

/* _cm_is_tas(): yes iff som (RETAIN) is @tas.
*/
static c3_o
_cm_is_tas(u3_atom som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3_cr_byte(i_w, som);

    if ( islower(c_c) || 
        (isdigit(c_c) && (0 != i_w) && ((len_w - 1) != i_w))
        || '-' == c_c )
    {
      continue;
    }
    return u3_no;
  }
  return u3_yes;
}

/* _cm_is_ta(): yes iff som (RETAIN) is @ta.
*/
static c3_o
_cm_is_ta(u3_noun som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3_cr_byte(i_w, som);

    if ( (c_c < 32) || (c_c > 127) ) {
      return u3_no;
    }
  }
  return u3_yes;
}

/* _cm_hex(): hex byte.
*/
c3_y _cm_hex(c3_y c_y)
{
  if ( c_y < 10 ) 
    return '0' + c_y; 
  else return 'a' + (c_y - 10);
}

/* _cm_in_pretty: measure/cut prettyprint.
*/
static c3_w
_cm_in_pretty(u3_noun som, c3_o sel_o, c3_c* str_c)
{
  if ( u3_so(u3du(som)) ) {
    c3_w sel_w, one_w, two_w;

    sel_w = 0;
    if ( u3_so(sel_o) ) {
      if ( str_c ) { *(str_c++) = '['; }
      sel_w += 1;
    }

    one_w = _cm_in_pretty(u3h(som), u3_yes, str_c);
    if ( str_c ) {
      str_c += one_w;
      *(str_c++) = ' ';
    }
    two_w = _cm_in_pretty(u3t(som), u3_no, str_c);
    if ( str_c ) { str_c += two_w; }

    if ( u3_so(sel_o) ) {
      if ( str_c ) { *(str_c++) = ']'; }
      sel_w += 1;
    }
    return one_w + two_w + 1 + sel_w;
  } 
  else {
    if ( som < 65536 ) {
      c3_c buf_c[6];
      c3_w len_w;

      snprintf(buf_c, 6, "%d", som);
      len_w = strlen(buf_c);

      if ( str_c ) { strcpy(str_c, buf_c); str_c += len_w; }
      return len_w;
    }
    else {
      c3_w len_w = u3_cr_met(3, som);

      if ( u3_so(_cm_is_tas(som, len_w)) ) {
        c3_w len_w = u3_cr_met(3, som);

        if ( str_c ) {
          *(str_c++) = '%'; 
          u3_cr_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
        }
        return len_w + 1;
      }
      else if ( u3_so(_cm_is_ta(som, len_w)) ) {
        if ( str_c ) {
          *(str_c++) = '\''; 
          u3_cr_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
          *(str_c++) = '\''; 
        }
        return len_w + 2;
      }
      else {
        c3_w len_w = u3_cr_met(3, som);
        c3_c *buf_c = malloc(2 + (2 * len_w) + 1);
        c3_w i_w = 0;
        c3_w a_w = 0;

        buf_c[a_w++] = '0';
        buf_c[a_w++] = 'e';
       
        for ( i_w = 0; i_w < len_w; i_w++ ) {
          c3_y c_y = u3_cr_byte(len_w - (i_w + 1), som);

          if ( (i_w == 0) && (c_y <= 0xf) ) {
            buf_c[a_w++] = _cm_hex(c_y);
          } else {
            buf_c[a_w++] = _cm_hex(c_y >> 4);
            buf_c[a_w++] = _cm_hex(c_y & 0xf);
          }
        }
        buf_c[a_w] = 0;
        len_w = a_w;

        if ( str_c ) { strcpy(str_c, buf_c); str_c += len_w; }

        free(buf_c);
        return len_w;
      }
    }
  }
}

/* u3_cm_pretty(): dumb prettyprint to string.
*/
c3_c* 
u3_cm_pretty(u3_noun som)
{
  c3_w len_w = _cm_in_pretty(som, u3_yes, 0);
  c3_c* pre_c = malloc(len_w + 1);

  _cm_in_pretty(som, u3_yes, pre_c);
  pre_c[len_w] = 0;
  return pre_c;
}

/* u3_cm_p(): dumb print with caption.
*/
void
u3_cm_p(const c3_c* cap_c, u3_noun som)
{
  c3_c* pre_c = u3_cm_pretty(som);

  printf("%s: %s\n", cap_c, pre_c);
  free(pre_c);
}
