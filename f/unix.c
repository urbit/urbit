/* f/unix.c
**
** This file is in the public domain.
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include "all.h"

/* _unix_term(): u2_yes iff `tat` should be printed as a term.
*/
static u2_bean
_unix_term(u2_atom tat)
{
  c3_w met_w = u2_met(3, tat);

  if ( met_w >= 2 ) {
    c3_y *buf_y = alloca(met_w);
    c3_w i_w;

    u2_bytes(0, met_w, buf_y, tat);

    for ( i_w=0; i_w < met_w; i_w++ ) {
      if ( ((buf_y[i_w] < 'a') || (buf_y[i_w] > 'x')) && (buf_y[i_w] != '-') ) {
        return u2_no;
      }
    }
    return u2_yes;
  }
  else return u2_no; 
}

static void _unix_dump(FILE*, u2_noun);

/* _unix_dump_in(): dump in cell to file.
*/
static void
_unix_dump_in(FILE*   fil,
              u2_noun som)
{
  if ( u2_no == u2_dust(som) ) {
    _unix_dump(fil, som);
  }
  else {
    _unix_dump(fil, u2_h(som));
    fprintf(fil, " ");
    _unix_dump_in(fil, u2_t(som));
  }
}

/* _unix_dump(): dump noun to file.
*/
static void
_unix_dump(FILE*   fil,
           u2_noun som)
{
  if ( u2_no == u2_dust(som) ) {
    mpz_t amp;

    if ( u2_yes == _unix_term(som) ) {
      c3_w met_w = u2_met(3, som);
      c3_y *buf_y = alloca(met_w + 1);

      u2_bytes(0, met_w, buf_y, som);
      buf_y[met_w] = 0;
      fprintf(fil, "%%%s", buf_y);
    }
    else {
      u2_mp(amp, som);
      gmp_fprintf(fil, "%Zd", amp);
      mpz_clear(amp);
    }
  }
  else {
    fputc('[', fil);
    _unix_dump(fil, u2_h(som));
    fprintf(fil, " ");
    _unix_dump_in(fil, u2_t(som));
    fputc(']', fil);
  }
}

static u2_noun _unix_scan(u2_wire, FILE*);

/* _unix_scan_cell(): scan cell or tuple.
*/
static u2_noun
_unix_scan_cell(u2_wire wir_r,
                FILE*   fil)
{
  u2_noun hed = _unix_scan(wir_r, fil);
  c3_i    c   = fgetc(fil);

  if ( c == ' ' ) {
    u2_noun tal = _unix_scan_cell(wir_r, fil);

    return u2_bn_cell(wir_r, hed, tal);
  }
  else { 
    c3_assert(c == ']');
    return hed;
  }
}

/* _unix_scan(): scan noun from file.
*/
static u2_noun
_unix_scan(u2_wire wir_r,
           FILE*   fil)
{
  c3_i c = fgetc(fil);

  if ( c == '[' ) {
    return _unix_scan_cell(wir_r, fil);
  } 
  else if ( c == '%' )  {
    c3_c buf[1025];

    fscanf(fil, "%1024[a-z-]", buf);
    return u2_bn_string(wir_r, buf);
  }
  else {
    mpz_t amp;

    ungetc(c, fil);
    mpz_init(amp);
    gmp_fscanf(fil, "%Zd", amp);

    return u2_bn_mp(wir_r, amp);
  }
}

/* u2_ux_read(): read a filesystem path/extension into an atom.
*/
u2_weak
u2_ux_read(u2_ray      wir_r,
           const c3_c* paf_c,
           const c3_c* ext_c)
{
  c3_w  len_w;
  c3_c* nam_c;

  if ( ext_c ) 
    len_w = strlen(paf_c) + 1 + strlen(ext_c);
  else len_w = strlen(paf_c);
  
  nam_c = alloca(len_w + 1);
  if ( ext_c ) {
    snprintf(nam_c, len_w + 1, "%s.%s", paf_c, ext_c);
  } else snprintf(nam_c, len_w + 1, "%s", paf_c);

  {
    c3_i        fid_i;
    struct stat sat_s;
    c3_w        fln_w;
    c3_c*       fil_c;
    u2_atom     fil;

    fid_i = open(nam_c, O_RDONLY, 0666);
    if ( (fid_i < 0) || (fstat(fid_i, &sat_s) < 0) ) {
      return u2_none;
    }

    fln_w = sat_s.st_size;
    fil_c = malloc(sat_s.st_size);

    if ( fln_w != read(fid_i, fil_c, fln_w) ) {
      return u2_none;
    }
    close(fid_i);

    fil = u2_rl_bytes(wir_r, fln_w, (c3_y *)fil_c); 
    free(fil_c);

    return fil;
  }
}

/* u2_ux_read_deep(): read a filesystem path as a generic noun.
*/
u2_weak
u2_ux_read_deep(u2_wire     wir_r,
                const c3_c* paf_c,
                const c3_c* ext_c)
{ 
  c3_w  len_w;
  c3_c* nam_c;

  if ( ext_c ) 
    len_w = strlen(paf_c) + 1 + strlen(ext_c);
  else len_w = strlen(paf_c);
  
  nam_c = alloca(len_w + 1);
  if ( ext_c ) {
    snprintf(nam_c, len_w + 1, "%s.%s", paf_c, ext_c);
  } else snprintf(nam_c, len_w + 1, "%s", paf_c);

  {
    FILE*   fil;
    u2_noun som;

    if ( !(fil = fopen(nam_c, "r")) ) {
      return u2_none;
    }
    som = _unix_scan(wir_r, fil);
    fclose(fil);
    return som;
  }
}

/* u2_ux_write(): write a path/extension as an atom.
*/
u2_bean
u2_ux_write(u2_wire     wir_r,
            u2_atom     som,
            const c3_c* paf_c,
            const c3_c* ext_c)
{
  c3_w  len_w;
  c3_c* nam_c;

  if ( ext_c ) 
    len_w = strlen(paf_c) + 1 + strlen(ext_c);
  else len_w = strlen(paf_c);
  
  nam_c = alloca(len_w + 1);
  if ( ext_c ) {
    snprintf(nam_c, len_w + 1, "%s.%s", paf_c, ext_c);
  } else snprintf(nam_c, len_w + 1, "%s", paf_c);

  {
    c3_i    fid_i;
    c3_w    fln_w;
    c3_y*   fil_y;

    fid_i = open(nam_c, O_WRONLY | O_CREAT, 0666);
    if ( fid_i < 0 ) {
      return u2_no;
    }
    fln_w = u2_met(3, som);
    fil_y = malloc(fln_w);
    u2_bytes(0, fln_w, fil_y, som);

    if ( fln_w != write(fid_i, fil_y, fln_w) ) {
      return u2_no;
    }
    close(fid_i);

    return u2_yes;
  }
}

/* u2_ux_write_deep(): write a path/extension as a generic noun.
*/
u2_bean
u2_ux_write_deep(u2_wire     wir_r,
                 u2_noun     som,
                 const c3_c* paf_c,
                 const c3_c* ext_c)
{
  c3_w  len_w;
  c3_c* nam_c;

  if ( ext_c ) 
    len_w = strlen(paf_c) + 1 + strlen(ext_c);
  else len_w = strlen(paf_c);
  
  nam_c = alloca(len_w + 1);
  if ( ext_c ) {
    snprintf(nam_c, len_w + 1, "%s.%s", paf_c, ext_c);
  } else snprintf(nam_c, len_w + 1, "%s", paf_c);

  {
    FILE*   fil;

    if ( !(fil = fopen(nam_c, "w")) ) {
      return u2_no;
    }
    _unix_dump(fil, som);
    fclose(fil);

    return u2_yes;
  }
}

/* u2_ux_fresh(): true iff `oxt` is as fresh as `ext`.
*/
u2_bean
u2_ux_fresh(const c3_c* paf_c,
            const c3_c* ext_c,
            const c3_c* oxt_c)
{
  c3_w  nam_w = strlen(paf_c) + 1 + strlen(ext_c);
  c3_w  nom_w = strlen(paf_c) + 1 + strlen(oxt_c);
  c3_c* nam_c = alloca(nam_w + 1);
  c3_c* nom_c = alloca(nom_w + 1);
  struct stat nam_stat, nom_stat;

  snprintf(nam_c, nam_w + 1, "%s.%s", paf_c, ext_c);
  snprintf(nom_c, nom_w + 1, "%s.%s", paf_c, oxt_c);

  if ( stat(nam_c, &nam_stat) < 0 ) {
    return u2_no;
  }
  else {
    if ( (stat(nom_c, &nom_stat) < 0) ||
#if defined(U2_OS_linux)
         (nam_stat.st_mtime > nom_stat.st_mtime)
#elif defined(U2_OS_osx)
         (nam_stat.st_mtimespec.tv_sec > nom_stat.st_mtimespec.tv_sec) ||
         ((nam_stat.st_mtimespec.tv_sec == (nam_stat.st_mtimespec.tv_sec)) &&
          (nam_stat.st_mtimespec.tv_nsec > nom_stat.st_mtimespec.tv_nsec))
#elif defined(U2_OS_bsd)
         (nam_stat.st_mtim.tv_sec > nom_stat.st_mtim.tv_sec) ||
         ((nam_stat.st_mtim.tv_sec == (nom_stat.st_mtim.tv_sec)) &&
          (nam_stat.st_mtim.tv_nsec > nom_stat.st_mtim.tv_nsec))
#else
         #error "port: file time compare"
#endif
         ) {
      return u2_no;
    }
    else return u2_yes;
  }
}
