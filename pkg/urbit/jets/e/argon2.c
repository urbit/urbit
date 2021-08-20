/* j/5/argon2.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* helpers
*/

  static int
  argon2_alloc(uint8_t** output, size_t bytes)
  {
    *output = u3a_malloc(bytes);
    return 1;
  }

  static void
  argon2_free(uint8_t* memory, size_t bytes)
  {
    u3a_free(memory);
  }

  static c3_t
  _cqear_unpack_type(c3_y* out, u3_atom in)
  {
    switch ( in ) {
      default:
        return 0;
      case c3__d:
        *out = urcrypt_argon2_d;
        return 1;
      case c3__i:
        *out = urcrypt_argon2_i;
        return 1;
      case c3__id:
        *out = urcrypt_argon2_id;
        return 1;
      case c3__u:
        *out = urcrypt_argon2_u;
        return 1;
    }
  }

/* functions
*/

  static u3_atom
  _cqe_argon2( // configuration params,
               u3_atom out, u3_atom type, u3_atom version,
               u3_atom threads, u3_atom mem_cost, u3_atom time_cost,
               u3_atom wik, u3_atom key, u3_atom wix, u3_atom extra,
               // input params
               u3_atom wid, u3_atom dat, u3_atom wis, u3_atom sat )
  {
    c3_y typ_u;
    c3_w out_w, wik_w, wix_w, wid_w, wis_w, ver_w, ted_w, mem_w, tim_w;

    if ( !(u3r_word_fit(&out_w, out) &&
           u3r_word_fit(&wik_w, wik) &&
           u3r_word_fit(&wix_w, wix) &&
           u3r_word_fit(&wid_w, wid) &&
           u3r_word_fit(&wis_w, wis)) ) {
      // too big to allocate
      return u3m_bail(c3__fail);
    }
    else if ( !(_cqear_unpack_type(&typ_u, type) &&
                u3r_word_fit(&ver_w, version) &&
                u3r_word_fit(&ted_w, threads) &&
                u3r_word_fit(&mem_w, mem_cost) &&
                u3r_word_fit(&tim_w, time_cost)) ) {
      return u3_none;
    }
    else {
      u3_atom ret;
      c3_y *key_y = u3r_bytes_alloc(0, wik_w, key),
           *ex_y  = u3r_bytes_alloc(0, wix_w, extra),
           *dat_y = u3r_bytes_alloc(0, wid_w, dat),
           *sat_y = u3r_bytes_alloc(0, wis_w, sat),
           *out_y = u3a_malloc(out_w);

      const c3_c* err_c = urcrypt_argon2(
          typ_u, ver_w, ted_w, mem_w, tim_w,
          wik_w, key_y,
          wix_w,  ex_y,
          wid_w, dat_y,
          wis_w, sat_y,
          out_w, out_y,
          &argon2_alloc,
          &argon2_free);

      u3a_free(key_y);
      u3a_free(ex_y);
      u3a_free(dat_y);
      u3a_free(sat_y);

      if ( NULL == err_c ) {
        ret = u3i_bytes(out_w, out_y);
      }
      else {
        ret = u3_none;
        u3l_log("argon2-error: %s", err_c);
      }

      u3a_free(out_y);
      return ret;
    }
  }

  u3_noun
  u3we_argon2(u3_noun cor)
  {
    u3_noun // configuration params
            out, type, version,
            threads, mem_cost, time_cost,
            wik, key, wix, extra,
            // input params
            wid, dat, wis, sat,
            // for use during unpacking
            wmsg, wsat, arg, brg, wkey, wext;

    // the hoon code for argon2 takes configuration parameters,
    // and then produces a gate. we jet that inner gate.
    // this does mean that the config params have gotten buried
    // pretty deep in the subject, hence the +510.
    if ( c3n == u3r_mean(cor, u3x_sam_2, &wmsg,
                              u3x_sam_3, &wsat,
                              510, &arg, 0) ||
                u3r_cell(wmsg, &wid, &dat) || u3ud(wid) || u3ud(dat) ||
                u3r_cell(wsat, &wis, &sat) || u3ud(wis) || u3ud(sat) ||
                //
                u3r_qual(arg, &out, &type, &version, &brg) ||
                u3ud(out) || u3ud(type) || u3ud(version) ||
                //
                u3r_qual(brg, &threads, &mem_cost, &time_cost, &arg) ||
                u3ud(threads) || u3ud(mem_cost) || u3ud(time_cost) ||
                //
                u3r_cell(arg, &wkey, &wext) ||
                u3r_cell(wkey, &wik, &key) || u3ud(wik) || u3ud(key) ||
                u3r_cell(wext, &wix, &extra) || u3ud(wix) || u3ud(extra)
       )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3l_punt("argon2",
          _cqe_argon2(out, type, version,
                      threads, mem_cost, time_cost,
                      wik, key, wix, extra,
                      wid, dat, wis, sat));
    }
  }
