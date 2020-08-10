/* j/5/argon2.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* helpers
*/

  static c3_t
  _cqear_unpack_word(c3_w *out, u3_atom in)
  {
    if ( u3r_met(5, in) > 1 ) {
      return 0;
    }
    else {
      *out = u3r_word(0, in);
      return 1;
    }
  }

  static c3_t
  _cqear_unpack_size(size_t *out, u3_atom in)
  {
    c3_w out_w;
    if ( _cqear_unpack_word(&out_w, in) ) {
      *out = out_w;
      return 1;
    }
    else {
      return 0;
    }
  }

  static c3_t
  _cqear_unpack_type(urcrypt_argon2_type *out, u3_atom in)
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

  static c3_y*
  _cqear_unpack_bytes(size_t size, u3_atom in)
  {
    c3_y* out = u3a_malloc(size);
    u3r_bytes(0, (c3_w) size, out, in);
    return out;
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
    size_t out_sz, key_sz, ex_sz, dat_sz, sat_sz;
    c3_w ver_w, ted_w, mem_w, tim_w;
    urcrypt_argon2_type typ_u;

    if ( !(_cqear_unpack_size(&out_sz, out) &&
           _cqear_unpack_type(&typ_u, type) &&
           _cqear_unpack_word(&ver_w, version) &&
           _cqear_unpack_word(&ted_w, threads) &&
           _cqear_unpack_word(&mem_w, mem_cost) &&
           _cqear_unpack_word(&tim_w, time_cost) &&
           _cqear_unpack_size(&key_sz, wik) &&
           _cqear_unpack_size(&ex_sz, wix) &&
           _cqear_unpack_size(&dat_sz, wid) &&
           _cqear_unpack_size(&sat_sz, wis)) ) {
      u3l_log("%s\r\n", "argon2-punt");
      return u3_none;
    }
    else {
      u3_atom ret;
      c3_y *key_y = _cqear_unpack_bytes(key_sz, key),
           *ex_y  = _cqear_unpack_bytes(ex_sz, extra),
           *dat_y = _cqear_unpack_bytes(dat_sz, dat),
           *sat_y = _cqear_unpack_bytes(sat_sz, sat),
           *out_y = u3a_malloc(out_sz);
      const c3_c* err_c = urcrypt_argon2(
          typ_u, ver_w, ted_w, mem_w, tim_w,
          key_sz, key_y,
          ex_sz,  ex_y,
          dat_sz, dat_y,
          sat_sz, sat_y,
          out_sz, out_y);

      u3a_free(key_y);
      u3a_free(ex_y);
      u3a_free(dat_y);
      u3a_free(sat_y);

      if ( NULL == err_c ) {
        ret = u3i_bytes(out_sz, out_y);
      }
      else {
        ret = u3_none;
        u3l_log("argon2-punt: %s\r\n", err_c);
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
      return _cqe_argon2(out, type, version,
                         threads, mem_cost, time_cost,
                         wik, key, wix, extra,
                         wid, dat, wis, sat);
    }
  }
