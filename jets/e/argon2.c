/* j/5/argon2.c
**
*/
#include "all.h"

#include <argon2.h>

/* functions
*/

  u3_noun
  u3qe_argon2(// configuration params,
              u3_atom out, u3_atom type, u3_atom version,
              u3_atom threads, u3_atom mem_cost, u3_atom time_cost,
              u3_atom wik, u3_atom key, u3_atom wix, u3_atom extra,
              // input params
              u3_atom wid, u3_atom dat, u3_atom wis, u3_atom sat)
  {
    c3_assert(_(u3a_is_cat(out)) && _(u3a_is_cat(type)) &&
              _(u3a_is_cat(version)) && _(u3a_is_cat(threads)) &&
              _(u3a_is_cat(mem_cost)) && _(u3a_is_cat(time_cost)) &&
              _(u3a_is_cat(wik)) && _(u3a_is_cat(wix)) &&
              _(u3a_is_cat(wid)) && _(u3a_is_cat(wis)));

    // flip endianness for argon2
    key = u3qc_rev(3, wik, key);
    extra = u3qc_rev(3, wix, extra);
    dat = u3qc_rev(3, wid, dat);
    sat = u3qc_rev(3, wis, sat);

    // atoms to byte arrays
    c3_y bytes_key[wik];
    u3r_bytes(0, wik, bytes_key, key);
    c3_y bytes_extra[wix];
    u3r_bytes(0, wix, bytes_extra, extra);
    c3_y bytes_dat[wid];
    u3r_bytes(0, wid, bytes_dat, dat);
    c3_y bytes_sat[wis];
    u3r_bytes(0, wis, bytes_sat, sat);

    c3_y outhash[out];
    argon2_context context = {
      outhash,             // output array, at least [digest length] in size
      out,                 // digest length
      bytes_dat,           // password array
      wid,                 // password length
      bytes_sat,           // salt array
      wis,                 // salt length
      bytes_key, wik,      // optional secret data
      bytes_extra, wix,    // optional associated data
      time_cost, mem_cost, threads, threads, // performance cost configuration
      version,             // algorithm version
      NULL, NULL,          // custom memory allocation / deallocation functions
      ARGON2_DEFAULT_FLAGS // by default only internal memory is cleared
    };

    int argon_res;
    switch(type)
    {
        case 100:   // %d
      argon_res = argon2d_ctx(&context);
      break;
      //
        case 105:   // %i
      argon_res = argon2i_ctx(&context);
      break;
      //
        case 117:   // %u
      argon_res = argon2u_ctx(&context);
      break;
      //
        case 25705: // %id
      argon_res = argon2id_ctx(&context);
      break;
      //
        default:
      fprintf(stderr, "\nunjetted argon2 variant %i\n", type);
      u3m_bail(c3__exit);
    }

    if(ARGON2_OK != argon_res) {
      fprintf(stderr, "\nargon2 error: %s\n", argon2_error_message(argon_res));
      u3m_bail(c3__exit);
    }

    u3z(key); u3z(extra); u3z(dat); u3z(sat);
    return u3kc_rev(3, out, u3i_bytes(out, outhash));
  }

  u3_noun
  u3we_argon2(u3_noun cor)
  {
    u3_noun // configuration params
            out, type, version,
            threads, mem_cost, time_cost,
            wik, key, wix, extra,
            // input params
            wid, dat, wis, sat;

    // the hoon code for argon2 takes configuration parameters,
    // and then produces a gate. we jet that inner gate.
    // this does mean that the config params have gotten buried
    // pretty deep in the subject, so we have to use large
    // tree addresses to get at them.
    if ( (c3n == u3r_mean(cor, u3x_sam_4, &wid,
                               u3x_sam_5, &dat,
                               u3x_sam_6, &wis,
                               u3x_sam_7, &sat,
                             //
                               1020,      &out,
                               2042,      &type,
                               4086,      &version,
                             //
                               8174,      &threads,
                               16350,     &mem_cost,
                               32702,     &time_cost,
                             //
                               130812,    &wik,
                               130813,    &key,
                               130814,    &wix,
                               130815,    &extra, 0)) ||
         (c3n == u3ud(out)) ||
         (c3n == u3ud(type)) ||
         (c3n == u3ud(version)) ||
         (c3n == u3ud(threads)) ||
         (c3n == u3ud(mem_cost)) ||
         (c3n == u3ud(time_cost)) ||
         (c3n == u3ud(wik)) ||
         (c3n == u3ud(key)) ||
         (c3n == u3ud(wix)) ||
         (c3n == u3ud(extra)) ||
         (c3n == u3ud(wid)) ||
         (c3n == u3ud(dat)) ||
         (c3n == u3ud(wis)) ||
         (c3n == u3ud(sat))
       )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qe_argon2(out, type, version,
                         threads, mem_cost, time_cost,
                         wik, key, wix, extra,
                         wid, dat, wis, sat);
    }
  }
