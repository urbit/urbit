/* j/5/argon2.c
**
*/
#include "all.h"

#include <argon2.h>

/* helpers
*/

  int argon2_alloc(uint8_t** output, size_t bytes)
  {
    size_t words = (bytes+3) / 4;
    *output = u3a_walloc(words);
    return (NULL != output);
  }

  void argon2_free(uint8_t* memory, size_t bytes)
  {
    u3a_wfree(memory);
  }

/* functions
*/

  u3_noun
  u3qe_argon2( // configuration params,
               u3_atom out,
               u3_atom type,
               u3_atom version,
               u3_atom threads,
               u3_atom mem_cost,
               u3_atom time_cost,
               u3_atom wik,
               u3_atom key,
               u3_atom wix,
               u3_atom extra,
               // input params
               u3_atom wid,
               u3_atom dat,
               u3_atom wis,
               u3_atom sat )
  {
    c3_assert( _(u3a_is_cat(out)) &&
               _(u3a_is_cat(type)) &&
               _(u3a_is_cat(version)) &&
               _(u3a_is_cat(threads)) &&
               _(u3a_is_cat(mem_cost)) &&
               _(u3a_is_cat(time_cost)) &&
               _(u3a_is_cat(wik)) &&
               _(u3a_is_cat(wix)) &&
               _(u3a_is_cat(wid)) &&
               _(u3a_is_cat(wis)) );

    // flip endianness for argon2
    key   = u3qc_rev(3, wik, key);
    extra = u3qc_rev(3, wix, extra);
    dat   = u3qc_rev(3, wid, dat);
    sat   = u3qc_rev(3, wis, sat);

    // XX Do these widths come from the user? What if they're wrong?

    // Copy `key` bytes into `key_buf`.
    c3_y key_buf[wik];
    u3r_bytes(0, wik, key_buf, key);

    // Copy `extra` bytes into `extra_buf`.
    c3_y extra_buf[wix];
    u3r_bytes(0, wix, extra_buf, extra);

    // Copy `dat` bytes into `dat_buf`.
    c3_y dat_buf[wid];
    u3r_bytes(0, wid, dat_buf, dat);

    // Copy `sat` bytes into `sat_buf`.
    c3_y sat_buf[wis];
    u3r_bytes(0, wis, sat_buf, sat);

    // Output buffer
    c3_y output_buf[out];

    argon2_context context = {
      .out          = output_buf,          // output array
      .outlen       = out,                 // digest length
      .pwd          = dat_buf,             // password
      .pwdlen       = wid,
      .salt         = sat_buf,             // salt
      .saltlen      = wis,
      .secret       = key_buf,             // optional secret data
      .secretlen    = wik,
      .ad           = extra_buf,           // optional associated data
      .adlen        = wix,
      .t_cost       = time_cost,           // number of passes
      .m_cost       = mem_cost,            // KB of memory requested
      .lanes        = threads,             // number of lanes
      .threads      = 1,                   // maximum number of threads
      .version      = version,             // algorithm version number
      .allocate_cbk = argon2_alloc,        // memory allocator
      .free_cbk     = argon2_free,         // memory deallocator
      .flags        = ARGON2_DEFAULT_FLAGS // don't clear secret or password
    };

    //
    // XX Temporary hack.
    //
    // What's the proper way to do this?
    //
    // The thing we need to do here, is to mask SIGPROF in the forked
    // threads. I guess we should do that RIGHT before we fork and undo
    // the block it RIGHT after, otherwise there will be undelivered SIGPROF
    // signals queued, which will give invalid profiling results.
    //
    // XX Disabled hack for a sec, since this is running without threads anyways.
    //
    // u3t_boff();

    int argon_res;
    switch ( type ) {
      default:
        fprintf(stderr, "\nunjetted argon2 variant %i\n", type);
        u3m_bail(c3__exit);
        break;
      //
      case c3__d:
        argon_res = argon2d_ctx(&context);
        break;
      //
      case c3__i:
        argon_res = argon2i_ctx(&context);
        break;
      //
      case c3__id:
        argon_res = argon2id_ctx(&context);
        break;
      //
      case c3__u:
        argon_res = argon2u_ctx(&context);
        break;
    }

    //
    // XX Temporary hack.
    //
    // XX Disabled hack for a sec, since this is running without threads anyways.
    //
    // u3t_boot();

    if ( ARGON2_OK != argon_res ) {
      fprintf(stderr, "\nargon2 error: %s\n", argon2_error_message(argon_res));
      u3m_bail(c3__exit);
    }

    u3z(key); u3z(extra); u3z(dat); u3z(sat);

    // Copy result from the stack.
    u3_noun result = u3i_bytes(out, output_buf);

    // XX Do we need to free `result`?
    return u3kc_rev(3, out, result);
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
      return u3qe_argon2(out, type, version,
                         threads, mem_cost, time_cost,
                         wik, key, wix, extra,
                         wid, dat, wis, sat);
    }
  }
