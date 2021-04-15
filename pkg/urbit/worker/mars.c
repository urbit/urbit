/* worker/mar.c
**
**  the main loop of a serf process.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include <vere/vere.h>
#include <vere/db/lmdb.h>

/*
::  peek=[gang (each path $%([%once @tas @tas path] [%beam @tas beam]))]
::  ovum=ovum
::
|$  [peek ovum]
|%
::  +task: from urth to mars
::
::    next steps:
::    - |mass should be a query of the serf directly
::    - add duct or vane stack for spinner
::
+$  task
  $%  [%live ?(%meld %pack) ~] :: XX rename
      [%exit ~]
      [%peek mil=@ peek]
      [%poke mil=@ ovum]  ::  XX replacement y/n
      [%sync ?(%cram %save) ~] :: XX remove cram?
  ==
::  +gift: from mars to urth
::
+$  gift
  $%  [%live ~]
      [%flog cord]
      [%slog pri=@ tank]
      [%peek p=(each (unit (cask)) goof)]
      [%poke p=(each (list ovum) (list goof))]
      [%ripe [pro=%2 kel=wynn] [who=@p fake=?] eve=@ mug=@]
      [%sync eve=@ mug=@]
  ==
--
*/

struct _cd_save {
  c3_o             ret_o;               //  result
  c3_d             eve_d;               //  first event
  c3_d             len_d;               //  number of events
  c3_y**           byt_y;               //  array of bytes
  size_t*          siz_i;               //  array of lengths
  struct _u3_disk* log_u;
  struct _u3_mars* mar_u;
};

// static u3_mars u3M;


static void
_cw_mars_flush(u3_mars* mar_u);

static void
_cw_mars_commit(u3_mars* mar_u);

static void
_cw_mars_commit_after_cb(uv_work_t* ted_u, c3_i sas_i)
{
  struct _cd_save* req_u = ted_u->data;

  if ( UV_ECANCELED == sas_i ) {
    c3_free(req_u->byt_y);
    c3_free(req_u->siz_i);
    c3_free(req_u);
  }
  else {
    u3_disk* log_u = req_u->log_u;
    u3_mars* mar_u = req_u->mar_u;

    ted_u->data  = 0;
    log_u->ted_o = c3n;

    //  XX write failed, fatal error
    //
    if ( c3n == req_u->ret_o ) {
      fprintf(stderr, "mars: commit fail\r\n");
      exit(1);
    }

    log_u->dun_d = req_u->eve_d + (req_u->len_d - 1ULL);

    {
      cw_fact* fac_u = mar_u->fac_u.ext_u;

      while ( fac_u && (fac_u->eve_d <= log_u->dun_d) ) {
        mar_u->fac_u.ext_u = fac_u->nex_u;
        c3_free(fac_u->hun_y);
        c3_free(fac_u);
        fac_u = mar_u->fac_u.ext_u;
      }
    }

    if ( !mar_u->fac_u.ext_u || !mar_u->fac_u.ext_u->nex_u ) {
      mar_u->fac_u.ent_u = mar_u->fac_u.ext_u;
    }

    c3_free(req_u->byt_y);
    c3_free(req_u->siz_i);
    c3_free(req_u);

    _cw_mars_commit(mar_u);
    _cw_mars_flush(mar_u);
  }
}

/* _cw_mars_commit_cb(): off the main thread, write event-batch.
*/
static void
_cw_mars_commit_cb(uv_work_t* ted_u)
{
  struct _cd_save* req_u = ted_u->data;
  req_u->ret_o = u3_lmdb_save(req_u->log_u->mdb_u,
                              req_u->eve_d,
                              req_u->len_d,
                              (void**)req_u->byt_y,
                              req_u->siz_i);
}

static void
_cw_mars_commit(u3_mars* mar_u)
{
  u3_disk* log_u = mar_u->log_u;

  if (  (c3n == log_u->ted_o)
     && (mar_u->dun_d > log_u->dun_d) )
  {
    cw_fact* fac_u = mar_u->fac_u.ext_u;
    c3_d     len_d = log_u->sen_d - log_u->dun_d;

    //  XX one allocation per save (or static lifetime with max length)
    //
    struct _cd_save* req_u = c3_malloc(sizeof(*req_u));
    req_u->mar_u = mar_u;
    req_u->log_u = log_u;
    req_u->ret_o = c3n;
    req_u->eve_d = log_u->dun_d + 1;
    req_u->len_d = len_d;
    req_u->byt_y = c3_malloc(len_d * sizeof(c3_y*));
    req_u->siz_i = c3_malloc(len_d * sizeof(size_t));

    // fprintf(stderr, "mars: commit batch %" PRIu64 " from %" PRIu64 "\r\n", len_d, log_u->dun_d+1);

    for ( c3_d i_d = 0ULL; i_d < len_d; ++i_d) {
      c3_assert( fac_u );
      c3_assert( (req_u->eve_d + i_d) == fac_u->eve_d );
      req_u->siz_i[i_d] = fac_u->len_i;
      req_u->byt_y[i_d] = fac_u->hun_y;

      fac_u = fac_u->nex_u;
    }

    //  XX should unlink from mar_u here
    //

    log_u->ted_o = c3y;
    log_u->ted_u.data = req_u;

    //  queue asynchronous work to happen on another thread
    //
    //  XX need the loop here
    //
    uv_queue_work(u3L, &log_u->ted_u, _cw_mars_commit_cb,
                                      _cw_mars_commit_after_cb);
  }
}

static void
_cw_mars_fact(u3_mars* mar_u,
              u3_noun    job,
              u3_noun    pro)
{
  {
    cw_fact* fac_u = c3_malloc(sizeof(*fac_u));
    fac_u->eve_d = mar_u->dun_d;
    fac_u->len_i = (size_t)u3_disk_etch(mar_u->log_u, job, mar_u->mug_l, &fac_u->hun_y);
    fac_u->nex_u = 0;

    if ( !mar_u->fac_u.ent_u ) {
      c3_assert( !mar_u->fac_u.ext_u );
      mar_u->fac_u.ent_u = mar_u->fac_u.ext_u = fac_u;
    }
    else {
      mar_u->fac_u.ent_u->nex_u = fac_u;
      mar_u->fac_u.ent_u = fac_u;
    }

    mar_u->log_u->sen_d++;

    _cw_mars_commit(mar_u);
    u3z(job);
  }

  {
    cw_gift* gif_u = c3_malloc(sizeof(*gif_u));
    gif_u->nex_u = 0;
    gif_u->sat_e = _cwe_gift_poke;
    gif_u->eve_d = mar_u->dun_d;

    u3s_jam_xeno(pro, &gif_u->len_d, &gif_u->hun_y);
    u3z(pro);

    if ( !mar_u->gif_u.ent_u ) {
      c3_assert( !mar_u->gif_u.ext_u );
      mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = gif_u;
    }
    else {
      mar_u->gif_u.ent_u->nex_u = gif_u;
      mar_u->gif_u.ent_u = gif_u;
    }
  }
}

static void
_cw_mars_gift(u3_mars* mar_u, u3_noun pro)
{
  cw_gift* gif_u = c3_malloc(sizeof(*gif_u));
  gif_u->nex_u = 0;
  gif_u->sat_e = _cwe_gift_rest;
  gif_u->ptr_v = 0;

  u3s_jam_xeno(pro, &gif_u->len_d, &gif_u->hun_y);
  u3z(pro);

  if ( !mar_u->gif_u.ent_u ) {
    c3_assert( !mar_u->gif_u.ext_u );
    mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = gif_u;
  }
  else {
    mar_u->gif_u.ent_u->nex_u = gif_u;
    mar_u->gif_u.ent_u = gif_u;
  }
}

/* _serf_make_crud():
*/
static u3_noun
_serf_make_crud(u3_noun job, u3_noun dud)
{
  u3_noun now, ovo, new;
  u3x_cell(job, &now, &ovo);

  new = u3nt(u3i_vint(u3k(now)),
             u3nt(u3_blip, c3__arvo, u3_nul),
             u3nt(c3__crud, dud, u3k(ovo)));

  u3z(job);
  return new;
}

static c3_o
_cw_mars_poke(c3_w     mil_w,
              u3_noun*   eve,
              u3_noun*   pro)
{
  u3_noun dat = u3_poke_sure(mil_w, u3k(*eve));

  if ( c3y == u3h(dat) ) {
    *pro = dat;
    return c3y;
  }
  else {
    u3_noun dud = u3k(u3t(dat));

    u3z(dat);
    *eve = _serf_make_crud(*eve, u3k(dud));
    dat  = u3_poke_sure(mil_w, u3k(*eve));  

    if ( c3y == u3h(dat) ) {
      *pro = dat;
      return c3y;
    }
    else {
      *pro = u3nt(c3n, dud, u3k(u3t(dat)));
      u3z(dat);
      return c3n;
    }
  }
}


static c3_o
_cw_mars_work(u3_mars* mar_u, u3_noun jar)
{
  u3_noun tag, dat, pro;

  if ( c3n == u3r_cell(jar, &tag, &dat) ) {
    fprintf(stderr, "mars: fail a\r\n");
    u3z(jar);
    return c3n;
  }

  switch ( tag ) {
    default: {
      fprintf(stderr, "mars: fail b\r\n");
      u3z(jar);
      return c3n;
    }

    case c3__poke: {
      u3_noun tim, job;
      c3_w  mil_w;

      if ( (c3n == u3r_cell(dat, &tim, &job)) ||
           (c3n == u3r_safe_word(tim, &mil_w)) )
      {
        fprintf(stderr, "mars: poke fail\r\n");
        u3z(jar);
        return c3n;
      }

      //  XX timestamp
      //
      {
        u3_noun now;
        struct timeval tim_u;
        gettimeofday(&tim_u, 0);

        now   = u3_time_in_tv(&tim_u);
        job = u3nc(now, u3k(job));
      }
      u3z(jar);

      mar_u->sen_d++;

      if ( c3y == _cw_mars_poke(mil_w, &job, &pro) ) {
        mar_u->dun_d = mar_u->sen_d;
        mar_u->mug_l = u3r_mug(u3A->roc);

        //  XX process effects
        //
        _cw_mars_fact(mar_u, job, u3nc(c3__poke, pro));
      }
      else {
        mar_u->sen_d = mar_u->dun_d;
        u3z(job);
        _cw_mars_gift(mar_u, u3nc(c3__poke, pro));
      }

      c3_assert( mar_u->dun_d == u3A->eve_d );
    } break;

    case c3__peek: {
      u3_noun tim, sam, pro;
      c3_w  mil_w;

      if ( (c3n == u3r_cell(dat, &tim, &sam)) ||
           (c3n == u3r_safe_word(tim, &mil_w)) )
      {
        u3z(jar);
        return c3n;
      }

      u3k(sam); u3z(jar);
      _cw_mars_gift(mar_u, u3nc(c3__peek, u3v_soft_peek(mil_w, sam)));
    } break;

    // XX remove /support cram?
    case c3__sync: {
      u3_noun nul;

      if (  (c3n == u3r_p(dat, c3__save, &nul))
         || (u3_nul != nul) )
      {
        u3z(jar);
        return c3n;
      }

      mar_u->sat_e = _cwe_mars_save;
    } break;

    //  $%  [%live ?(%meld %pack) ~] :: XX rename
    //
    case c3__live: {
      u3_noun com, nul;

      if ( (c3n == u3r_cell(dat, &com, &nul)) ||
           (u3_nul != nul) )
      {
        u3z(jar);
        return c3n;
      }

      switch ( com ) {
        default: {
          u3z(jar);
          return c3n;
        }

        case c3__pack: {
          u3z(jar);
          u3a_print_memory(stderr, "serf: pack: gained", u3m_pack());
        } break;

        case c3__meld: {
          u3z(jar);
          u3u_meld();
        } break;
      }

      _cw_mars_gift(mar_u, u3nc(c3__live, u3_nul));
    } break;

    case c3__exit: {
      mar_u->sat_e = _cwe_mars_exit;
      fprintf(stderr, "mars set exit\r\n");
    } break;
  }

  return c3y;
}

static u3_weak
_cw_mars_cue(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  u3_weak jar;

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'B');
#endif

  jar = u3s_cue_xeno_with(mar_u->sil_u, len_d, hun_y);

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'E');
#endif

  return jar;
}

static void
_cw_mars_flush(u3_mars* mar_u)
{
top:
  {
    cw_gift* gif_u = mar_u->gif_u.ext_u;

    //  XX gather i/o
    //
    while (  gif_u
          && (  (_cwe_gift_rest == gif_u->sat_e)
             || (gif_u->eve_d <= mar_u->log_u->dun_d)) )
    {
      u3_newt_send(mar_u->out_u, gif_u->len_d, gif_u->hun_y);

      mar_u->gif_u.ext_u = gif_u->nex_u;
      c3_free(gif_u);
      gif_u = mar_u->gif_u.ext_u;
    }

    if ( !mar_u->gif_u.ext_u ) {
      mar_u->gif_u.ent_u = 0;
    }
  }

  if (  (_cwe_mars_work != mar_u->sat_e)
     && (mar_u->log_u->dun_d == mar_u->dun_d) )
  {
    if ( _cwe_mars_save == mar_u->sat_e ) {
      u3e_save();
      _cw_mars_gift(mar_u,
        u3nt(c3__sync, u3i_chubs(1, &mar_u->dun_d), mar_u->mug_l));
      mar_u->sat_e = _cwe_mars_work;
      goto top;
    }
    else if ( _cwe_mars_exit == mar_u->sat_e ) {
      //  XX exit cb ?
      //
      u3e_save();
      fprintf(stderr, "mars: exit\r\n");
      exit(0);
    }
  }
}

c3_o
u3_mars_kick(u3_mars* mar_u, c3_d len_d, c3_y* hun_y)
{
  c3_o ret_o = c3n;

  //  XX optimize for save/cram w/ peek-next
  //
  if ( _cwe_mars_work == mar_u->sat_e ) {
    u3_weak jar = _cw_mars_cue(mar_u, len_d, hun_y);

    //  parse errors are fatal
    // 
    if (  (u3_none == jar)
       || (c3n == _cw_mars_work(mar_u, jar)) )
    {
      fprintf(stderr, "mars: bad\r\n");
      //  XX error cb?
      //
      exit(1);
    }
    
    //  XX u3_serf_post()
    //
    // _cw_serf_step_trace();

    ret_o = c3y;
  }

  _cw_mars_flush(mar_u);

  return ret_o;
}

u3_mars*
u3_mars_init(u3_disk* log_u,
             u3_moat* inn_u,
             u3_mojo* out_u,
             c3_c*    dir_c,
             u3_cue_xeno* sil_u)
{
  if ( log_u->dun_d != u3A->eve_d ) {
    fprintf(stderr, "mars: init ret\r\n");
    return 0;
  }
  else {
    c3_assert( log_u->sen_d == log_u->dun_d );

    u3_mars* mar_u = c3_malloc(sizeof(*mar_u));
    mar_u->sil_u = sil_u;
    mar_u->log_u = log_u;
    mar_u->inn_u = inn_u;
    mar_u->out_u = out_u;
    mar_u->sen_d = mar_u->dun_d = log_u->dun_d;
    mar_u->mug_l = u3r_mug(u3A->roc);
    mar_u->pac_o = mar_u->rec_o = mar_u->mut_o = c3n;
    mar_u->sac   = u3_nul;
    mar_u->sat_e = _cwe_mars_work;
    mar_u->fac_u.ent_u = mar_u->fac_u.ext_u = 0;
    mar_u->gif_u.ent_u = mar_u->gif_u.ext_u = 0;
    mar_u->xit_f = 0;

    return mar_u;
  }
}
