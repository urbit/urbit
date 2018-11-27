// XX temporarily preserved for pre-boot validation
// to be ported to king.c

/* v/sist.c
**
*/
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <curl/curl.h>
#include <uv.h>

#include "all.h"
#include "vere/vere.h"

#if defined(U3_OS_linux)
#include <stdio_ext.h>
#define fpurge(fd) __fpurge(fd)
#define DEVRANDOM "/dev/urandom"
#else
#define DEVRANDOM "/dev/random"
#endif


/* _sist_curl_alloc(): allocate a response buffer for curl
*/
static size_t
_sist_curl_alloc(void* dat_v, size_t uni_t, size_t mem_t, uv_buf_t* buf_u)
{
  size_t siz_t = uni_t * mem_t;
  buf_u->base = realloc(buf_u->base, 1 + siz_t + buf_u->len);

  if ( 0 == buf_u->base ) {
    fprintf(stderr, "out of memory\n");
    u3_lo_bail();
  }

  memcpy(buf_u->base + buf_u->len, dat_v, siz_t);
  buf_u->len += siz_t;
  buf_u->base[buf_u->len] = 0;

  return siz_t;
}

/* _sist_post_json(): POST JSON to url_c
*/
static uv_buf_t
_sist_post_json(c3_c* url_c, uv_buf_t lod_u)
{
  CURL *curl;
  CURLcode result;
  long cod_l;
  struct curl_slist* hed_u = 0;

  uv_buf_t buf_u = uv_buf_init(c3_malloc(1), 0);

  if ( !(curl = curl_easy_init()) ) {
    fprintf(stderr, "failed to initialize libcurl\n");
    u3_lo_bail();
  }

  hed_u = curl_slist_append(hed_u, "Accept: application/json");
  hed_u = curl_slist_append(hed_u, "Content-Type: application/json");
  hed_u = curl_slist_append(hed_u, "charsets: utf-8");

  // XX require TLS, pin default cert?

  curl_easy_setopt(curl, CURLOPT_URL, url_c);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, _sist_curl_alloc);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void*)&buf_u);
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, hed_u);

  // note: must be terminated!
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, lod_u.base);

  result = curl_easy_perform(curl);
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &cod_l);

  // XX retry?
  if ( CURLE_OK != result ) {
    fprintf(stderr, "failed to fetch %s: %s\n",
                    url_c, curl_easy_strerror(result));
    u3_lo_bail();
  }
  if ( 300 <= cod_l ) {
    fprintf(stderr, "error fetching %s: HTTP %ld\n", url_c, cod_l);
    u3_lo_bail();
  }

  curl_easy_cleanup(curl);
  curl_slist_free_all(hed_u);

  return buf_u;
}

/* _sist_oct_to_buf(): +octs to uv_buf_t
*/
static uv_buf_t
_sist_oct_to_buf(u3_noun oct)
{
  if ( c3n == u3a_is_cat(u3h(oct)) ) {
    u3_lo_bail();
  }

  c3_w len_w  = u3h(oct);
  c3_y* buf_y = c3_malloc(1 + len_w);
  buf_y[len_w] = 0;

  u3r_bytes(0, len_w, buf_y, u3t(oct));

  u3z(oct);
  return uv_buf_init((void*)buf_y, len_w);
}

/* _sist_buf_to_oct(): uv_buf_t to +octs
*/
static u3_noun
_sist_buf_to_oct(uv_buf_t buf_u)
{
  u3_noun len = u3i_words(1, (c3_w*)&buf_u.len);

  if ( c3n == u3a_is_cat(len) ) {
    u3_lo_bail();
  }

  return u3nc(len, u3i_bytes(buf_u.len, (const c3_y*)buf_u.base));
}

/* _sist_eth_rpc(): ethereum JSON RPC with request/response as +octs
*/
static u3_noun
_sist_eth_rpc(c3_c* url_c, u3_noun oct)
{
  return _sist_buf_to_oct(_sist_post_json(url_c, _sist_oct_to_buf(oct)));
}

/* _sist_dawn_fail(): pre-boot validation failed
*/
static void
_sist_dawn_fail(u3_noun who, u3_noun rac, u3_noun sas)
{
  u3_noun how = u3dc("scot", 'p', u3k(who));
  c3_c* how_c = u3r_string(u3k(how));

  c3_c* rac_c;

  switch (rac) {
    default: c3_assert(0);
    case c3__czar: {
      rac_c = "galaxy";
      break;
    }
    case c3__king: {
      rac_c = "star";
      break;
    }
    case c3__duke: {
      rac_c = "planet";
      break;
    }
    case c3__earl: {
      rac_c = "moon";
      break;
    }
    case c3__pawn: {
      rac_c = "comet";
      break;
    }
  }

  fprintf(stderr, "dawn: invalid keys for %s '%s'\r\n", rac_c, how_c);

  // XX deconstruct sas, print helpful error messages
  u3m_p("pre-boot error", u3t(sas));

  u3z(how);
  free(how_c);
  u3_lo_bail();
}

/* _sist_dawn_turf(): override contract domains with -H
*/
static u3_noun
_sist_dawn_turf(c3_c* dns_c)
{
  u3_noun tuf;

  u3_noun par = u3v_wish("thos:de-purl:html");
  u3_noun dns = u3i_string(dns_c);
  u3_noun rul = u3dc("rush", u3k(dns), u3k(par));

  if ( (u3_nul == rul) || (c3n == u3h(u3t(rul))) ) {
    fprintf(stderr, "boot: invalid domain specified with -H %s\r\n", dns_c);
    // bails, won't return
    u3_lo_bail();
    return u3_none;
  }
  else {
    fprintf(stderr, "boot: overriding network domains with %s\r\n", dns_c);
    u3_noun dom = u3t(u3t(rul));
    tuf = u3nc(u3k(dom), u3_nul);
  }

  u3z(par); u3z(dns); u3z(rul);

  return tuf;
}

/* _sist_dawn(): produce %dawn boot card - validate keys and query contract
*/
static u3_noun
_sist_dawn(u3_noun sed)
{
  u3_noun url, bok, pon, zar, tuf, sap;

  u3_noun who = u3h(sed);
  u3_noun rac = u3do("clan:title", u3k(who));

  // load snapshot if exists
  if ( 0 != u3_Host.ops_u.ets_c ) {
    fprintf(stderr, "boot: loading ethereum snapshot\r\n");
    u3_noun raw_snap = u3ke_cue(u3m_file(u3_Host.ops_u.ets_c));
    sap = u3nc(u3_nul, raw_snap);
  }
  else {
    sap = u3_nul;
  }

  // ethereum gateway as (unit purl)
  if ( 0 == u3_Host.ops_u.eth_c ) {
    if ( c3__czar == rac ) {
      fprintf(stderr, "boot: galaxy requires ethereum gateway via -e\r\n");
      // bails, won't return
      u3_lo_bail();
      return u3_none;
    }

    url = u3_nul;
  }
  else {
    u3_noun par = u3v_wish("auru:de-purl:html");
    u3_noun lur = u3i_string(u3_Host.ops_u.eth_c);
    u3_noun rul = u3dc("rush", u3k(lur), u3k(par));

    if ( u3_nul == rul ) {
      if ( c3__czar == rac ) {
        fprintf(stderr, "boot: galaxy requires ethereum gateway via -e\r\n");
        // bails, won't return
        u3_lo_bail();
        return u3_none;
      }

      url = u3_nul;
    }
    else {
      // auru:de-purl:html parses to (pair user purl)
      // we need (unit purl)
      // (we're using it to avoid the +hoke weirdness)
      // XX revisit upon merging with release-candidate
      url = u3nc(u3_nul, u3k(u3t(u3t(rul))));
    }

    u3z(par); u3z(lur); u3z(rul);
  }

  // XX require https?
  c3_c* url_c = ( 0 != u3_Host.ops_u.eth_c ) ?
    u3_Host.ops_u.eth_c :
    "https://ropsten.infura.io/v3/196a7f37c7d54211b4a07904ec73ad87";

  {
    fprintf(stderr, "boot: retrieving latest block\r\n");

    if ( c3y == u3_Host.ops_u.etn ) {
      bok = u3do("bloq:snap:dawn", u3k(u3t(sap)));
    }
    else {
      // @ud: block number
      u3_noun oct = u3v_wish("bloq:give:dawn");
      u3_noun kob = _sist_eth_rpc(url_c, u3k(oct));
      bok = u3do("bloq:take:dawn", u3k(kob));

      u3z(oct); u3z(kob);
    }
  }

  {
    // +hull:constitution:ethe: on-chain state
    u3_noun hul;

    if ( c3y == u3_Host.ops_u.etn ) {
      hul = u3dc("hull:snap:dawn", u3k(who), u3k(u3t(sap)));
    }
    else {
      if ( c3__pawn == rac ) {
        // irrelevant, just bunt +hull
        hul = u3v_wish("*hull:constitution:ethe");
      }
      else {
        u3_noun oct;

        if ( c3__earl == rac ) {
          u3_noun seg = u3do("^sein:title", u3k(who));
          u3_noun ges = u3dc("scot", 'p', u3k(seg));
          c3_c* seg_c = u3r_string(ges);

          fprintf(stderr, "boot: retrieving %s's public keys (for %s)\r\n",
                                              seg_c, u3_Host.ops_u.who_c);
          oct = u3dc("hull:give:dawn", u3k(bok), u3k(seg));

          free(seg_c);
          u3z(seg); u3z(ges);
        }
        else {
          fprintf(stderr, "boot: retrieving %s's public keys\r\n",
                                             u3_Host.ops_u.who_c);
          oct = u3dc("hull:give:dawn", u3k(bok), u3k(who));
        }

        u3_noun luh = _sist_eth_rpc(url_c, u3k(oct));
        hul = u3dc("hull:take:dawn", u3k(who), u3k(luh));

        u3z(oct); u3z(luh);
      }
    }

    // +live:dawn: network state
    // XX actually make request
    // u3_noun liv = _sist_get_json(parent, /some/url)
    u3_noun liv = u3_nul;

    fprintf(stderr, "boot: verifying keys\r\n");

    // (each sponsor=(unit ship) error=@tas)
    u3_noun sas = u3dt("veri:dawn", u3k(sed), u3k(hul), u3k(liv));

    if ( c3n == u3h(sas) ) {
      // bails, won't return
      _sist_dawn_fail(who, rac, sas);
      return u3_none;
    }

    // (unit ship): sponsor
    // produced by +veri:dawn to avoid coupling to +hull structure
    pon = u3k(u3t(sas));

    u3z(hul); u3z(liv); u3z(sas);
  }

  {
    if ( c3y == u3_Host.ops_u.etn ) {
      zar = u3do("czar:snap:dawn", u3k(u3t(sap)));

      if ( 0 != u3_Host.ops_u.dns_c ) {
        tuf = _sist_dawn_turf(u3_Host.ops_u.dns_c);
      }
      else {
        tuf = u3do("turf:snap:dawn", u3k(u3t(sap)));
      }
    }
    else {
      {
        fprintf(stderr, "boot: retrieving galaxy table\r\n");

        // (map ship [=life =pass]): galaxy table
        u3_noun oct = u3do("czar:give:dawn", u3k(bok));
        u3_noun raz = _sist_eth_rpc(url_c, u3k(oct));
        zar = u3do("czar:take:dawn", u3k(raz));

        u3z(oct); u3z(raz);
      }

      if ( 0 != u3_Host.ops_u.dns_c ) {
        tuf = _sist_dawn_turf(u3_Host.ops_u.dns_c);
      }
      else {
        fprintf(stderr, "boot: retrieving network domains\r\n");

        // (list turf): ames domains
        u3_noun oct = u3do("turf:give:dawn", u3k(bok));
        u3_noun fut = _sist_eth_rpc(url_c, u3k(oct));
        tuf = u3do("turf:take:dawn", u3k(fut));

        u3z(oct); u3z(fut);
      }
    }
  }

  u3z(rac);

  // [%dawn seed sponsor galaxies domains block eth-url snap]
  return u3nc(c3__dawn, u3nq(sed, pon, zar, u3nq(tuf, bok, url, sap)));
}

/* _sist_zen(): get OS entropy.
*/
static u3_noun
_sist_zen(void)
{
  c3_w rad_w[16];

  c3_rand(rad_w);
  return u3i_words(16, rad_w);
}

/* _sist_come(): mine a comet.
*/
static u3_noun
_sist_come(void)
{
  u3_noun tar, eny, sed;

  // XX choose from list, at random, &c
  tar = u3dc("slav", 'p', u3i_string("~marzod"));
  eny = _sist_zen();

  {
    u3_noun sar = u3dc("scot", 'p', u3k(tar));
    c3_c* tar_c = u3r_string(sar);

    fprintf(stderr, "boot: mining a comet under %s\r\n", tar_c);
    free(tar_c);
    u3z(sar);
  }

  sed = u3dc("come:dawn", u3k(tar), u3k(eny));

  {
    u3_noun who = u3dc("scot", 'p', u3k(u3h(sed)));
    c3_c* who_c = u3r_string(who);

    fprintf(stderr, "boot: found comet %s\r\n", who_c);
    free(who_c);
    u3z(who);
  }

  u3z(tar); u3z(eny);

  return sed;
}

/* sist_key(): parse a private key-file.
*/
static u3_noun
sist_key(u3_noun des)
{
  u3_noun sed, who;

  u3_noun eds = u3dc("slaw", c3__uw, u3k(des));

  if ( u3_nul == eds ) {
    c3_c* sed_c = u3r_string(des);
    fprintf(stderr, "dawn: invalid private keys: %s\r\n", sed_c);
    free(sed_c);
    u3_lo_bail();
  }

  if ( 0 == u3_Host.ops_u.who_c ) {
    fprintf(stderr, "dawn: -w required\r\n");
    u3_lo_bail();
  }

  u3_noun woh = u3i_string(u3_Host.ops_u.who_c);
  u3_noun whu = u3dc("slaw", 'p', u3k(woh));

  if ( u3_nul == whu ) {
    fprintf(stderr, "dawn: invalid ship specificed with -w %s\r\n",
                                               u3_Host.ops_u.who_c);
    u3_lo_bail();
  }

  // +seed:able:jael: private key file
  sed = u3ke_cue(u3k(u3t(eds)));
  who = u3h(sed);

  if ( c3n == u3r_sing(who, u3t(whu)) ) {
    u3_noun how = u3dc("scot", 'p', u3k(who));
    c3_c* how_c = u3r_string(u3k(how));
    fprintf(stderr, "dawn: mismatch between -w %s and -K %s\r\n",
                                               u3_Host.ops_u.who_c, how_c);

    u3z(how);
    free(how_c);
    u3_lo_bail();
  }

  u3z(woh); u3z(whu); u3z(des); u3z(eds);

  return sed;
}

/* u3_sist_boot(): restore or create.
*/
void
u3_sist_boot(void)
{
  if ( c3n == u3_Host.ops_u.nuu ) {
    _sist_rest();

    if ( c3y == u3A->fak ) {
      c3_c* who_c = u3r_string(u3dc("scot", 'p', u3k(u3A->own)));
      fprintf(stderr, "fake: %s\r\n", who_c);
      free(who_c);

      // XX review persistent options

      // disable networking
      u3_Host.ops_u.net = c3n;
      // disable battery hashes
      u3_Host.ops_u.has = c3y;
      u3C.wag_w |= u3o_hashless;
    }

    // process pending events
    u3_raft_work();
  }
  else {
    u3_noun pig, who;

    if ( 0 != u3_Host.ops_u.fak_c ) {
      u3_noun whu = u3dc("slaw", 'p', u3i_string(u3_Host.ops_u.fak_c));

      if ( (u3_nul == whu) ) {
        fprintf(stderr, "fake: invalid ship: %s\r\n", u3_Host.ops_u.fak_c);
        u3_lo_bail();
      }
      else {
        u3_noun rac = u3do("clan:title", u3k(u3t(whu)));

        if ( c3__pawn == rac ) {
          fprintf(stderr, "fake comets are disallowed\r\n");
          u3_lo_bail();
        }

        u3z(rac);
      }

      fprintf(stderr, "fake: %s\r\n", u3_Host.ops_u.fak_c);

      u3A->fak = c3y;
      who = u3k(u3t(whu));
      pig = u3nc(c3__fake, u3k(who));

      u3z(whu);
    }
    else {
      u3_noun sed;

      if ( 0 != u3_Host.ops_u.key_c ) {
        u3_noun des = u3m_file(u3_Host.ops_u.key_c);
        sed = sist_key(des);
      }
      else if ( 0 != u3_Host.ops_u.gen_c ) {
        u3_noun des = u3i_string(u3_Host.ops_u.gen_c);
        sed = sist_key(des);
      }
      else {
        sed = _sist_come();
      }

      u3A->fak = c3n;
      pig = _sist_dawn(u3k(sed));
      who = u3k(u3h(u3h(u3t(pig))));

      u3z(sed);
    }

    u3A->own = who;

    // initialize ames
    {
      u3_noun tuf = (c3y == u3A->fak) ? u3_nul : u3h(u3t(u3t(u3t(u3t(pig)))));
      // with a fake event to bring up listeners and configure domains
      u3_ames_ef_turf(u3k(tuf));
      // and real effect to set the output duct
      u3_ames_ef_bake();
    }

    // initialize %behn
    u3_behn_ef_bake();

    // Authenticate and initialize terminal.
    u3_term_ef_bake(pig);

    // queue initial galaxy sync
    {
      u3_noun rac = u3do("clan:title", u3k(u3A->own));

      if ( c3__czar == rac ) {
        u3_unix_ef_initial_into();
      }

      u3z(rac);
    }

    // Create the event log
    _sist_zest();
  }
}
