/* mill/open.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _open_all(): (%all p)
*/
static u4_noun
_open_all(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_nim) ) {
    return u4_k_cell(lane, u4_atom_rock, u4_noun_0);
  } 
  else if ( u4_n_zero(u4_ct(p_nim)) ) {
    return u4_ch(p_nim);
  }
  else {
    return u4_k_trel
        (lane, u4_atom_and, u4_ch(p_nim), _open_all(m, u4_ct(p_nim)));
  }
}

/* _open_any(): (%any p)
*/
static u4_noun
_open_any(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_nim) ) {
    return u4_k_cell(lane, u4_atom_rock, u4_noun_1);
  } 
  else if ( u4_n_zero(u4_ct(p_nim)) ) {
    return u4_ch(p_nim);
  }
  else {
    return u4_k_trel
        (lane, u4_atom_or, u4_ch(p_nim), _open_any(m, u4_ct(p_nim)));
  }
}

/* _open_bolt(): (%bolt p)
*/
static u4_noun
_open_bolt(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel(lane, u4_atom_load, u4_noun_0, p_nim);
}

/* _open_goat(): (%goat p)
*/
static u4_noun
_open_goat(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, 
     u4_atom_lamb,
     u4_k_trel(lane, u4_atom_roll, u4_noun_3, u4_noun_0),
     u4_k_trel
     (lane, 
       u4_atom_fix,
       u4_k_cell(lane, u4_atom_limb, u4_noun_4),
       p_nim));
}

/* _open_loop(): (%loop p)
*/
static u4_noun
_open_loop(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_link, 
           u4_k_trel(lane, u4_atom_load, u4_noun_0, p_nim),
           u4_k_trel(lane, u4_atom_look, u4_noun_0, u4_noun_0));
}

/* _open_not(): (%not p)
*/
static u4_noun
_open_not(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_if,
           p_nim,
           u4_k_cell(lane, u4_atom_rock, u4_noun_1),
           u4_k_cell(lane, u4_atom_rock, u4_noun_0));
}

/* _open_pop(): (%pop p)
*/
static u4_noun
_open_pop(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_link,
           p_nim,
           u4_k_trel(lane, u4_atom_look, u4_noun_0, u4_noun_0));
}

/* _open_tupl(): (%tupl p)
*/
static u4_noun
_open_tupl(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(p_nim) ) {
    return u4_exit;
  }
  else {
    if ( u4_n_zero(u4_ct(p_nim)) ) {
      return u4_ch(p_nim);
    }
    else return u4_k_cell(lane, u4_ch(p_nim), _open_tupl(m, u4_ct(p_nim)));
  }
}

/* _open_and(): (%and p q)
*/
static u4_noun
_open_and(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_if, 
           p_nim,
           q_nim,
           u4_k_cell(lane, u4_atom_rock, u4_noun_1));
}

#if 0
/* _open_coat(): (%coat p q)
*/
static u4_noun
_open_coat(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, u4_atom_wrap,
           u4_k_cell
            (lane, 
             u4_k_trel(lane, p_nim, u4_atom_limb, u4_noun_1),
             u4_noun_0),
           q_nim);
}
#endif

/* _open_book(): (%book p)
*/
static u4_noun
_open_book(u4_milr m, u4_noun p_nim)
{
  u4_lane lane = m->lane;

  return u4_k_cell(lane, u4_atom_load, u4_log_tupl(lane, p_nim));
}

/* _open_nilk(): (%nilk p q)
*/
static u4_noun
_open_nilk(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel(lane, u4_atom_link, q_nim, p_nim);
}

/* _open_no(): (%no p q)
*/
static u4_noun
_open_no(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_if, p_nim, u4_k_cell(lane, u4_atom_limb, u4_noun_0), q_nim);
}

/* _open_so(): (%so p q)
*/
static u4_noun
_open_so(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_if, p_nim, q_nim, u4_k_cell(lane, u4_atom_limb, u4_noun_0));
}

/* _open_or(): (%or p q)
*/
static u4_noun
_open_or(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane, u4_atom_if, 
           p_nim,
           u4_k_cell(lane, u4_atom_rock, u4_noun_0),
           q_nim);
}

/* _open_pose(): (%pose p q)
*/
static u4_noun
_open_pose(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return 
    u4_k_trel
     (lane,
      u4_atom_link,
      u4_k_cell(lane, p_nim, u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
      q_nim);
}

/* _open_post(): (%post p q)
*/
static u4_noun
_open_post(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, 
     u4_atom_link,
     u4_k_trel
       (lane, 
        u4_atom_bend, 
        p_nim, 
        u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
     q_nim);
}

/* _open_roll(): (%roll p q)
*/
static u4_noun
_open_roll(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_trel
    (lane, 
     u4_atom_cast,
     u4_k_qual(lane, u4_atom_raw, p_nim, u4_atom_rock, u4_noun_0),
     u4_k_cell(lane, u4_atom_rock, q_nim));
}

/* _open_punt(): (%punt p q)
*/
static u4_noun
_open_punt(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane,
     u4_atom_comb,
     u4_k_trel(lane, u4_atom_look, u4_noun_0, u4_noun_0),
     u4_k_cell
      (lane,
       u4_k_cell(lane, u4_k_cell(lane, u4_atom_limb, u4_noun_4), q_nim),
       u4_noun_0),
     p_nim);
}

/* _open_boot(): (%boot p q r)
*/
static u4_noun
_open_boot(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;
  u4_gene bof;

  bof = u4_n_zero(r_nim)
          ? u4_k_cell(lane, u4_atom_rock, u4_noun_0)
          : u4_k_trel
              (lane, u4_atom_link,
                     u4_k_cell(lane, u4_atom_rock, u4_noun_0),
                     u4_k_cell(lane, u4_atom_load, u4_log_tupl(lane, r_nim)));

  return u4_k_trel
    (lane, u4_atom_link,
           u4_k_cell
             (lane, u4_k_trel
                      (lane, u4_atom_coat, 
                             p_nim,
                             u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
                    bof),
           q_nim);
}

/* _comb_bends()::
*/
static u4_noun
_comb_bends(u4_lane lane, u4_noun q_nim)
{
  if ( u4_n_zero(q_nim) ) {
    return u4_noun_0;
  }
  else {
    u4_noun iq_nim  = u4_ch(q_nim);
    u4_noun tq_nim  = u4_ct(q_nim);
    u4_noun piq_nim = u4_ch(iq_nim);
    u4_noun qiq_nim = u4_ct(iq_nim);

    return u4_k_cell
      (lane, 
       u4_k_cell
        (lane, 
         piq_nim,
         u4_k_trel
          (lane, 
           u4_atom_link, 
           u4_k_cell(lane, u4_atom_limb, u4_noun_3),
           qiq_nim)),
      _comb_bends(lane, tq_nim));
  }
}

/* _open_comb(): (%comb p q r)
*/
static u4_noun
_open_comb(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(q_nim) ) {
    return u4_k_trel(lane, u4_atom_link, r_nim, p_nim);
  }
  else {
    return u4_k_trel
      (lane, 
       u4_atom_link,
       u4_k_trel(lane, r_nim, u4_atom_limb, u4_noun_1),
       u4_k_trel
        (lane, 
         u4_atom_bend,
         _comb_bends(lane, q_nim),
         u4_k_trel
           (lane, 
            u4_atom_link, 
            u4_k_cell(lane, u4_atom_limb, u4_noun_2), 
            p_nim
           )
        )
      );
  }
}

/* _gear_arg()::
*/
static u4_noun
_gear_arg(u4_milr m, u4_noun q_nim)
{
  u4_lane lane    = m->lane;
  u4_noun iq_nim  = u4_ch(q_nim);
  u4_noun tq_nim  = u4_ct(q_nim);
  u4_noun piq_nim = u4_ch(iq_nim);
  u4_noun qiq_nim = u4_ct(iq_nim);
  u4_noun fos     = u4_k_trel(lane, u4_atom_coat, piq_nim, qiq_nim);

  if ( u4_n_zero(tq_nim) ) {
    return fos;
  }
  else {
    return u4_k_cell(lane, fos, _gear_arg(m, tq_nim));
  }
}

/* _open_gear(): (%gear p q r)
*/
static u4_noun
_open_gear(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;

  return 
    u4_k_trel
     (lane, 
      u4_atom_link,
      u4_k_cell
       (lane, 
        _gear_arg(m, q_nim), 
        u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
      u4_k_cell
       (lane,
        u4_atom_bolt,
        u4_k_trel(lane, u4_atom_cast, p_nim, r_nim)));
}

/* _open_pull(): (%pull p q r)
*/
static u4_noun
_open_pull(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;

  return 
    u4_k_trel
     (lane,
      u4_atom_link,
      u4_k_cell
       (lane, 
        u4_k_trel(lane, u4_atom_coat, p_nim, r_nim),
        u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
      q_nim);
}

/* _open_push(): (%push p q r)
*/
static u4_noun
_open_push(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;

  return 
    u4_k_trel
     (lane,
      u4_atom_link,
      u4_k_cell
       (lane, 
        u4_k_trel(lane, u4_atom_coat, p_nim, q_nim),
        u4_k_cell(lane, u4_atom_limb, u4_noun_1)),
      r_nim);
}

/* _fan_cases()::
*/
static u4_noun
_fan_cases(u4_milr m, u4_noun ppip_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;
 
  if ( u4_n_zero(ppip_nim) ) {
    return u4_noun_0;
  }
  else {
    u4_noun ippip_nim = u4_ch(ppip_nim);
    u4_noun tppip_nim = u4_ct(ppip_nim);

    return u4_k_cell
      (lane, u4_k_trel(lane, u4_atom_like, q_nim, ippip_nim),
             _fan_cases(m, tppip_nim, q_nim));
  }
}

/* _open_fan(): (%fan p q)
*/
static u4_noun
_open_fan(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(q_nim) ) {
    return u4_k_cell(lane, u4_atom_limb, u4_noun_0);
  }
  else {
    u4_noun iq_nim   = u4_ch(q_nim);      // first
    u4_noun tq_nim   = u4_ct(q_nim);      // rest
    u4_noun piq_nim  = u4_ch(iq_nim);     // test
    u4_noun qiq_nim  = u4_ct(iq_nim);     // product
    u4_noun ppiq_nim = u4_ch(piq_nim);    // cases
    u4_noun qpiq_nim = u4_ct(piq_nim);    // guard

    return u4_k_qual
     (lane, 
      u4_atom_if,
      u4_k_trel
       (lane, 
        u4_atom_and, 
        u4_k_cell(lane, u4_atom_any, _fan_cases(m, ppiq_nim, p_nim)),
        qpiq_nim),
      qiq_nim,
      _open_fan(m, p_nim, tq_nim));
  }
}

/* _open_fix(): (%fix p q)
*/
static u4_noun
_open_fix(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  if ( u4_n_zero(q_nim) ) {
    return u4_k_cell(lane, u4_atom_limb, u4_noun_0);
  }
  else {
    return u4_k_qual
      (lane, u4_atom_if,
             u4_k_trel(lane, u4_atom_like, p_nim, u4_ch(q_nim)),
             p_nim,
             _open_fix(m, p_nim, u4_ct(q_nim)));
  }
}

/* _fit_list()::
*/
static u4_noun
_fit_list(u4_lane lane, u4_noun q_nim)
{
  if ( u4_n_zero(q_nim) ) {
    return u4_noun_0;
  }
  else {
    u4_noun iq_nim = u4_ch(q_nim);        // first
    u4_noun tq_nim = u4_ct(q_nim);        // rest
    u4_noun piq_nim  = u4_ch(iq_nim);     // test
    u4_noun ppiq_nim = u4_ch(piq_nim);    // cases

    return u4_log_cat(lane, ppiq_nim, _fit_list(lane, tq_nim));
  }
}

/* _open_fit(): (%fit p q)
*/
static u4_noun
_open_fit(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual
    (lane,
     u4_atom_safe,
     u4_k_trel(lane, u4_atom_fix, p_nim, _fit_list(lane, q_nim)),
     p_nim,
     u4_k_trel(lane, u4_atom_fan, p_nim, q_nim));
}

/* _open_lamb(): (%lamb p q)
*/
static u4_noun
_open_lamb(u4_milr m, u4_noun p_nim, u4_noun q_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual(lane, u4_atom_pose, p_nim, u4_atom_bolt, q_nim);
}

/* _open_un(): (%un p q r)
*/
static u4_noun
_open_un(u4_milr m, u4_noun p_nim, u4_noun q_nim, u4_noun r_nim)
{
  u4_lane lane = m->lane;

  return u4_k_qual(lane, u4_atom_if, p_nim, r_nim, q_nim);
}

/* _mill_open(): open macro.
*/
u4_noun
_mill_open(u4_milr m,
           u4_noun nim)
{
  u4_noun p_nim, q_nim, r_nim;

  if ( u4_b_p(nim, u4_atom_all, &p_nim) ) {
    return _open_all(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_any, &p_nim) ) {
    return _open_any(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_bolt, &p_nim) ) {
    return _open_bolt(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_book, &p_nim) ) {
    return _open_book(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_goat, &p_nim) ) {
    return _open_goat(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_loop, &p_nim) ) {
    return _open_loop(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_not, &p_nim) ) {
    return _open_not(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_pop, &p_nim) ) {
    return _open_pop(m, p_nim);
  }
  if ( u4_b_p(nim, u4_atom_tupl, &p_nim) ) {
    return _open_tupl(m, p_nim);
  }

  if ( u4_b_pq(nim, u4_atom_and, &p_nim, &q_nim) ) {
    return _open_and(m, p_nim, q_nim);
  }
#if 0
  if ( u4_b_pq(nim, u4_atom_coat, &p_nim, &q_nim) ) {
    return _open_coat(m, p_nim, q_nim);
  }
#endif
  if ( u4_b_pq(nim, u4_atom_fan, &p_nim, &q_nim) ) {
    return _open_fan(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_fix, &p_nim, &q_nim) ) {
    return _open_fix(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_fit, &p_nim, &q_nim) ) {
    return _open_fit(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_lamb, &p_nim, &q_nim) ) {
    return _open_lamb(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_nilk, &p_nim, &q_nim) ) {
    return _open_nilk(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_no, &p_nim, &q_nim) ) {
    return _open_no(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_or, &p_nim, &q_nim) ) {
    return _open_or(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_pose, &p_nim, &q_nim) ) {
    return _open_pose(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_post, &p_nim, &q_nim) ) {
    return _open_post(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_punt, &p_nim, &q_nim) ) {
    return _open_punt(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_roll, &p_nim, &q_nim) ) {
    return _open_roll(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_site, &p_nim, &q_nim) ) {
    return q_nim;
  }
  if ( u4_b_pq(nim, u4_atom_so, &p_nim, &q_nim) ) {
    return _open_so(m, p_nim, q_nim);
  }
  if ( u4_b_pq(nim, u4_atom_spot, &p_nim, &q_nim) ) {
    return q_nim;
  }
  
  if ( u4_b_pqr(nim, u4_atom_boot, &p_nim, &q_nim, &r_nim) ) {
    return _open_boot(m, p_nim, q_nim, r_nim);
  }
  if ( u4_b_pqr(nim, u4_atom_comb, &p_nim, &q_nim, &r_nim) ) {
    return _open_comb(m, p_nim, q_nim, r_nim);
  }
  if ( u4_b_pqr(nim, u4_atom_gear, &p_nim, &q_nim, &r_nim) ) {
    return _open_gear(m, p_nim, q_nim, r_nim);
  }
  if ( u4_b_pqr(nim, u4_atom_pull, &p_nim, &q_nim, &r_nim) ) {
    return _open_pull(m, p_nim, q_nim, r_nim);
  }
  if ( u4_b_pqr(nim, u4_atom_push, &p_nim, &q_nim, &r_nim) ) {
    return _open_push(m, p_nim, q_nim, r_nim);
  }
  if ( u4_b_pqr(nim, u4_atom_safe, &p_nim, &q_nim, &r_nim) ) {
    return r_nim;
  }
  if ( u4_b_pqr(nim, u4_atom_un, &p_nim, &q_nim, &r_nim) ) {
    return _open_un(m, p_nim, q_nim, r_nim);
  }

  return nim;
}
