/* fake/ugly.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

  /** Forward declarations.
  **/
    static u4_noun _ylgu_noun_(u4_lane, u4_noun, u4_pb *);
    static u4_sb _ugly_noun(u4_xb *, u4_noun);

/* _ylgu_byte(): return 1 and advance (*pb) if (text, *pb) is (xb).
*/
static u4_t
_ylgu_byte(u4_noun text,
           u4_xb   xb,
           u4_pb   *pb)
{
  if ( u4_a_byte(text, *pb) == xb ) {
    *pb += 1;
    return 1;
  }
  else return 0;
}

/* _ylgu_white(): return 1 and advance (*pb) while (text, *pb) is white.
*/
static u4_t
_ylgu_white(u4_noun text,
            u4_pb   *pb)
{
  u4_t t_white = 0;

  while ( 1 ) {
    u4_xb xb = u4_a_byte(text, *pb);

    if ( (xb != ' ') && (xb != '\n') ) {
      return t_white;
    }
    else t_white = 1;

    *pb += 1;
  }
}

/* _ylgu_dec_(): parse a decimal.
*/
static u4_noun
_ylgu_dec_(u4_lane lane,
           u4_noun text,
           u4_pb   *pb)
{
  mpz_t mp;

  mpz_init(mp);
  while ( 1 ) {
    u4_xb xb = u4_a_byte(text, *pb);
    u4_xb xb_digit; 

    if ( (xb >= '0') && (xb <= '9') ) {
      xb_digit = (xb - '0');
      *pb += 1;
    }
    else {
      break;
    }
    mpz_mul_ui(mp, mp, 10);
    mpz_add_ui(mp, mp, xb_digit);
  }
  return u4_k_atom_gmp(lane, mp);
}

/* _ylgu_hex_(): parse a hexadecimal.
*/
static u4_noun
_ylgu_hex_(u4_lane lane,
           u4_noun text,
           u4_pb   *pb)
{
  mpz_t mp;

  if ( !_ylgu_byte(text, '0', pb) ) {
    return u4_bull;
  }
  if ( !_ylgu_byte(text, 'x', pb) ) {
    return u4_bull;
  }
 
  mpz_init(mp);
  while ( 1 ) {
    u4_xb xb = u4_a_byte(text, *pb);
    u4_xb xb_digit; 

    if ( (xb >= '0') && (xb <= '9') ) {
      xb_digit = (xb - '0');
      *pb += 1;
    }
    else if ( (xb >= 'a') && (xb <= 'f') ) {
      xb_digit = 10 + (xb - 'a');
      *pb += 1;
    }
    else {
      break;
    }
    mpz_mul_ui(mp, mp, 16);
    mpz_add_ui(mp, mp, xb_digit);
  }
  return u4_k_atom_gmp(lane, mp);
}

/* _ylgu_term(): parse a term.
*/
static u4_noun
_ylgu_term_(u4_lane lane,
            u4_noun text,
            u4_pb   *pb)
{
  u4_pb pb_term = *pb;

  /* Measure the term.
  */
  {
    while ( 1 ) {
      u4_xb xb = u4_a_byte(text, pb_term);

      if (    ((xb >= 'a') && (xb <= 'z')) 
           || (xb == '-') 
           || ((xb >= '0') && (xb <= '9')) )
      {
        pb_term += 1;
      }
      else break;
    }
  }

  /* Load the term.
  */
  {
    u4_sb sb_term = (pb_term - *pb);
    u4_xb *xb_buf = malloc(sb_term);
    u4_noun term;

    u4_a_bytes(text, xb_buf, *pb, sb_term);
    term = u4_k_atom_sb(lane, xb_buf, sb_term);

    free(xb_buf);

    *pb = pb_term;
    return term;
  }
}

/* _ylgu_quote(): parse a quote.
*/
static u4_noun
_ylgu_quote_(u4_lane lane,
             u4_noun text,
             u4_pb   *pb)
{
  if ( !_ylgu_byte(text, '"', pb) ) {
    return u4_bull;
  }

  {
    u4_sb sb_quote = 0;

    /* Measure the quote.
    */
    {
      u4_pb pb_quote = *pb;

      while ( 1 ) {
        u4_xb xb = u4_a_byte(text, pb_quote);

        if ( xb == '"' ) {
          pb_quote += 1;
          break;
        }
        else if ( (xb < 32) || (xb > 126) ) {
          return u4_bull;
        }
        else if ( xb == 92 ) {
          u4_xb xb_next = u4_a_byte(text, pb_quote + 1);

          if ( (xb_next == '"') || (xb_next == 92) || (xb_next == 'n') ) {
            pb_quote += 2;
            sb_quote += 1;
          }
          else return u4_bull;
        }
        else {
          pb_quote += 1;
          sb_quote += 1;
        }
      }
    }

    /* Load the quote.
    */
    {
      u4_xb   *xb_buf  = malloc(sb_quote);
      u4_noun pb_text  = *pb;
      u4_noun pb_quote = *pb;
      u4_noun quote;

      while ( 1 ) {
        u4_xb xb = u4_a_byte(text, pb_quote);

        if ( xb == '"' ) {
          pb_quote += 1;
          break;
        }
        else if ( (xb < 32) || (xb > 126) ) {
          return u4_bull;
        }
        else if ( xb == 92 ) {
          u4_xb xb_next = u4_a_byte(text, pb_quote + 1);

          if ( xb_next == 'n' ) {
            xb_next = 10;
          }
          xb_buf[pb_text - *pb] = xb_next;
          pb_quote += 2;
          pb_text += 1;
        }
        else {
          xb_buf[pb_text - *pb] = xb;
          pb_quote += 1;
          pb_text += 1;
        }
      }
      quote = u4_k_atom_sb(lane, xb_buf, sb_quote);

      free(xb_buf);

      *pb = pb_quote;
      return quote;
    }
  }
}

/* _ylgu_tuple_in(): parse a tuple's contents, without whitespace.
*/
static u4_noun
_ylgu_tuple_in_(u4_lane lane,
                u4_noun text,
                u4_pb   *pb)
{
  u4_pb pb_x, pb_y, pb_z;
  u4_noun noun_a, noun_b;

  pb_x = *pb;
  noun_a = _ylgu_noun_(lane, text, &pb_x);

  if ( noun_a == u4_bull ) {
    return u4_bull;
  }
  else {
    pb_y = pb_x;
    if ( !_ylgu_white(text, &pb_y) ) {
      *pb = pb_x;
      return noun_a;
    }

    pb_z = pb_y;
    noun_b = _ylgu_tuple_in_(lane, text, &pb_z);

    if ( noun_b == u4_bull ) {
      *pb = pb_x;
      return noun_a;
    }
    else {
      *pb = pb_z;
      return u4_k_cell(lane, noun_a, noun_b);
    }
  }
}

/* _ylgu_tuple_(): parse a tuple, including parens.
*/
static u4_noun
_ylgu_tuple_(u4_lane lane,
             u4_noun text,
             u4_pb   *pb)
{
  u4_noun tuple;

  if ( !_ylgu_byte(text, '(', pb) ) {
    return u4_bull;
  }
  _ylgu_white(text, pb);

  tuple = _ylgu_tuple_in_(lane, text, pb);
  if ( u4_bull == tuple ) {
    return u4_bull;
  }

  _ylgu_white(text, pb);
  if ( !_ylgu_byte(text, ')', pb) ) {
    return u4_bull;
  }
  return tuple;
}

/* _ylgu_log_in(): parse a log's contents, without whitespace.
*/
static u4_noun
_ylgu_log_in_(u4_lane lane,
              u4_noun text,
              u4_pb   *pb)
{
  u4_pb pb_x, pb_y, pb_z;
  u4_noun noun_a, noun_b;

  pb_x = *pb;
  noun_a = _ylgu_noun_(lane, text, &pb_x);

  if ( noun_a == u4_bull ) {
    return u4_bull;
  }
  else {
    pb_y = pb_x;
    if ( !_ylgu_white(text, &pb_y) ) {
      *pb = pb_x;
      return u4_k_cell(lane, noun_a, u4_noun_0);
    }

    pb_z = pb_y;
    noun_b = _ylgu_log_in_(lane, text, &pb_z);

    if ( noun_b == u4_bull ) {
      *pb = pb_x;
      return u4_k_cell(lane, noun_a, u4_noun_0);
    }
    else {
      *pb = pb_z;
      return u4_k_cell(lane, noun_a, noun_b);
    }
  }
}

/* _ylgu_log_(): parse a log, including braces.
*/
static u4_noun
_ylgu_log_(u4_lane lane,
           u4_noun text,
           u4_pb   *pb)
{
  u4_noun log;

  if ( !_ylgu_byte(text, '{', pb) ) {
    return u4_bull;
  }
  _ylgu_white(text, pb);

  log = _ylgu_log_in_(lane, text, pb);
  if ( u4_bull == log ) {
    log = u4_noun_0;
  }

  _ylgu_white(text, pb);
  if ( !_ylgu_byte(text, '}', pb) ) {
    return u4_bull;
  }
  return log;
}

/* _ylgu_tab_():
**
**   Parse a tab, at *pb in (text).
*/
static u4_noun
_ylgu_tab_(u4_lane lane,
           u4_noun text,
           u4_pb   *pb)
{
  if ( !_ylgu_byte(text, ':', pb) ) {
    return u4_bull;
  }
  else {
    u4_road_lane_in(lane, road, pro)
    {
      u4_noun log = _ylgu_log_(u4_cap(road), text, pb);

      if ( u4_bull == log ) {
        pro = u4_bull;
      }
      else pro = u4_tab_add_log(u4_hat(road), log, u4_noun_0);
    }
    u4_road_lane_out(lane, road, pro)
  }
}

/* _ylgu_bag_():
**
**   Parse a bag, at *pb in (text).
*/
static u4_noun
_ylgu_bag_(u4_lane lane,
           u4_noun text,
           u4_pb   *pb)
{
  if ( !_ylgu_byte(text, '=', pb) ) {
    return u4_bull;
  }
  else {
    u4_road_lane_in(lane, road, pro)
    {
      u4_noun log = _ylgu_log_(u4_cap(road), text, pb);

      if ( u4_bull == log ) {
        pro = u4_bull;
      }
      else pro = u4_bag_add_log(u4_hat(road), log, u4_noun_0);
    }
    u4_road_lane_out(lane, road, pro)
  }
}

/* _ylgu_noun(): 
**
**   Parse an ugly noun, at *pb in (text).  Set *pb to the next byte
**   to parse.
*/
static u4_noun
_ylgu_noun_(u4_lane lane,
            u4_noun text,
            u4_pb   *pb)
{
  u4_xb xb_this = u4_a_byte(text, *pb);

  if ( xb_this == '(' ) {
    return _ylgu_tuple_(lane, text, pb);
  }
  else if ( xb_this == '{' ) {
    return _ylgu_log_(lane, text, pb);
  }
  else if ( xb_this == ':' ) {
    return _ylgu_tab_(lane, text, pb);
  }
  else if ( xb_this == '=' ) {
    return _ylgu_bag_(lane, text, pb);
  }
  else if ( xb_this == '0' ) {
    u4_xb xb_next = u4_a_byte(text, (1 + *pb));

    if ( xb_next == 'x' ) {
      return _ylgu_hex_(lane, text, pb);
    }
    else {
      return _ylgu_dec_(lane, text, pb);
    }
  }
  else if ( (xb_this >= '1') && (xb_this <= '9') ) {
    return _ylgu_dec_(lane, text, pb);
  }
  else if ( ((xb_this >= 'a') && (xb_this <= 'z')) ||
            ((xb_this >= 'A') && (xb_this <= 'Z')) )
  {
    return _ylgu_term_(lane, text, pb);
  }
  else if ( xb_this == '"' ) {
    return _ylgu_quote_(lane, text, pb);
  }
  else return u4_bull;
}

/* _scan_ylgu_(): scan an ugly text.
*/
static u4_noun
_scan_ylgu_(u4_lane lane,
            u4_noun text)
{
  u4_noun noun;
  u4_pb   pb = 0;

  _ylgu_white(text, &pb);
  noun = _ylgu_tuple_in_(lane, text, &pb);

  if ( u4_bull == noun ) {
    return u4_bull;
  }
  _ylgu_white(text, &pb);

  if ( pb != u4_a_bin(text, 3) ) {
    return u4_bull;
  }
  return noun;
}

/* _ugly_dec: dump and/or measure (atom) in (xb_ugly), as a decimal.
*/
static u4_sb
_ugly_dec(u4_xb   *xb_ugly,
          u4_atom atom)
{
  if ( u4_n_zero(atom) ) {
    if ( xb_ugly ) xb_ugly[0] = '0'; return 1;
  }
  else {
    mpz_t mp;
    u4_sb sb_digits = 0;

    u4_a_gmp(atom, mp);
    while ( mpz_cmp_ui(mp, 0) != 0 ) {
      mpz_tdiv_q_ui(mp, mp, 10);
      sb_digits++;
    }
    if ( xb_ugly ) {
      u4_a_gmp(atom, mp);
      mpz_get_str((char *)xb_ugly, 10, mp);
    }
    return sb_digits;
  }
}

/* _ugly_hex: dump and/or measure (atom) in (xb_ugly), as a hexadecimal.
*/
static u4_sb
_ugly_hex(u4_xb   *xb_ugly,
          u4_atom atom)
{
  if ( u4_n_zero(atom) ) {
    if ( xb_ugly ) xb_ugly[0] = '0'; return 1;
  }
  else {
    mpz_t mp;
    u4_sb sb_digits = u4_a_bin(atom, 2);

    if ( xb_ugly ) {
      u4_a_gmp(atom, mp);
      mpz_get_str((char *)xb_ugly, 16, mp);
    }
    return sb_digits;
  }
}

/* _ugly_atom(): dump and/or measure (atom) in (xb_ugly).
*/
static u4_sb
_ugly_atom(u4_xb   *xb_ugly,
           u4_atom atom)
{
  if ( u4_a_bin(atom, 3) <= 1 ) {
    return _ugly_dec(xb_ugly, atom);
  }
  else {
    u4_sb sb = 0;

    if ( xb_ugly ) xb_ugly[sb] = '0'; sb++;
    if ( xb_ugly ) xb_ugly[sb] = 'x'; sb++;

    return sb + _ugly_hex((xb_ugly ? (xb_ugly + sb) : 0), atom);
  }
}

/* _ugly_cell(): dump and/or measure (cell) in (xb_ugly).
*/
static u4_sb
_ugly_cell(u4_xb   *xb_ugly,
           u4_cell cell)
{
  u4_sb sb = 0;

  sb = _ugly_noun(xb_ugly, u4_ch(cell));

  if ( xb_ugly ) xb_ugly[sb] = ' '; sb++;

  if ( u4_n_atom(u4_ct(cell)) ) {
    sb += _ugly_atom((xb_ugly ? (xb_ugly + sb) : 0), u4_ct(cell));
  }
  else sb += _ugly_cell((xb_ugly ? (xb_ugly + sb) : 0), u4_ct(cell));

  return sb;
}

/* _ugly_noun(): dump and/or measure (noun) in (xb_ugly).
*/
static u4_sb
_ugly_noun(u4_xb   *xb_ugly,
           u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    return _ugly_atom(xb_ugly, noun);
  }
  else {
    u4_sb sb = 0;

    if ( xb_ugly ) xb_ugly[sb] = '('; sb++;
    sb += _ugly_cell((xb_ugly ? (xb_ugly + sb) : 0), noun);
    if ( xb_ugly ) xb_ugly[sb] = ')'; sb++;

    return sb;
  }
}

/* u4_ugly_dump():
**
**   Convert (noun) to an ugly dump.
*/
u4_noun
u4_ugly_dump(u4_lane lane,
             u4_noun noun)
{
  u4_sb  sb_ugly;
  u4_xb *xb_ugly;

  sb_ugly = _ugly_noun(0, noun);
  xb_ugly = malloc(sb_ugly + 1);

  _ugly_noun(xb_ugly, noun);
  xb_ugly[sb_ugly] = '\n';

  return u4_k_atom_sb(lane, xb_ugly, sb_ugly + 1);
}

/* u4_ylgu_scan_():
**
**   Scap (noun) with the ylgu parser.
*/
u4_noun
u4_ylgu_scan_(u4_lane lane,
             u4_noun noun)
{
  return _scan_ylgu_(lane, noun);
}

/* u4_ylgu_scan_cl_():
**
**  Scap (cl_text) with the ylgu parser.
*/
u4_noun
u4_ylgu_scan_cl_(u4_lane     lane,
                 const u4_cl *cl_text)
{
  u4_road_lane_in(lane, road, pro)
  {
    u4_noun text = u4_k_atom_c(u4_cap(road), cl_text);

    pro = _scan_ylgu_(u4_hat(road), text);
  }
  u4_road_lane_out(lane, road, pro)
}
