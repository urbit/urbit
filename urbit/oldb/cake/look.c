/* cake/look.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* u4_n_atom():
**
**   Return 1 if (noun) is an atom.
*/
u4_t
u4_n_atom(u4_noun noun)
{
  if ( u4_noun_is_cod(noun) ) {
    return 1;
  }
  else {
    u4_pin pin = noun;
    
    return u4_pin_is_atom(pin);
  }
}

/* u4_n_cell():
**
**   Return 1 if (noun) is a cell.
*/
u4_t
u4_n_cell(u4_noun noun)
{
  if ( u4_noun_is_cod(noun) ) {
    return 0;
  }
  else {
    u4_pin pin = noun;
    
    return u4_pin_is_cell(pin);
  }
}

/* u4_n_trel():
** u4_n_qual():
** u4_n_quil():
**
**   Return 1 if (noun) is a trel, qual or quil.
*/
u4_t u4_n_trel(u4_noun noun) 
  { return u4_n_cell(noun) && u4_n_cell(u4_ct(noun)); }

u4_t u4_n_qual(u4_noun noun)
  { return u4_n_cell(noun) && u4_n_trel(u4_ct(noun)); }

u4_t u4_n_quil(u4_noun noun)
  { return u4_n_cell(noun) && u4_n_qual(u4_ct(noun)); }

/* u4_n_size():
**
**   Return number of subnouns in (a).
*/
u4_xw
u4_n_size(u4_noun a)
{
  if ( u4_n_atom(a) ) {
    return 1;
  }
  else return (u4_n_size(u4_ch(a)) + u4_n_size(u4_ct(a)));
}

/* u4_n_eq():
**
**   Return 1 if (a) and (b) are the same noun.
*/
u4_t
u4_n_eq(u4_noun a,
        u4_noun b)
{
  if ( a == b ) {
    return 1;
  }
  else {
    u4_t t_atom_a = u4_n_atom(a);
    u4_t t_atom_b = u4_n_atom(b);

    if ( t_atom_a != t_atom_b ) {
      return 0;
    }
    else {
      u4_t t_atom = t_atom_a;

      if ( !t_atom ) {
        return u4_n_eq(u4_c_head(a), u4_c_head(b)) && 
               u4_n_eq(u4_c_tail(a), u4_c_tail(b));
      }
      else {
        if ( u4_noun_is_cod(a) || u4_noun_is_cod(b) ) {
          return 0;
        }
        else {
          u4_pin pin_a = a;
          u4_pin pin_b = b;

          if ( u4_pin_is_fort(pin_a) || u4_pin_is_fort(pin_b) ) {
            return u4_stub;
          } 
          else {
            u4_sw sw_a = u4_open_atom_sw(pin_a);
            u4_sw sw_b = u4_open_atom_sw(pin_b);

            if ( sw_a != sw_b ) {
              return 0;
            }
            else {
              u4_sw sw = sw_a;
              u4_pw pw;

              for ( pw = 0; pw < sw; pw++ ) {
                if ( u4_open_atom_xw(pin_a, pw) != 
                     u4_open_atom_xw(pin_b, pw) )
                {
                  return 0;
                }
              }
              return 1;
            }
          }
        }
      }
    }
  }
}
              
/* u4_n_eq_c():
**
**   Return 1 if (noun) equals the C string (cl).
*/
u4_t
u4_n_eq_c(u4_noun noun,
          u4_cl   *cl)
{
  if ( u4_n_cell(noun) ) {
    return 0;
  } 
  else {
    u4_sb sb;

    if ( u4_a_bin(noun, 3) != (sb = strlen(cl)) ) {
      return 0;
    }
    else {
      u4_cl *cl_noun = alloca(sb + 1);

      u4_a_bytes(noun, (u4_xb *)cl_noun, 0, sb + 1);
      return !strcmp(cl, cl_noun);
    }
  }
}

/* u4_c_head():
**
**   Return the head of (cell).
*/
u4_noun
u4_c_head(u4_cell cell)
{
  if ( u4_bull == cell ) {
    u4_bug("head: bull", cell);
    return u4_trip;
  }
  else if ( u4_noun_is_cod(cell) ) {
    u4_bug("head: flat", cell);
    return u4_trip;
  }
  else {
    u4_pin pin_cell = cell;

    if ( u4_pin_is_atom(pin_cell) ) {
      u4_bug("head: flat", cell);
      return u4_trip;
    }
    else if ( u4_pin_is_fort(pin_cell) ) {
      return u4_stub;
    }
    else {
      return u4_open_cell_head(pin_cell);
    }
  }
}

/* u4_c_tail():
**
**   Return the tail of (cell).
*/
u4_noun
u4_c_tail(u4_cell cell)
{
  if ( u4_bull == cell ) {
    u4_bug("tail: bull", cell);
    return u4_trip;
  }
  else if ( u4_noun_is_cod(cell) ) {
    u4_bug("tail: flat", cell);
    return u4_trip;
  }
  else {
    u4_pin pin_cell = cell;

    if ( u4_pin_is_atom(pin_cell) ) {
      u4_bug("tail: flat", cell);
      return u4_trip;
    }
    else if ( u4_pin_is_fort(pin_cell) ) {
      return u4_stub;
    }
    else {
      return u4_open_cell_tail(pin_cell);
    }
  }
}

/* u4_c_head_exit():
**
**   Return the head of (noun), or exit if (noun) is flat.
*/
u4_noun
u4_c_head_exit(u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    return u4_exit;
  }
  else return u4_ch(noun);
}

/* u4_c_tail_exit():
**
**   Return the tail of (noun), or exit if (noun) is flat.
*/
u4_noun
u4_c_tail_exit(u4_noun noun)
{
  if ( u4_n_atom(noun) ) {
    return u4_exit;
  }
  else return u4_ct(noun);
}

/* u4_c_cell(): 
** u4_c_trel():
** u4_c_qual():
** u4_c_quil():
**
**   Unpack (cell, trel, qual, quil).
*/
u4_t 
u4_c_cell(u4_noun cell, 
          u4_noun *a, 
          u4_noun *b)
{ 
  if ( !u4_n_cell(cell) ) {
    return 0;
  }

  *a = u4_ch(cell); 
  *b = u4_ct(cell);
  return 1;
}

u4_t 
u4_c_trel(u4_noun trel, 
          u4_noun *a, 
          u4_noun *b, 
          u4_noun *c)
{ 
  if ( !u4_n_cell(trel) ) {
    return 0;
  }
  *a = u4_ch(trel); 

  return u4_c_cell(u4_ct(trel), b, c); 
}

u4_t 
u4_c_qual(u4_noun qual, 
          u4_noun *a, 
          u4_noun *b, 
          u4_noun *c, 
          u4_noun *d)
{
  if ( !u4_n_cell(qual) ) {
    return 0;
  }
  *a = u4_ch(qual); 

  return u4_c_trel(u4_ct(qual), b, c, d);
}

u4_t 
u4_c_quil(u4_noun quil, 
          u4_noun *a, 
          u4_noun *b, 
          u4_noun *c, 
          u4_noun *d, 
          u4_noun *e)
{ 
  if ( !u4_n_cell(quil) ) {
    return 0;
  }
  *a = u4_ch(quil); 

  return u4_c_qual(u4_ct(quil), b, c, d, e);
}

/* u4_c_log():
**
**   Unpack a log of length (xw), ignoring the tail.
*/
void
u4_c_log(u4_log  log,
         u4_xw   xw,
         u4_noun *nouns)
{
  u4_xw i;

  for ( i=0; i < xw; i++ ) {
    nouns[i] = u4_ch(log);
    log = u4_ct(log);
  }
}

/* u4_c_tuple():
**
**   Unpack a tuple of (xw) nouns.
*/
void
u4_c_tuple(u4_noun tuple,
           u4_xw   xw,
           u4_noun *nouns)
{
  u4_assert(xw);
  {
    u4_xw i;

    for ( i=0; i < (xw - 1); i++ ) {
      nouns[i] = u4_ch(tuple);
      tuple = u4_ct(tuple);
    }
    nouns[i] = tuple;
  }
}

/* _nnub_mash(): map (w_nud) to another 32-bit word.  Shamelessly copied
** from u3, as u3/watt mug is not quite the same alg as nub.
*/
static uint32_t
_nnub_mash(uint32_t w_nud) 
{
  static uint32_t w_cog[256] = {
    0x352598d8, 0xba9cd382, 0xa9afcd63, 0xfe8c511a, 0xf40f7a8d, 0x6220ecc5,
    0xf8820121, 0x99267e88, 0xdee5b579, 0x7824fff6, 0x7c8f831a, 0x766ff6ec,
    0xb0cd88c6, 0x1994625d, 0xccffce0a,  0xc100eda, 0xda4952df, 0x62bcfdef,
    0xf00fc9e2, 0x3a30bdd8, 0xd156e14b, 0x22b9bd9c, 0xeb9f040e, 0x9570fa83,
    0x151f4b17, 0x4c680e34, 0xc62f6816, 0x5c43205e, 0xe2ddee5a, 0x87652d85,
    0x80722ba6, 0xa158b454, 0x5022a039, 0xbc457a9d, 0x3ce83919, 0x7990bd4e,
    0xf44c697e, 0xf60ee7be, 0xdd68fcd2, 0xd3bfabae, 0x2ba16060, 0x23171ec6,
    0x9b9e2cec, 0x683fa466, 0x103b81cf, 0x40acd8dc, 0xe14077a2, 0x123e5e9f,
    0xcfbeae41,  0xf4bb673, 0xf537ea14, 0xae87124c, 0xaed1093b, 0x513ce98d,
    0x5f606388, 0x1e458e1d, 0x88fd1802, 0x3b7aa391, 0x5caf8a56, 0xbddd8a18,
    0x9187837c, 0xc5e102b2, 0xf192cbcd, 0xb2078d1a, 0xf593e52b, 0xbbc96404,
    0x2fe5b7f9, 0x5eab8eac, 0x65a14a04, 0x6dd6f5f8,  0x8835b42, 0x713fffa2,
    0x3cab1b6f, 0x63dd42a0, 0xb81a7b1d, 0x29079afa, 0xdecc1486, 0x18494012,
    0x68d8efa2, 0x873b540e, 0x3052b642, 0x81f4f7ca, 0x44337e55, 0x86c7973e,
    0x83539726, 0x8e498a90, 0xe808ec02, 0x48d76271, 0xdeb0aa12, 0xb7a9e735,
    0x8f1d0a6e, 0x8a1fd93a, 0xd2e1d0d1, 0x9093fa5f, 0xbe74ea0b, 0xd3ea1b9d,
    0x51a573dd, 0x51634e92, 0xb3275702, 0x4c41d2c6, 0x6717fa39, 0x56a8a01c,
    0x948328b1, 0xdbaf1fa2, 0x3bffc889, 0x7fb44be7, 0x475d8534, 0x5eba8280,
    0xc090c33b,  0x35857a4, 0x95b1eb1f, 0x5dcd3652, 0x1b973827, 0x5e24070c,
    0x7367a5f9,   0xf3ef4e, 0xb3bcada3, 0xcf3e4033, 0x1a9d0bd3, 0xa46b9dd7,
    0xa234566b, 0x61f4a52d, 0x44844992, 0xfaffdd23, 0x14187d04, 0x7c600063,
    0xa9417956, 0xd10faca2, 0x78eea53d, 0x474c90cf, 0x37f42e48, 0xa3765510,
    0xe6ffe2fb, 0xe83350f0, 0x3e5ec58d, 0xdeeedce8, 0xb5c0da93, 0xa01ebf2b,
    0xb3ed287a, 0x13beec00,  0x26c78ab, 0xc845a9e2, 0xb6315b6a, 0xe5d5238e,
    0x62dc97c6, 0x6a52c674, 0x370f9e65, 0xe777884a, 0x3beb6b4d, 0x95cdc493,
    0xb265b0ee, 0xac4fc47f, 0xc415321c, 0xe9fc60eb, 0x4237062e, 0x566dd09c,
     0x5af69ed, 0xa7589924, 0xe030c8ed, 0x3f52439e, 0x85ac98d2, 0x2026a870,
    0xfdf004ca, 0x28329ab3, 0x70c78bcc, 0x111a1094, 0x9036b901, 0x712f6316,
    0x4354f3e7,  0x617eda8, 0x7fe89a8b, 0x637d01f5,  0xfa54e14, 0x72fb1b49,
    0xd82afef8, 0x1055a07d,  0xf4c5845, 0x4f112d16, 0xe2c0936d, 0x96923c87,
    0xf5b59c3c,  0x3a1f284, 0xc63a1157, 0x72553aee, 0x88502921, 0xbdf13b8c,
    0xf54a4761, 0x54447c57, 0xeefbdb16, 0x19c11553, 0x1b06df20, 0xe4395998,
    0x3bf794dc, 0x261c8938, 0x875f1bb9, 0x8b29fd75, 0xc50dca03, 0xe95eacb5,
    0x121c9f64, 0xdce5b8fa, 0x167cf21d, 0xab1f9401, 0xc7eb6480, 0x48f0ac44,
    0x31325eca, 0x70186f9e, 0x9887957d, 0xfd0dbfee, 0xb799e1be, 0xb1c38ca7,
     0xc32efad, 0xfc36ca9d, 0xff307dd2, 0x7b941dc3, 0x9182f20d, 0x78a5c74c,
     0xbc99c47, 0xf82cb4ad, 0x4bbb0ecf, 0xbc816aba, 0xa47337b1, 0x5242aa7a,
    0x40b92e52, 0x3f55045c, 0x6deac45d, 0x52c0256a,  0x209e49e, 0x381585e2,
    0x2d4774c2, 0x5fb51d73, 0x1b456773, 0x180d4405, 0x4b72be7f, 0x54464f51,
    0xfbfbdac7, 0xb9df9e63, 0x12ad4163, 0x5d56c751, 0x6d4b9a3e, 0x75d9c4e1,
    0x1804e2a5, 0x3e77ee18, 0x12924b1a,  0xcfd4d14, 0xc5cd423e, 0xe3533b5b,
    0xa53ef834, 0x6f8b9c36, 0xb9e06f14, 0x76d680b9, 0x52ca46d8,  0x8445336,
    0xbb412e03, 0x2ae29f22, 0xeec357b8, 0x45bd2fb7
  };
  
  return w_cog[(w_nud >> 0) & 255] +
         w_cog[(w_nud >> 8) & 255] +
         w_cog[(w_nud >> 16) & 255] +
         w_cog[(w_nud >> 24) & 255];
}

#if 0
/* _nub_xw(): equivalent to _nub_pin_atom(), for (xw).
*/
static u4_nub
_nub_xw(u4_xw xw)
{
  u4_xw xw_seed = 0x18d0a625;

  if ( !xw ) {
    return xw_seed;
  }
  else {
    while ( 1 ) {
      u4_xw xw_hash = xw_seed;

      xw_hash ^= xw;
      xw_hash = u4_sbox_mix(xw_hash);
      xw_hash &= 0x7fffffff;

      if ( xw_hash ) {
        return xw_hash;
      }
      else xw_seed++;
    }
  }
} 
#endif

/* _nub_pin_atom(): compute the nub of (pin), an open atom.
*/
static u4_nub
_nub_pin_atom(u4_pin_open_atom pin)
{
#if 1
  uint32_t w_len = u4_a_bin(pin, 5);
  uint32_t w_zon = 0x18d0a625;
  uint32_t w_i;

  while ( 1 ) {
    uint32_t w_gid = w_zon;
    uint32_t w_dav;

    for ( w_i=0; w_i < w_len; w_i++ ) {
      w_gid ^= u4_a_word(pin, w_i);
      w_gid = _nnub_mash(w_gid);
    }
    w_dav = 0x7fffffff & w_gid;

    if ( w_dav ) {
      return w_dav;
    } 
    else w_zon++;
  }
#else
  u4_sw sw      = u4_open_atom_sw(pin);
  u4_xw xw_seed = 0x18d0a625;

  while ( 1 ) {
    u4_xw xw_hash = xw_seed;
    u4_pw pw;

    for ( pw=0; pw < sw; pw++ ) {
      u4_xw xw_pw = u4_open_atom_xw(pin, pw);

      xw_hash ^= xw_pw;
      xw_hash = u4_sbox_mix(xw_hash);
    }
    xw_hash &= 0x7fffffff;

    if ( xw_hash ) {
      return xw_hash;
    }
    else xw_seed++;
  }
#endif
}

/* _nub_pin_cell(): compute the nub of (pin), an open cell.
*/
static u4_nub
_nub_pin_cell(u4_pin_open_cell pin)
{
#if 1
  u4_noun hed   = u4_ch(pin);
  u4_noun tel   = u4_ct(pin);
  uint32_t   w_lus = u4_n_nub(hed);
  uint32_t   w_biq = u4_n_nub(tel);
  uint32_t   w_hur = (w_lus ^ (w_biq >> 24) ^ (w_biq << 8));

  while ( 1 ) {
    uint32_t w_dav = 0x7fffffff & _nnub_mash(w_hur);

    if ( w_dav ) {
      return w_dav;
    } 
    else w_hur++;
  }
#else
  u4_noun head = u4_open_cell_head(pin);
  u4_noun tail = u4_open_cell_tail(pin);
  {
    u4_nub nub_head = u4_n_nub(head);
    u4_nub nub_tail = u4_n_nub(tail);
    u4_xw  xw_both  = ((~nub_head) ^ (nub_tail >> 24) ^ (nub_tail << 8));
 
    return _nub_xw(xw_both);
  }
#endif
}

/* u4_n_nub():
**
**  Return the nub [31-bit nonzero insecure hash] of (noun).
*/
u4_nub
u4_n_nub(u4_noun noun)
{
  u4_assert(noun != u4_bull);

  if ( u4_noun_is_cod(noun) ) {
    return _nub_pin_atom(noun);
  }
  else {
    u4_pin pin = noun;

    if ( u4_pin_is_fort(pin) ) {
      return u4_stub;
    }
    else {
      u4_nub nub;

      if ( (nub = u4_open_nub(pin)) ) {
        return nub;
      }
      else {
        if ( u4_pin_is_atom(pin) ) {
          nub = _nub_pin_atom(pin);
        }
        else nub = _nub_pin_cell(pin);

        u4_open_nub(pin) = nub;
        return nub;
      }
    }
  }
}

/* u4_n_nib():
**
**   Return the nub of the nub of (noun).
*/
u4_nub
u4_n_nib(u4_noun noun)
{
  u4_xw xw_nub = u4_n_nub(noun);

  return u4_n_nub(u4_cod_in(xw_nub));
}

/* u4_n_true():
**
**   Return 1 if (noun) is 0, 0 if (noun) is 1.  Otherwise, trip.
*/
u4_t
u4_n_true(u4_noun noun)
{
  if ( u4_n_eq(u4_noun_0, noun) ) {
    return 1;
  }
  else if ( u4_n_eq(u4_noun_1, noun) ) {
    return 0;
  }
  else return u4_trip;
}

/* u4_n_snip_():
**
**   Return nock(b (0 a)), or u4_bull if there is no such.
*/
u4_noun
u4_n_snip_(u4_atom a,
           u4_noun b)
{
  u4_st st = (u4_a_bin(a, 0) - 1);
  u4_pt i;
  mpz_t mp_a;

  u4_a_gmp(a, mp_a);

  for ( i=0; i < st; i++ ) {
    u4_st st_at = (st - (i + 1));

    if ( u4_n_atom(b) ) {
      mpz_clear(mp_a);

      return u4_bull;
    }
    else {
      if ( (mpz_tstbit(mp_a, st_at) == 0) ) {
        b = u4_ch(b);
      } else {
        b = u4_ct(b);
      }
    }
  }
  mpz_clear(mp_a);
  return b;
}

/* u4_a_bin(): 
**
**   Return the size of (atom) in bits, rounded up to
**   (1 << gt). 
**
**   For example, (gt = 3) returns the size in bytes.
*/
u4_st
u4_a_bin(u4_atom atom,
         u4_gt   gt)
{
  u4_sw sw_atom;
  u4_xw xw_last;
  u4_sw sw_rest;

  if ( u4_noun_is_cod(atom) ) {
    xw_last = u4_cod_out(atom);
    sw_rest = 0;
    sw_atom = xw_last ? 1 : 0;
  } 
  else {
    u4_pin pin_atom = atom;

    if ( u4_pin_is_cell(pin_atom) ) {
      return u4_trip;
    }
    else if ( u4_pin_is_fort(pin_atom) ) {
      return u4_stub;
    } 
    else {
      sw_atom = u4_open_atom_sw(pin_atom);
      xw_last = u4_open_atom_xw(pin_atom, (sw_atom - 1));
      sw_rest = (sw_atom - 1);
    }
  }
 
  switch ( gt ) {
    case 0:
    case 1:
    case 2: {
      u4_st st_rest = (sw_rest << 5);
      u4_st st_last = u4_word_bits(xw_last);
      u4_st st = (st_rest + st_last);

      return u4_bblock(st, gt);
    }
    case 3: {
      u4_sb sb_rest = (sw_rest << 2);
      u4_sb sb_last = u4_word_bytes(xw_last);
      u4_sb sb = (sb_rest + sb_last);

      return sb;
    }
    case 4: {
      u4_sh sh_rest = (sw_rest << 1);
      u4_sh sh_last = u4_word_halves(xw_last);
      u4_sh sh = (sh_rest + sh_last);

      return sh;
    }
    default: {
      return u4_bblock(sw_atom, (gt - 5));
    }
  }
}

/* u4_a_bit():
**
**   Return bit (pt) of (atom).
*/
u4_t
u4_a_bit(u4_atom atom,
         u4_pt   pt)
{
  if ( u4_noun_is_cod(atom) ) {
    if ( pt >= 31 ) {
      return 0;
    }
    else return (1 & (u4_cod_out(atom) >> pt));
  }
  else {
    u4_pin pin_atom = atom;

    if ( u4_pin_is_cell(pin_atom) ) {
      return u4_trip;
    }
    else if ( u4_pin_is_fort(pin_atom) ) {
      return u4_stub;
    } 
    else {
      u4_pw pw = (pt >> 5);
      u4_hw hw = (pt & 31);
      {
        u4_sw sw_atom = u4_open_atom_sw(pin_atom);

        if ( pw >= sw_atom ) {
          return 0;
        }
        else return (1 & (u4_open_atom_xw(pin_atom, pw) >> hw));
      }
    }
  }
}

/* u4_a_byte():
**
**   Return byte (pb) of (atom).
*/
u4_xb
u4_a_byte(u4_atom atom,
          u4_pb   pb)
{
  u4_pw  pw = (pb >> 2);
  u4_hbw hbw = (pb & 3);
  {
    u4_xw xw = u4_a_word(atom, pw);
    u4_xb xb = (255 & (xw >> (8 * hbw)));

    return xb;
  }
}

/* u4_a_word():
**
**   Return word (pw) of (atom).
*/
u4_xw
u4_a_word(u4_atom atom,
          u4_pw   pw)
{
  if ( u4_noun_is_cod(atom) ) {
    if ( pw != 0 ) {
      return 0;
    }
    else return u4_cod_out(atom);
  }
  else {
    u4_pin pin_atom = atom;

    if ( u4_pin_is_cell(pin_atom) ) {
      return u4_trip;
    } 
    else if ( u4_pin_is_fort(pin_atom) ) {
      return u4_stub;
    } 
    else {
      u4_sw sw_atom = u4_open_atom_sw(pin_atom);

      if ( pw >= sw_atom ) {
        return 0;
      }
      else return u4_open_atom_xw(pin_atom, pw);
    }
  }
}

/* u4_a_words():
**
**   Copy words from (atom) into (xw), starting at (pw)
**   and continuing for (sw).
**
**   (xw) must be of length (sw).
*/
void
u4_a_words(u4_atom atom,
           u4_xw   *xw,
           u4_pw   pw,
           u4_sw   sw)
{
  if ( !sw ) {
    if ( u4_n_cell(atom) ) {
      u4_trip;
    }
  }
  else {
    u4_i i;

    if ( u4_noun_is_cod(atom) ) {
      u4_xw xw_atom = u4_cod_out(atom);

      if ( pw == 0 ) {
        *xw = xw_atom;
        xw++;
        sw--;
      }

      for ( i=0; i < sw; i++ ) {
        xw[i] = 0;
      }
    }
    else {
      u4_pin pin_atom = atom;

      if ( u4_pin_is_cell(pin_atom) ) {
        u4_trip;
      }
      else if ( u4_pin_is_fort(pin_atom) ) {
        u4_stub;
      } 
      else {
        u4_sw sw_atom = u4_open_atom_sw(pin_atom);
        
        if ( pw >= sw_atom ) {
          for ( i=0; i < sw; i++ ) {
            xw[i] = 0;
          }
        }
        else {
          if ( (pw + sw) <= sw_atom ) {
            for ( i=0; i < sw; i++ ) {
              xw[i] = u4_open_atom_xw(pin_atom, (pw + i));
            }
          }
          else {
            u4_sw sw_out = (pw + sw) - sw_atom;
            u4_sw sw_in  = (sw - sw_out);

            for ( i=0; i < sw_in; i++ ) {
              xw[i] = u4_open_atom_xw(pin_atom, (pw + i));
            }
            for ( i=0; i < sw_out; i++ ) {
              xw[sw_in + i] = 0;
            }
          }
        }
      }
    }
  }
}

/* u4_a_bytes(): 
**
**   Copy bytes from (atom) into (xb), starting at (pb)
**   and continuing for (sb).
**
**   (xb) must be of length (sb).
*/
void
u4_a_bytes(u4_atom atom,
           u4_xb   *xb,
           u4_pb   pb,
           u4_sb   sb)
{
  if ( !sb ) {
    if ( u4_n_cell(atom) ) {
      u4_trip;
    }
  }
  else {
    u4_i i;

    if ( u4_noun_is_cod(atom) ) {
      u4_xw xw_atom = u4_cod_out(atom);

      for ( i=0; i < sb; i++ ) {
        if ( (pb + i) >= 4 ) {
          xb[i] = 0;
        }
        else {
          xb[i] = 255 & (xw_atom >> (8 * (pb + i)));
        }
      }
    }
    else {
      u4_pin pin_atom = atom;

      if ( u4_pin_is_cell(pin_atom) ) {
        u4_trip;
      }
      else if ( u4_pin_is_fort(pin_atom) ) {
        u4_stub;
      } 
      else {
        u4_sw sw_atom = u4_open_atom_sw(pin_atom);

        /* Efficiency: hand-coding may help.
        */
        for ( i=0; i < sb; i++ ) {
          u4_pw  pw_atom  = ((pb + i) >> 2);
          u4_hbw hbw_atom = ((pb + i) & 3);

          if ( pw_atom >= sw_atom ) {
            xb[i] = 0;
          }
          else {
            u4_xw xw_atom = u4_open_atom_xw(pin_atom, pw_atom);
            u4_xb xb_atom = (255 & (xw_atom >> (8 * hbw_atom)));

            xb[i] = xb_atom;
          }
        }
      }
    }
  }
}

/* u4_a_bits(): 
**
**   Xor bits from (atom) into (xw), starting at (hw) in
**   (xw[0]), reading from (pt) for (st).
**
**   (xw) must be of length (u4_bblock(hw + st, 5)).
*/
void
u4_a_bits(u4_atom atom,
          u4_xw   *xw,
          u4_hw   hw,
          u4_pt   pt,
          u4_st   st)
{
  u4_stub;
}

/* u4_a_gmp():
**
**   Copy (atom) into (mp).
**
**   The caller must free (mp) with mpz_clear, or equivalent.
*/
void
u4_a_gmp(u4_atom atom,
         mpz_t   mp)
{
  if ( u4_noun_is_cod(atom) ) {
    u4_xw xw_atom = u4_cod_out(atom);

    mpz_init_set_ui(mp, xw_atom);
  }
  else {
    u4_pin pin_atom = atom;

    if ( u4_pin_is_cell(pin_atom) ) {
      u4_trip;
    }
    else if ( u4_pin_is_fort(pin_atom) ) {
      u4_stub;
    } 
    else {
      u4_sw sw_atom = u4_open_atom_sw(pin_atom);

      if ( sw_atom >> 27 ) {
        u4_trip;
      }
      else {
        u4_st st_atom = (sw_atom << 5);

        /* Efficiency: horrible.  Import directly from cake.
        */
        mpz_init2(mp, st_atom);
        {
          u4_xw *xw = alloca(sw_atom << 2);
          u4_pw pw;

          for ( pw=0; pw < sw_atom; pw++ ) {
            xw[pw] = u4_open_atom_xw(pin_atom, pw);
          }
          mpz_import(mp, sw_atom, -1, 4, 0, 0, xw);
        }
      }
    }
  }
}

/* u4_a_wbail():
**
**   Produce (atom) as a 32-bit word, or bail with (code).
*/
u4_xw
u4_a_wbail(u4_atom           atom,
           enum u4_bail_code bail_code)
{
  if ( u4_n_cell(atom) ) {
    return u4_bail_out(bail_code);
  }
  else if ( u4_noun_is_cod(atom) ) {
    return u4_cod_out(atom);
  }
  else {
    u4_pin pin_atom = atom;

    if ( u4_pin_is_cell(pin_atom) ) {
      return u4_trip;
    }
    else if ( u4_pin_is_fort(pin_atom) ) {
      return u4_stub;
    } 
    else {
      u4_sw sw_atom = u4_open_atom_sw(pin_atom);

      if ( sw_atom == 1 ) {
        return u4_open_atom_xw(pin_atom, 0);
      }
      else return u4_bail_out(bail_code);
    }
  }
  return u4_stub;
}


/* u4_b_fork(): 
**
**   True iff, in (noun), (*p) and (*q) are a bush fork.
*/
u4_t
u4_b_fork(u4_noun noun,
          u4_noun *p,
          u4_noun *q)
{
  if ( u4_n_cell(noun) && u4_n_cell(u4_ch(noun)) ) {
    if ( p ) *p = u4_ch(noun);
    if ( q ) *q = u4_ct(noun);
    return 1;
  }
  return 0;
}

/* u4_b_p():
**
**   True iff (noun) is of the form (stem *p).
*/
u4_t 
u4_b_p(u4_noun noun,
       u4_atom stem,
       u4_noun *p)
{
  if ( u4_n_cell(noun) && u4_n_eq(u4_ch(noun), stem) ) {
    if (p) *p = u4_ct(noun);
    return 1;
  }
  return 0;
}

/* u4_b_pq():
**
**   True iff (noun) is of the form (stem *p *q).
*/
u4_t 
u4_b_pq(u4_noun noun,
        u4_atom stem,
        u4_noun *p,
        u4_noun *q)
{
  if ( u4_n_cell(noun) && u4_n_eq(u4_ch(noun), stem) ) {
    u4_noun noun_3 = u4_ct(noun);

    if ( u4_n_cell(noun_3) ) {
      if (p) *p = u4_ch(noun_3);
      if (q) *q = u4_ct(noun_3);
      return 1;
    }
  }
  return 0;
}

/* u4_b_pqr():
**
**   True iff (noun) is of the form (stem *p *q *r).
*/
u4_t 
u4_b_pqr(u4_noun noun,
         u4_atom stem,
         u4_noun *p,
         u4_noun *q,
         u4_noun *r)
{
  if ( u4_n_cell(noun) && u4_n_eq(u4_ch(noun), stem) ) {
    u4_noun noun_3 = u4_ct(noun);

    if ( u4_n_cell(noun_3) ) {
      u4_noun noun_7 = u4_ct(noun_3);

      if ( u4_n_cell(noun_7) ) {
        if (p) *p = u4_ch(noun_3);
        if (q) *q = u4_ch(noun_7);
        if (r) *r = u4_ct(noun_7);
        return 1;
      }
    }
  }
  return 0;
}

/* u4_b_pqrs():
**
**   True iff (noun) is of the form (stem *p *q *r *s).
*/
u4_t 
u4_b_pqrs(u4_noun noun,
          u4_atom stem,
          u4_noun *p,
          u4_noun *q,
          u4_noun *r, 
          u4_noun *s)
{
  if ( u4_n_cell(noun) && u4_n_eq(u4_ch(noun), stem) ) {
    u4_noun noun_3 = u4_ct(noun);

    if ( u4_n_cell(noun_3) ) {
      u4_noun noun_7 = u4_ct(noun_3);

      if ( u4_n_cell(noun_7) ) {
        u4_noun noun_15 = u4_ct(noun_7);

        if ( u4_n_cell(noun_15) ) {
          if (p) *p = u4_ch(noun_3);
          if (q) *q = u4_ch(noun_7);
          if (r) *r = u4_ch(noun_15);
          if (s) *s = u4_ct(noun_15);
          return 1;
        }
      }
    }
  }
  return 0;
}
