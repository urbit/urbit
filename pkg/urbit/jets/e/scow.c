/* j/3/scow.c
**
*/
#include "all.h"

#include <ctype.h>
#include <stdio.h>  // for fprintf
#include <math.h>   // for ceil/floor

static
c3_y to_digit(c3_y tig)
{
  if (tig >= 10) {
    return 87 + tig;
  } else {
    return '0' + tig;
  }
}

static
c3_y to_w_digit(c3_y tig)
{
  if (tig == 63) {
    return '~';
  } else if (tig == 62) {
    return '-';
  } else if (tig >= 36) {
    return 29 + tig;
  } else if (tig >= 10) {
    return 87 + tig;
  } else {
    return '0' + tig;
  }
}

// gives the characters for a four 'digit' small hex atom.
static
void
_x_co_four(c3_w src, c3_y* a, c3_y* b, c3_y* c, c3_y* d)
{
  *d = to_digit(src & 0xF);
  src >>= 4;
  *c = to_digit(src & 0xF);
  src >>= 4;
  *b = to_digit(src & 0xF);
  src >>= 4;
  *a = to_digit(src & 0xF);
}

// The parser always prints two digits on 0 in y-co.
static
void
_y_co_two(c3_w src, c3_y* a, c3_y* b)
{
  *b = to_digit(src % 10);
  *a = to_digit(src / 10);
}

//
static
u3_noun
_add_year(c3_w year, u3_noun out)
{
  while (year > 0) {
    out = u3nc(to_digit(year % 10), out);
    year = year / 10;
  }

  return out;
}

static
u3_noun
_print_da(u3_noun cor, u3_atom raw_da)
{
  u3_noun hok = u3j_cook("u3we_scow_print_da", u3k(cor), "yore");
  u3_noun yod = u3n_slam_on(hok, u3k(raw_da));

  u3_noun out = 0;

  u3_atom age, year, month, day, hour, min, sec, f;
  if (c3n == u3r_mean(yod, 4, &age,
                      5, &year,
                      6, &month,
                      14, &day,
                      30, &hour,
                      62, &min,
                      126, &sec,
                      127, &f,
                      0)) {
    return u3m_bail(c3__exit);
  }

  if (f != 0) {
    u3_noun f_list = u3qb_flop(f);

    for (u3_noun cur = f_list;
         _(u3a_is_cell(cur));
         cur = u3t(cur)) {
      if (_(u3a_is_cat(u3h(cur)))) {
        c3_y a, b, c, d;
        _x_co_four(u3h(cur), &a, &b, &c, &d);
        out = u3nq('.', a, b, u3nt(c, d, out));
      } else {
        // No way to deal with big atoms. fall back.
        u3z(yod);
        u3z(out);
        u3z(f_list);
        return u3_none;
      }
    }

    u3z(f_list);
    out = u3nc('.', out);
  }

  // if there isn't a hex list and the h/m/s are all 0, skip printing hours.
  if (f != 0 || hour != 0 || min != 0 || sec != 0) {
    if (!_(u3a_is_cat(hour)) ||
        !_(u3a_is_cat(min)) ||
        !_(u3a_is_cat(sec))) {
      // Input is weird, fallback to nock.
      u3z(yod);
      u3z(out);
      return u3_none;
    }

    c3_y sa, sb, ma, mb, ha, hb;
    _y_co_two(sec, &sa, &sb);
    out = u3nq('.', sa, sb, out);

    _y_co_two(min, &ma, &mb);
    out = u3nq('.', ma, mb, out);

    _y_co_two(hour, &ha, &hb);
    out = u3nq('.', ha, hb, out);

    out = u3nc('.', out);
  }

  // We always print the Y.M.D. Unlike others, these numbers are unconstrained
  // by length, but in practice, the month number and day number can only be up
  // to two digits because of +yore. We still need to remove 0 prefixes,
  // though.
  if (!_(u3a_is_cat(day)) || day > 99 ||
      !_(u3a_is_cat(month)) || month > 99 ||
      !_(u3a_is_cat(year))) {
    // Input is weird, fallback to nock.
    u3z(yod);
    u3z(out);
    return u3_none;
  }

  c3_y da, db;
  _y_co_two(day, &da, &db);
  out = u3nc(db, out);
  if (da != '0') {
    out = u3nc(da, out);
  }
  out = u3nc('.', out);

  c3_y ma, mb;
  _y_co_two(month, &ma, &mb);
  out = u3nc(mb, out);
  if (ma != '0') {
    out = u3nc(ma, out);
  }
  out = u3nc('.', out);

  // suffix the year with a '-' for BC dates
  if (age == c3n) {
    out = u3nc('-', out);
  }

  // The year part is the only place where we have to explicitly loop over the
  // input because it can be arbitrarily large or small.
  out = _add_year(year, out);

  out = u3nc('~', out);

  u3z(yod);
  return out;
}

static
u3_noun
_print_p(u3_atom cor, u3_atom p)
{
  // Scramble the raw number to the concealed version.
  u3_noun ob = u3j_cook("u3we_scow_ob_p", u3k(cor), "ob");
  u3_noun hok = u3j_cook("u3we_scow_fein_p", ob, "fein");
  u3_atom sxz = u3n_slam_on(hok, u3k(p));

  // Simple galaxy case
  if (c3y == u3qa_lth(sxz, 256)) {
    c3_y a, b, c;
    u3_po_to_suffix(sxz, &a, &b, &c);
    u3z(sxz);
    return u3nq('~', a, b, u3nc(c, 0));
  }

  u3_atom dyy = u3qc_met(4, sxz);
  if (!_(u3a_is_cat(dyy))) {
    u3z(sxz);
    u3z(dyy);
    return u3_none;
  }

  u3_noun list = 0;
  for (c3_w imp = 0; imp != dyy; ++imp) {
    c3_w log = u3qc_end(4, 1, sxz);
    c3_w prefix = u3qc_rsh(3, 1, log);
    c3_w suffix = u3qc_end(3, 1, log);

    c3_y a, b, c, d, e, f;
    u3_po_to_prefix(prefix, &a, &b, &c);
    u3_po_to_suffix(suffix, &d, &e, &f);

    if (imp % 4 == 0) {
      if (imp != 0) {
        list = u3nt('-', '-', list);
      }
    } else {
      list = u3nc('-', list);
    }

    list = u3nq(a, b, c, u3nq(d, e, f, list));

    sxz = u3qc_rsh(4, 1, sxz);
  }

  u3z(sxz);
  return u3nc('~', list);
}

static
u3_noun
_print_ud(u3_atom ud)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(ud);

  do {
    if (between == 3) {
      list = u3nc('.', list);
      between = 0;
    }

    list = u3nc(u3ka_add(u3qa_mod(ud, 10), '0'), list);
    between++;
    ud = u3ka_div(ud, 10);
  } while (ud != 0);

  return list;
}

////////////////////////////////////////////////////////////////////////////

#define SIG 0x7e
#define HEP 0x2d

u3_noun
_u3i_chars(c3_c* str_c)
{
  u3_atom a = (str_c[2] << 16) | (str_c[1] << 8) | str_c[0];
  return a;
}

u3_noun
_u3_po_to_prefix(u3_noun id)
{
  u3_atom a;
  switch (id) {
    case 0: a = _u3i_chars("doz"); break;
    case 1: a = _u3i_chars("mar"); break;
    case 2: a = _u3i_chars("bin"); break;
    case 3: a = _u3i_chars("wan"); break;
    case 4: a = _u3i_chars("sam"); break;
    case 5: a = _u3i_chars("lit"); break;
    case 6: a = _u3i_chars("sig"); break;
    case 7: a = _u3i_chars("hid"); break;
    case 8: a = _u3i_chars("fid"); break;
    case 9: a = _u3i_chars("lis"); break;
    case 10: a = _u3i_chars("sog"); break;
    case 11: a = _u3i_chars("dir"); break;
    case 12: a = _u3i_chars("wac"); break;
    case 13: a = _u3i_chars("sab"); break;
    case 14: a = _u3i_chars("wis"); break;
    case 15: a = _u3i_chars("sib"); break;
    case 16: a = _u3i_chars("rig"); break;
    case 17: a = _u3i_chars("sol"); break;
    case 18: a = _u3i_chars("dop"); break;
    case 19: a = _u3i_chars("mod"); break;
    case 20: a = _u3i_chars("fog"); break;
    case 21: a = _u3i_chars("lid"); break;
    case 22: a = _u3i_chars("hop"); break;
    case 23: a = _u3i_chars("dar"); break;
    case 24: a = _u3i_chars("dor"); break;
    case 25: a = _u3i_chars("lor"); break;
    case 26: a = _u3i_chars("hod"); break;
    case 27: a = _u3i_chars("fol"); break;
    case 28: a = _u3i_chars("rin"); break;
    case 29: a = _u3i_chars("tog"); break;
    case 30: a = _u3i_chars("sil"); break;
    case 31: a = _u3i_chars("mir"); break;
    case 32: a = _u3i_chars("hol"); break;
    case 33: a = _u3i_chars("pas"); break;
    case 34: a = _u3i_chars("lac"); break;
    case 35: a = _u3i_chars("rov"); break;
    case 36: a = _u3i_chars("liv"); break;
    case 37: a = _u3i_chars("dal"); break;
    case 38: a = _u3i_chars("sat"); break;
    case 39: a = _u3i_chars("lib"); break;
    case 40: a = _u3i_chars("tab"); break;
    case 41: a = _u3i_chars("han"); break;
    case 42: a = _u3i_chars("tic"); break;
    case 43: a = _u3i_chars("pid"); break;
    case 44: a = _u3i_chars("tor"); break;
    case 45: a = _u3i_chars("bol"); break;
    case 46: a = _u3i_chars("fos"); break;
    case 47: a = _u3i_chars("dot"); break;
    case 48: a = _u3i_chars("los"); break;
    case 49: a = _u3i_chars("dil"); break;
    case 50: a = _u3i_chars("for"); break;
    case 51: a = _u3i_chars("pil"); break;
    case 52: a = _u3i_chars("ram"); break;
    case 53: a = _u3i_chars("tir"); break;
    case 54: a = _u3i_chars("win"); break;
    case 55: a = _u3i_chars("tad"); break;
    case 56: a = _u3i_chars("bic"); break;
    case 57: a = _u3i_chars("dif"); break;
    case 58: a = _u3i_chars("roc"); break;
    case 59: a = _u3i_chars("wid"); break;
    case 60: a = _u3i_chars("bis"); break;
    case 61: a = _u3i_chars("das"); break;
    case 62: a = _u3i_chars("mid"); break;
    case 63: a = _u3i_chars("lop"); break;
    case 64: a = _u3i_chars("ril"); break;
    case 65: a = _u3i_chars("nar"); break;
    case 66: a = _u3i_chars("dap"); break;
    case 67: a = _u3i_chars("mol"); break;
    case 68: a = _u3i_chars("san"); break;
    case 69: a = _u3i_chars("loc"); break;
    case 70: a = _u3i_chars("nov"); break;
    case 71: a = _u3i_chars("sit"); break;
    case 72: a = _u3i_chars("nid"); break;
    case 73: a = _u3i_chars("tip"); break;
    case 74: a = _u3i_chars("sic"); break;
    case 75: a = _u3i_chars("rop"); break;
    case 76: a = _u3i_chars("wit"); break;
    case 77: a = _u3i_chars("nat"); break;
    case 78: a = _u3i_chars("pan"); break;
    case 79: a = _u3i_chars("min"); break;
    case 80: a = _u3i_chars("rit"); break;
    case 81: a = _u3i_chars("pod"); break;
    case 82: a = _u3i_chars("mot"); break;
    case 83: a = _u3i_chars("tam"); break;
    case 84: a = _u3i_chars("tol"); break;
    case 85: a = _u3i_chars("sav"); break;
    case 86: a = _u3i_chars("pos"); break;
    case 87: a = _u3i_chars("nap"); break;
    case 88: a = _u3i_chars("nop"); break;
    case 89: a = _u3i_chars("som"); break;
    case 90: a = _u3i_chars("fin"); break;
    case 91: a = _u3i_chars("fon"); break;
    case 92: a = _u3i_chars("ban"); break;
    case 93: a = _u3i_chars("mor"); break;
    case 94: a = _u3i_chars("wor"); break;
    case 95: a = _u3i_chars("sip"); break;
    case 96: a = _u3i_chars("ron"); break;
    case 97: a = _u3i_chars("nor"); break;
    case 98: a = _u3i_chars("bot"); break;
    case 99: a = _u3i_chars("wic"); break;
    case 100: a = _u3i_chars("soc"); break;
    case 101: a = _u3i_chars("wat"); break;
    case 102: a = _u3i_chars("dol"); break;
    case 103: a = _u3i_chars("mag"); break;
    case 104: a = _u3i_chars("pic"); break;
    case 105: a = _u3i_chars("dav"); break;
    case 106: a = _u3i_chars("bid"); break;
    case 107: a = _u3i_chars("bal"); break;
    case 108: a = _u3i_chars("tim"); break;
    case 109: a = _u3i_chars("tas"); break;
    case 110: a = _u3i_chars("mal"); break;
    case 111: a = _u3i_chars("lig"); break;
    case 112: a = _u3i_chars("siv"); break;
    case 113: a = _u3i_chars("tag"); break;
    case 114: a = _u3i_chars("pad"); break;
    case 115: a = _u3i_chars("sal"); break;
    case 116: a = _u3i_chars("div"); break;
    case 117: a = _u3i_chars("dac"); break;
    case 118: a = _u3i_chars("tan"); break;
    case 119: a = _u3i_chars("sid"); break;
    case 120: a = _u3i_chars("fab"); break;
    case 121: a = _u3i_chars("tar"); break;
    case 122: a = _u3i_chars("mon"); break;
    case 123: a = _u3i_chars("ran"); break;
    case 124: a = _u3i_chars("nis"); break;
    case 125: a = _u3i_chars("wol"); break;
    case 126: a = _u3i_chars("mis"); break;
    case 127: a = _u3i_chars("pal"); break;
    case 128: a = _u3i_chars("las"); break;
    case 129: a = _u3i_chars("dis"); break;
    case 130: a = _u3i_chars("map"); break;
    case 131: a = _u3i_chars("rab"); break;
    case 132: a = _u3i_chars("tob"); break;
    case 133: a = _u3i_chars("rol"); break;
    case 134: a = _u3i_chars("lat"); break;
    case 135: a = _u3i_chars("lon"); break;
    case 136: a = _u3i_chars("nod"); break;
    case 137: a = _u3i_chars("nav"); break;
    case 138: a = _u3i_chars("fig"); break;
    case 139: a = _u3i_chars("nom"); break;
    case 140: a = _u3i_chars("nib"); break;
    case 141: a = _u3i_chars("pag"); break;
    case 142: a = _u3i_chars("sop"); break;
    case 143: a = _u3i_chars("ral"); break;
    case 144: a = _u3i_chars("bil"); break;
    case 145: a = _u3i_chars("had"); break;
    case 146: a = _u3i_chars("doc"); break;
    case 147: a = _u3i_chars("rid"); break;
    case 148: a = _u3i_chars("moc"); break;
    case 149: a = _u3i_chars("pac"); break;
    case 150: a = _u3i_chars("rav"); break;
    case 151: a = _u3i_chars("rip"); break;
    case 152: a = _u3i_chars("fal"); break;
    case 153: a = _u3i_chars("tod"); break;
    case 154: a = _u3i_chars("til"); break;
    case 155: a = _u3i_chars("tin"); break;
    case 156: a = _u3i_chars("hap"); break;
    case 157: a = _u3i_chars("mic"); break;
    case 158: a = _u3i_chars("fan"); break;
    case 159: a = _u3i_chars("pat"); break;
    case 160: a = _u3i_chars("tac"); break;
    case 161: a = _u3i_chars("lab"); break;
    case 162: a = _u3i_chars("mog"); break;
    case 163: a = _u3i_chars("sim"); break;
    case 164: a = _u3i_chars("son"); break;
    case 165: a = _u3i_chars("pin"); break;
    case 166: a = _u3i_chars("lom"); break;
    case 167: a = _u3i_chars("ric"); break;
    case 168: a = _u3i_chars("tap"); break;
    case 169: a = _u3i_chars("fir"); break;
    case 170: a = _u3i_chars("has"); break;
    case 171: a = _u3i_chars("bos"); break;
    case 172: a = _u3i_chars("bat"); break;
    case 173: a = _u3i_chars("poc"); break;
    case 174: a = _u3i_chars("hac"); break;
    case 175: a = _u3i_chars("tid"); break;
    case 176: a = _u3i_chars("hav"); break;
    case 177: a = _u3i_chars("sap"); break;
    case 178: a = _u3i_chars("lin"); break;
    case 179: a = _u3i_chars("dib"); break;
    case 180: a = _u3i_chars("hos"); break;
    case 181: a = _u3i_chars("dab"); break;
    case 182: a = _u3i_chars("bit"); break;
    case 183: a = _u3i_chars("bar"); break;
    case 184: a = _u3i_chars("rac"); break;
    case 185: a = _u3i_chars("par"); break;
    case 186: a = _u3i_chars("lod"); break;
    case 187: a = _u3i_chars("dos"); break;
    case 188: a = _u3i_chars("bor"); break;
    case 189: a = _u3i_chars("toc"); break;
    case 190: a = _u3i_chars("hil"); break;
    case 191: a = _u3i_chars("mac"); break;
    case 192: a = _u3i_chars("tom"); break;
    case 193: a = _u3i_chars("dig"); break;
    case 194: a = _u3i_chars("fil"); break;
    case 195: a = _u3i_chars("fas"); break;
    case 196: a = _u3i_chars("mit"); break;
    case 197: a = _u3i_chars("hob"); break;
    case 198: a = _u3i_chars("har"); break;
    case 199: a = _u3i_chars("mig"); break;
    case 200: a = _u3i_chars("hin"); break;
    case 201: a = _u3i_chars("rad"); break;
    case 202: a = _u3i_chars("mas"); break;
    case 203: a = _u3i_chars("hal"); break;
    case 204: a = _u3i_chars("rag"); break;
    case 205: a = _u3i_chars("lag"); break;
    case 206: a = _u3i_chars("fad"); break;
    case 207: a = _u3i_chars("top"); break;
    case 208: a = _u3i_chars("mop"); break;
    case 209: a = _u3i_chars("hab"); break;
    case 210: a = _u3i_chars("nil"); break;
    case 211: a = _u3i_chars("nos"); break;
    case 212: a = _u3i_chars("mil"); break;
    case 213: a = _u3i_chars("fop"); break;
    case 214: a = _u3i_chars("fam"); break;
    case 215: a = _u3i_chars("dat"); break;
    case 216: a = _u3i_chars("nol"); break;
    case 217: a = _u3i_chars("din"); break;
    case 218: a = _u3i_chars("hat"); break;
    case 219: a = _u3i_chars("nac"); break;
    case 220: a = _u3i_chars("ris"); break;
    case 221: a = _u3i_chars("fot"); break;
    case 222: a = _u3i_chars("rib"); break;
    case 223: a = _u3i_chars("hoc"); break;
    case 224: a = _u3i_chars("nim"); break;
    case 225: a = _u3i_chars("lar"); break;
    case 226: a = _u3i_chars("fit"); break;
    case 227: a = _u3i_chars("wal"); break;
    case 228: a = _u3i_chars("rap"); break;
    case 229: a = _u3i_chars("sar"); break;
    case 230: a = _u3i_chars("nal"); break;
    case 231: a = _u3i_chars("mos"); break;
    case 232: a = _u3i_chars("lan"); break;
    case 233: a = _u3i_chars("don"); break;
    case 234: a = _u3i_chars("dan"); break;
    case 235: a = _u3i_chars("lad"); break;
    case 236: a = _u3i_chars("dov"); break;
    case 237: a = _u3i_chars("riv"); break;
    case 238: a = _u3i_chars("bac"); break;
    case 239: a = _u3i_chars("pol"); break;
    case 240: a = _u3i_chars("lap"); break;
    case 241: a = _u3i_chars("tal"); break;
    case 242: a = _u3i_chars("pit"); break;
    case 243: a = _u3i_chars("nam"); break;
    case 244: a = _u3i_chars("bon"); break;
    case 245: a = _u3i_chars("ros"); break;
    case 246: a = _u3i_chars("ton"); break;
    case 247: a = _u3i_chars("fod"); break;
    case 248: a = _u3i_chars("pon"); break;
    case 249: a = _u3i_chars("sov"); break;
    case 250: a = _u3i_chars("noc"); break;
    case 251: a = _u3i_chars("sor"); break;
    case 252: a = _u3i_chars("lav"); break;
    case 253: a = _u3i_chars("mat"); break;
    case 254: a = _u3i_chars("mip"); break;
    case 255: a = _u3i_chars("fip"); break;
    default: u3m_bail(c3__exit);
  }
  return a;
}

u3_noun
_u3_po_to_suffix(u3_noun id)
{
  u3_atom a;
  switch (id) {
    case 0: a = _u3i_chars("zod"); break;
    case 1: a = _u3i_chars("nec"); break;
    case 2: a = _u3i_chars("bud"); break;
    case 3: a = _u3i_chars("wes"); break;
    case 4: a = _u3i_chars("sev"); break;
    case 5: a = _u3i_chars("per"); break;
    case 6: a = _u3i_chars("sut"); break;
    case 7: a = _u3i_chars("let"); break;
    case 8: a = _u3i_chars("ful"); break;
    case 9: a = _u3i_chars("pen"); break;
    case 10: a = _u3i_chars("syt"); break;
    case 11: a = _u3i_chars("dur"); break;
    case 12: a = _u3i_chars("wep"); break;
    case 13: a = _u3i_chars("ser"); break;
    case 14: a = _u3i_chars("wyl"); break;
    case 15: a = _u3i_chars("sun"); break;
    case 16: a = _u3i_chars("ryp"); break;
    case 17: a = _u3i_chars("syx"); break;
    case 18: a = _u3i_chars("dyr"); break;
    case 19: a = _u3i_chars("nup"); break;
    case 20: a = _u3i_chars("heb"); break;
    case 21: a = _u3i_chars("peg"); break;
    case 22: a = _u3i_chars("lup"); break;
    case 23: a = _u3i_chars("dep"); break;
    case 24: a = _u3i_chars("dys"); break;
    case 25: a = _u3i_chars("put"); break;
    case 26: a = _u3i_chars("lug"); break;
    case 27: a = _u3i_chars("hec"); break;
    case 28: a = _u3i_chars("ryt"); break;
    case 29: a = _u3i_chars("tyv"); break;
    case 30: a = _u3i_chars("syd"); break;
    case 31: a = _u3i_chars("nex"); break;
    case 32: a = _u3i_chars("lun"); break;
    case 33: a = _u3i_chars("mep"); break;
    case 34: a = _u3i_chars("lut"); break;
    case 35: a = _u3i_chars("sep"); break;
    case 36: a = _u3i_chars("pes"); break;
    case 37: a = _u3i_chars("del"); break;                                                                                                                                                                                       
    case 38: a = _u3i_chars("sul"); break;                                                                                                                                                                                       
    case 39: a = _u3i_chars("ped"); break;                                                                                                                                                                                       
    case 40: a = _u3i_chars("tem"); break;                                                                                                                                                                                       
    case 41: a = _u3i_chars("led"); break;                                                                                                                                                                                       
    case 42: a = _u3i_chars("tul"); break;                                                                                                                                                                                       
    case 43: a = _u3i_chars("met"); break;                                                                                                                                                                                       
    case 44: a = _u3i_chars("wen"); break;                                                                                                                                                                                       
    case 45: a = _u3i_chars("byn"); break;                                                                                                                                                                                       
    case 46: a = _u3i_chars("hex"); break;                                                                                                                                                                                       
    case 47: a = _u3i_chars("feb"); break;                                                                                                                                                                                       
    case 48: a = _u3i_chars("pyl"); break;                                                                                                                                                                                       
    case 49: a = _u3i_chars("dul"); break;
    case 50: a = _u3i_chars("het"); break;
    case 51: a = _u3i_chars("mev"); break;
    case 52: a = _u3i_chars("rut"); break;
    case 53: a = _u3i_chars("tyl"); break;
    case 54: a = _u3i_chars("wyd"); break;
    case 55: a = _u3i_chars("tep"); break;
    case 56: a = _u3i_chars("bes"); break;
    case 57: a = _u3i_chars("dex"); break;
    case 58: a = _u3i_chars("sef"); break;
    case 59: a = _u3i_chars("wyc"); break;
    case 60: a = _u3i_chars("bur"); break;
    case 61: a = _u3i_chars("der"); break;
    case 62: a = _u3i_chars("nep"); break;
    case 63: a = _u3i_chars("pur"); break;
    case 64: a = _u3i_chars("rys"); break;
    case 65: a = _u3i_chars("reb"); break;
    case 66: a = _u3i_chars("den"); break;
    case 67: a = _u3i_chars("nut"); break;
    case 68: a = _u3i_chars("sub"); break;
    case 69: a = _u3i_chars("pet"); break;
    case 70: a = _u3i_chars("rul"); break;
    case 71: a = _u3i_chars("syn"); break;
    case 72: a = _u3i_chars("reg"); break;
    case 73: a = _u3i_chars("tyd"); break;
    case 74: a = _u3i_chars("sup"); break;
    case 75: a = _u3i_chars("sem"); break;
    case 76: a = _u3i_chars("wyn"); break;
    case 77: a = _u3i_chars("rec"); break;
    case 78: a = _u3i_chars("meg"); break;
    case 79: a = _u3i_chars("net"); break;
    case 80: a = _u3i_chars("sec"); break;
    case 81: a = _u3i_chars("mul"); break;
    case 82: a = _u3i_chars("nym"); break;
    case 83: a = _u3i_chars("tev"); break;
    case 84: a = _u3i_chars("web"); break;
    case 85: a = _u3i_chars("sum"); break;
    case 86: a = _u3i_chars("mut"); break;
    case 87: a = _u3i_chars("nyx"); break;
    case 88: a = _u3i_chars("rex"); break;
    case 89: a = _u3i_chars("teb"); break;
    case 90: a = _u3i_chars("fus"); break;
    case 91: a = _u3i_chars("hep"); break;
    case 92: a = _u3i_chars("ben"); break;
    case 93: a = _u3i_chars("mus"); break;
    case 94: a = _u3i_chars("wyx"); break;
    case 95: a = _u3i_chars("sym"); break;
    case 96: a = _u3i_chars("sel"); break;
    case 97: a = _u3i_chars("ruc"); break;
    case 98: a = _u3i_chars("dec"); break;
    case 99: a = _u3i_chars("wex"); break;
    case 100: a = _u3i_chars("syr"); break;
    case 101: a = _u3i_chars("wet"); break;
    case 102: a = _u3i_chars("dyl"); break;
    case 103: a = _u3i_chars("myn"); break;
    case 104: a = _u3i_chars("mes"); break;
    case 105: a = _u3i_chars("det"); break;
    case 106: a = _u3i_chars("bet"); break;
    case 107: a = _u3i_chars("bel"); break;
    case 108: a = _u3i_chars("tux"); break;
    case 109: a = _u3i_chars("tug"); break;
    case 110: a = _u3i_chars("myr"); break;
    case 111: a = _u3i_chars("pel"); break;
    case 112: a = _u3i_chars("syp"); break;
    case 113: a = _u3i_chars("ter"); break;
    case 114: a = _u3i_chars("meb"); break;
    case 115: a = _u3i_chars("set"); break;
    case 116: a = _u3i_chars("dut"); break;
    case 117: a = _u3i_chars("deg"); break;
    case 118: a = _u3i_chars("tex"); break;
    case 119: a = _u3i_chars("sur"); break;
    case 120: a = _u3i_chars("fel"); break;
    case 121: a = _u3i_chars("tud"); break;
    case 122: a = _u3i_chars("nux"); break;
    case 123: a = _u3i_chars("rux"); break;
    case 124: a = _u3i_chars("ren"); break;
    case 125: a = _u3i_chars("wyt"); break;
    case 126: a = _u3i_chars("nub"); break;
    case 127: a = _u3i_chars("med"); break;
    case 128: a = _u3i_chars("lyt"); break;
    case 129: a = _u3i_chars("dus"); break;
    case 130: a = _u3i_chars("neb"); break;
    case 131: a = _u3i_chars("rum"); break;
    case 132: a = _u3i_chars("tyn"); break;
    case 133: a = _u3i_chars("seg"); break;
    case 134: a = _u3i_chars("lyx"); break;
    case 135: a = _u3i_chars("pun"); break;
    case 136: a = _u3i_chars("res"); break;
    case 137: a = _u3i_chars("red"); break;
    case 138: a = _u3i_chars("fun"); break;
    case 139: a = _u3i_chars("rev"); break;
    case 140: a = _u3i_chars("ref"); break;
    case 141: a = _u3i_chars("mec"); break;
    case 142: a = _u3i_chars("ted"); break;
    case 143: a = _u3i_chars("rus"); break;
    case 144: a = _u3i_chars("bex"); break;
    case 145: a = _u3i_chars("leb"); break;
    case 146: a = _u3i_chars("dux"); break;
    case 147: a = _u3i_chars("ryn"); break;
    case 148: a = _u3i_chars("num"); break;
    case 149: a = _u3i_chars("pyx"); break;
    case 150: a = _u3i_chars("ryg"); break;
    case 151: a = _u3i_chars("ryx"); break;
    case 152: a = _u3i_chars("fep"); break;
    case 153: a = _u3i_chars("tyr"); break;
    case 154: a = _u3i_chars("tus"); break;
    case 155: a = _u3i_chars("tyc"); break;
    case 156: a = _u3i_chars("leg"); break;
    case 157: a = _u3i_chars("nem"); break;
    case 158: a = _u3i_chars("fer"); break;
    case 159: a = _u3i_chars("mer"); break;
    case 160: a = _u3i_chars("ten"); break;
    case 161: a = _u3i_chars("lus"); break;
    case 162: a = _u3i_chars("nus"); break;
    case 163: a = _u3i_chars("syl"); break;
    case 164: a = _u3i_chars("tec"); break;
    case 165: a = _u3i_chars("mex"); break;
    case 166: a = _u3i_chars("pub"); break;
    case 167: a = _u3i_chars("rym"); break;
    case 168: a = _u3i_chars("tuc"); break;
    case 169: a = _u3i_chars("fyl"); break;
    case 170: a = _u3i_chars("lep"); break;
    case 171: a = _u3i_chars("deb"); break;
    case 172: a = _u3i_chars("ber"); break;
    case 173: a = _u3i_chars("mug"); break;
    case 174: a = _u3i_chars("hut"); break;
    case 175: a = _u3i_chars("tun"); break;
    case 176: a = _u3i_chars("byl"); break;
    case 177: a = _u3i_chars("sud"); break;
    case 178: a = _u3i_chars("pem"); break;
    case 179: a = _u3i_chars("dev"); break;
    case 180: a = _u3i_chars("lur"); break;
    case 181: a = _u3i_chars("def"); break;
    case 182: a = _u3i_chars("bus"); break;
    case 183: a = _u3i_chars("bep"); break;
    case 184: a = _u3i_chars("run"); break;
    case 185: a = _u3i_chars("mel"); break;
    case 186: a = _u3i_chars("pex"); break;
    case 187: a = _u3i_chars("dyt"); break;
    case 188: a = _u3i_chars("byt"); break;
    case 189: a = _u3i_chars("typ"); break;
    case 190: a = _u3i_chars("lev"); break;
    case 191: a = _u3i_chars("myl"); break;
    case 192: a = _u3i_chars("wed"); break;
    case 193: a = _u3i_chars("duc"); break;
    case 194: a = _u3i_chars("fur"); break;
    case 195: a = _u3i_chars("fex"); break;
    case 196: a = _u3i_chars("nul"); break;
    case 197: a = _u3i_chars("luc"); break;
    case 198: a = _u3i_chars("len"); break;
    case 199: a = _u3i_chars("ner"); break;
    case 200: a = _u3i_chars("lex"); break;
    case 201: a = _u3i_chars("rup"); break;
    case 202: a = _u3i_chars("ned"); break;
    case 203: a = _u3i_chars("lec"); break;
    case 204: a = _u3i_chars("ryd"); break;
    case 205: a = _u3i_chars("lyd"); break;
    case 206: a = _u3i_chars("fen"); break;
    case 207: a = _u3i_chars("wel"); break;
    case 208: a = _u3i_chars("nyd"); break;
    case 209: a = _u3i_chars("hus"); break;
    case 210: a = _u3i_chars("rel"); break;
    case 211: a = _u3i_chars("rud"); break;
    case 212: a = _u3i_chars("nes"); break;
    case 213: a = _u3i_chars("hes"); break;
    case 214: a = _u3i_chars("fet"); break;
    case 215: a = _u3i_chars("des"); break;
    case 216: a = _u3i_chars("ret"); break;
    case 217: a = _u3i_chars("dun"); break;
    case 218: a = _u3i_chars("ler"); break;
    case 219: a = _u3i_chars("nyr"); break;
    case 220: a = _u3i_chars("seb"); break;
    case 221: a = _u3i_chars("hul"); break;
    case 222: a = _u3i_chars("ryl"); break;
    case 223: a = _u3i_chars("lud"); break;
    case 224: a = _u3i_chars("rem"); break;
    case 225: a = _u3i_chars("lys"); break;
    case 226: a = _u3i_chars("fyn"); break;
    case 227: a = _u3i_chars("wer"); break;
    case 228: a = _u3i_chars("ryc"); break;
    case 229: a = _u3i_chars("sug"); break;
    case 230: a = _u3i_chars("nys"); break;
    case 231: a = _u3i_chars("nyl"); break;
    case 232: a = _u3i_chars("lyn"); break;
    case 233: a = _u3i_chars("dyn"); break;
    case 234: a = _u3i_chars("dem"); break;
    case 235: a = _u3i_chars("lux"); break;
    case 236: a = _u3i_chars("fed"); break;
    case 237: a = _u3i_chars("sed"); break;
    case 238: a = _u3i_chars("bec"); break;
    case 239: a = _u3i_chars("mun"); break;
    case 240: a = _u3i_chars("lyr"); break;
    case 241: a = _u3i_chars("tes"); break;
    case 242: a = _u3i_chars("mud"); break;
    case 243: a = _u3i_chars("nyt"); break;
    case 244: a = _u3i_chars("byr"); break;
    case 245: a = _u3i_chars("sen"); break;
    case 246: a = _u3i_chars("weg"); break;
    case 247: a = _u3i_chars("fyr"); break;
    case 248: a = _u3i_chars("mur"); break;
    case 249: a = _u3i_chars("tel"); break;
    case 250: a = _u3i_chars("rep"); break;
    case 251: a = _u3i_chars("teg"); break;
    case 252: a = _u3i_chars("pec"); break;
    case 253: a = _u3i_chars("nel"); break;
    case 254: a = _u3i_chars("nev"); break;
    case 255: a = _u3i_chars("fes"); break;
    default: u3m_bail(c3__exit);
  }
  return a;
}

static
u3_noun
__print_p(c3_w p)
{
  // Scramble the raw number to the concealed version.
  u3_noun sxz = u3qe_fein_ob(p);
  c3_w prefix, suffix;
  u3_noun pre, suf;
  u3_atom aus = 0;
  fprintf(stderr, "p: %d\n", p);
  u3m_p("sxz", sxz);
  return u3_none;

  // Galaxies
  if (c3y == u3qa_lth(sxz, 256)) {
    suf = _u3_po_to_suffix(sxz);
    u3z(sxz);
    aus = suf;
    aus = u3qa_add(u3qc_lsh(2, 2, aus), SIG);
    u3z(suf);
    return aus;
  }

  u3_atom dyy = u3qc_met(4, sxz);   // number of @p components, 2^2^4
  if (!_(u3a_is_cat(dyy))) {
    u3z(sxz);
    u3z(dyy);
    return u3_none;
  }

  // Stars, planets, moons, and comets
  for (c3_w imp = 0; imp != dyy; ++imp) {
    c3_w log = u3qc_end(4, 1, sxz);
    prefix = u3qc_rsh(3, 1, log);
    suffix = u3qc_end(3, 1, log);

    pre = _u3_po_to_prefix(prefix);
    suf = _u3_po_to_suffix(suffix);

    // if not rightmost component, prepend hep first
    if (imp > 0) {
      aus = u3qa_add(u3qc_lsh(2, 2, aus), HEP);
    }
    // if middle term, prepend additional hex
    if (imp == 4) {
      aus = u3qa_add(u3qc_lsh(2, 2, aus), HEP);
    }

    aus = u3qa_add(u3qc_lsh(2, (imp>0)?6:0, aus), suf);
    aus = u3qa_add(u3qc_lsh(2, 6, aus), pre);

    sxz = u3qc_rsh(4, 1, sxz);
  }
  aus = u3qa_add(u3qc_lsh(2, 2, aus), SIG);
  u3z(sxz);
  u3z(pre);
  u3z(suf);
  return aus;
}

#undef SIG
#undef HEP

#define DOT 0x2e

static
u3_noun
__print_ud(c3_w ud)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  c3_w wrt = 0;
  c3_y tig;

  // make byte array for characters
  //  number of digits
  c3_w dig = (ud > 0) ? (c3_w)floor(log10((double)ud) + 1) : 1;
  //  number of dots
  c3_w blx = (ud > 0) ? (c3_w)floor(log10((double)ud-1.0) + 1) / 3 : 0;
  c3_w len = dig + blx;
  c3_y* aus = (c3_y*)u3a_malloc(len*sizeof(c3_y));

  c3_w idx = len-1;
  do {
    if (between == 3) {
      aus[idx--] = DOT;
      between = 0;
    }

    tig = ud % 10;
    wrt = to_digit(tig);
    aus[idx--] = wrt;
    between++;
    ud = ud / 10;
  } while (ud != 0);

  u3_noun aus_return = u3i_bytes(len, aus);
  u3a_free(aus);
  u3m_p("ud> aus_return", aus_return);
  return aus_return;
}

#undef DOT


#define DOT 0x2e
#define ZED 0x30
#define WEE 0x77

static
u3_noun
__print_uw(c3_w uw)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  c3_w wrt = 0;
  c3_y tig;

  // make byte array for characters
  //  number of digits
  c3_w dig = (uw > 0) ? (c3_w)floor(log10((double)uw)/log10(64.0) + 1) : 1;
  //  number of dots
  c3_w blx = (uw > 0) ? (c3_w)floor(log10((double)uw-1.0)/log10(64.0) + 1) / 5 : 0;
  c3_w len = dig + blx + 2;
  c3_y* aus = (c3_y*)u3a_malloc(len*sizeof(c3_y));

  c3_w idx = len-1;
  do {
    if (between == 5) {
      aus[idx--] = DOT;
      between = 0;
    }

    tig = uw % 64;
    wrt = to_w_digit(tig);
    aus[idx--] = wrt;
    between++;
    uw = uw / 64;
  } while (uw != 0);
  aus[1] = WEE;
  aus[0] = ZED;

  u3_noun aus_return = u3i_bytes(len, aus);
  u3a_free(aus);
  u3m_p("uw> aus_return", aus_return);
  return aus_return;
}

#undef DOT
#undef ZED
#undef WEE

////////////////////////////////////////////////////////////////////////////

static
u3_noun
_print_uv(u3_atom uv)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(uv);

  do {
    if (between == 5) {
      list = u3nc('.', list);
      between = 0;
    }

    c3_y tig = u3qa_mod(uv, 32);
    list = u3nc(to_digit(tig), list);
    between++;
    uv = u3ka_div(uv, 32);
  } while (uv != 0);

  return u3nt('0', 'v', list);
}

// works

#define DOT 0x2e
#define ZED 0x30
#define VEE 0x76

static
u3_noun
__print_uv(c3_w uv)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  c3_w wrt = 0;
  c3_y tig;

  // make byte array for characters
  //  number of digits
  c3_w dig = (uv > 0) ? (c3_w)floor(log10((double)uv)/log10(32.0) + 1) : 1;
  //  number of dots
  c3_w blx = (uv > 0) ? (c3_w)floor(log10((double)uv-1.0)/log10(32.0) + 1) / 5 : 0;
  c3_w len = dig + blx + 2;
  c3_y* aus = (c3_y*)u3a_malloc(len*sizeof(c3_y));

  c3_w idx = len-1;
  do {
    if (between == 5) {
      aus[idx--] = DOT;
      between = 0;
    }

    tig = uv % 32;
    wrt = to_digit(tig);
    aus[idx--] = wrt;
    between++;
    uv = uv / 32;
  } while (uv != 0);
  aus[1] = VEE;
  aus[0] = ZED;

  u3_noun aus_return = u3i_bytes(len, aus);
  u3a_free(aus);
  u3m_p("uv> aus_return", aus_return);
  return aus_return;
}

#undef DOT
#undef ZED
#undef VEE

static
u3_noun
_print_uw(u3_atom uw)
{
  // number of characters printed "between" periods.
  c3_i between = 0;
  u3_noun list = 0;

  // increase input refcount to be consumed in u3ka_div(), which will free each
  // intermediary state.
  u3k(uw);

  do {
    if (between == 5) {
      list = u3nc('.', list);
      between = 0;
    }

    c3_y tig = u3qa_mod(uw, 64);
    list = u3nc(to_w_digit(tig), list);
    between++;
    uw = u3ka_div(uw, 64);
  } while (uw != 0);

  return u3nt('0', 'w', list);
}

u3_noun
u3we_scow(u3_noun cor)
{
  u3_atom mod;
  u3_atom atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                           u3x_sam_3, &atom, 0)) {
    return u3m_bail(c3__exit);
  }
  u3m_p("scow> %x", atom);

  u3_noun knot;
  knot = u3we_scot(cor);
  if (knot == u3_none) {
    return knot;
  }
  u3_noun ret = u3qe_trip(knot);
  u3z(knot);
  u3m_p("scow> %x", ret);
  return ret;
}

u3_noun
u3we_scot(u3_noun cor)
{
  u3_atom mod;
  u3_atom atom;

  if (c3n == u3r_mean(cor, u3x_sam_2, &mod,
                           u3x_sam_3, &atom, 0)) {
    return u3m_bail(c3__exit);
  }
  //return u3_none;

  u3_noun tape;
  mpz_t u_mp;
  switch (mod) {
    case c3__da:
      //return __print_da(cor, atom);
      mpz_clear(u_mp);
      return u3_none;
      tape = _print_da(cor, atom);
      break;

    case 'p':
      mpz_clear(u_mp);
      return u3_none;
      u3r_mp(u_mp, atom);

      // if too big, send back to Hoon
      if (!(mpz_fits_ulong_p(u_mp))) {  // check against unsigned long int, 32 bits
        mpz_clear(u_mp);
        return u3_none;
      } else {
        c3_w uv_int = mpz_get_ui(u_mp);
        mpz_clear(u_mp);
        return __print_p(uv_int);
      }
      break;

    case c3__ud:
      u3r_mp(u_mp, atom);

      // if too big, send back to Hoon
      if (!(mpz_fits_ulong_p(u_mp))) {  // check against unsigned long int, 32 bits
        mpz_clear(u_mp);
        return u3_none;
      } else {
        c3_w uv_int = mpz_get_ui(u_mp);
        mpz_clear(u_mp);
        return __print_ud(uv_int);
      }
      break;

    case c3__uv:
      u3r_mp(u_mp, atom);

      // if too big, send back to Hoon
      if (!(mpz_fits_ulong_p(u_mp))) {  // check against unsigned long int, 32 bits
        mpz_clear(u_mp);
        return u3_none;
      } else {
        c3_w uv_int = mpz_get_ui(u_mp);
        mpz_clear(u_mp);
        return __print_uv(uv_int);
      }
      break;

    case c3__uw:
      u3r_mp(u_mp, atom);

      // if too big, send back to Hoon
      if (!(mpz_fits_ulong_p(u_mp))) {  // check against unsigned long int, 32 bits
        mpz_clear(u_mp);
        return u3_none;
      } else {
        c3_w uw_int = mpz_get_ui(u_mp);
        mpz_clear(u_mp);
        return __print_uw(uw_int);
      }
      break;

    default:
      return u3_none;
  }

  if (tape == u3_none) {
    return tape;
  }
  u3_noun ret = u3qc_rap(3, tape);
  u3z(tape);
  return ret;
}
