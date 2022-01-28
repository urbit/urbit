/* j/5/en_json.c
**
*/
#include "all.h"

/* helper structs
*/

typedef struct _json_buffer {
  c3_y *buf_y;
  c3_w len_w;
} json_buffer;

/* constants
*/

const c3_y _JSON_NULL[] = "null";
const c3_y _JSON_TRUE[] = "true";
const c3_y _JSON_FALSE[] = "false";
const c3_y _JSON_NEWLINE[] = "\\n";
const c3_y _JSON_DOQ[] = "\\\"";
const c3_y _JSON_BAS[] = "\\\\";
const c3_y _JSON_DEL[] = "\\u007f";
const c3_y *_JSON_UNICODES[] = {
  "\\u0000",  // 0
  "\\u0001",  // 1
  "\\u0002",  // 2
  "\\u0003",  // 3
  "\\u0004",  // 4
  "\\u0005",  // 5
  "\\u0006",  // 6
  "\\u0007",  // 7
  "\\u0008",  // 8
  "\\u0009",  // 9
  "\\u000a",  // 10
  "\\u000b",  // 11
  "\\u000c",  // 12
  "\\u000d",  // 13
  "\\u000e",  // 14
  "\\u000f",  // 15
  "\\u0010",  // 16
  "\\u0011",  // 17
  "\\u0012",  // 18
  "\\u0013",  // 19
  "\\u0014",  // 20
  "\\u0015",  // 21
  "\\u0016",  // 22
  "\\u0017",  // 23
  "\\u0018",  // 24
  "\\u0019",  // 25
  "\\u001a",  // 26
  "\\u001b",  // 27
  "\\u001c",  // 28
  "\\u001d",  // 29
  "\\u001e",  // 30
  "\\u001f",  // 31
};

/* forward declarations
*/

static c3_w
_measure(u3_noun a);

static void
_serialize(json_buffer*, u3_noun);

/* core jet logic
*/

static void
_append_char(json_buffer *buf_u, c3_y c_y)
{
  buf_u->buf_y[(buf_u->len_w)++] = c_y;
}

static void
_append_text(json_buffer *buf_u, const c3_y *buf_y, c3_w len_w)
{
  memcpy(&(buf_u->buf_y[buf_u->len_w]), buf_y, len_w);
  buf_u->len_w += len_w;
}

static c3_w
_measure_loobean(u3_noun a)
{
  switch ( a ) {
    default: u3m_bail(c3__exit);
    case c3y: return sizeof(_JSON_TRUE) - 1;
    case c3n: return sizeof(_JSON_FALSE) - 1;
  }
}

static void
_serialize_loobean(json_buffer *buf_u, u3_noun a)
{
  switch ( a ) {
    default: c3_assert(0);
    case c3y: return _append_text(buf_u, _JSON_TRUE, sizeof(_JSON_TRUE) - 1);
    case c3n: return _append_text(buf_u, _JSON_FALSE, sizeof(_JSON_FALSE) - 1);
  }
}

static c3_w
_measure_number(u3_noun a)
{
  if ( _(u3du(a)) ) {
    u3m_bail(c3__exit);
  }

  return u3r_met(3, a);
}

static void
_serialize_number(json_buffer *buf_u, u3_noun a)
{
  const c3_y *byt_y;

  // XX assumes little-endian
  //
  if ( c3y == u3a_is_cat(a) ) {
    byt_y = (c3_y*)&a;
  }
  else {
    u3a_atom* vat_u = u3a_to_ptr(a);
    byt_y = (c3_y*)vat_u->buf_w;
  }

  _append_text(buf_u, byt_y, u3r_met(3, a));
}

static c3_w
_measure_string(u3_noun a)
{
  if ( _(u3du(a)) ) {
    u3m_bail(c3__exit);
  }

  c3_w len_w = u3r_met(3, a);
  c3_w siz_w = 0;

  for (c3_w i = 0; i < len_w; ++i) {
    c3_y c_y = u3r_byte(i, a);

    switch ( c_y ) {
      case 0 ... 9:
      case 11 ... 31: {
        siz_w += 6;
      } break;

      case 10: {
        siz_w += sizeof(_JSON_NEWLINE) - 1;
      } break;

      case 34: {
        siz_w += sizeof(_JSON_DOQ) - 1;
      } break;

      case 92: {
        siz_w += sizeof(_JSON_BAS) - 1;
      } break;

      case 127: {
        siz_w += sizeof(_JSON_DEL) - 1;
      } break;

      default: {
        siz_w += 1;
      } break;
    }
  }

  // surrounding double quotes
  return (siz_w + 2);
}

static void
_serialize_string(json_buffer *buf_u, u3_noun a)
{
  c3_w len_w = u3r_met(3, a);

  _append_char(buf_u, '"');
  for (c3_w i = 0; i < len_w; ++i) {
    c3_y c_y = u3r_byte(i, a);

    switch ( c_y ) {
      case 0 ... 9:
      case 11 ... 31: {
        _append_text(buf_u, _JSON_UNICODES[c_y], 6);
      } break;

      case 10: {
        _append_text(buf_u, _JSON_NEWLINE, sizeof(_JSON_NEWLINE) - 1);
      } break;

      case 34: {
        _append_text(buf_u, _JSON_DOQ, sizeof(_JSON_DOQ) - 1);
      } break;

      case 92: {
        _append_text(buf_u, _JSON_BAS, sizeof(_JSON_BAS) - 1);
      } break;

      case 127: {
        _append_text(buf_u, _JSON_DEL, sizeof(_JSON_DEL) - 1);
      } break;

      default: {
        _append_char(buf_u, c_y);
      } break;
    }
  }
  _append_char(buf_u, '"');
}

static c3_w
_measure_array(u3_noun a)
{
  if ( u3_nul != a ) {
    u3_noun i, t = a;
    // array open brace
    c3_w    siz_w = 1;

    while ( u3_nul != t ) {
      u3x_cell(t, &i, &t);
      siz_w += _measure(i);
      // comma or array close brace
      siz_w += 1;
    }

    return siz_w;
  }
  else {
    // empty array
    return 2;
  }
}

static void
_serialize_array(json_buffer *buf_u, u3_noun a)
{
  _append_char(buf_u, '[');

  if ( u3_nul != a ) {
    u3_noun i, t = a;

    while ( u3_nul != t ) {
      u3x_cell(t, &i, &t);
      _serialize(buf_u, i);
      _append_char(buf_u, ',');
    }
    
    // Remove trailing comma from array contents
    --buf_u->len_w;
  }

  _append_char(buf_u, ']');
}

static c3_w
_measure_object_helper(u3_noun a)
{
  c3_w siz_w = 0;

  if ( u3_nul != a ) {
    u3_noun n_a, l_a, r_a;
    u3_noun pn_a, qn_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &pn_a, &qn_a);

    siz_w += _measure_object_helper(r_a);
    siz_w += _measure_object_helper(l_a);

    siz_w += _measure_string(pn_a);
    siz_w += _measure(qn_a);

    // colon and comma (or closing brace)
    siz_w += 2;
  }

  return siz_w;
}

static void
_serialize_object_helper(json_buffer *buf_u, u3_noun a)
{
  if ( u3_nul != a ) {
    u3_noun n_a, l_a, r_a;
    u3_noun pn_a, qn_a;
    u3x_trel(a, &n_a, &l_a, &r_a);
    u3x_cell(n_a, &pn_a, &qn_a);

    // order is important to match unjetted tree traversal
    _serialize_object_helper(buf_u, r_a);

    _serialize_string(buf_u, pn_a);
    _append_char(buf_u, ':');
    _serialize(buf_u, qn_a);
    _append_char(buf_u, ',');

    _serialize_object_helper(buf_u, l_a);
  }
}

static c3_w
_measure_object(u3_noun a)
{
  if ( u3_nul != a ) {
    // opening brace
    return 1 + _measure_object_helper(a);
  }
  else {
    // empty object
    return 2;
  }
}

static void
_serialize_object(json_buffer *buf_u, u3_noun a)
{
  _append_char(buf_u, '{');

  if ( u3_nul != a ) {
    _serialize_object_helper(buf_u, a);

    // Remove trailing comma from object contents
    --buf_u->len_w;
  }

  _append_char(buf_u, '}');
}

static c3_w
_measure(u3_noun a)
{
  if ( u3_nul == a ) {
    return sizeof(_JSON_NULL) - 1;
  }
  else {
    u3_noun s, p;
    u3x_cell(a, &s, &p);

    switch ( s ) {
      default: u3m_bail(c3__fail);
      case c3__a: return _measure_array(p);
      case c3__o: return _measure_object(p);
      case c3__b: return _measure_loobean(p);
      case c3__n: return _measure_number(p);
      case c3__s: return _measure_string(p);
    }
  }
}

static void
_serialize(json_buffer *buf_u, u3_noun a)
{
  if ( u3_nul == a ) {
    _append_text(buf_u, _JSON_NULL, sizeof(_JSON_NULL) - 1);
  }
  else {
    u3_noun s, p;
    u3x_cell(a, &s, &p);

    switch ( s ) {
      default: c3_assert(0);
      case c3__a: return _serialize_array(buf_u, p);
      case c3__o: return _serialize_object(buf_u, p);
      case c3__b: return _serialize_loobean(buf_u, p);
      case c3__n: return _serialize_number(buf_u, p);
      case c3__s: return _serialize_string(buf_u, p);
    }
  }
}

/* jet interface functions
*/

u3_atom
u3qe_en_json(u3_noun a)
{
  u3i_slab     sab_u;
  json_buffer  bof_u;
  json_buffer *buf_u = &bof_u;
  c3_w         siz_w = _measure(a);

  u3i_slab_init(&sab_u, 3, siz_w);
  buf_u->buf_y = sab_u.buf_y;
  buf_u->len_w = 0;

  // note that it's structurally integral to call measure before serialize
  _serialize(buf_u, a);

  return u3i_slab_mint_bytes(&sab_u);
}

u3_atom
u3ke_en_json(u3_noun a)
{
  u3_atom res = u3qe_en_json(a);
  u3z(a);
  return res;
}

u3_atom
u3we_en_json(u3_noun cor)
{
  return u3qe_en_json(u3x_at(u3x_sam, cor));
}
