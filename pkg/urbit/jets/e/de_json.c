/* j/5/de_json.c
**
*/
#include "all.h"
#include "pdjson.h"

/* custom code for calling parser imported from:
** https://github.com/skeeto/pdjson
*/

typedef struct _u3qedj_coll {
  // key flag
  c3_b    key_b : 1;
  // collection (list for array, map for object)
  u3_noun col;
  // store context for recursive arrays/objects:
  //   - pointer to tail for array
  //   - key for object
  union {
    u3_noun* tel;
    u3_atom  key;
  };
} u3qedj_coll;

static u3qedj_coll*
_push_stack(const u3a_pile *pil_u)
{
  u3qedj_coll *res_u = u3a_push(pil_u);
  u3a_pile_sane(pil_u);

  res_u->col = u3_nul;
  res_u->key = u3_none;

  return res_u;
}

static u3qedj_coll*
_pop_stack(const u3a_pile *pil_u)
{
  return u3a_pop(pil_u);
}

static void
_close_stack(const u3a_pile *pil_u)
{
  while ( c3n == u3a_pile_done(pil_u) ) {
    u3qedj_coll *tak_u = u3a_peek(pil_u);

    u3z(tak_u->col);
    if ( tak_u->key_b ) {
      u3z(tak_u->key);
    }

    u3a_drop(pil_u);
  }
}

static void
_close_on_error(json_stream *sam_u, const u3a_pile *pil_u)
{
  _close_stack(pil_u);
  json_close(sam_u);
}

static u3_atom
_json_get_string_as_atom(json_stream *sam_u) {
  // length returned by json_get_string includes the trailing null byte
  // it's possible for json_get_string to return a length of 0, but only if:
  //    - it's called directly after init
  //    - it's called directly after init_string
  size_t      len_i;
  const c3_c *str_c = json_get_string(sam_u, &len_i);
  return (len_i <= 1) ?
          u3_nul :
          u3i_bytes(len_i - 1, (const c3_y *)str_c);
}

static u3_noun
_parse(u3_atom txt)
{
  //
  // vars
  //

  u3qedj_coll *tak_u;

  json_allocator loc_u = {u3a_malloc, u3a_realloc, u3a_free};
  json_stream    sem_u;
  json_stream*   sam_u = &sem_u;

  u3a_pile  pel_u;
  u3a_pile *pil_u = &pel_u;

  u3_noun res = u3_none;
  u3_noun val;

  const c3_y *byt_y;
  c3_d        cnt_d;
  c3_w        len_w = u3r_met(3, txt);

  //
  // initialization
  //

  // XX assumes little-endian
  //
  if ( c3y == u3a_is_cat(txt) ) {
    byt_y = (c3_y*)&txt;
  }
  else {
    u3a_atom* vat_u = u3a_to_ptr(txt);
    byt_y = (c3_y*)vat_u->buf_w;
  }
  json_open_buffer(sam_u, byt_y, len_w);
  json_set_allocator(sam_u, &loc_u);
  u3a_pile_prep(pil_u, sizeof(u3qedj_coll));

  //
  // core logic
  //

  while ( json_peek(sam_u) != JSON_DONE ) {
    switch ( json_next(sam_u) ) {
      // unreachable barring programming error
      default: c3_assert(0);

      case JSON_ARRAY:
      case JSON_OBJECT: {
        tak_u = _push_stack(pil_u);
      } continue;

      case JSON_ARRAY_END: {
        val = u3nc(c3__a, tak_u->col);
        tak_u = _pop_stack(pil_u);
      } break;

      case JSON_OBJECT_END: {
        val = u3nc(c3__o, tak_u->col);
        tak_u = _pop_stack(pil_u);
      } break;

      case JSON_STRING: {
        if ( (json_get_context(sam_u, &cnt_d) == JSON_OBJECT) && (cnt_d & 1) ) {
          // since object key must be followed by value, skip ahead
          tak_u->key = _json_get_string_as_atom(sam_u);
          tak_u->key_b = 1;
          continue;
        }
        else {
          val = u3nc(c3__s, _json_get_string_as_atom(sam_u));
          break;
        }
      }

      case JSON_NUMBER: {
        // read number from string in the JSON reparser
        val = u3nc(c3__n, _json_get_string_as_atom(sam_u));
      } break;

      case JSON_TRUE: {
        val = u3nc(c3__b, c3y);
      } break;

      case JSON_FALSE: {
        val = u3nc(c3__b, c3n);
      } break;
        
      case JSON_NULL: {
        val = u3_nul;
      } break;

      case JSON_ERROR: {
        _close_on_error(sam_u, pil_u);
        return u3_nul;
      } break;
    }

    switch ( json_get_context(sam_u, &cnt_d) ) {
      // unreachable barring programming error
      default: c3_assert(0);

      case JSON_DONE: {
        res = val;
      } break;

      case JSON_ARRAY: {
        u3_noun* nex;
        u3_noun* hed;

        if ( tak_u->col == u3_nul ) {
          nex = &(tak_u->col);
        }
        else {
          nex = tak_u->tel;
        }

        *nex = u3i_defcons(&hed, &(tak_u->tel));
        *hed = val;
        *(tak_u->tel) = u3_nul;
      } break;

      case JSON_OBJECT: {
        // odd cnt_d and unset key weeded out by continue command on key
        c3_assert(!(cnt_d & 1));
        c3_assert(tak_u->key != u3_none);
        // cnt_d == 0 weeded out by continue command on array/object open
        c3_assert(cnt_d);

        tak_u->col = u3kdb_put(tak_u->col, tak_u->key, val);
        tak_u->key_b = 0;
        tak_u->key = u3_none;
      } break;
    }
  }

  //
  // clean up
  //

  c3_assert(c3y == u3a_pile_done(pil_u));

  // skip over whitespce
  while ( json_isspace(json_source_peek(sam_u)) ) {
    json_source_get(sam_u);
  }

  json_close(sam_u);

  // return null if trailing trash/multiple JSON objects
  if ( json_get_position(sam_u) != len_w ) {
    u3z(res);
    return u3_nul;
  }
  else {
    return u3nc(u3_nul, res);
  }
}

/* jet interface functions
*/

u3_noun
u3qe_de_json(u3_atom a)
{
  return _parse(a);
}

u3_noun
u3ke_de_json(u3_atom a)
{
  u3_noun res = u3qe_de_json(a);
  u3z(a);
  return res;
}

u3_noun
u3we_de_json(u3_noun cor)
{
  return u3qe_de_json(u3x_atom(u3x_at(u3x_sam, cor)));
}
