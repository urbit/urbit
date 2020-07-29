/* noun/urth.c
**
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <ctype.h>

#include "all.h"
#include "ur/hashcons.h"

/* _cu_met_3(): atom bytewidth a la u3r_met(3, ...)
*/
static inline c3_w
_cu_met_3(u3a_atom* vat_u)
{
  c3_w  len_w = vat_u->len_w;
  c3_w* buf_w = vat_u->buf_w;

  if ( !len_w ) {
    return 0;
  }
  else {
    c3_w gal_w = len_w - 1;
    c3_w daz_w = buf_w[gal_w];

    return (gal_w << 2)
            + ((daz_w >> 24) ? 4 : (daz_w >> 16) ? 3 : (daz_w >> 8) ? 2 : 1);
  }
}

//  XX this is morally correct, but not useful
//  for deduplicating the loom
//
#if 0
static inline ur_nref
_cu_atom_to_ref(u3a_atom* vat_u, ur_root_t *r)
{
  ur_nref ref;

  switch ( vat_u->len_w ) {
    case 2: {
      ref = ur_coin64(r, ( ((c3_d)vat_u->buf_w[1]) << 32
                         | ((c3_d)vat_u->buf_w[0]) ));
    } break;

    case 1: {
      ref = ur_coin64(r, (c3_d)vat_u->buf_w[0]);
    } break;


    default: {
      c3_assert( vat_u->len_w );

      c3_y* byt_y = (c3_y*)vat_u->buf_w;
      c3_w  len_w = _cu_met_3(vat_u);

      ref = ur_coin_bytes(r, byt_y, (c3_d)len_w);
    } break;
  }

  return ref;
}
#endif

/* _cu_atom_to_ref(): indirect u3 atom to ur_nref.
*/
static inline ur_nref
_cu_atom_to_ref(u3a_atom* vat_u, ur_root_t *r)
{
  c3_assert( vat_u->len_w );

  c3_y* byt_y = (c3_y*)vat_u->buf_w;
  c3_w  len_w = _cu_met_3(vat_u);

  return ur_coin_bytes(r, byt_y, (c3_d)len_w);
}

/* _cu_box_check(): check loom allocation box for relocation pointer.
*/
static inline c3_o
_cu_box_check(u3a_noun* som_u, ur_nref* ref)
{
  u3a_box* box_u = u3a_botox(som_u);
  c3_w*    box_w = (void*)box_u;

  if ( 0xffffffff == box_w[0] ) {
    *ref = ( ((c3_d)box_w[2]) << 32
           | ((c3_d)box_w[1]) );
    return c3y;
  }

  return c3n;
}

/* _cu_box_stash(): overwrite an allocation box with relocation pointer.
*/
static inline void
_cu_box_stash(u3a_noun* som_u, ur_nref ref)
{
  u3a_box* box_u = u3a_botox(som_u);
  c3_w*    box_w = (void*)box_u;

  //  overwrite u3a_atom with reallocated reference
  //
  box_w[0] = 0xffffffff;
  box_w[1] = ref & 0xffffffff;
  box_w[2] = ref >> 32;
}

//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head cell=^]
//        [%tail cell=^ hed-mug=@]
//    ==
//

#define STACK_ROOT 0
#define STACK_HEAD 1
#define STACK_TAIL 2

typedef struct _cu_frame_s
{
  c3_y      tag_y;
  u3a_cell* cel_u;
  ur_nref     ref;
} _cu_frame;

typedef struct _cu_stack_s
{
  c3_w       pre_w;
  c3_w       siz_w;
  c3_w       fil_w;
  _cu_frame* fam_u;
} _cu_stack;

/* _cu_stack_push(): push a "stack" frame.
*/
static inline void
_cu_stack_push(_cu_stack *s, c3_y tag_y, u3a_cell* cel_u, ur_nref ref)
{
  if ( s->fil_w == s->siz_w ) {
    c3_w nex_w = s->pre_w + s->siz_w;
    s->fam_u   = c3_realloc(s->fam_u, nex_w * sizeof(*s->fam_u));
    s->pre_w   = s->siz_w;
    s->siz_w   = nex_w;
  }

  _cu_frame* fam_u = &(s->fam_u[s->fil_w++]);
  fam_u->tag_y = tag_y;
  fam_u->cel_u = cel_u;
  fam_u->ref   = ref;
}

/* _cu_from_loom(): reallocate [a] off loom, in [r].
*/
static ur_nref
_cu_from_loom(ur_root_t *r, u3_noun a)
{
  ur_nref   ref;

  _cu_stack s = { .pre_w = 89, .siz_w = 144, .fil_w = 0, .fam_u = 0 };
  s.fam_u = c3_malloc((s.pre_w + s.siz_w) * sizeof(*s.fam_u));
  _cu_stack_push(&s, STACK_ROOT, 0, 0);

  advance: {
    //  u3 direct == ur direct
    //
    if ( c3y == u3a_is_cat(a) ) {
      ref = (ur_nref)a;
      goto retreat;
    }
    else {
      u3a_noun* som_u = u3a_to_ptr(a);

      //  all bits set == already reallocated
      //
      if ( c3y == _cu_box_check(som_u, &ref) ) {
        goto retreat;
      }
      else if ( c3y == u3a_is_atom(a) ) {
        ref = _cu_atom_to_ref((u3a_atom*)som_u, r);
        _cu_box_stash(som_u, ref);
        goto retreat;
      }
      else {
        u3a_cell* cel_u = (u3a_cell*)som_u;
        _cu_stack_push(&s, STACK_HEAD, cel_u, 0);
        a = cel_u->hed;
        goto advance;
      }
    }
  }

  retreat: {
    _cu_frame fam_u = s.fam_u[--s.fil_w];

    switch ( fam_u.tag_y ) {
      default:          c3_assert(0);
      case STACK_ROOT:  break;

      case STACK_HEAD: {
        _cu_stack_push(&s, STACK_TAIL, fam_u.cel_u, ref);
        a = fam_u.cel_u->tel;
        goto advance;
      }

      case STACK_TAIL: {
        u3a_cell* cel_u = fam_u.cel_u;
        ref = ur_cons(r, fam_u.ref, ref);
        _cu_box_stash((u3a_noun*)cel_u, ref);
        goto retreat;
      }
    }
  }

  free(s.fam_u);

  return ref;
}

/* _cu_ref_to_noun(): lookup/allocate [ref] on the loom.
*/
static u3_noun
_cu_ref_to_noun(ur_nref ref, u3_noun* vat, u3_noun* cel)
{
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: {
      if ( 0x7fffffffULL > ref ) {
        return (u3_atom)ref;
      }
      else {
        c3_w wor_w[2];

        wor_w[0] = ref & 0xffffffff;
        wor_w[1] = ref >> 32;

        return u3i_words(2, wor_w);
      }
    } break;

    case ur_iatom:  return vat[ur_nref_idx(ref)];

    case ur_icell:  return cel[ur_nref_idx(ref)];
  }
}

typedef struct _cu_vec_s {
  ur_nvec_t* vec_u;
  ur_root_t* rot_u;
} _cu_vec;

/* _cu_hamt_walk(): reallocate key/value pair in hamt walk.
*/
static void
_cu_hamt_walk(u3_noun kev, void* ptr)
{
  _cu_vec*   dat_u = (_cu_vec*)ptr;
  ur_nvec_t* vec_u = dat_u->vec_u;
  ur_root_t* rot_u = dat_u->rot_u;

  vec_u->refs[vec_u->fill++] = _cu_from_loom(rot_u, kev);
}

/* u3u_uniq(): hash-cons roots off-loom, reallocate on loom.
*/
void
u3u_uniq(void)
{
  c3_assert( &(u3H->rod_u) == u3R );

  ur_root_t *r = ur_hcon_init();

  //  allow read/write on the whole loom, bypassing page tracking
  //
  if ( 0 != mprotect((void *)u3_Loom, u3a_bytes, (PROT_READ | PROT_WRITE)) ) {
    c3_assert(0);
  }

  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);

  ur_nref  ken = _cu_from_loom(r, u3A->roc);

  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);


  c3_w   cod_w = u3h_wyt(u3R->jed.cod_p);
  ur_nvec_t  v;

  fprintf(stderr, "hc: cold count %u\r\n", cod_w);

  {
    _cu_vec dat_u = { .vec_u = &v, .rot_u = r };
    ur_nvec_init(&v, cod_w);
    u3h_walk_with(u3R->jed.cod_p, _cu_hamt_walk, &dat_u);
  }


  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);

  u3m_pave(c3y, c3n);
  // XX wtf?
  u3R->jed.hot_p = u3h_new();

  u3_atom *vat;
  u3_noun *cel;

  {
    ur_atoms_t *atoms = &(r->atoms);
    uint64_t    *lens = atoms->lens;
    uint8_t    **byts = atoms->bytes;
    uint64_t  i, fill = atoms->fill;

    vat = calloc(fill, sizeof(u3_atom));

    for ( i = 0; i < fill; i++ ) {
      vat[i] = u3i_bytes(lens[i], byts[i]);
      //  XX mug?
    }
  }

  {
    ur_cells_t *cells = &(r->cells);
    ur_nref     *heds = cells->heads, *tals = cells->tails;
    uint64_t  i, fill = cells->fill;
    u3_noun  hed, tal;

    cel = calloc(fill, sizeof(u3_noun));

    for ( i = 0; i < fill; i++ ) {
      hed = _cu_ref_to_noun(heds[i], vat, cel);
      tal = _cu_ref_to_noun(tals[i], vat, cel);
      cel[i] = u3nc(hed, tal);
      //  XX mug?
    }
  }

  u3A->roc = cel[ur_nref_idx(ken)];

  {
    uint32_t  i;
    ur_nref ref;
    u3_noun kev;

    for ( i = 0; i < cod_w; i++) {
      ref = v.refs[i];
      kev = cel[ur_nref_idx(ref)];
      u3h_put(u3R->jed.cod_p, u3h(kev), u3k(u3t(kev)));
      u3z(kev);
    }
  }

  //  mark all pages dirty
  //
  memset((void*)u3P.dit_w, 0xff, u3a_pages >> 3);
}
