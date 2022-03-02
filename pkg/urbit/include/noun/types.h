//! @file types.h

#ifndef U3_TYPES_H
#define U3_TYPES_H

#include "c/types.h"

//! Pointer offset into u3_Loom.
//!
//! Use a `_p` suffix for variables of this type, which should be declared with
//! u3p().
typedef c3_w u3_post;
#define u3p(type) u3_post

//! Tagged noun pointer.
//!
//! A noun pointer can be interpreted as one of three types depending on the
//! value of bits 31 and 30:
//! ```
//! ----------------------------------------------
//!  bit 31 | bit 30 | type of noun       | alias
//! ----------------------------------------------
//!    0    |   *    | direct 31-bit atom | cat
//!    1    |   0    | indirect atom      | pug
//!    1    |   1    | indirect cell      | pom
//!
//! ```
//! If a noun is an indirect atom or an indirect cell, bits 0-29 are a word
//! offset from u3_Loom. See u3_post.
typedef c3_w u3_noun;

//! u3_noun which may be u3_none (i.e. not a noun).
//! Maybe a noun. If not a noun, then value is u3_none.
typedef u3_noun u3_weak;
#define u3_none (u3_noun)0xffffffff

//! Atom.
typedef u3_noun u3_atom;

//! Term, which is an atom with aura (`@tas`).
typedef u3_noun u3_term;

//! Cell.
typedef u3_noun u3_cell;

//! Unary noun function.
typedef u3_noun (*u3_funk)(u3_noun);

//! Binary noun function.
typedef u3_noun (*u3_funq)(u3_noun, u3_noun);

#endif /* ifndef U3_TYPES_H */
