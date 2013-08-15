/* include/fake/fake.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* A bit: u4_noun_0 or u4_noun_1.
    */
      typedef u4_atom u4_bit;

    /* A twig: subtree address.
    **
    ** 1 is the root; 2n is the head, 2n+1 is the tail.
    */
      typedef u4_atom u4_twig;

    /* A log: a classic rightward list.
    **
    ** A cow is any noun, as an item in a log.
    */
      typedef u4_noun u4_log;
      typedef u4_noun u4_cow;

    /* A bag: either (0) or ((pig) l.bag r.bag).
    **
    ** A pig is any noun, as an item in a bag.
    **
    ** A bag implements a logical set (not a multiset) as a priority
    ** tree or "treap," ordered by pig nub and prioritized by pig nib.  A bag
    ** may not contain duplicate pigs.
    **
    ** (Prioritization ensures that any set of pigs maps to one unique
    ** bag, regardless of insertion order.)
    */
      typedef u4_noun u4_pig;
      typedef u4_noun u4_bag;

    /* A tab: either (0) or ((tag dog) l.tab r.tab).
    **
    ** A tag or dog is any noun.  A hog is a (tag dog) cell.
    **
    ** A tab implements a logical set of (tag dog) pairs as a priority
    ** tree or "treap," ordered by tag nub and prioritized by tag nib.  A tab
    ** may not contain duplicate tags.
    **
    ** (Prioritization ensures that any set (tag dog) maps to one unique
    ** bag, regardless of insertion order.)
    **
    */
      typedef u4_noun u4_tab;
      typedef u4_noun u4_tag;
      typedef u4_noun u4_dog;
      typedef u4_cell u4_hog;

    /* Pseudo-parameterized types.
    **
    ** u4_log_(type) is a log of u4_type.
    */
#     define u4_log_(type) u4_log
#     define u4_bag_(type) u4_bag
#     define u4_tab_(type) u4_tab

    /* A prep, for dumping.
    **
    **  (text) | (.glue *prep) | (.nail *prep)
    */
      typedef u4_noun u4_prep;

    /* A dump, or list of printable lines.
    */
      typedef u4_log u4_dump;

    /* A text label.
    */
      typedef u4_noun u4_mote;

    /* Units.
    */
      typedef u4_noun u4_unit;

  /** Dependent includes.
  **/
#   include "u4/fake/op.h"
#   include "u4/fake/log.h"
#   include "u4/fake/bag.h"
#   include "u4/fake/tab.h"
#   include "u4/fake/goof.h"
#   include "u4/fake/pump.h"
#   include "u4/fake/ugly.h"
