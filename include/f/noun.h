/* include/f/noun.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_noun: tagged pointer.
    **
    **  If bit 31 is 0, a u2_noun is a direct 31-bit atom ("cat").
    **  If bit 31 is 1 and bit 30 0, an indirect atom ("pug").
    **  If bit 31 is 1 and bit 30 1, an indirect cell ("pom").
    **
    ** Bits 0-29 are a word offset against u2_Loom.
    */
      typedef c3_w    u2_noun;

    /* u2_none - out-of-band noun.
    */
#     define u2_none  (u2_noun)0xffffffff

    /* Informative typedefs.  Use if you like.
    */
      typedef u2_noun u2_atom;          //  must be atom
      typedef u2_noun u2_term;          //  @tas
      typedef u2_noun u2_mote;          //  @tas
      typedef u2_noun u2_cell;          //  must be cell
      typedef u2_noun u2_trel;          //  must be triple
      typedef u2_noun u2_qual;          //  must be quadruple
      typedef u2_noun u2_quin;          //  must be quintuple
      typedef u2_noun u2_bean;          //  loobean: 0 == u2_yes, 1 == u2_no
      typedef u2_noun u2_weak;          //  may be u2_none

    /* u2_atom, u2_cell: logical atom and cell structures.
    */
      typedef struct {
        c3_w mug_w;
      } u2_cs_noun;

      typedef struct {
        c3_w mug_w;
        c3_w len_w;
        c3_w buf_w[0];
      } u2_cs_atom;

      typedef struct {
        c3_w    mug_w;
        u2_noun hed; 
        u2_noun tel;
      } u2_cs_cell;

    /* Inside a noun.
    */
#     define u2_co_is_cat(som)    (((som) >> 31) ? u2_no : u2_yes)
#     define u2_co_is_dog(som)    (((som) >> 31) ? u2_yes : u2_no)

#     define u2_co_is_pug(som)    ((2 == ((som) >> 30)) ? u2_yes : u2_no)
#     define u2_co_is_pom(som)    ((3 == ((som) >> 30)) ? u2_yes : u2_no)
#     define u2_co_to_off(som)    ((som) & 0x3fffffff)
#     define u2_co_to_ptr(som)    (u2_co_into(u2_co_to_off(som)))
#     define u2_co_to_wtr(som)    ((c3_w *)u2_co_to_ptr(som))
#     define u2_co_to_pug(off)    (off | 0x80000000)
#     define u2_co_to_pom(off)    (off | 0xc0000000)

#     define u2_co_is_atom(som)    u2_or(u2_co_is_cat(som), \
                                         u2_co_is_pug(som))
#     define u2_co_is_cell(som)    u2_co_is_pom(som)
#     define u2_co_de_twin(dog, dog_w)  ((dog & 0xc0000000) | u2_co_outa(dog_w))

#     define u2_co_h(som) \
        ( u2_so(u2_co_is_cell(som)) \
           ? ( ((u2_cs_cell *)u2_co_to_ptr(som))->hed )\
           : u2_cm_bail(c3__exit) )

#     define u2_co_t(som) \
        ( u2_so(u2_co_is_cell(som)) \
           ? ( ((u2_cs_cell *)u2_co_to_ptr(som))->tel )\
           : u2_cm_bail(c3__exit) )

