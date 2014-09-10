/* include/f/noun.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_noun: tagged pointer.
    **
    **  If bit 31 is 0, a u3_noun is a direct 31-bit atom ("cat").
    **  If bit 31 is 1 and bit 30 0, an indirect atom ("pug").
    **  If bit 31 is 1 and bit 30 1, an indirect cell ("pom").
    **
    ** Bits 0-29 are a word offset against u3_Loom.
    */
      typedef c3_w    u3_noun;

    /* u3_none - out-of-band noun.
    */
#     define u3_none  (u3_noun)0xffffffff

    /* Informative typedefs.  Use if you like.
    */
      typedef u3_noun u3_atom;              //  must be atom
      typedef u3_noun u3_term;              //  @tas
      typedef u3_noun u3_mote;              //  @tas
      typedef u3_noun u3_cell;              //  must be cell
      typedef u3_noun u3_trel;              //  must be triple
      typedef u3_noun u3_qual;              //  must be quadruple
      typedef u3_noun u3_quin;              //  must be quintuple
      typedef u3_noun u3_bean;              //  loobean: 0 == u3_yes, 1 == u3_no
      typedef u3_noun u3_weak;              //  may be u3_none
      typedef u3_noun (*u3_funk)(u2_noun);

  /**  Typedefs.
  **/
    /* u3_funk: C function producing noun.
    typedef

    /* u3_atom, u3_cell: logical atom and cell structures.
    */
      typedef struct {
        c3_w mug_w;
      } u3_cs_noun;

      typedef struct {
        c3_w mug_w;
        c3_w len_w;
        c3_w buf_w[0];
      } u3_cs_atom;

      typedef struct {
        c3_w    mug_w;
        u3_noun hed; 
        u3_noun tel;
      } u3_cs_cell;

    /* Inside a noun.
    */
#     define u3_co_is_cat(som)    (((som) >> 31) ? u3_no : u3_yes)
#     define u3_co_is_dog(som)    (((som) >> 31) ? u3_yes : u3_no)

#     define u3_co_is_pug(som)    ((2 == ((som) >> 30)) ? u3_yes : u3_no)
#     define u3_co_is_pom(som)    ((3 == ((som) >> 30)) ? u3_yes : u3_no)
#     define u3_co_to_off(som)    ((som) & 0x3fffffff)
#     define u3_co_to_ptr(som)    (u3_co_into(u3_co_to_off(som)))
#     define u3_co_to_wtr(som)    ((c3_w *)u3_co_to_ptr(som))
#     define u3_co_to_pug(off)    (off | 0x80000000)
#     define u3_co_to_pom(off)    (off | 0xc0000000)

#     define u3_co_is_atom(som)    u3_or(u3_co_is_cat(som), \
                                         u3_co_is_pug(som))
#     define u3_co_is_cell(som)    u3_co_is_pom(som)
#     define u3_co_de_twin(dog, dog_w)  ((dog & 0xc0000000) | u3_co_outa(dog_w))

#     define u3_co_h(som) \
        ( u3_so(u3_co_is_cell(som)) \
           ? ( ((u3_cs_cell *)u3_co_to_ptr(som))->hed )\
           : u3_cm_bail(c3__exit) )

#     define u3_co_t(som) \
        ( u3_so(u3_co_is_cell(som)) \
           ? ( ((u3_cs_cell *)u3_co_to_ptr(som))->tel )\
           : u3_cm_bail(c3__exit) )

