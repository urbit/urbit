/* include/cake/open.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* Common structure between cell and atom.
    */
      struct u4_wasp_open {
        u4_nub nub;
      };

    /* A open cell.  Just your basic cell.
    */
      struct u4_wasp_open_cell {
        u4_nub  nub;
        u4_noun head;
        u4_noun tail;
      };

    /* A open atom.  Just your basic atom.
    **
    ** (sw) may not be 0.  xw[sw - 1] may not be 0.  If (sw) is 1,
    ** xw[0] must exceed 0x3fffffff, ie, must be out of the cod range.
    */
      struct u4_wasp_open_atom {
        u4_nub  nub;
        u4_sw   sw;
        u4_xw   xw[0];
      };


  /** Macros.
  **/
    /* Nub access.
    */
#     define u4_open_nub(pin_open) \
        ( u4_pin_in(pin, open, nub) ) 

    /* Cell access.
    */
#     define u4_open_cell_head(pin) \
        ( u4_pin_in(pin, open_cell, head) )
#     define u4_open_cell_tail(pin) \
        ( u4_pin_in(pin, open_cell, tail) )

    /* Atom access.
    */
#     define u4_open_atom_sw(pin) \
        ( u4_pin_in(pin, open_atom, sw) )
#     define u4_open_atom_xw(pin, pw) \
        ( u4_pin_at(pin, (u4_wasp_to(open_atom, xw) + pw)) )
