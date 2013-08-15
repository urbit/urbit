/* include/cake/pin.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* A pin.
    **
    ** A pin is a noun that is not a cod.  Bit 31 must be 0.
    **
    ** A pin has two components: a 2-bit mode and a 29-bit bar.
    ** Basically, the mode is a pointer tag, the bar is a pointer.
    **
    ** Bit 0 is the atom bit.  0 is cell, 1 is atom.
    ** Bit 1 is the fort bit.  0 is open, 1 is fortified.
    **
    ** Bits 2-30 are a bar to a wasp that matches the mode. 
    */
      typedef u4_xw u4_pin;

    /* Open pin subtypes.
    */
      typedef u4_pin u4_pin_open;
      typedef u4_pin u4_pin_open_cell;
      typedef u4_pin u4_pin_open_atom;

    /* Fortified pin subtypes.  (No structure is shared.)
    */
      typedef u4_pin u4_pin_fort_cell;
      typedef u4_pin u4_pin_fort_atom;


  /** Macros.
  **/
    /* Pin bar extraction.
    */
#     define u4_pin_bar(pin)    ( (pin) >> 2 )
#     define u4_pin_xw(pin)     ( u4_bar_xw(u4_pin_bar(pin)) )
#     define u4_pin_at(pin, pw) ( u4_bar_at(u4_pin_bar(pin), (pw)) )

#     define u4_pin_in(pin, name, field) \
        ( u4_bar_at(u4_pin_bar(pin), u4_wasp_to(name, field)) )

    /* Pin extraction.
    */
#     define u4_pin_is_atom(pin)  ( (pin) & 1 )
#     define u4_pin_is_open(pin)  ( !((pin) & 2) )

#     define u4_pin_is_cell(pin)  ( !u4_pin_is_atom(pin) )
#     define u4_pin_is_fort(pin)  ( !u4_pin_is_open(pin) )

    /* Pin construction.
    */
#     define u4_pin_make(bar, fort, atom) \
        ( ((bar) << 2) | ((fort) << 1) | (atom) )
