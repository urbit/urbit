/* include/cake/cake.h
**
** This file is in the public domain.
*/
  /** Tunable constants.
  **/
    /* Log of the number of words in the cake.
    **
    ** The maximum value is 28.
    **
    ** 16 is 256 Kbytes.
    ** 24 is 64 Mbytes.
    ** 28 is 1 Gbyte.
    */
#     define u4_cake_gw   28

    /* Number of words in the cake.
    */
#     define u4_cake_sw   (1 << u4_cake_gw)


  /** Global variables.
  **/
    /* The cake itself.
    */
      U4_GLOBAL u4_xw U4_Cake[u4_cake_sw];


  /** Data types.
  **/
    /* A foo, or 28-bit cake pointer.  The address of a cake word.
    **
    ** The maximum size of a cake is thus 1GB.
    */
      typedef u4_xw u4_foo;
   
    /* A bar, or 29-bit cake pointer, with wrapped memory.
    **
    ** Bits 31 and 30 in a bar are 0.
    ** Bit 29 is the east/west bit, or "pole" - 0 is west, 1 is east.
    ** Bits 0-28 are the spot, a word pointer.
    **
    ** Each bar maps to one and only one foo.
    **
    ** In a west bar, foo == spot.
    ** In an east bar, foo == (u4_cake_sw - (spot + 1)).
    **
    ** If foo == bar, bar is "west."  Otherwise it is "east."  End 0
    ** is west, pole 1 is east.
    **
    **  |----------------------------------|
    **  |--->west                  east<---|
    **  |----------------------------------|
    **  0                                  u4_cake_sw
    **
    ** What is the motivation for this weird scheme?
    **
    ** Wrapped memory defines two partitions which share the same
    ** block of address space, but consume it in opposite directions.
    ** This trades off increased reference latency for a partition
    ** without memory fragmentation.
    **
    ** Because each stack allocates by incrementing, allocation
    ** logic can be pole-independent.  Probably in production 
    ** code this should be reversed (foo == spot on both poles),
    ** resulting in more complexity and slightly lower latency.
    **
    ** Wrapped memory can be considered a simple form of virtual 
    ** memory.  While it is much simpler than an MMU page table,
    ** there is no trivial way to implement it with an MMU.
    */
      typedef u4_xw u4_bar;
      typedef u4_xw u4_bar_west;
      typedef u4_xw u4_bar_east;
      typedef u4_t  u4_pole;
    
    /* A spot, or relative distance within foo or bar.
    */
      typedef u4_xw u4_spot;

  /** Macros.
  **/
    /* The word at a foo.
    */
#     define u4_foo_xw(foo) \
        ( U4_Cake[foo] )

    /* Bar components.
    */
#     define u4_pole_west 0
#     define u4_pole_east 1

#     define u4_bar_pole(bar) \
        ((bar) >> 28)
#     define u4_bar_spot(bar) \
        ((bar) & ((1 << 28) - 1))

#     define u4_bar_make(pole, spot) \
        ( ((pole) << 28) | (spot) )

    /* Bar addressing.
    */
#     define u4_bar_foo(bar) \
        ( u4_bar_pole(bar) \
            ? (u4_cake_sw - (u4_bar_spot(bar) + 1)) \
            : (bar) )

    /* Bar dereferencing.
    */
#     define u4_bar_xw(bar) \
        u4_foo_xw(u4_bar_foo(bar))

    /* True iff two bars, one east and one west, cross.
    **
    ** Don't cross the beams!  "Trust me.  It will be bad."
    */
#     define u4_bar_cross(bar_a, bar_b) \
        ( (u4_bar_spot(bar_a) + u4_bar_spot(bar_b) + 1) >= u4_cake_sw )

    /* Bar indirection.
    **
    ** Bar structure addressing is downward - in bar terms, of course.
    */
#     define u4_bar_at(bar, pw) \
        ( u4_bar_xw((bar) - (pw)) )
#     define u4_bar_in(bar, name, field) \
        ( u4_bar_at(bar, u4_wasp_to(name, field)) )

    /* Wasps.
    **
    ** A wasp is a structure in which all fields are word-aligned.
    ** We can store a wasp in a cake and use macros to access it.
    **
    ** Define a wasp as struct u4_wasp_(name).
    */
#     define u4_wasp_to(name, field) \
        ( u4_c_pb(struct u4_wasp_##name, field) >> 2 )

#     define u4_wasp_sw(name) \
        ( sizeof(struct u4_wasp_##name) >> 2 )

  
  /** Dependent includes.
  **/
#   include "u4/cake/bail.h"
#   include "u4/cake/noun.h"
#   include "u4/cake/pin.h"
#   include "u4/cake/open.h"
#   include "u4/cake/road.h"
#   include "u4/cake/look.h"
#   include "u4/cake/make.h"
#   include "u4/cake/hard.h"
