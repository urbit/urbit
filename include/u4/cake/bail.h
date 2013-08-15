/* include/cake/bail.h
**
** This file is in the public domain.
*/
  /** Global variables.
  **/
    U4_GLOBAL struct {
      jmp_buf jmpbuf;
    } 
    U4_Bail;
 

  /** Data types.
  **/
    /* Bail codes.
    */
      enum u4_bail_code {
        u4_bail_not = 0,

        /* Exit: a formal infinite loop.
        */
          u4_bail_exit,

        /* Tank: insufficient memory.
        */
          u4_bail_tank,

        /* Trip: internal inconsistency.
        */
          u4_bail_trip,

        /* Stub: unimplemented feature.
        */
          u4_bail_stub
      };

  /** Macros.
  **/
    /* Conveniences.
    */
#     define u4_exit u4_bail_out(u4_bail_exit)
#     define u4_tank u4_bail_out(u4_bail_tank)
#     define u4_trip u4_bail_out(u4_bail_trip)
#     define u4_stub u4_bail_out(u4_bail_stub)

    /* Trip-powered assertions.
    */
#if 0
#     define u4_assert(x) \
        ( (x) ? u4_bull : u4_trip )
#else 
#     define u4_assert(x) \
        assert(x)
#endif
 
    /* Bailing in.
    **
    **   if ( u4_bail_in ) {
    **     // handle error
    **   } else {
    **     // do something
    **   }
    */
#     define u4_bail_in \
        setjmp(U4_Bail.jmpbuf)


  /** Functions.
  **/
    /* u4_bail_out():
    **
    **   Bail out with (code).
    */
      u4_int
      u4_bail_out(enum u4_bail_code code);
