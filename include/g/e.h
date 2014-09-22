/* include/g/e.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u3_ce_fault(): handle a memory event with libsigsegv protocol.
    */
      static c3_i
      u3_ce_fault(void* adr_v, c3_i ser_i);

    /* u3_ce_load(): try to load image from checkpoint directory.
    */
      u3_o
      u3_ce_load(const c3_c* pax_c, c3_p adr_p, c3_w len_w)

    /* u3_ce_make(): create new image with checkpoint.
    */
      u3_o
      u3_ce_make(const c3_c* pax_c);

    /* u3_ce_sync(): save changes with checkpoint dir.
    */
      u3_ce_sync(void);
