/* i/n/e.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u3e_fault(): handle a memory event with libsigsegv protocol.
    */
      c3_i
      u3e_fault(void* adr_v, c3_i ser_i);

    /* u3e_save():
    */
      void
      u3e_save(void);

    /* u3e_live(): start the persistence system.  Return c3y if no image.
    */
      c3_o
      u3e_live(c3_o nuu_o, c3_c* dir_c);

    /* u3e_yolo(): disable dirty page tracking, read/write whole loom.
    */
      c3_o
      u3e_yolo(void);

    /* u3e_foul(): dirty all the pages of the loom.
    */
      void
      u3e_foul(void);
