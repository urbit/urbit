/* include/g/e.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* u3_ce_fault(): handle a memory event with libsigsegv protocol.
    */
      c3_i
      u3_ce_fault(void* adr_v, c3_i ser_i);

    /* u3_ce_save():
    */
      void
      u3_ce_save(void);

    /* u3_ce_boot(): start the memory system.
    */
      void 
      u3_ce_boot(c3_o nuu_o, c3_o bug_o, c3_c* cpu_c);

    /* u3_ce_init(): start the environment, with/without checkpointing.
    */
      void
      u3_ce_init(c3_o chk_o);

    /* u3_ce_grab(): garbage-collect the world, plus extra roots.
    */
      void
      u3_ce_grab(c3_c* cap_c, u3_noun som, ...);  // terminate with u3_none

    /* u3_ce_dirty(): count dirty pages.
    */
      c3_w
      u3_ce_dirty(void);
