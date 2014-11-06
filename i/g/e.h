/* include/g/e.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_ce_line: control line.
    */
      typedef struct _u3_ce_line {
        c3_w pag_w; 
        c3_w mug_w;
      } u3_ce_line;

    /* u3_ce_control: memory change, control file.
    */
      typedef struct _u3_ce_control {
        c3_d evt_d;                         //  event number
        c3_w nor_w;                         //  new page count north
        c3_w sou_w;                         //  new page count south
        c3_w pgs_w;                         //  number of changed pages
        u3_ce_line mem_u[0];                //  per page
      } u3_ce_control;

    /* u3_cs_patch: memory change, top level.
    */
      typedef struct _u3_cs_patch {         
        c3_i           ctl_i;
        c3_i           mem_i;
        u3_ce_control* con_u;
      } u3_cs_patch;

    /* u3_ce_image: memory segment, open file.
    */
      typedef struct _u3_ce_image {
        c3_c* nam_c;                        //  segment name
        c3_i  fid_i;                        //  open file, or 0
        c3_w  pgs_w;                        //  length in pages
      } u3_ce_image;

    /* u3_ce_pool: entire memory system.
    */
      typedef struct _u3_ce_pool {
        c3_c*        cpu_c;                     //  path to 
        c3_d         evt_d;                     //  last patch written at event
        c3_w         dit_w[u3_ca_pages >> 5];   //  touched since last save
        u3_ce_image  nor_u;                     //  north segment
        u3_ce_image  sou_u;                     //  south segment
      } u3_ce_pool;


  /** Globals.
  **/
    /* u3_Pool / u3P: global memory control.
    */
      c3_global u3_ce_pool u3_Pool;
#     define u3P u3_Pool


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

