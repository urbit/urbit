/* i/n/e.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3e_line: control line.
    */
      typedef struct _u3e_line {
        c3_w pag_w;
        c3_w mug_w;
      } u3e_line;

    /* u3e_control: memory change, control file.
    */
      typedef struct _u3e_control {
        c3_d evt_d;                         //  event number
        c3_w nor_w;                         //  new page count north
        c3_w sou_w;                         //  new page count south
        c3_w pgs_w;                         //  number of changed pages
        u3e_line mem_u[0];                  //  per page
      } u3e_control;

    /* u3_cs_patch: memory change, top level.
    */
      typedef struct _u3_cs_patch {
        c3_i           ctl_i;
        c3_i           mem_i;
        u3e_control* con_u;
      } u3_ce_patch;

    /* u3e_image: memory segment, open file.
    */
      typedef struct _u3e_image {
        c3_c* nam_c;                        //  segment name
        c3_i  fid_i;                        //  open file, or 0
        c3_w  pgs_w;                        //  length in pages
      } u3e_image;

    /* u3e_pool: entire memory system.
    */
      typedef struct _u3e_pool {
        c3_c*        dir_c;                     //  path to
        c3_d         evt_d;                     //  last patch written at event
        c3_w         dit_w[u3a_pages >> 5];   //  touched since last save
        u3e_image  nor_u;                     //  north segment
        u3e_image  sou_u;                     //  south segment
      } u3e_pool;


  /** Globals.
  **/
    /* u3_Pool / u3P: global memory control.
    */
      c3_global u3e_pool u3e_Pool;
#     define u3P u3e_Pool


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

    /* u3e_live_new(): start the persistence system.
    */
      c3_o
      u3e_live_new(c3_c* dir_c);

    /* u3e_dirty(): count dirty pages.
    */
      c3_w
      u3e_dirty(void);

