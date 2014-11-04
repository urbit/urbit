/* include/n/save.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3_cs_line: control line.
    */
      typedef struct _u3_cs_line {
        c3_w pag_w; 
        c3_w mug_w;
      } u3_cs_line;

    /* u3_cs_control: memory change, control file.
    */
      typedef struct _u3_cs_control {
        c3_d evt_d;                         //  event number
        c3_w nor_w;                         //  new page count north
        c3_w sou_w;                         //  new page count south
        c3_w pgs_w;                         //  number of changed pages
        u3_cs_line mem_u[0];                //  per page
      } u3_cs_control;

    /* u3_cs_patch: memory change, top level.
    */
      typedef struct _u3_cs_patch {         
        c3_i           ctl_i;
        c3_i           mem_i;
        u3_cs_control* con_u;
      } u3_cs_patch;

    /* u3_cs_image: memory segment, open file.
    */
      typedef struct _u3_cs_image {
        c3_c* nam_c;                        //  segment name
        c3_i  fid_i;                        //  open file, or 0
        c3_w  pgs_w;                        //  length in pages
      } u3_cs_image;

    /* u3_cs_pool: entire memory system.
    */
      typedef struct _u3_cs_pool {
        c3_c*        cpu_c;                     //  path to 
        c3_d         evt_d;                     //  last patch written at event
        c3_w         dit_w[u3_cc_pages >> 5];   //  touched since last save
        u3_cs_image  nor_u;                     //  north segment
        u3_cs_image  sou_u;                     //  south segment
      } u3_cs_pool;
 
