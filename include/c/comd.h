/* include/comd.h
**
** This file is in the public domain.
*/
  /** Functions.
  **/
    /* c3_comd_init(): 
    **
    **   Initialize the readline console.  Return the history filename.
    */
      c3_c*                                                       //  produce
      c3_comd_init(void);

    /* c3_comd_line():
    **
    **   Read a line from the console, saving to history file `fel`.
    **
    **   Returns 0 iff the console has exited.
    */
      c3_c*                                                       //  produce
      c3_comd_line(const c3_c *fel_c, 
                   const c3_c *prm_c);                            //  retain
