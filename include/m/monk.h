/* include/m/monk.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u2_monk_stage: a kernel generation.
    */
      typedef struct {
        c3_y    sag_y;                              //  stage number
        c3_t    tac_t;                              //  trace bit
        c3_t    sam_t;                              //  sample bit
        u2_life ken;                                //  watt kernel
        u2_life cod;                                //  command shell
      }
      * u2_monk_stage;

    /* u2_monk_process: a per-process data structure.
    */
      typedef struct { 
        /* Nock engine.
        */
          u2_wire wir_r; 

        /* Global environment.
        */
          struct {
            struct {
              c3_c *hom_c;                          //  home/dev prefix
              c3_c *sys_c;                          //  system prefix
            } dir;

            struct {
              FILE* out_f;                          //  output file
              FILE* err_f;                          //  error file/0 syslog
              c3_y  pri_y;                          //  upto log priority
            } flo;
          } moc;
        
        /* Stage list - freshest first.
        */
          u2_monk_stage sag_s;                      //  stage list
      } 
      * u2_monk_process;

  /** Global variables.  Use only for error channel.
  **/
#   ifdef U2_GLOBAL
      u2_monk_process u2_Process;
#   else
      extern u2_monk_process u2_Process;
#   endif

  /** Functions.
  **/
    /* u2_mo_boot(): boot monk.  send output/errors to fds.
    **
    ** (If err_f is 0, uses syslog or at least should.)
    */
      u2_monk_process                                             //  produce
      u2_mo_boot(FILE *out_f,
                 FILE *err_f);

    /* u2_mo_done(): terminate monk, freeing everything.
    */
      void
      u2_mo_done(u2_monk_process ces_p);                          //  submit

    /* u2_mo_err(): error printf with priority.
    */
      void
      u2_mo_err(u2_monk_process ces_p,                            //  retain
                c3_y            pri_y,                            //  7 to 0
                const c3_c*     str_c,                            //  retain
                ...);

    /* u2_mo_erv(): error vprintf.
    */
      void
      u2_mo_erv(u2_monk_process ces_p,                            //  retain
                c3_y            pri_y,                            //  7 to 0
                const c3_c*     str_c,                            //  retain 
                va_list         arg_v);                           //  retain

    /* u2_mo_line(): execute a command line.
    */
      void
      u2_mo_line(u2_monk_process ces_p,                           //  retain
                 c3_c*           lin_c);                          //  retain 
 
