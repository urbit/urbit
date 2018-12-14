/* i/n/o.h
**
** This file is in the public domain.
*/
  /** Data structures.
  **/
    /* u3o_config: process / system configuration.
    */
      typedef struct _u3o_config {
        u3_noun who;                          //  single identity
        c3_c*   dir_c;                        //  execution directory (pier)
        c3_w    wag_w;                        //  flags (both ways)
      } u3o_config;

    /* u3o_flag: process/system flags.
    **
    **  _debug flags are set outside u3 and heard inside it.
    **  _check flags are set inside u3 and heard outside it.
    */
      enum u3o_flag {                         //  execution flags
        u3o_debug_ram =     0x1,              //  debug: gc
        u3o_debug_cpu =     0x2,              //  debug: profile
        u3o_check_corrupt = 0x4,              //  check: gc memory
        u3o_check_fatal =   0x8,              //  check: unrecoverable
        u3o_verbose =       0x10,             //  be remarkably wordy
        u3o_dryrun =        0x20,             //  don't touch checkpoint
        u3o_quiet =         0x40,             //  disable ~&
        u3o_hashless =      0x80,             //  disable hashboard
        u3o_trace =         0x100             //  enables trace dumping
      };

  /** Globals.
  **/
    /* u3_Config / u3C: global memory control.
    */
      c3_global u3o_config u3o_Config;
#     define u3C u3o_Config

