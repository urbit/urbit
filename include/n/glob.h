/* include/f/glob.h
**
** This file is in the public domain.
*/
  /** Globals.
  **/
    /* u3_Loom: base of loom, as a word pointer.
    */
      c3_global c3_w* u3_Loom;
#       define u3L  u3_Loom

    /* u3_Home / u3H: root of thread.  Always north.
    */
      c3_global u3_road* u3_Home;
#       define u3H  u3_Home

    /* u3_Road / u3R: current road (thread-local).
    */
      c3_global u3_road* u3_Road;
#       define u3R  u3_Road

    /* u3_Dash: jet dashboard.
    */
      c3_global u3_cs_dash u3_Dash;
#     define u3D u3_Dash
