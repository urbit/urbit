/* include/n/glob.h
**
** This file is in the public domain.
*/
  /** Globals.
  **/
    /* u3_Loom: base of loom, as a word pointer.
    */
      c3_global c3_w* u3_Loom;

    /* u3_Home / u3H: root of thread.  Always north.
    */
      c3_global u3_cs_home* u3_Home;
#       define u3H  u3_Home
#       define u3A  (&(u3_Home->arv_u))

    /* u3_Road / u3R: current road (thread-local).
    */
      c3_global u3_road* u3_Road;
#       define u3R  u3_Road

    /* u3_Dash: jet dashboard.
    */
      extern u3_cs_dash u3_Dash;
#     define u3D u3_Dash

    /* u3_Pool / u3P: global memory control.
    */
      extern u3_cs_pool u3_Pool;
#     define u3P u3_Pool

    /* u3_Code: memory code.
    */
#ifdef U3_MEMORY_DEBUG
      c3_global c3_w u3_Code;
#endif

    /* u3_Nock: nock instruction counter.
    */
      c3_global c3_d u3_Nock;
#     define u3N u3_Nock
