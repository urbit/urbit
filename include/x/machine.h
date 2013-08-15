/* include/x/machine.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_x   (xeno machine)
**
**   u3_xl  (xeno internal - loom; nouns and memory)
**   u3_xn  (xeno internal - nock; the nock interpreter)
**   u3_xq  (xeno internal - watt; the built-in kernel)
**   u3_xj  (xeno internal - jato; the optimixation layer)
**
** Description:
**
**   xeno: top-level internal interfaces.
*/
  /** Data types.
  **/
    struct u3_xeno;

    /* u3_xeno, x: 
    **
    **   The xeno core.
    **
    **   Zeno is a layered interpreter.  In general, lower layers do
    **   not call higher layers - exceptions are noted.  Lower layers
    **   never have direct access to the data of higher layers, but
    **   higher layers can both read and write lower-layer data.
    **
    **   From low to high, the layers are:
    **    
    **      loom    nouns and memory management
    **      nock    generic nock interpreter
    **      watt    boot nouns
    **      jato    accelerated execution
    **      sith    tracing and debugging infrastructure
    */
      struct u3_xeno {
        /** Layer: loom.
        **/
          struct u3_loom l;

        /** Layer: nock.
        ***
        ***   The main interpreter loop is at the nock level,
        ***   but calls jato and debug for obvious reasons.
        **/
          struct {
            /* lab: agenda stack
            */
            u3_ray lab_ray;

            /* bas: basket stack
            */
            u3_ray bas_ray;
          } n;

        /** Layer: jato.  Built-in performance assistance.
        **/
          struct {
            /* lum: exception return
            ** rod: jet array
            ** opt: optimixation level (0-15)
            */
            jmp_buf          jmp_lum;
            struct u3_xj_jet *jet_rod;
            c3_w             w_opt;
          } j;
      };
      typedef struct u3_xeno *u3_x;

    /* u3_x_bench: performance statistics.
    */
      struct u3_x_bench {
        /* ruy: step count
        ** cop: copy count
        ** vil: west watermark (start)
        ** tew: east watermark (start) 
        ** max: west watermark (max)
        ** buc: east watermark (max)
        */
        c3_d ruy_d;
        c3_w cop_w;
        c3_w vil_w, tew_w, max_w, buc_w;
      };


  /** Functions.
  **/
    /* u3_x_new():
    **
    **   Create a xeno core, mallocing (1 << y_a) words of memory.
    **   Return 0 if malloc fails.  Free with free().
    */
      u3_x
      u3_x_new(c3_y y_a);

    /* u3_x_do():
    **
    **   Execute (nock a b), returning u3_none on failure.
    */
      u3_rat
      u3_x_do(u3_x   x,
              u3_fox a,
              u3_fox b);

    /* u3_x_run():
    **
    **   Execute (nock b c) and set *a to the result.  If this succeeds,
    **   return 0.  Otherwise, return u3_cm_exit or u3_cm_fail.
    **
    **   If x_bench is nonxero, set benchmark data.
    */
      u3_fox
      u3_x_run(u3_x               x,
               u3_fox*            a,
               u3_fox             b,
               u3_fox             c,
               struct u3_x_bench* d);
    
    /* u3_x_reset():
    **
    **   Reset a failed core 
    */
    /* ux_x_mung():
    **
    **   As ux_x_run(), but [b] is gate and [c] is sample.
    */
      u3_fox
      u3_x_mung(u3_x              x,
                u3_fox            *a,
                u3_fox            b,
                u3_fox            c,
                struct u3_x_bench *d);
