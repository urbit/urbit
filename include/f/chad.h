/* include/chad.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* u2_loom_pear: overloaded name-value pair.
    */
      typedef struct {
        /*  [nam val]:     name-value pair
        **  [u2_none ray]: subtable
        **  [u2_none 0]:   empty
        */
        u2_weak nam;
        u2_weak val;
      } u2_loom_pear;

#define   u2_pear_nam(per_r)  *u2_at(per_r, u2_loom_pear, nam)
#define   u2_pear_val(per_r)  *u2_at(per_r, u2_loom_pear, nam)

    /* u2_loom_chad:
    **
    **    16-way mug-powered hashtable.  If wun != u2_none,
    **    table is not allocated.
    */
      typedef struct {
        u2_loom_pear dol_p[16];
      } u2_loom_chad;

#define   u2_chad_dol(cad_r, i_w) \
            ( (cad_r) + ((i_w) * c3_wiseof(u2_loom_pear)) )

#define   u2_chad_dol_nam(cad_r, i_w) u2_pear_nam(u2_chad_dol(cad_r, i_w))
#define   u2_chad_dol_val(cad_r, i_w) u2_pear_val(u2_chad_dol(cad_r, i_w))


  /** Functions.
  **/
    /* u2_ch_init():
    **
    **   Initialize empty chad.
    */
      void
      u2_ch_init(u2_ray cad_r);

    /* u2_ch_find():
    **
    **   Find value for `nam` in `cad`, or return `u2_none`.
    */
      u2_weak
      u2_ch_find(u2_ray  cad_r,
                 u2_noun nam);
    
    /* u2_ch_find_cell():
    **
    **   Find value for `[hed tal]` in `cad`, or return `u2_none`.
    */
      u2_weak
      u2_ch_find_cell(u2_ray  cad_r,
                      u2_noun hed,
                      u2_noun tal);

    /* u2_ch_find_mixt():
    **
    **   Find value for `[hed tal]` in `cad`, or return `u2_none`.
    */
      u2_weak
      u2_ch_find_mixt(u2_ray      cad_r,
                      const c3_c* hed_c,
                      u2_noun     tal);
      
    /* u2_ch_save():
    **
    **   Save `val` under `nam` in `cad`, allocating in `ral`.
    **   Return `u2_none` iff allocation fails.  Asserts on duplicate.
    */
      u2_weak                                                     //  retain
      u2_ch_save(u2_ray  ral_r,
                 u2_ray  cad_r,
                 u2_noun nam,                                     //  retain
                 u2_noun val);                                    //  retain

    /* u2_ch_save_cell():
    **
    **   Save `val` under `[hed tal]` in `cad`, allocating in `ral`.
    **   Return `u2_none` iff allocation fails.  Asserts on duplicate.
    */
      u2_weak                                                     //  retain
      u2_ch_save_cell(u2_ray  ral_r,
                      u2_ray  cad_r,
                      u2_noun hed,                                //  retain
                      u2_noun tal,                                //  retain
                      u2_noun val);                               //  retain

    /* u2_ch_save_mixt():
    **
    **   Save `val` under `[hed tal]` in `cad`, allocating in `ral`.
    **   Return `u2_none` iff allocation fails.  Asserts on duplicate.
    **
    **   Caller retains ownership of arguments; callee retains result.
    */
      u2_weak                                                     //  retain
      u2_ch_save_mixt(u2_ray      ral_r,
                      u2_ray      cad_r,
                      const c3_c* hed_c,
                      u2_noun     tal,                            //  retain
                      u2_noun     val);                           //  retain
