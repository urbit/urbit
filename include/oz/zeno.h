/* include/z/zeno.h
**
** This file is in the public domain.
**
** Prefixes:
**
**   u3_z   (zeno)
**   u3_zi  (zeno internal)
**
** Description:
**
**   zeno: accelerated interpreter.
*/
  /** Data types.
  **/
    /* Nouns.
    */
      /* u3_z_src: source package
      ** u3_z_mold: type
      ** u3_z_shoe: pair [mold noun], noun in mold
      ** u3_z_path: source path
      ** u3_z_stat: execution status
      */
        typedef u3_fox u3_z_src;
        typedef u3_fox u3_z_mold;
        typedef u3_fox u3_z_shoe;
        typedef u3_fox u3_z_path;

      /* u3_z_stat:
      **
      **      0       :: computing is not complete
      **      [0 c]   :: (eq c (nolt a b))
      **
      **      %exit   :: computation does not complete
      **      %fail   :: computation exceeded resources
      **      [%lose [%path %spot] %message]   :: local error
      **    [%carp [%path %spot] %message]   :: global error
      */
        typedef u3_fox u3_q_stat;


    /* z_core, z: 
    **
    **   The zeno core.
    */
      struct u3_z_core {
        /** Layer: clam.  Should perhaps be flattened.
        **/
          struct u3_l_core l;

        /* lab: agenda stack.
        ** fic: boot kernel (mold)
        ** vad: boot kernel (formula)
        ** pex: 
        */
        u3_ray ray_lab;
        u3_fox fic;
        u3_fox vad;


      };
      typedef struct u3_z_core *u3_z;

  /** Functions.
  **/
    /** External.
    ***
    *** zeno looks just like quat - but remember to use u3_z_step and
    *** u3_z_spin if you want actual performance.
    ***
    *** Otherwise, 
    **/
      /* u3_z_new():
      **
      **   Create a zeno core, mallocing (1 << y_a) words of memory.
      **   Return 0 if malloc fails.  Free with free().
      **
      **   Rely only on jets of priority <= (y_b), testing all others.
      **   y_b is 0 for full nock, 15 for full speed.
      */
        u3_z
        u3_z_new(u3_y y_a,
                 u3_y y_b);

      /* u3_z_spin():
      **
      **   As u3_z_step(), until complete.
      */
        u3_fox
        u3_z_spin(u3_z z);

      /* u3_z_step():
      **
      **   Step (z).
      */ 
        u3_z_stat
        u3_z_step(u3_z z);


