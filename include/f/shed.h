/* include/shed.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* u2_seal: identification in objective battery declaration.
    **
    **  |?
    **    lef=*term
    **    [std=*term kel=@]
    **    [ven=*term pro=*term kel=@]
    **    [ven=*term pro=*term ver=@ kel=@]
    **  ==
    */
      typedef u2_noun u2_seal;

    /* u2_clue: programmer's declaration hint
    **
    **  [bud=*tool sil=*seal nut=*(list &[p=*term q=*tool])]
    */
      typedef u2_noun u2_clue;

    /* u2_disc: declaration layer (list should be map)
    **
    **  [sil=*seal nut=*(list &[p=*term q=*tool])]
    */
      typedef u2_noun u2_disc;

    /* u2_chip: complete battery record
    **
    **  [dac=*disc bat=* pet=<~ [axe=*axis led=*chip]>]
    */
      typedef u2_noun u2_chip;

    /* u2_loom_shed: jet registration hangar.
    */
      typedef struct {
        u2_loom_rail o;

        /* cad_c: hash from battery to chip.
        */
        u2_loom_chad cad_c;

        /* dip_c: hash from battery, term to formula.
        */
        u2_loom_chad dip_c;
      } u2_loom_shed;

#       define u2_shed_cad_r(sad_r)  u2_aftr(sad_r, u2_loom_shed, cad_c)
#       define u2_shed_dip_r(sad_r)  u2_aftr(sad_r, u2_loom_shed, dip_c)

  /** Functions.
  **/
    /* u2_sh_init(): 
    **
    **   Initialize shed, with parent if any.
    */
      void
      u2_sh_init(u2_wire wir_r);
 
    /* u2_sh_find(): find chip by core, or none.  Includes validate.
    */
      u2_weak                                                     //  senior
      u2_sh_find(u2_wire wir_r,
                 u2_noun cor);                                    //  retain

    /* u2_sh_mine(): 
    **
    **   Register and/or replace core.
    */
      u2_noun                                                     //  transfer
      u2_sh_mine(u2_wire wir_r,
                 u2_noun hod,                                     //  retain
                 u2_noun cor);                                    //  transfer

    /* u2_sh_cook():
    **
    **   Produce hook formula from chip, or u2_none.
    */
      u2_weak                                                     //  senior
      u2_sh_cook(u2_wire     wir_r,
                 u2_noun     xip,                                 //  retain
                 const c3_c* tam_c);

    /* u2_sh_look():
    **
    **   Produce hook formula from core, or u2_none.
    */
      u2_weak                                                     //  senior
      u2_sh_look(u2_wire     wir_r,
                 u2_noun     cor,                                 //  retain
                 const c3_c* tam_c);
