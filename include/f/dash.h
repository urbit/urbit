/* include/dash.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* u2_chop: identification in objective battery declaration.
    **
    **  |?
    **    lef=*term
    **    [std=*term kel=@]
    **    [ven=*term pro=*term kel=@]
    **    [ven=*term pro=*term ver=@ kel=@]
    **  ==
    */
      // typedef u2_noun u2_chop;

    /* u2_clue: programmer's declaration hint
    **
    **  [bud=*tool cop=*chop pic=*(list &[p=*term q=*tool])]
    */
      // typedef u2_noun u2_clue;

    /* u2_chip: complete battery record (XX: list should be map)
    **
    **  :*  dac=[cop=*chop pic=*(list <[p=*term q=*tool]>)]
    **      bat=*
    **      pet=<~ [axe=*axis led=*chip]>
    **  --
    */
      // typedef u2_noun u2_chip;

  /** Functions.
  **/
    /* u2_ds_find(): find chip by core, or none.
    */
      u2_weak                                                     //  senior
      u2_ds_find(u2_wire wir_r,
                 u2_noun cor);                                    //  retain

    /* u2_ds_mine(): 
    **
    **   Register and/or replace core.
    */
      u2_noun                                                     //  produce
      u2_ds_mine(u2_wire wir_r,
                 u2_noun clu,                                     //  retain
                 u2_noun cor);                                    //  submit

    /* u2_ds_look():
    **
    **   Produce hook formula from core, or u2_none.
    */
      u2_weak                                                     //  produce
      u2_ds_look(u2_wire     wir_r,
                 u2_noun     cor,                                 //  retain
                 const c3_c* tam_c);                              //  retain

    /* u2_ds_fire():
    **
    **   Fire formula from core.
    */
      u2_weak                                                     //  produce
      u2_ds_fire(u2_wire     wir_r,
                 u2_noun     cor,                                 //  retain
                 const c3_c* tam_c);                              //  retain
