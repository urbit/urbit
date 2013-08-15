/* include/fake/egg.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* A root operator: (%bump | %deep | %flat | %goto | %not | %same).
    */
      typedef u4_noun u4_root;

    /* A skeleton type.
    **
    **  0
    **  | (%pair skel skel)
    **  | (%skin name skel)
    **  | (%film skel)
    **  | (%bone noun)
    */
      typedef u4_noun u4_skel;

    /* A small number.
    */
      typedef u4_atom u4_step;

    /* A lower-case ASCII string.
    */
      typedef u4_atom u4_name;

    /* A limb number.
    */
      typedef u4_atom u4_hook;

    /* An abstract component, resolved to hook by skel:
    **
    **    (%face name)
    **  | (%limb hook)
    **  | (%bank step)
    **  | (%line step axe) 
    **  | (%nest axe axe)
    */
      typedef u4_noun u4_axe;

    /* A constant:
    **
    **    (coin coin)
    **  | (%code egg)
    **  | (%noun noun)
    **  | (%like skel coin)
    **  | (%log  *coin)
    **  | (%bag  *coin)
    **  | (%tab  *(name coin))
    **  | (%lib  *(name egg))
    */
      typedef u4_noun u4_coin;

    /* An (axe egg) pair.
    */
      typedef u4_noun u4_nail;

    /* A (name egg) pair.
    */
      typedef u4_noun u4_hand;

    /* A (skel egg) pair.
    */
      typedef u4_noun u4_gate;

    /* A (skel egg egg) triple.
    */
      typedef u4_noun u4_tube;

    /* A form, or Ut formula.
    */
      typedef u4_noun u4_form;

    /* A (skel form) pair.
    */
      typedef u4_noun u4_lion;

    /* A (hook skel) pair.
    */
      typedef u4_noun u4_part;

    /* A (hook form) pair.
    */
      typedef u4_noun u4_poke;

    /* A egg, or Ut function source.
    **
    ** See the code for details.
    */
      typedef u4_noun u4_egg;

    /* Operators.
    */
#     define u4_op_goto   u4_noun_3
#     define u4_op_deep   u4_noun_4
#     define u4_op_bump   u4_noun_5
#     define u4_op_same   u4_noun_6


  /** Functions.
  **/
    /** External functions.
    **/
      /* u4_egg_bake():
      **
      **   Compile (egg) to an Ut formula.
      */
        u4_noun
        u4_egg_bake(u4_lane lane,
                    u4_noun egg);

    /** Internal constructors.
    **/
      /** Formula.
      **/
        /* u4_form_exit(): (0 0).
        */
          u4_form
          u4_form_exit(u4_lane lane);

        /* u4_form_snip(): (0 atom).
        */
          u4_form
          u4_form_snip(u4_lane lane,
                       u4_atom atom);

        /* u4_form_link(): (0 a b).
        */
          u4_form
          u4_form_link(u4_lane lane,
                       u4_form form_a,
                       u4_form form_b);

        /* u4_form_rock(): (1 noun).
        */
          u4_form
          u4_form_rock(u4_lane lane,
                       u4_noun noun);

        /* u4_form_op(): ((2 | 3 | 4 | 5) form).
        */
          u4_form
          u4_form_op(u4_lane lane,
                     u4_atom atom,
                     u4_form form);

        /* u4_form_if(): (2 a b c).
        */
          u4_form
          u4_form_if(u4_lane lane,
                     u4_form form_a,
                     u4_form form_b,
                     u4_form form_c);

        /* u4_form_by(): (3 (a (0 1)) 1 b).
        */
          u4_form
          u4_form_by(u4_lane lane,
                     u4_form form_a,
                     u4_form form_b);

        /* u4_form_loop(): (3 ((0 1) (1 a)) 1 (3 (0 1) (0 3))).
        */
          u4_form
          u4_form_loop(u4_lane lane,
                       u4_form form);

        /* u4_form_kick(): (3 form 1 (3 (0 1) (0 hook))).
        */
          u4_form
          u4_form_kick(u4_lane lane,
                       u4_hook hook,
                       u4_form form);

        /* u4_form_mark(): (7 a b).
        */
          u4_form
          u4_form_mark(u4_lane lane,
                       u4_form form_a,
                       u4_form form_b);

        /* u4_form_hint(): (8 hint form).
        */
          u4_form
          u4_form_hint(u4_lane lane,
                       u4_noun hint,
                       u4_noun form);

        /* u4_form_cell(): (a b) or equivalent.
        */
          u4_form
          u4_form_cell(u4_lane lane,
                       u4_form form_a,
                       u4_form form_b);
        
        /* u4_form_cons(): as u4_form_cell(), with (log).
        */
          u4_form
          u4_form_cons(u4_lane lane,
                       u4_log  log_form);

        /* u4_form_blank(): a form that produces a trivial member of (skel).
        */
          u4_form
          u4_form_blank(u4_lane lane,
                        u4_skel skel);

        /* u4_form_and(): assemble (log_form) as a short-circuit and.
        */
          u4_form
          u4_form_and(u4_lane lane,
                      u4_log  log_form);

        /* u4_form_or(): assemble (log_form) as a short-circuit or.
        */
          u4_form
          u4_form_or(u4_lane lane,
                     u4_log  log_form);

        /* u4_form_probe(): a form that tests (hook) as matching (skel).
        */
          u4_form 
          u4_form_probe(u4_lane lane,
                        u4_skel skel,
                        u4_hook hook);

        /* u4_form_trap(): if skel@hook fits and form_a, form_b, else form_c.
        */
          u4_form
          u4_form_trap(u4_lane lane,
                       u4_skel skel,
                       u4_hook hook,
                       u4_form form_a,
                       u4_form form_b,
                       u4_form form_c);

        /* u4_form_try(): if skel@hook fits, form_a, else form_b.
        */
          u4_form
          u4_form_try(u4_lane lane,
                      u4_skel skel,
                      u4_hook hook,
                      u4_form form_a,
                      u4_form form_b);

        /* u4_form_hike(): produce snip[hook_hike] with (log_poke).*(hook form).
        */
          u4_form
          u4_form_hike(u4_lane lane,
                       u4_hook hook_hike,
                       u4_log  log_poke);

        /* u4_form_use(): kick (axe) on nearest bank in (skel).
        */
          u4_form
          u4_form_use(u4_lane lane,
                      u4_skel skel,
                      u4_axe  axe);


        /* u4_form_egg(): using (skel), formulate (egg).
        */
          u4_form
          u4_form_egg(u4_lane lane,
                      u4_skel skel,
                      u4_egg  egg);

      /** Lion.
      **/
        /* u4_lion_egg(): using (skel), lionize (egg).
        */
          u4_lion 
          u4_lion_egg(u4_lane lane, 
                      u4_skel skel, 
                      u4_egg  egg);

      /** Part.
      **/
        /* u4_part_axe(): resolve (axe) in (skel) to (part).
        */
          u4_part
          u4_part_axe(u4_lane lane,
                      u4_skel skel,
                      u4_axe  axe);

        /* u4_part_axe_(): resolve (axe) in (skel) to (part), or bull.
        */
          u4_part
          u4_part_axe_(u4_lane lane,
                       u4_skel skel,
                       u4_axe  axe);

      /** Skeleton.
      **/
        /* u4_skel_pair(): (.pair skel_a skel_b), or equivalent.
        */
          u4_skel
          u4_skel_pair(u4_lane lane,
                       u4_skel skel_a,
                       u4_skel skel_b);

        /* u4_skel_bone(): (.bone noun).
        */
          u4_skel
          u4_skel_bone(u4_lane lane,
                       u4_noun noun);

        /* u4_skel_skin(): (.skin name skel)
        */
          u4_skel
          u4_skel_skin(u4_lane lane,
                       u4_name name,
                       u4_skel skel);

        /* u4_skel_film(): (.film skel)
        */
          u4_skel
          u4_skel_film(u4_lane lane,
                       u4_skel skel);

        /* u4_skel_tab(): 
        **
        **   Produce a minimal skeleton following (tab).
        */
          u4_skel
          u4_skel_tab(u4_lane lane,
                      u4_tab  tab);

        /* u4_skel_axe(): resolve (axe) in (skel) to (skel), or exit.
        */
          u4_hook
          u4_skel_axe(u4_lane lane,
                      u4_skel skel,
                      u4_axe  axe);

        /* u4_skel_zip(): 
        **
        **   Patch (skel_a) at (hook), replacing (skel_b) with (skel_c).
        */
          u4_skel
          u4_skel_zip(u4_lane lane,
                      u4_skel skel_a,
                      u4_hook hook,
                      u4_skel skel_b,
                      u4_skel skel_c);

        /* u4_skel_coin():
        **
        **   Produce the skeleton for (coin).
        */
          u4_skel
          u4_skel_coin(u4_lane lane,
                       u4_coin coin);

      /** Hook.
      **/
        /* u4_hook_l(): (hook + hook).
        */
          u4_hook 
          u4_hook_l(u4_lane lane, 
                    u4_hook hook);

        /* u4_hook_r(): (hook + hook + 1).
        */
          u4_hook 
          u4_hook_r(u4_lane lane, 
                    u4_hook hook);

        /* u4_hook_sub(): (hook_b) within (hook_a).
        */
          u4_hook
          u4_hook_sub(u4_lane lane,
                      u4_hook hook_a, 
                      u4_hook hook_b);

        /* u4_hook_form(): (hook) if (form) is (0 atom), bull otherwise.
        */
          u4_hook
          u4_hook_form_(u4_lane lane,
                        u4_form form);

        /* u4_hook_axe(): resolve (axe) in (skel) to (hook), or exit.
        */
          u4_hook
          u4_hook_axe(u4_lane lane,
                      u4_skel skel,
                      u4_axe  axe);

