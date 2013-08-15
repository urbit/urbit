/* include/mill/mill.h
**
** This file is in the public domain.
*/
  /** Data types.
  **/
    /* Some kinds of nouns.
    */
      typedef u4_noun u4_lump;
      typedef u4_noun u4_type;
      typedef u4_noun u4_gene;
      typedef u4_atom u4_axis;
      typedef u4_atom u4_term;
      typedef u4_noun u4_herb;  /* (type gene) */
      typedef u4_noun u4_loaf;  /* (type form) */
#if 0
      typedef u4_noun u4_path;  /* *(term | (atom term)) */
      typedef u4_noun u4_drop;  /* (path gene) */
      typedef u4_noun u4_coat;  /* *(weld) */
#endif

    /* The main mill structure.
    */
      typedef struct _u4_mill {
        /* For all internal allocation.
        */
        u4_lane lane;

        /* Debug depth.
        */
        u4_atom rux;

        /* Site (source package being compiled, in top-level safe)
        */
        u4_noun zud;

        /* Spot (position in source).
        */
        u4_noun nix;

        /* Trap - ((list {[p=(text) q=(site) r=(spot)]}))
        */
        u4_noun meb;

        /* Set (type gene) currently in play.
        */
        u4_bag fan;

        /* Set (type gene) to memoize _mill_nice().
        */
        u4_bag duf;

        /* Tab (type flag) to memoize _mill_null().
        */
        u4_tab dam;

        /* Tab ((type gene) type) to memoize _mill_play().
        */
        u4_tab niq;

        /* Tab ((type type) flag) to memoize _mill_cong().
        */
        u4_tab vus;

        /* Tab ((type gene) form) to memoize _mill_make().
        */
        u4_tab rof;

        /* Bag (type gene) to memoize _mill_safe().
        */
        u4_bag tyx;
      } 
        *u4_milr;


  /** Functions.
  **/
    /** Public interfaces.
    **/
      /* u4_mill(): 
      **
      **   Convert (type gene) to (type form).
      */
        u4_loaf
        u4_mill(u4_lane lane,
                u4_type nes,
                u4_gene zyl);

    /** Private functions.
    **/
      /* _mill_and(): bake and.
      */
        u4_form
        _mill_and(u4_milr m,
                  u4_form zeg,
                  u4_form dac);

      /* _mill_bark(): compute [axis type] for a bend.
      */
        u4_noun
        _mill_bark(u4_milr m,
                   u4_type ter,
                   u4_gene nuv);

      /* _mill_both(): fuse pair.
      */
        u4_form
        _mill_both(u4_milr m,
                   u4_type nem,
                   u4_type dif);

      /* _mill_cold(): test constant.
      */
        u4_t
        _mill_cold(u4_milr m,
                   u4_noun dil,
                   u4_type zer);

      /* _mill_comp(): compose a pipe.
      */
        u4_form
        _mill_comp(u4_milr m,
                   u4_form mal,
                   u4_form buz);

      /* _mill_cond(): compose an if.
      */
        u4_form
        _mill_cond(u4_milr m,
                   u4_form nef,
                   u4_form buz,
                   u4_form dav);

      /* _mill_cong(): test congruence.
      */
        u4_t
        _mill_cong(u4_milr m,
                   u4_type mis,
                   u4_type gul);

      /* _mill_cons(): compose a pair.
      */
        u4_form
        _mill_cons(u4_milr m,
                   u4_form vor,
                   u4_form sed);

      /* _mill_cull(): prune for computation.
      */
        u4_t
        _mill_cull(u4_milr m,
                   u4_type lof,
                   u4_log  bup);   /* (type) */

      /* _mill_deep(): test non-atomicity.
      */
        u4_t
        _mill_deep(u4_milr m,
                   u4_type gaf);

      /* _mill_dust(): mutate a type.
      */
        u4_type
        _mill_dust(u4_milr m,
                   u4_noun lor,
                   u4_type bex);

      /* _mill_diff(): test difference.
      */
        u4_form
        _mill_diff(u4_milr m,
                   u4_axis nar,
                   u4_type dol,
                   u4_type sef);

      /* _mill_dump(): type to prep.
      */
        u4_prep
        _mill_dump(u4_milr m,
                   u4_type daf);

      /* _mill_dung(): gene to prep.
      */
        u4_prep
        _mill_dung(u4_milr m,
                   u4_gene lub);

      /* _mill_durf(): formula to prep.
      */
        u4_prep
        _mill_durf(u4_milr m,
                   u4_form rid);

      /* _mill_eith(): fork pair.
      */
        u4_type
        _mill_eith(u4_milr m,
                   u4_type lef,
                   u4_type gap);

      /* _mill_fail(): failage.
      */
        u4_noun
        _mill_fail(u4_milr m,
                   const u4_cl *cl_msg);

      /* _mill_fish(): test form.
      */
        u4_form
        _mill_fish(u4_milr m,
                   u4_axis pol,
                   u4_type das);
#if 0
      /* _mill_find(): seek in path.
      */
        u4_loaf
        _mill_find(u4_milr m,
                   u4_bag  gil,
                   u4_type naf,
                   u4_path fes,
                   u4_coat rov);
#endif        
      /* _mill_flap(): grow a bone.
      */
        u4_type
        _mill_flap(u4_milr m,
                   u4_axis fep,
                   u4_type mac);

      /* _mill_flat(): test atomicity.
      */
        u4_t
        _mill_flat(u4_milr m,
                   u4_type dur);

      /* _mill_flay(): strip decorations.
      */
        u4_type
        _mill_flay(u4_milr m,
                   u4_type naf);

      /* _mill_gene(): lump to gene.
      */
        u4_gene
        _mill_gene(u4_milr m,
                   u4_lump rad);

      /* _mill_gold(): constant type.
      */
        u4_t
        _mill_gold(u4_milr m,
                   u4_type rul);

      /* _mill_hack(): cut type.
      */
        u4_type
        _mill_hack(u4_milr m,
                   u4_axis fis,
                   u4_type bun);

      /* _mill_hike(): mutate form.
      */
        u4_form
        _mill_hike(u4_milr m,
                   u4_axis nov,
                   u4_log  fap);  /* (axis form) */

      /* _mill_hook(): hook from gene.
      */
        u4_axis
        _mill_hook(u4_milr m,
                   u4_type nit,
                   u4_gene col);

      /* _mill_hunt(): analyze branch test.
      */
        u4_noun
        _mill_hunt(u4_milr m,
                   u4_type met,
                   u4_gene nif);

      /* _mill_iron(): static cone.
      */
        u4_t
        _mill_iron(u4_milr m,
                   u4_type bil);

      /* _mill_jack(): specialize.
      */
        u4_type
        _mill_jack(u4_milr m,
                   u4_axis lap,
                   u4_type zim,
                   u4_type noc);

      /* _mill_look(): read attribute.
      */
        u4_noun
        _mill_look(u4_milr m,
                   u4_type zog,
                   u4_atom nat,
                   u4_term cog);

      /* _mill_make(): assemble formula.
      */
        u4_form
        _mill_make(u4_milr m,
                   u4_type ber,
                   u4_gene tol);

      /* _mill_type(): normalize type.
      */
        u4_type
        _mill_type(u4_milr m,
                   u4_lump muf);

      /* _mill_molt(): compile bend changes.
      */
        u4_log
        _mill_molt(u4_milr m, 
                   u4_axis meg, 
                   u4_type ger, 
                   u4_type juk,
                   u4_log  sol);

      /* _mill_not(): invert boolean.
      */
        u4_type
        _mill_not(u4_milr m,
                  u4_form zet);

      /* _mill_null(): true if type is empty.
      */
        u4_t
        _mill_null(u4_milr m,
                   u4_type nis);

      /* _mill_open(): expand gene.
      */
        u4_noun
        _mill_open(u4_milr m,
                   u4_noun nim);

      /* _mill_or(): bake or.
      */
        u4_form
        _mill_or(u4_milr m,
                 u4_form bos,
                 u4_form nif);

      /* _mill_orth(): orthogonality.
      */
        u4_t
        _mill_orth(u4_milr m,
                   u4_type ris,
                   u4_type gel);
      
      /* _mill_pack(): recompose and compress.
      */
        u4_type
        _mill_pack(u4_milr m,
                   u4_log  dun,
                   u4_type zog);

      /* _mill_peek(): check port safety.
      */
        u4_t
        _mill_peek(u4_milr m,
                   u4_bag  gil,
                   u4_type zog,
                   u4_atom fol,
                   u4_term nit);

      /* _mill_pick(): expand type, lightly.
      */
        u4_type
        _mill_pick(u4_milr m,
                   u4_type gaz);

      /* _mill_play(): type inference, top level.
      */
        u4_type
        _mill_play(u4_milr m,
                   u4_type zeb,
                   u4_gene gil);

      /* _mill_poke(): demote for assignment.
      */
        u4_type
        _mill_poke(u4_milr m,
                   u4_axis lap,
                   u4_type zim,
                   u4_type noc);

      /* _mill_pull(): fuse with bar.
      */
        u4_type
        _mill_pull(u4_milr m,
                   u4_log  rol,  /* (type) */
                   u4_type cag);

      /* _mill_read(): read attribute.
      */
        u4_loaf
        _mill_read(u4_milr m,
                   u4_type zog,
                   u4_atom nat,
                   u4_term cog);

      /* _mill_reap(): expand type.
      */
        u4_type
        _mill_reap(u4_milr m,
                   u4_type gaz);

      /* _mill_repo(): replay post.
      */
        u4_type
        _mill_repo(u4_milr m,
                   u4_type bir,
                   u4_gene fug);

      /* _mill_safe(): check type.
      */
        u4_t
        _mill_safe(u4_milr m,
                   u4_type naf, 
                   u4_gene dug);

      /* _mill_safe_in(): check type, with recursion control.
      */
        u4_t
        _mill_safe_in(u4_milr m,
                      u4_bag  gil,
                      u4_type naf,
                      u4_gene dug);

      /* _mill_slip(): hack bar.
      */
        u4_type
        _mill_slip(u4_milr m,
                   u4_axis feg,
                   u4_log  cot);  /* (type) */

      /* _mill_suss(): test dust correctness.
      **
      ** lor: ((list {* [(axis) *]}))
      */
        u4_t
        _mill_suss(u4_milr m,
                   u4_bag  gil,
                   u4_type naf,
                   u4_log  p_dug,
                   u4_log  q_dug);

      /* _mill_trap(): save trap.
      */
      void
      _mill_trap(u4_milr m,
                 const u4_cl *cl_msg);

      /* _mill_trig(): separate into left, right, center.
      **
      ** lor, *zif, *gam, *nog: ((list {* [(axis) *]}))
      */
        void
        _mill_trig(u4_lane lane,
                   u4_log  lor,
                   u4_log *zif,
                   u4_log *gam, 
                   u4_log *nog);

      /* _mill_take(): test for assignment.
      */
        u4_t
        _mill_take(u4_milr m,
                   u4_axis lap,
                   u4_type zim,
                   u4_type noc);

