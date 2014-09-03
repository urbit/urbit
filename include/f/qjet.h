/* include/f/qjet.h
**
** This file is in the public domain.
*/
  /** Direct C interfaces.
  **/
    /** Tier 1.
    **/
      u2_noun u2_cqa_add(u2_atom, u2_atom);
      u2_noun u2_cqa_dec(u2_atom);
      u2_noun u2_cqa_div(u2_atom, u2_atom);
      u2_noun u2_cqa_gte(u2_atom, u2_atom);
      u2_noun u2_cqa_gth(u2_atom, u2_atom);
      u2_noun u2_cqa_inc(u2_atom);
      u2_noun u2_cqa_lte(u2_atom, u2_atom);
      u2_noun u2_cqa_lth(u2_atom, u2_atom);
      u2_noun u2_cqa_mul(u2_atom, u2_atom);
      u2_noun u2_cqa_sub(u2_atom, u2_atom);

    /** Tier 2.
    **/
      u2_noun u2_cqb_bind(u2_noun, u2_noun);
      u2_noun u2_cqb_clap(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqb_drop(u2_noun);
      u2_noun u2_cqb_flop(u2_noun);
      u2_noun u2_cqb_lent(u2_noun);
      u2_noun u2_cqb_levy(u2_noun, u2_noun);
      u2_noun u2_cqb_lien(u2_noun, u2_noun);
      u2_noun u2_cqb_need(u2_noun);
      u2_noun u2_cqb_reel(u2_noun, u2_noun);
      u2_noun u2_cqb_roll(u2_noun, u2_noun); 
      u2_noun u2_cqb_skim(u2_noun, u2_noun);
      u2_noun u2_cqb_skip(u2_noun, u2_noun);
      u2_noun u2_cqb_snag(u2_atom, u2_noun);
      u2_noun u2_cqb_sort(u2_noun, u2_noun);
      u2_noun u2_cqb_turn(u2_noun, u2_noun);
      u2_noun u2_cqb_weld(u2_noun, u2_noun);

    /** Tier 3.
    **/
      u2_noun u2_cqc_bex(u2_atom);
      u2_noun u2_cqc_can(u2_atom, u2_noun);
      u2_noun u2_cqc_cap(u2_atom);
      u2_noun u2_cqc_cat(u2_atom, u2_atom, u2_atom);
      u2_noun u2_cqc_con(u2_atom, u2_atom);
      u2_noun u2_cqc_cut(u2_atom, u2_atom, u2_atom, u2_atom);
      u2_noun u2_cqc_dis(u2_atom, u2_atom);
      u2_noun u2_cqc_dor(u2_atom, u2_atom);
      u2_noun u2_cqc_end(u2_atom, u2_atom, u2_atom);
      u2_noun u2_cqc_gor(u2_atom, u2_atom);
      u2_noun u2_cqc_hor(u2_atom, u2_atom);
      u2_noun u2_cqc_lsh(u2_atom, u2_atom, u2_atom);
      u2_noun u2_cqc_mas(u2_atom);
      u2_noun u2_cqc_met(u2_atom, u2_atom);
      u2_noun u2_cqc_mix(u2_atom, u2_atom);
      u2_noun u2_cqc_peg(u2_atom, u2_atom);
      u2_noun u2_cqc_rap(u2_atom, u2_noun);
      u2_noun u2_cqc_rip(u2_atom, u2_atom);
      u2_noun u2_cqc_rsh(u2_atom, u2_atom, u2_atom);
      u2_noun u2_cqc_trip(u2_atom);
      u2_noun u2_cqc_vor(u2_atom, u2_atom);

    /** Tier 4.
    **/
      u2_noun u2_cqdb_gas(u2_noun, u2_noun);
      u2_noun u2_cqdb_get(u2_noun, u2_noun);
      u2_bean u2_cqdb_has(u2_noun, u2_noun);
      u2_noun u2_cqdb_int(u2_noun, u2_noun);
      u2_noun u2_cqdb_put(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqdb_uni(u2_noun, u2_noun);
      u2_noun u2_cqdi_gas(u2_noun, u2_noun);
      u2_bean u2_cqdi_has(u2_noun, u2_noun);
      u2_noun u2_cqdi_int(u2_noun, u2_noun);
      u2_noun u2_cqdi_put(u2_noun, u2_noun);
      u2_noun u2_cqdi_tap(u2_noun, u2_noun);
      u2_noun u2_cqdi_uni(u2_noun, u2_noun);

    /** Tier 5.
    **/
      u2_noun u2_cqe_cue(u2_atom);
      u2_noun u2_cqe_jam(u2_atom);
      u2_noun u2_cqe_mat(u2_atom);
      u2_noun u2_cqe_rub(u2_atom, u2_atom);

    /** Tier 6.
    **/
#     define j2_ut_van_fan  28
#     define j2_ut_van_rib  58
#     define j2_ut_van_vrf  59
#     define j2_ut_van_vet  118
#     define j2_ut_van_fab  119

      u2_noun u2_cqf_bull(u2_noun, u2_noun);
      u2_noun u2_cqf_cell(u2_noun, u2_noun);
      u2_noun u2_cqf_comb(u2_noun, u2_noun);
      u2_noun u2_cqf_cons(u2_noun, u2_noun);
      u2_noun u2_cqf_core(u2_noun, u2_noun);
      u2_noun u2_cqf_cube(u2_noun, u2_noun);
      u2_noun u2_cqf_face(u2_noun, u2_noun);
      u2_noun u2_cqf_fine(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqf_fitz(u2_noun, u2_noun);
      u2_noun u2_cqf_flan(u2_noun, u2_noun);
      u2_noun u2_cqf_flay(u2_noun);
      u2_noun u2_cqf_flip(u2_noun);
      u2_noun u2_cqf_flor(u2_noun, u2_noun);
      u2_noun u2_cqf_fork(u2_noun, u2_noun);
      u2_noun u2_cqf_hike(u2_noun, u2_noun);
      u2_noun u2_cqf_look(u2_noun, u2_noun);
      u2_noun u2_cqf_type(u2_noun);

      u2_noun u2_cqfl_bunt(u2_noun, u2_noun); 
      u2_noun u2_cqfl_whip(u2_noun, u2_noun, u2_noun);

      u2_noun u2_cqfp_hack(u2_noun, u2_noun);
      u2_noun u2_cqfp_late(u2_noun);
      u2_noun u2_cqfp_open(u2_noun, u2_noun);
      u2_noun u2_cqfp_rake(u2_noun);

      u2_noun u2_cqfu_burn(u2_noun, u2_noun);
      u2_noun u2_cqfu_busk(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_bust(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_conk(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_crop(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_cull(u2_noun, u2_noun, u2_bean, u2_atom, u2_noun);
      u2_noun u2_cqfu_duck(u2_noun, u2_noun); 
      u2_noun u2_cqfu_dung(u2_noun, u2_noun cap, u2_noun);
      u2_noun u2_cqfu_dunq(u2_noun, const c3_c*, u2_noun);
      u2_noun u2_cqfu_find(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_fink(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_fire(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_firm(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_fish(u2_noun, u2_noun, u2_atom);
      u2_noun u2_cqfu_fond(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_fuse(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_gain(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_heal(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_lose(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_mint(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_mull(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_bean u2_cqfu_nest(u2_noun, u2_noun, u2_bean, u2_noun);
      u2_bean u2_cqfu_orth(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_park(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_peek(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_play(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_repo(u2_noun, u2_noun);
      u2_noun u2_cqfu_rest(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_seek(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_seep(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_shep(u2_noun, const c3_c*, u2_noun, u2_noun);
      u2_noun u2_cqfu_shew(u2_noun, u2_noun); 
      u2_noun u2_cqfu_sift(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_snub(u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_tack(u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_tock(u2_noun, u2_noun, u2_noun, u2_noun, u2_noun);
      u2_noun u2_cqfu_wrap(u2_noun, u2_noun, u2_noun);
