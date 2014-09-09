/* include/f/qjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3_cqa_add(u3_atom, u3_atom);
    u3_noun u3_cqa_dec(u3_atom);
    u3_noun u3_cqa_div(u3_atom, u3_atom);
    u3_noun u3_cqa_gte(u3_atom, u3_atom);
    u3_noun u3_cqa_gth(u3_atom, u3_atom);
    u3_noun u3_cqa_inc(u3_atom);
    u3_noun u3_cqa_lte(u3_atom, u3_atom);
    u3_noun u3_cqa_lth(u3_atom, u3_atom);
    u3_noun u3_cqa_mod(u3_atom, u3_atom);
    u3_noun u3_cqa_mul(u3_atom, u3_atom);
    u3_noun u3_cqa_sub(u3_atom, u3_atom);

  /** Tier 2.
  **/
    u3_noun u3_cqb_bind(u3_noun, u3_noun);
    u3_noun u3_cqb_clap(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqb_drop(u3_noun);
    u3_noun u3_cqb_flop(u3_noun);
    u3_noun u3_cqb_lent(u3_noun);
    u3_noun u3_cqb_levy(u3_noun, u3_noun);
    u3_noun u3_cqb_lien(u3_noun, u3_noun);
    u3_noun u3_cqb_need(u3_noun);
    u3_noun u3_cqb_reel(u3_noun, u3_noun);
    u3_noun u3_cqb_roll(u3_noun, u3_noun); 
    u3_noun u3_cqb_skim(u3_noun, u3_noun);
    u3_noun u3_cqb_skip(u3_noun, u3_noun);
    u3_noun u3_cqb_scag(u3_atom, u3_noun);
    u3_noun u3_cqb_slag(u3_atom, u3_noun);
    u3_noun u3_cqb_snag(u3_atom, u3_noun);
    u3_noun u3_cqb_sort(u3_noun, u3_noun);
    u3_noun u3_cqb_turn(u3_noun, u3_noun);
    u3_noun u3_cqb_weld(u3_noun, u3_noun);

  /** Tier 3.
  **/
    u3_noun u3_cqc_bex(u3_atom);
    u3_noun u3_cqc_can(u3_atom, u3_noun);
    u3_noun u3_cqc_cap(u3_atom);
    u3_noun u3_cqc_cat(u3_atom, u3_atom, u3_atom);
    u3_noun u3_cqc_con(u3_atom, u3_atom);
    u3_noun u3_cqc_cut(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3_cqc_dis(u3_atom, u3_atom);
    u3_noun u3_cqc_dor(u3_atom, u3_atom);
    u3_noun u3_cqc_end(u3_atom, u3_atom, u3_atom);
    u3_noun u3_cqc_gor(u3_atom, u3_atom);
    u3_noun u3_cqc_hor(u3_atom, u3_atom);
    u3_noun u3_cqc_lsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3_cqc_mas(u3_atom);
    u3_noun u3_cqc_met(u3_atom, u3_atom);
    u3_noun u3_cqc_mix(u3_atom, u3_atom);
    u3_noun u3_cqc_peg(u3_atom, u3_atom);
    u3_noun u3_cqc_rap(u3_atom, u3_noun);
    u3_noun u3_cqc_rip(u3_atom, u3_atom);
    u3_noun u3_cqc_rsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3_cqc_vor(u3_atom, u3_atom);

  /** Tier 4.
  **/
    u3_noun u3_cqdb_gas(u3_noun, u3_noun);
    u3_noun u3_cqdb_get(u3_noun, u3_noun);
    u3_bean u3_cqdb_has(u3_noun, u3_noun);
    u3_noun u3_cqdb_int(u3_noun, u3_noun);
    u3_noun u3_cqdb_put(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqdb_uni(u3_noun, u3_noun);
    u3_noun u3_cqdi_gas(u3_noun, u3_noun);
    u3_bean u3_cqdi_has(u3_noun, u3_noun);
    u3_noun u3_cqdi_int(u3_noun, u3_noun);
    u3_noun u3_cqdi_mer(u3_noun, u3_noun);
    u3_noun u3_cqdi_put(u3_noun, u3_noun);
    u3_noun u3_cqdi_tap(u3_noun, u3_noun);
    u3_noun u3_cqdi_uni(u3_noun, u3_noun);

  /** Tier 5.
  **/
    u3_noun u3_cqe_cue(u3_atom);
    u3_noun u3_cqe_jam(u3_atom);
    u3_noun u3_cqe_mat(u3_atom);
    u3_noun u3_cqe_rub(u3_atom, u3_atom);
    u3_noun u3_cqe_lore(u3_atom);
    u3_noun u3_cqe_loss(u3_noun, u3_noun);
    u3_noun u3_cqe_repg(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqe_rexp(u3_noun, u3_noun);
    u3_noun u3_cqe_trip(u3_atom);

    u3_noun u3_cqea_de(u3_atom, u3_atom);
    u3_noun u3_cqea_en(u3_atom, u3_atom);

    u3_noun u3_cqe_shax(u3_atom);
    u3_noun u3_cqe_shas(u3_atom, u3_atom);
    u3_noun u3_cqe_shal(u3_atom, u3_atom);

    u3_noun u3_cqeo_raw(u3_atom, u3_atom);

    u3_noun u3_cqer_sun(u3_atom);
    u3_noun u3_cqer_mul(u3_atom, u3_atom);
    u3_noun u3_cqer_div(u3_atom, u3_atom);
    u3_noun u3_cqer_add(u3_atom, u3_atom);
    u3_noun u3_cqer_sub(u3_atom, u3_atom);
    u3_noun u3_cqer_lte(u3_atom, u3_atom);
    u3_noun u3_cqer_lth(u3_atom, u3_atom);
    u3_noun u3_cqer_gte(u3_atom, u3_atom);
    u3_noun u3_cqer_gth(u3_atom, u3_atom);


  /** Tier 6.
  **/
    u3_noun u3_cqf_bull(u3_noun, u3_noun);
    u3_noun u3_cqf_cell(u3_noun, u3_noun);
    u3_noun u3_cqf_comb(u3_noun, u3_noun);
    u3_noun u3_cqf_cons(u3_noun, u3_noun);
    u3_noun u3_cqf_core(u3_noun, u3_noun);
    u3_noun u3_cqf_cube(u3_noun, u3_noun);
    u3_noun u3_cqf_face(u3_noun, u3_noun);
    u3_noun u3_cqf_fine(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqf_fitz(u3_noun, u3_noun);
    u3_noun u3_cqf_flan(u3_noun, u3_noun);
    u3_noun u3_cqf_flay(u3_noun);
    u3_noun u3_cqf_flip(u3_noun);
    u3_noun u3_cqf_flor(u3_noun, u3_noun);
    u3_noun u3_cqf_fork(u3_noun, u3_noun);
    u3_noun u3_cqf_hike(u3_noun, u3_noun);
    u3_noun u3_cqf_look(u3_noun, u3_noun);
    u3_noun u3_cqf_slot(u3_atom, u3_noun);
    u3_noun u3_cqf_type(u3_noun);

    u3_noun u3_cqfl_bunt(u3_noun, u3_noun); 
    u3_noun u3_cqfl_whip(u3_noun, u3_noun, u3_noun);

    u3_noun u3_cqfp_hack(u3_noun, u3_noun);
    u3_noun u3_cqfp_late(u3_noun);
    u3_noun u3_cqfp_open(u3_noun, u3_noun);
    u3_noun u3_cqfp_rake(u3_noun);

#   define u3_cqfu_van_fan  28
#   define u3_cqfu_van_rib  58
#   define u3_cqfu_van_vrf  59
#   define u3_cqfu_van_vet  118
#   define u3_cqfu_van_fab  119

    u3_noun u3_cqfu_burn(u3_noun, u3_noun);
    u3_noun u3_cqfu_busk(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_bust(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_conk(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_crop(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_cull(u3_noun, u3_noun, u3_bean, u3_atom, u3_noun);
    u3_noun u3_cqfu_duck(u3_noun, u3_noun); 
    u3_noun u3_cqfu_dung(u3_noun, u3_noun cap, u3_noun);
    u3_noun u3_cqfu_dunq(u3_noun, const c3_c*, u3_noun);
    u3_noun u3_cqfu_find(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_fino(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_fink(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_fire(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_firm(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_fish(u3_noun, u3_noun, u3_atom);
    u3_noun u3_cqfu_fond(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_fuse(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_gain(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_heal(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_lose(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_mint(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_mull(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_bean u3_cqfu_nest(u3_noun, u3_noun, u3_bean, u3_noun);
    u3_bean u3_cqfu_orth(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_park(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_peek(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_play(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_repo(u3_noun, u3_noun);
    u3_noun u3_cqfu_rest(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_seek(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_seep(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_shep(u3_noun, const c3_c*, u3_noun, u3_noun);
    u3_noun u3_cqfu_shew(u3_noun, u3_noun); 
    u3_noun u3_cqfu_sift(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_snub(u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_tack(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_tock(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3_cqfu_wrap(u3_noun, u3_noun, u3_noun);
