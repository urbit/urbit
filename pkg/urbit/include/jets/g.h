/* include/f/qjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3ga_add(u3_atom, u3_atom);
    u3_noun u3ga_dec(u3_atom);
    u3_noun u3ga_div(u3_atom, u3_atom);
    u3_noun u3ga_gte(u3_atom, u3_atom);
    u3_noun u3ga_gth(u3_atom, u3_atom);
    u3_noun u3ga_inc(u3_atom);
    u3_noun u3ga_lte(u3_atom, u3_atom);
    u3_noun u3ga_lth(u3_atom, u3_atom);
    u3_noun u3ga_mod(u3_atom, u3_atom);
    u3_noun u3ga_mul(u3_atom, u3_atom);
    u3_noun u3ga_sub(u3_atom, u3_atom);

  /** Tier 2.
  **/
    u3_noun u3gb_bind(u3_noun, u3_noun);
    u3_noun u3gb_clap(u3_noun, u3_noun, u3_noun);
    u3_noun u3gb_drop(u3_noun);
    u3_noun u3gb_flop(u3_noun);
    u3_noun u3gb_lent(u3_noun);
    u3_noun u3gb_levy(u3_noun, u3_noun);
    u3_noun u3gb_lien(u3_noun, u3_noun);
    u3_noun u3gb_murn(u3_noun, u3_noun);
    u3_noun u3gb_need(u3_noun);
    u3_noun u3gb_reap(u3_atom, u3_noun);
    u3_noun u3gb_reel(u3_noun, u3_noun);
    u3_noun u3gb_roll(u3_noun, u3_noun);
    u3_noun u3gb_skid(u3_noun, u3_noun);
    u3_noun u3gb_skim(u3_noun, u3_noun);
    u3_noun u3gb_skip(u3_noun, u3_noun);
    u3_noun u3gb_scag(u3_atom, u3_noun);
    u3_noun u3gb_slag(u3_atom, u3_noun);
    u3_noun u3gb_snag(u3_atom, u3_noun);
    u3_noun u3gb_sort(u3_noun, u3_noun);
    u3_noun u3gb_turn(u3_noun, u3_noun);
    u3_noun u3gb_weld(u3_noun, u3_noun);

  /** Tier 3.
  **/
    u3_noun u3gc_bex(u3_atom);
    u3_noun u3gc_xeb(u3_atom);
    u3_noun u3gc_can(u3_atom, u3_noun);
    u3_noun u3gc_cap(u3_atom);
    u3_noun u3gc_cat(u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_con(u3_atom, u3_atom);
    u3_noun u3gc_cut(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_dis(u3_atom, u3_atom);
    u3_noun u3gc_dor(u3_atom, u3_atom);
    u3_noun u3gc_dvr(u3_atom, u3_atom);
    u3_noun u3gc_end(u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_gor(u3_atom, u3_atom);
    u3_noun u3gc_hor(u3_atom, u3_atom);
    u3_noun u3gc_lsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_mas(u3_atom);
    u3_noun u3gc_met(u3_atom, u3_atom);
    u3_noun u3gc_mix(u3_atom, u3_atom);
    u3_noun u3gc_muk(u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_peg(u3_atom, u3_atom);
    u3_noun u3gc_pow(u3_atom, u3_atom);
    u3_noun u3gc_rap(u3_atom, u3_noun);
    u3_noun u3gc_rep(u3_atom, u3_noun);
    u3_noun u3gc_rip(u3_atom, u3_atom);
    u3_noun u3gc_rsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3gc_sqt(u3_atom);
    u3_noun u3gc_vor(u3_atom, u3_atom);

  /** Tier 4.
  **/
    u3_noun u3gdb_bif(u3_noun, u3_noun);
    u3_noun u3gdb_dif(u3_noun, u3_noun);
    u3_noun u3gdb_gas(u3_noun, u3_noun);
    u3_noun u3gdb_get(u3_noun, u3_noun);
    u3_noun u3gdb_has(u3_noun, u3_noun);
    u3_noun u3gdb_int(u3_noun, u3_noun);
    u3_noun u3gdb_put(u3_noun, u3_noun, u3_noun);
#   define u3gdb_tap u3gdi_tap
    u3_noun u3gdb_uni(u3_noun, u3_noun);

    u3_noun u3gdi_bif(u3_noun, u3_noun);
    u3_noun u3gdi_dif(u3_noun, u3_noun);
    u3_noun u3gdi_gas(u3_noun, u3_noun);
    u3_noun u3gdi_has(u3_noun, u3_noun);
    u3_noun u3gdi_int(u3_noun, u3_noun);
    u3_noun u3gdi_mer(u3_noun, u3_noun);
    u3_noun u3gdi_put(u3_noun, u3_noun);
    u3_noun u3gdi_tap(u3_noun);
    u3_noun u3gdi_uni(u3_noun, u3_noun);
    u3_noun u3gdi_wyt(u3_noun);

  /** Tier 5.
  **/
    u3_noun u3ge_cue(u3_atom);
    u3_noun u3ge_jam(u3_atom);
    u3_noun u3ge_mat(u3_atom);
    u3_noun u3ge_rub(u3_atom, u3_atom);
    u3_noun u3ge_lore(u3_atom);
    u3_noun u3ge_loss(u3_noun, u3_noun);
    u3_noun u3ge_lune(u3_atom);
    u3_noun u3ge_repg(u3_noun, u3_noun, u3_noun);
    u3_noun u3ge_rexp(u3_noun, u3_noun);
    u3_noun u3ge_trip(u3_atom);

    u3_noun u3gea_ecba_en(u3_atom, u3_atom);
    u3_noun u3gea_ecba_de(u3_atom, u3_atom);
    u3_noun u3gea_ecbb_en(u3_atom, u3_atom);
    u3_noun u3gea_ecbb_de(u3_atom, u3_atom);
    u3_noun u3gea_ecbc_en(u3_atom, u3_atom);
    u3_noun u3gea_ecbc_de(u3_atom, u3_atom);

    u3_noun u3gea_cbca_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3gea_cbca_de(u3_atom, u3_atom, u3_atom);
    u3_noun u3gea_cbcb_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3gea_cbcb_de(u3_atom, u3_atom, u3_atom);
    u3_noun u3gea_cbcc_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3gea_cbcc_de(u3_atom, u3_atom, u3_atom);

    u3_noun u3gea_de(u3_atom, u3_atom);
    u3_noun u3gea_en(u3_atom, u3_atom);

    u3_noun u3ges_hsh(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_hsl(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom,
                      u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_pbk(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_pbl(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom, u3_atom);

    u3_noun u3ge_shax(u3_atom);
    u3_noun u3ge_shay(u3_atom, u3_atom);
    u3_noun u3ge_shas(u3_atom, u3_atom);
    u3_noun u3ge_shal(u3_atom, u3_atom);

    u3_noun u3geo_raw(u3_atom, u3_atom);

    u3_noun u3gef_drg(u3_noun, u3_atom);
    u3_noun u3gef_lug(u3_noun, u3_noun, u3_atom, u3_atom);

    u3_noun u3ger_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3ger_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3ger_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3ger_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3ger_sqt(u3_atom, u3_atom);
    u3_noun u3ger_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3ger_lth(u3_atom, u3_atom);
    u3_noun u3ger_lte(u3_atom, u3_atom);
    u3_noun u3ger_equ(u3_atom, u3_atom);
    u3_noun u3ger_gte(u3_atom, u3_atom);
    u3_noun u3ger_gth(u3_atom, u3_atom);

    u3_noun u3get_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3get_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3get_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3get_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3get_sqt(u3_atom, u3_atom);
    u3_noun u3get_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3get_lth(u3_atom, u3_atom);
    u3_noun u3get_lte(u3_atom, u3_atom);
    u3_noun u3get_equ(u3_atom, u3_atom);
    u3_noun u3get_gte(u3_atom, u3_atom);
    u3_noun u3get_gth(u3_atom, u3_atom);

    u3_noun u3geq_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3geq_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3geq_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3geq_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3geq_sqt(u3_atom, u3_atom);
    u3_noun u3geq_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3geq_lth(u3_atom, u3_atom);
    u3_noun u3geq_lte(u3_atom, u3_atom);
    u3_noun u3geq_equ(u3_atom, u3_atom);
    u3_noun u3geq_gte(u3_atom, u3_atom);
    u3_noun u3geq_gth(u3_atom, u3_atom);

    u3_noun u3ges_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_sqt(u3_atom, u3_atom);
    u3_noun u3ges_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3ges_lth(u3_atom, u3_atom);
    u3_noun u3ges_lte(u3_atom, u3_atom);
    u3_noun u3ges_equ(u3_atom, u3_atom);
    u3_noun u3ges_gte(u3_atom, u3_atom);
    u3_noun u3ges_gth(u3_atom, u3_atom);

  /** Tier 6.
  **/
    u3_noun u3gf_bull(u3_noun, u3_noun);
    u3_noun u3gf_cell(u3_noun, u3_noun);
    u3_noun u3gf_comb(u3_noun, u3_noun);
    u3_noun u3gf_cons(u3_noun, u3_noun);
    u3_noun u3gf_core(u3_noun, u3_noun);
    u3_noun u3gf_cube(u3_noun, u3_noun);
    u3_noun u3gf_face(u3_noun, u3_noun);
    u3_noun u3gf_fine(u3_noun, u3_noun, u3_noun);
    u3_noun u3gf_fitz(u3_noun, u3_noun);
    u3_noun u3gf_flan(u3_noun, u3_noun);
    u3_noun u3gf_flay(u3_noun);
    u3_noun u3gf_flip(u3_noun);
    u3_noun u3gf_flor(u3_noun, u3_noun);
    u3_noun u3gf_forq(u3_noun, u3_noun);
    u3_noun u3gf_fork(u3_noun);
    u3_noun u3gf_grof(u3_noun);
    u3_noun u3gf_help(u3_noun, u3_noun);
    u3_noun u3gf_hike(u3_noun, u3_noun);
    u3_noun u3gf_look(u3_noun, u3_noun);
    u3_noun u3gf_loot(u3_noun, u3_noun);
    u3_noun u3gf_slot(u3_atom, u3_noun);
    u3_noun u3gf_type(u3_noun);

    u3_noun u3gfl_bunt(u3_noun, u3_noun);
    u3_noun u3gfl_whip(u3_noun, u3_noun, u3_noun);

    u3_noun u3gfp_hack(u3_noun, u3_noun);
    u3_noun u3gfp_late(u3_noun);
    u3_noun u3gfp_open(u3_noun, u3_noun);
    u3_noun u3gfp_rake(u3_noun);

#   define u3gfu_van_fan  28
#   define u3gfu_van_rib  58
#   define u3gfu_van_vrf  59
#   define u3gfu_van_vet  118
#   define u3gfu_van_fab  119

    u3_noun u3gfu_burn(u3_noun, u3_noun);
    u3_noun u3gfu_busk(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_buss(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_bust(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_conk(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_crop(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_cull(u3_noun, u3_noun, u3_noun, u3_atom, u3_noun);
    u3_noun u3gfu_duck(u3_noun, u3_noun);
    u3_noun u3gfu_dung(u3_noun, u3_noun cap, u3_noun);
    u3_noun u3gfu_dunq(u3_noun, const c3_c*, u3_noun);
    void    u3gfu_dump(u3_noun, const c3_c*, u3_noun);
    u3_noun u3gfu_fond(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_finc(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_fink(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_fire(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_firm(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_fish(u3_noun, u3_noun, u3_atom);
    u3_noun u3gfu_fuse(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_gain(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_heal(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_lose(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_mint(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_mull(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_nest(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_nost(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_orth(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_peek(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_peel(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_play(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_repo(u3_noun, u3_noun);
    u3_noun u3gfu_rest(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_shep(u3_noun, const c3_c*, u3_noun, u3_noun);
    u3_noun u3gfu_shew(u3_noun, u3_noun);
    u3_noun u3gfu_sift(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_snub(u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_tack(u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_toss(u3_noun, u3_noun, u3_noun, u3_noun, u3_noun);
    u3_noun u3gfu_wrap(u3_noun, u3_noun, u3_noun);
