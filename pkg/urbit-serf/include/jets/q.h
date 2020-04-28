/* include/f/qjet.h
**
** This file is in the public domain.
*/
  /** Tier 1.
  **/
    u3_noun u3qa_add(u3_atom, u3_atom);
    u3_noun u3qa_dec(u3_atom);
    u3_noun u3qa_div(u3_atom, u3_atom);
    u3_noun u3qa_gte(u3_atom, u3_atom);
    u3_noun u3qa_gth(u3_atom, u3_atom);
    u3_noun u3qa_inc(u3_atom);
    u3_noun u3qa_lte(u3_atom, u3_atom);
    u3_noun u3qa_lth(u3_atom, u3_atom);
    u3_noun u3qa_mod(u3_atom, u3_atom);
    u3_noun u3qa_mul(u3_atom, u3_atom);
    u3_noun u3qa_sub(u3_atom, u3_atom);

  /** Tier 2.
  **/
    u3_noun u3qb_bind(u3_noun, u3_noun);
    u3_noun u3qb_clap(u3_noun, u3_noun, u3_noun);
    u3_noun u3qb_drop(u3_noun);
    u3_noun u3qb_flop(u3_noun);
    u3_noun u3qb_lent(u3_noun);
    u3_noun u3qb_levy(u3_noun, u3_noun);
    u3_noun u3qb_lien(u3_noun, u3_noun);
    u3_noun u3qb_murn(u3_noun, u3_noun);
    u3_noun u3qb_need(u3_noun);
    u3_noun u3qb_reap(u3_atom, u3_noun);
    u3_noun u3qb_reel(u3_noun, u3_noun);
    u3_noun u3qb_roll(u3_noun, u3_noun);
    u3_noun u3qb_skid(u3_noun, u3_noun);
    u3_noun u3qb_skim(u3_noun, u3_noun);
    u3_noun u3qb_skip(u3_noun, u3_noun);
    u3_noun u3qb_scag(u3_atom, u3_noun);
    u3_noun u3qb_slag(u3_atom, u3_noun);
    u3_noun u3qb_snag(u3_atom, u3_noun);
    u3_noun u3qb_sort(u3_noun, u3_noun);
    u3_noun u3qb_turn(u3_noun, u3_noun);
    u3_noun u3qb_weld(u3_noun, u3_noun);

  /** Tier 3.
  **/
    u3_noun u3qc_bex(u3_atom);
    u3_noun u3qc_xeb(u3_atom);
    u3_noun u3qc_can(u3_atom, u3_noun);
    u3_noun u3qc_cap(u3_atom);
    u3_noun u3qc_cat(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_con(u3_atom, u3_atom);
    u3_noun u3qc_cut(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_dis(u3_atom, u3_atom);
    u3_noun u3qc_dor(u3_atom, u3_atom);
    u3_noun u3qc_dvr(u3_atom, u3_atom);
    u3_noun u3qc_end(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_gor(u3_atom, u3_atom);
    u3_noun u3qc_lsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_mas(u3_atom);
    u3_noun u3qc_met(u3_atom, u3_atom);
    u3_noun u3qc_mix(u3_atom, u3_atom);
    u3_noun u3qc_mor(u3_atom, u3_atom);
    u3_noun u3qc_muk(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_peg(u3_atom, u3_atom);
    u3_noun u3qc_pow(u3_atom, u3_atom);
    u3_noun u3qc_rap(u3_atom, u3_noun);
    u3_noun u3qc_rep(u3_atom, u3_noun);
    u3_noun u3qc_rev(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_rip(u3_atom, u3_atom);
    u3_noun u3qc_rsh(u3_atom, u3_atom, u3_atom);
    u3_noun u3qc_swp(u3_atom, u3_atom);
    u3_noun u3qc_sqt(u3_atom);

  /** Tier 4.
  **/
    u3_noun u3qdb_bif(u3_noun, u3_noun);
    u3_noun u3qdb_dif(u3_noun, u3_noun);
    u3_noun u3qdb_gas(u3_noun, u3_noun);
    u3_noun u3qdb_get(u3_noun, u3_noun);
    u3_noun u3qdb_has(u3_noun, u3_noun);
    u3_noun u3qdb_int(u3_noun, u3_noun);
    u3_noun u3qdb_put(u3_noun, u3_noun, u3_noun);
#   define u3qdb_tap u3qdi_tap
    u3_noun u3qdb_uni(u3_noun, u3_noun);

    u3_noun u3qdi_bif(u3_noun, u3_noun);
    u3_noun u3qdi_dif(u3_noun, u3_noun);
    u3_noun u3qdi_gas(u3_noun, u3_noun);
    u3_noun u3qdi_has(u3_noun, u3_noun);
    u3_noun u3qdi_int(u3_noun, u3_noun);
    u3_noun u3qdi_put(u3_noun, u3_noun);
    u3_noun u3qdi_tap(u3_noun);
    u3_noun u3qdi_uni(u3_noun, u3_noun);
    u3_noun u3qdi_wyt(u3_noun);

  /** Tier 5.
  **/
    u3_noun u3qe_cue(u3_atom);
    u3_noun u3qe_jam(u3_atom);
    u3_noun u3qe_mat(u3_atom);
    u3_noun u3qe_rub(u3_atom, u3_atom);
    u3_noun u3qe_lore(u3_atom);
    u3_noun u3qe_loss(u3_noun, u3_noun);
    u3_noun u3qe_lune(u3_atom);
    u3_noun u3qe_repg(u3_noun, u3_noun, u3_noun);
    u3_noun u3qe_rexp(u3_noun, u3_noun);
    u3_noun u3qe_trip(u3_atom);

    u3_noun u3qea_ecba_en(u3_atom, u3_atom);
    u3_noun u3qea_ecba_de(u3_atom, u3_atom);
    u3_noun u3qea_ecbb_en(u3_atom, u3_atom);
    u3_noun u3qea_ecbb_de(u3_atom, u3_atom);
    u3_noun u3qea_ecbc_en(u3_atom, u3_atom);
    u3_noun u3qea_ecbc_de(u3_atom, u3_atom);

    u3_noun u3qea_cbca_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3qea_cbca_de(u3_atom, u3_atom, u3_atom);
    u3_noun u3qea_cbcb_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3qea_cbcb_de(u3_atom, u3_atom, u3_atom);
    u3_noun u3qea_cbcc_en(u3_atom, u3_atom, u3_atom);
    u3_noun u3qea_cbcc_de(u3_atom, u3_atom, u3_atom);

    u3_noun u3qea_de(u3_atom, u3_atom);
    u3_noun u3qea_en(u3_atom, u3_atom);

    u3_noun u3qes_hsh(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_hsl(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom,
                      u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_pbk(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_pbl(u3_atom, u3_atom, u3_atom, u3_atom, u3_atom, u3_atom);

    u3_noun u3qe_shax(u3_atom);
    u3_noun u3qe_shay(u3_atom, u3_atom);
    u3_noun u3qe_shas(u3_atom, u3_atom);
    u3_noun u3qe_shal(u3_atom, u3_atom);
    u3_noun u3qe_sha1(u3_atom, u3_atom);

    u3_noun u3qe_hmac(u3_noun, u3_atom, u3_atom,
                      u3_atom, u3_atom, u3_atom, u3_atom);


    u3_noun u3qe_argon2(u3_atom, u3_atom, u3_atom,
                        u3_atom, u3_atom, u3_atom,
                        u3_atom, u3_atom, u3_atom, u3_atom,
                        u3_atom, u3_atom, u3_atom, u3_atom);

    u3_noun u3qe_blake(u3_atom wid, u3_atom dat,
                       u3_atom wik, u3_atom dak, u3_atom out);

    u3_noun u3qe_ripe(u3_atom wid, u3_atom dat);

    u3_noun u3qe_make(u3_atom has, u3_atom prv);
    u3_noun u3qe_reco(u3_atom has, u3_atom sig_v, u3_atom sig_r, u3_atom sig_s);
    u3_noun u3qe_sign(u3_atom has, u3_atom prv);

    u3_noun u3qeo_raw(u3_atom, u3_atom);

    u3_noun u3qef_drg(u3_noun, u3_atom);
    u3_noun u3qef_lug(u3_noun, u3_noun, u3_atom, u3_atom);

    u3_noun u3qer_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3qer_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3qer_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3qer_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3qer_sqt(u3_atom, u3_atom);
    u3_noun u3qer_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qer_lth(u3_atom, u3_atom);
    u3_noun u3qer_lte(u3_atom, u3_atom);
    u3_noun u3qer_equ(u3_atom, u3_atom);
    u3_noun u3qer_gte(u3_atom, u3_atom);
    u3_noun u3qer_gth(u3_atom, u3_atom);

    u3_noun u3qet_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3qet_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3qet_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3qet_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3qet_sqt(u3_atom, u3_atom);
    u3_noun u3qet_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qet_lth(u3_atom, u3_atom);
    u3_noun u3qet_lte(u3_atom, u3_atom);
    u3_noun u3qet_equ(u3_atom, u3_atom);
    u3_noun u3qet_gte(u3_atom, u3_atom);
    u3_noun u3qet_gth(u3_atom, u3_atom);

    u3_noun u3qeq_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3qeq_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3qeq_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3qeq_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3qeq_sqt(u3_atom, u3_atom);
    u3_noun u3qeq_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qeq_lth(u3_atom, u3_atom);
    u3_noun u3qeq_lte(u3_atom, u3_atom);
    u3_noun u3qeq_equ(u3_atom, u3_atom);
    u3_noun u3qeq_gte(u3_atom, u3_atom);
    u3_noun u3qeq_gth(u3_atom, u3_atom);

    u3_noun u3qes_add(u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_sub(u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_mul(u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_div(u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_sqt(u3_atom, u3_atom);
    u3_noun u3qes_fma(u3_atom, u3_atom, u3_atom, u3_atom);
    u3_noun u3qes_lth(u3_atom, u3_atom);
    u3_noun u3qes_lte(u3_atom, u3_atom);
    u3_noun u3qes_equ(u3_atom, u3_atom);
    u3_noun u3qes_gte(u3_atom, u3_atom);
    u3_noun u3qes_gth(u3_atom, u3_atom);

  /** Tier 6.
  **/
    u3_noun u3qf_bull(u3_noun, u3_noun);
    u3_noun u3qf_cell(u3_noun, u3_noun);
    u3_noun u3qf_comb(u3_noun, u3_noun);
    u3_noun u3qf_cons(u3_noun, u3_noun);
    u3_noun u3qf_core(u3_noun, u3_noun);
    u3_noun u3qf_cube(u3_noun, u3_noun);
    u3_noun u3qf_face(u3_noun, u3_noun);
    u3_noun u3qf_fine(u3_noun, u3_noun, u3_noun);
    u3_noun u3qf_fitz(u3_noun, u3_noun);
    u3_noun u3qf_flan(u3_noun, u3_noun);
    u3_noun u3qf_flay(u3_noun);
    u3_noun u3qf_flip(u3_noun);
    u3_noun u3qf_flor(u3_noun, u3_noun);
    u3_noun u3qf_forq(u3_noun, u3_noun);
    u3_noun u3qf_fork(u3_noun);
    u3_noun u3qf_grof(u3_noun);
    u3_noun u3qf_hint(u3_noun, u3_noun);
    u3_noun u3qf_hike(u3_noun, u3_noun);
    u3_noun u3qf_look(u3_noun, u3_noun);
    u3_noun u3qf_loot(u3_noun, u3_noun);
    u3_noun u3qf_slot(u3_atom, u3_noun);
    u3_noun u3qf_type(u3_noun);

    u3_noun u3qfl_bunt(u3_noun, u3_noun);
    u3_noun u3qfl_whip(u3_noun, u3_noun, u3_noun);

    u3_noun u3qfr_fish(u3_noun, u3_noun, u3_noun, u3_noun);

    u3_noun u3qfp_hack(u3_noun, u3_noun);
    u3_noun u3qfp_late(u3_noun);
    u3_noun u3qfp_open(u3_noun, u3_noun, u3_noun);
    u3_noun u3qfp_nepo(u3_noun, u3_noun);
    u3_noun u3qfp_rake(u3_noun);

#   define u3qfu_van_fan  28
#   define u3qfu_van_rib  58
#   define u3qfu_van_vrf  59
#   define u3qfu_van_vet  118
#   define u3qfu_van_fab  119

    void u3qf_test(const c3_c*, u3_noun);
