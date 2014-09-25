/* j/dash.c
**
** This file is in the public domain.
*/
#include "all.h"

static u3_cs_harm _mood__hoon_add_a[] = {{".2", u3_cwa_add, u3_yes}, {}};
static u3_cs_harm _mood__hoon_dec_a[] = {{".2", u3_cwa_dec, u3_yes}, {}};
static u3_cs_harm _mood__hoon_div_a[] = {{".2", u3_cwa_div, u3_yes}, {}};
static u3_cs_harm _mood__hoon_gte_a[] = {{".2", u3_cwa_gte, u3_yes}, {}};
static u3_cs_harm _mood__hoon_gth_a[] = {{".2", u3_cwa_gth, u3_yes}, {}};
static u3_cs_harm _mood__hoon_lte_a[] = {{".2", u3_cwa_lte, u3_yes}, {}};
static u3_cs_harm _mood__hoon_lth_a[] = {{".2", u3_cwa_lth, u3_yes}, {}};
static u3_cs_harm _mood__hoon_mod_a[] = {{".2", u3_cwa_mod, u3_yes}, {}};
static u3_cs_harm _mood__hoon_mul_a[] = {{".2", u3_cwa_mul, u3_yes}, {}};
static u3_cs_harm _mood__hoon_sub_a[] = {{".2", u3_cwa_sub, u3_yes}, {}};

static u3_cs_harm _mood__hoon_bind_a[] = {{".2", u3_cwb_bind, u3_yes}, {}};
static u3_cs_harm _mood__hoon_clap_a[] = {{".2", u3_cwb_clap, u3_yes}, {}};
static u3_cs_harm _mood__hoon_drop_a[] = {{".2", u3_cwb_drop, u3_yes}, {}};
static u3_cs_harm _mood__hoon_flop_a[] = {{".2", u3_cwb_flop, u3_yes}, {}};
static u3_cs_harm _mood__hoon_lent_a[] = {{".2", u3_cwb_lent, u3_yes}, {}};
static u3_cs_harm _mood__hoon_levy_a[] = {{".2", u3_cwb_levy, u3_yes}, {}};
static u3_cs_harm _mood__hoon_lien_a[] = {{".2", u3_cwb_lien, u3_yes}, {}};
static u3_cs_harm _mood__hoon_need_a[] = {{".2", u3_cwb_need, u3_yes}, {}};
static u3_cs_harm _mood__hoon_reel_a[] = {{".2", u3_cwb_reel, u3_yes}, {}};
static u3_cs_harm _mood__hoon_roll_a[] = {{".2", u3_cwb_roll, u3_yes}, {}};
static u3_cs_harm _mood__hoon_skim_a[] = {{".2", u3_cwb_skim, u3_yes}, {}};
static u3_cs_harm _mood__hoon_skip_a[] = {{".2", u3_cwb_skip, u3_yes}, {}};
// static u3_cs_harm _mood__hoon_scag_a[] = {{".2", u3_cwb_scag, u3_yes}, {}};
static u3_cs_harm _mood__hoon_slag_a[] = {{".2", u3_cwb_slag, u3_yes}, {}};
static u3_cs_harm _mood__hoon_snag_a[] = {{".2", u3_cwb_snag, u3_yes}, {}};
// static u3_cs_harm _mood__hoon_sort_a[] = {{".2", u3_cwb_sort, u3_yes}, {}};
static u3_cs_harm _mood__hoon_turn_a[] = {{".2", u3_cwb_turn, u3_yes}, {}};
static u3_cs_harm _mood__hoon_weld_a[] = {{".2", u3_cwb_weld, u3_yes}, {}};

static u3_cs_harm _mood__hoon_bex_a[] = {{".2", u3_cwc_bex, u3_yes}, {}};
static u3_cs_harm _mood__hoon_can_a[] = {{".2", u3_cwc_can, u3_yes}, {}};
static u3_cs_harm _mood__hoon_cap_a[] = {{".2", u3_cwc_cap, u3_yes}, {}};
static u3_cs_harm _mood__hoon_cat_a[] = {{".2", u3_cwc_cat, u3_yes}, {}};
static u3_cs_harm _mood__hoon_con_a[] = {{".2", u3_cwc_con, u3_yes}, {}};
static u3_cs_harm _mood__hoon_cut_a[] = {{".2", u3_cwc_cut, u3_yes}, {}};
static u3_cs_harm _mood__hoon_dis_a[] = {{".2", u3_cwc_dis, u3_yes}, {}};
static u3_cs_harm _mood__hoon_dor_a[] = {{".2", u3_cwc_dor, u3_yes}, {}};
static u3_cs_harm _mood__hoon_end_a[] = {{".2", u3_cwc_end, u3_yes}, {}};
static u3_cs_harm _mood__hoon_gor_a[] = {{".2", u3_cwc_gor, u3_yes}, {}};
static u3_cs_harm _mood__hoon_hor_a[] = {{".2", u3_cwc_hor, u3_yes}, {}};
static u3_cs_harm _mood__hoon_lsh_a[] = {{".2", u3_cwc_lsh, u3_yes}, {}};
static u3_cs_harm _mood__hoon_mas_a[] = {{".2", u3_cwc_mas, u3_yes}, {}};
static u3_cs_harm _mood__hoon_met_a[] = {{".2", u3_cwc_met, u3_yes}, {}};
static u3_cs_harm _mood__hoon_mix_a[] = {{".2", u3_cwc_mix, u3_yes}, {}};
static u3_cs_harm _mood__hoon_mug_a[] = {{".2", u3_cwc_mug, u3_yes}, {}};
static u3_cs_harm _mood__hoon_peg_a[] = {{".2", u3_cwc_peg, u3_yes}, {}};
static u3_cs_harm _mood__hoon_rap_a[] = {{".2", u3_cwc_rap, u3_yes}, {}};
static u3_cs_harm _mood__hoon_rip_a[] = {{".2", u3_cwc_rip, u3_yes}, {}};
static u3_cs_harm _mood__hoon_rsh_a[] = {{".2", u3_cwc_rsh, u3_yes}, {}};
static u3_cs_harm _mood__hoon_vor_a[] = {{".2", u3_cwc_vor, u3_yes}, {}};

static u3_cs_harm _mood__hoon__po_ind_a[] = {{".2", u3_cwcp_ind}, {}};
static u3_cs_harm _mood__hoon__po_ins_a[] = {{".2", u3_cwcp_ins}, {}};
static u3_cs_harm _mood__hoon__po_tod_a[] = {{".2", u3_cwcp_tod}, {}};
static u3_cs_harm _mood__hoon__po_tos_a[] = {{".2", u3_cwcp_tos}, {}};
static u3_cs_core _mood__hoon__po_d[] =
  { { "ind", _mood__hoon__po_ind_a },
    { "ins", _mood__hoon__po_ins_a },
    { "tod", _mood__hoon__po_tod_a },
    { "tos", _mood__hoon__po_tos_a },
    {}
  };

static u3_cs_harm _mood__hoon__by_gas_a[] = {{".2", u3_cwdb_gas, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_get_a[] = {{".2", u3_cwdb_get, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_has_a[] = {{".2", u3_cwdb_has, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_int_a[] = {{".2", u3_cwdb_int, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_put_a[] = {{".2", u3_cwdb_put, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_tap_a[] = {{".2", u3_cwdb_tap, u3_yes}, {}};
static u3_cs_harm _mood__hoon__by_uni_a[] = {{".2", u3_cwdb_uni, u3_yes}, {}};
static u3_cs_core _mood__hoon__by_d[] =
  { { "gas", _mood__hoon__by_gas_a },
    { "get", _mood__hoon__by_get_a },
    { "has", _mood__hoon__by_has_a },
    { "int", _mood__hoon__by_int_a },
    { "put", _mood__hoon__by_put_a },
    { "tap", _mood__hoon__by_tap_a },
    { "uni", _mood__hoon__by_uni_a },
    {}
  };

static u3_cs_harm _mood__hoon__in_gas_a[] = {{".2", u3_cwdi_gas}, {}};
static u3_cs_harm _mood__hoon__in_has_a[] = {{".2", u3_cwdi_has}, {}};
static u3_cs_harm _mood__hoon__in_mer_a[] = {{".2", u3_cwdi_mer}, {}};
static u3_cs_harm _mood__hoon__in_int_a[] = {{".2", u3_cwdi_int}, {}};
static u3_cs_harm _mood__hoon__in_put_a[] = {{".2", u3_cwdi_put}, {}};
static u3_cs_harm _mood__hoon__in_tap_a[] = {{".2", u3_cwdi_tap}, {}};
static u3_cs_harm _mood__hoon__in_uni_a[] = {{".2", u3_cwdi_uni}, {}};
static u3_cs_core _mood__hoon__in_d[] =
  { { "gas", _mood__hoon__in_gas_a },
    { "has", _mood__hoon__in_has_a },
    { "mer", _mood__hoon__in_mer_a },
    { "int", _mood__hoon__in_int_a },
    { "put", _mood__hoon__in_put_a },
    { "tap", _mood__hoon__in_tap_a },
    { "uni", _mood__hoon__in_uni_a },
    {}
  };

static u3_cs_harm _mood__hoon_cue_a[] = {{".2", u3_cwe_cue}, {}};
static u3_cs_harm _mood__hoon_jam_a[] = {{".2", u3_cwe_jam}, {}};
static u3_cs_harm _mood__hoon_mat_a[] = {{".2", u3_cwe_mat}, {}};
static u3_cs_harm _mood__hoon_rub_a[] = {{".2", u3_cwe_rub}, {}};
static u3_cs_harm _mood__hoon_lore_a[] = {{".2", u3_cwe_lore}, {}};
static u3_cs_harm _mood__hoon_loss_a[] = {{".2", u3_cwe_loss}, {}};
static u3_cs_harm _mood__hoon_mink_a[] = {{".2", u3_cwe_mink}, {}};
static u3_cs_harm _mood__hoon_mule_a[] = {{".2", u3_cwe_mule}, {}};
static u3_cs_harm _mood__hoon_repg_a[] = {{".2", u3_cwe_repg}, {}};
static u3_cs_harm _mood__hoon_rexp_a[] = {{".2", u3_cwe_rexp}, {}};
static u3_cs_harm _mood__hoon_trip_a[] = {{".2", u3_cwe_trip}, {}};

static u3_cs_harm _mood__hoon__aesc_en_a[] = {{".2", u3_cwea_en}, {}};
static u3_cs_harm _mood__hoon__aesc_de_a[] = {{".2", u3_cwea_en}, {}};
static u3_cs_core _mood__hoon__aesc_d[] =
  { { "en", _mood__hoon__aesc_en_a },
    { "de", _mood__hoon__aesc_de_a },
    {}
  };

static u3_cs_harm _mood__hoon__bend_fun_a[] = {{".2", u3_cwe_bend_fun}, {}};
static u3_cs_core _mood__hoon__bend_d[] =
  { { "fun", _mood__hoon__bend_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__cold_fun_a[] = {{".2", u3_cwe_cold_fun}, {}};
static u3_cs_core _mood__hoon__cold_d[] =
  { { "fun", _mood__hoon__cold_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__cook_fun_a[] = {{".2", u3_cwe_cook_fun}, {}};
static u3_cs_core _mood__hoon__cook_d[] =
  { { "fun", _mood__hoon__cook_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__comp_fun_a[] = {{".2", u3_cwe_comp_fun}, {}};
static u3_cs_core _mood__hoon__comp_d[] =
  { { "fun", _mood__hoon__comp_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__easy_fun_a[] = {{".2", u3_cwe_easy_fun}, {}};
static u3_cs_core _mood__hoon__easy_d[] =
  { { "fun", _mood__hoon__easy_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__glue_fun_a[] = {{".2", u3_cwe_glue_fun}, {}};
static u3_cs_core _mood__hoon__glue_d[] =
  { { "fun", _mood__hoon__glue_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__here_fun_a[] = {{".2", u3_cwe_here_fun}, {}};
static u3_cs_core _mood__hoon__here_d[] =
  { { "fun", _mood__hoon__here_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__just_fun_a[] = {{".2", u3_cwe_just_fun}, {}};
static u3_cs_core _mood__hoon__just_d[] =
  { { "fun", _mood__hoon__just_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__mask_fun_a[] = {{".2", u3_cwe_mask_fun}, {}};
static u3_cs_core _mood__hoon__mask_d[] =
  { { "fun", _mood__hoon__mask_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__shim_fun_a[] = {{".2", u3_cwe_shim_fun}, {}};
static u3_cs_core _mood__hoon__shim_d[] =
  { { "fun", _mood__hoon__shim_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__stag_fun_a[] = {{".2", u3_cwe_stag_fun}, {}};
static u3_cs_core _mood__hoon__stag_d[] =
  { { "fun", _mood__hoon__stag_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__stew_fun_a[] = {{".2", u3_cwe_stew_fun}, {}};
static u3_cs_core _mood__hoon__stew_d[] =
  { { "fun", _mood__hoon__stew_fun_a },
    {}
  };
static u3_cs_harm _mood__hoon__stir_fun_a[] = {{".2", u3_cwe_stir_fun}, {}};
static u3_cs_core _mood__hoon__stir_d[] =
  { { "fun", _mood__hoon__stir_fun_a },
    {}
  };

static u3_cs_harm _mood__hoon__og_raw_a[] = {{".2", u3_cweo_raw}, {}};
static u3_cs_core _mood__hoon__og_d[] =
  { { "raw", _mood__hoon__og_raw_a },
    {}
  };

static u3_cs_harm _mood__hoon__rd_sun_a[] = {{".2", u3_cwer_sun}, {}};
static u3_cs_harm _mood__hoon__rd_mul_a[] = {{".2", u3_cwer_mul}, {}};
static u3_cs_harm _mood__hoon__rd_div_a[] = {{".2", u3_cwer_div}, {}};
static u3_cs_harm _mood__hoon__rd_add_a[] = {{".2", u3_cwer_add}, {}};
static u3_cs_harm _mood__hoon__rd_sub_a[] = {{".2", u3_cwer_sub}, {}};
static u3_cs_harm _mood__hoon__rd_lte_a[] = {{".2", u3_cwer_lte}, {}};
static u3_cs_harm _mood__hoon__rd_lth_a[] = {{".2", u3_cwer_lth}, {}};
static u3_cs_harm _mood__hoon__rd_gte_a[] = {{".2", u3_cwer_gte}, {}};
static u3_cs_harm _mood__hoon__rd_gth_a[] = {{".2", u3_cwer_gth}, {}};
static u3_cs_core _mood__hoon__rd_d[] =
  { { "sun", _mood__hoon__rd_sun_a },
    { "mul", _mood__hoon__rd_mul_a },
    { "div", _mood__hoon__rd_div_a },
    { "add", _mood__hoon__rd_add_a },
    { "sub", _mood__hoon__rd_sub_a },
    { "lte", _mood__hoon__rd_lte_a },
    { "lth", _mood__hoon__rd_lth_a },
    { "gte", _mood__hoon__rd_gte_a },
    { "gth", _mood__hoon__rd_gth_a },
    {}
  };

static u3_cs_harm _mood__hoon__coed__ed_puck_a[] = {{".2", u3_cwee_puck}, {}};
static u3_cs_harm _mood__hoon__coed__ed_sign_a[] = {{".2", u3_cwee_sign}, {}};
static u3_cs_harm _mood__hoon__coed__ed_veri_a[] = {{".2", u3_cwee_veri}, {}};
static u3_cs_core _mood__hoon__coed__ed_d[] =
  { { "sign", _mood__hoon__coed__ed_sign_a },
    { "puck", _mood__hoon__coed__ed_puck_a },
    { "veri", _mood__hoon__coed__ed_veri_a },
    {}
  };
static u3_cs_core _mood__hoon__coed_d[] =
  { { "ed", 0, _mood__hoon__coed__ed_d },
    {}
  };

static u3_cs_harm _mood__hoon_pfix_a[] = {{".2", u3_cwe_pfix}, {}};
static u3_cs_harm _mood__hoon_plug_a[] = {{".2", u3_cwe_plug}, {}};
static u3_cs_harm _mood__hoon_pose_a[] = {{".2", u3_cwe_pose}, {}};
static u3_cs_harm _mood__hoon_sfix_a[] = {{".2", u3_cwe_sfix}, {}};
static u3_cs_harm _mood__hoon_shax_a[] = {{".2", u3_cwe_shax}, {}};
static u3_cs_harm _mood__hoon_shas_a[] = {{".2", u3_cwe_shas}, {}};
static u3_cs_harm _mood__hoon_shal_a[] = {{".2", u3_cwe_shal}, {}};

static u3_cs_harm _mood__hoon_bull_a[] = {{".2", u3_cwf_bull}, {}};
static u3_cs_harm _mood__hoon_cell_a[] = {{".2", u3_cwf_cell}, {}};
static u3_cs_harm _mood__hoon_comb_a[] = {{".2", u3_cwf_comb}, {}};
static u3_cs_harm _mood__hoon_cons_a[] = {{".2", u3_cwf_cons}, {}};
static u3_cs_harm _mood__hoon_core_a[] = {{".2", u3_cwf_core}, {}};
static u3_cs_harm _mood__hoon_cube_a[] = {{".2", u3_cwf_cube}, {}};
static u3_cs_harm _mood__hoon_face_a[] = {{".2", u3_cwf_face}, {}};
static u3_cs_harm _mood__hoon_fitz_a[] = {{".2", u3_cwf_fitz}, {}};
static u3_cs_harm _mood__hoon_flan_a[] = {{".2", u3_cwf_flan}, {}};
static u3_cs_harm _mood__hoon_flay_a[] = {{".2", u3_cwf_flay}, {}};
static u3_cs_harm _mood__hoon_flip_a[] = {{".2", u3_cwf_flip}, {}};
static u3_cs_harm _mood__hoon_flor_a[] = {{".2", u3_cwf_flor}, {}};
static u3_cs_harm _mood__hoon_fork_a[] = {{".2", u3_cwf_fork}, {}};
static u3_cs_harm _mood__hoon_hike_a[] = {{".2", u3_cwf_hike}, {}};
static u3_cs_harm _mood__hoon_look_a[] = {{".2", u3_cwf_look}, {}};

static u3_cs_harm _mood__hoon__ut_busk_a[] = {{".2", u3_cwfu_busk}, {}};
static u3_cs_harm _mood__hoon__ut_bust_a[] = {{".2", u3_cwfu_bust}, {}};
static u3_cs_harm _mood__hoon__ut_conk_a[] = {{".2", u3_cwfu_conk}, {}};
static u3_cs_harm _mood__hoon__ut_crop_a[] = {{".2", u3_cwfu_crop}, {}};
static u3_cs_harm _mood__hoon__ut_cull_a[] = {{".2", u3_cwfu_cull}, {}};
static u3_cs_harm _mood__hoon__ut_find_a[] = {{".2", u3_cwfu_find}, {}};
static u3_cs_harm _mood__hoon__ut_fino_a[] = {{".2", u3_cwfu_fino}, {}};
static u3_cs_harm _mood__hoon__ut_fink_a[] = {{".2", u3_cwfu_fink}, {}};
static u3_cs_harm _mood__hoon__ut_fire_a[] = {{".2", u3_cwfu_fire}, {}};
static u3_cs_harm _mood__hoon__ut_firm_a[] = {{".2", u3_cwfu_firm}, {}};
static u3_cs_harm _mood__hoon__ut_fish_a[] = {{".2", u3_cwfu_fish}, {}};
static u3_cs_harm _mood__hoon__ut_fuse_a[] = {{".2", u3_cwfu_fuse}, {}};
static u3_cs_harm _mood__hoon__ut_heal_a[] = {{".2", u3_cwfu_heal}, {}};
static u3_cs_harm _mood__hoon__ut_mint_a[] = {{".2", u3_cwfu_mint}, {}};
static u3_cs_harm _mood__hoon__ut_mull_a[] = {{".2", u3_cwfu_mull}, {}};
static u3_cs_harm _mood__hoon__ut_nest_a[] = {{".2", u3_cwfu_nest}, {}};
static u3_cs_harm _mood__hoon__ut_park_a[] = {{".2", u3_cwfu_park}, {}};
static u3_cs_harm _mood__hoon__ut_peek_a[] = {{".2", u3_cwfu_peek}, {}};
static u3_cs_harm _mood__hoon__ut_play_a[] = {{".2", u3_cwfu_play}, {}};
static u3_cs_harm _mood__hoon__ut_rest_a[] = {{".2", u3_cwfu_rest}, {}};
static u3_cs_harm _mood__hoon__ut_seek_a[] = {{".2", u3_cwfu_seek}, {}};
static u3_cs_harm _mood__hoon__ut_seep_a[] = {{".2", u3_cwfu_seep}, {}};
static u3_cs_harm _mood__hoon__ut_snub_a[] = {{".2", u3_cwfu_snub}, {}};
static u3_cs_harm _mood__hoon__ut_tock_a[] = {{".2", u3_cwfu_tock}, {}};
static u3_cs_harm _mood__hoon__ut_wrap_a[] = {{".2", u3_cwfu_wrap}, {}};

static u3_cs_core _mood__hoon__ut_d[] =
  { { "busk", _mood__hoon__ut_busk_a },
    { "bust", _mood__hoon__ut_bust_a },
    { "conk", _mood__hoon__ut_conk_a },
    { "crop", _mood__hoon__ut_crop_a },
    { "cull", _mood__hoon__ut_cull_a },
    { "find", _mood__hoon__ut_find_a },
    { "fino", _mood__hoon__ut_fino_a },
    { "fink", _mood__hoon__ut_fink_a },
    { "fire", _mood__hoon__ut_fire_a },
    { "firm", _mood__hoon__ut_firm_a },
    { "fish", _mood__hoon__ut_fish_a },
    { "fuse", _mood__hoon__ut_fuse_a },
    { "heal", _mood__hoon__ut_heal_a },
    { "mint", _mood__hoon__ut_mint_a },
    { "mull", _mood__hoon__ut_mull_a },
    { "nest", _mood__hoon__ut_nest_a },
    { "park", _mood__hoon__ut_park_a },
    { "peek", _mood__hoon__ut_peek_a },
    { "play", _mood__hoon__ut_play_a },
    { "rest", _mood__hoon__ut_rest_a },
    { "seek", _mood__hoon__ut_seek_a },
    { "seep", _mood__hoon__ut_seep_a },
    { "snub", _mood__hoon__ut_snub_a },
    { "tock", _mood__hoon__ut_tock_a },
    { "wrap", _mood__hoon__ut_wrap_a },
    {}
  };
static u3_cs_harm _mood__hoon__ut_a[] = 
  { {"burn", u3_cwfu_burn},
    {"repo", u3_cwfu_repo},
    {}
  };

static u3_cs_harm _mood__hoon__ap_a[] = 
  { // {"hack", u3_cwfp_open},
    // {"late", u3_cwfp_open},
    {"open", u3_cwfp_open},
    {"rake", u3_cwfp_rake},
    {}
  };

#if 0
static u3_cs_harm _mood__hoon__al_a[] = 
  { {"bunt", u3_cwfl_bunt},
    {"whip", u3_cwfl_whip},
    {}
  };
#endif

static u3_cs_core _mood__hoon_d[] = 
  { { "add", _mood__hoon_add_a },
    { "dec", _mood__hoon_dec_a },
    { "div", _mood__hoon_div_a },
    { "gte", _mood__hoon_gte_a },
    { "gth", _mood__hoon_gth_a },
    { "lte", _mood__hoon_lte_a },
    { "lth", _mood__hoon_lth_a },
    { "mod", _mood__hoon_mod_a },
    { "mul", _mood__hoon_mul_a },
    { "sub", _mood__hoon_sub_a },
#if 1
    { "bind", _mood__hoon_bind_a },
    { "clap", _mood__hoon_clap_a },
    { "drop", _mood__hoon_drop_a },
    { "flop", _mood__hoon_flop_a },
    { "lent", _mood__hoon_lent_a },
    { "levy", _mood__hoon_levy_a },
    { "lien", _mood__hoon_lien_a },
    { "need", _mood__hoon_need_a },
    { "reel", _mood__hoon_reel_a },
    { "roll", _mood__hoon_roll_a },
    { "skim", _mood__hoon_skim_a },
    { "skip", _mood__hoon_skip_a },
//  { "scag", _mood__hoon_scag_a },  // this jet is actually broken
    { "slag", _mood__hoon_slag_a },
    { "snag", _mood__hoon_snag_a },
//  { "sort", _mood__hoon_sort_a },
    { "turn", _mood__hoon_turn_a },
    { "weld", _mood__hoon_weld_a },

    { "bex", _mood__hoon_bex_a },
    { "can", _mood__hoon_can_a },
    { "cap", _mood__hoon_cap_a },
    { "cat", _mood__hoon_cat_a },
    { "con", _mood__hoon_con_a },
    { "cut", _mood__hoon_cut_a },
    { "dis", _mood__hoon_dis_a },
    { "dor", _mood__hoon_dor_a },
    { "end", _mood__hoon_end_a },
    { "gor", _mood__hoon_gor_a },
    { "hor", _mood__hoon_hor_a },
    { "lsh", _mood__hoon_lsh_a },
    { "mas", _mood__hoon_mas_a },
    { "met", _mood__hoon_met_a },
    { "mix", _mood__hoon_mix_a },
    { "mug", _mood__hoon_mug_a },
    { "peg", _mood__hoon_peg_a },
    { "rap", _mood__hoon_rap_a },
    { "rip", _mood__hoon_rip_a },
    { "rsh", _mood__hoon_rsh_a },
    { "vor", _mood__hoon_vor_a },

    { "po", 0, _mood__hoon__po_d },

    { "by", 0, _mood__hoon__by_d },
    { "in", 0, _mood__hoon__in_d },

    { "cue", _mood__hoon_cue_a },
    { "jam", _mood__hoon_jam_a },
    { "mat", _mood__hoon_mat_a },
    { "rub", _mood__hoon_rub_a },
    { "lore", _mood__hoon_lore_a },
    { "loss", _mood__hoon_loss_a },
    { "mink", _mood__hoon_mink_a },
    { "mule", _mood__hoon_mule_a },
    { "repg", _mood__hoon_repg_a },
    { "rexp", _mood__hoon_rexp_a },
    { "trip", _mood__hoon_trip_a },

    { "aesc", 0, _mood__hoon__aesc_d },
    { "bend", 0, _mood__hoon__bend_d },
    { "cold", 0, _mood__hoon__cold_d },
    { "comp", 0, _mood__hoon__comp_d },
    { "cook", 0, _mood__hoon__cook_d },
    { "easy", 0, _mood__hoon__easy_d },
    { "coed", 0, _mood__hoon__coed_d },
    { "glue", 0, _mood__hoon__glue_d },
    { "here", 0, _mood__hoon__here_d },
    { "just", 0, _mood__hoon__just_d },
    { "mask", 0, _mood__hoon__mask_d },
    { "shim", 0, _mood__hoon__shim_d },
    { "stag", 0, _mood__hoon__stag_d },
    { "stew", 0, _mood__hoon__stew_d },
    { "stir", 0, _mood__hoon__stir_d },

    { "og", 0, _mood__hoon__og_d },
    { "rd", 0, _mood__hoon__rd_d },
    { "coed", 0, _mood__hoon__coed_d },

    { "pfix", _mood__hoon_pfix_a },
    { "plug", _mood__hoon_plug_a },
    { "pose", _mood__hoon_pose_a },
    { "sfix", _mood__hoon_sfix_a },

    { "shax", _mood__hoon_shax_a },
    { "shas", _mood__hoon_shas_a },
    { "shal", _mood__hoon_shal_a },

    { "bull", _mood__hoon_bull_a },
    { "cell", _mood__hoon_cell_a },
    { "comb", _mood__hoon_comb_a },
    { "cons", _mood__hoon_cons_a },
    { "core", _mood__hoon_core_a },
    { "cube", _mood__hoon_cube_a },
    { "face", _mood__hoon_face_a },
    { "fitz", _mood__hoon_fitz_a },
    { "flan", _mood__hoon_flan_a },
    { "flay", _mood__hoon_flay_a },
    { "flip", _mood__hoon_flip_a },
    { "flor", _mood__hoon_flor_a },
    { "fork", _mood__hoon_fork_a },
    { "hike", _mood__hoon_hike_a },
    { "look", _mood__hoon_look_a },

    { "ap", _mood__hoon__ap_a },
    // { "al", _mood__hoon__al_a },
    { "ut", _mood__hoon__ut_a, _mood__hoon__ut_d },
#endif
    {}
  };

static u3_cs_core _mood_d[] =
  { { "hoon", 0, _mood__hoon_d },
    {}
  };

static u3_cs_core _k164_d[] =
  { { "mood", 0, _mood_d },
    {}
  };

static u3_cs_core _d[] = {
  { "k164", 0, _k164_d},
  {}
};

u3_cs_dash 
u3_Dash = {
  _d,
  0,
  0
};
