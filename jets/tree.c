/* j/tree.c
**
*/
#include "all.h"

  static u3j_harm _149_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _149_hex_down_d[] =
      { { "mark", _149_hex_down_mark_a },
        {}
      };

  static u3j_harm _149_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _149_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _149_hex_lune_a[] = {{".2", u3we_lune}, {}};

static u3j_core _149_hex_d[] =
  { { "down", 0, _149_hex_down_d },
    { "lore", _149_hex_lore_a },
    { "loss", _149_hex_loss_a },
    { "lune", _149_hex_lune_a },
    {}
  };

/* layer five
*/
  static u3j_harm _149_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static u3j_harm _149_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static u3j_harm _149_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static u3j_harm _149_pen_core_a[] = {{".2", u3wf_core}, {}};
  static u3j_harm _149_pen_face_a[] = {{".2", u3wf_face}, {}};
  static u3j_harm _149_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static u3j_harm _149_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static u3j_harm _149_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static u3j_harm _149_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static u3j_harm _149_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static u3j_harm _149_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static u3j_harm _149_pen_look_a[] = {{".2", u3wf_look}, {}};

    static u3j_harm _149_pen__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
    static u3j_harm _149_pen__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
    static u3j_harm _149_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    // static u3j_harm _149_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _149_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static u3j_harm _149_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static u3j_harm _149_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static u3j_harm _149_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static u3j_harm _149_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static u3j_harm _149_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static u3j_harm _149_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static u3j_harm _149_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static u3j_harm _149_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static u3j_harm _149_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static u3j_harm _149_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
  static u3j_core _149_pen__ut_d[] =
    { 
      { "burn", _149_pen__ut_burn_a },
      { "conk", _149_pen__ut_conk_a },
      { "crop", _149_pen__ut_crop_a },
      { "fond", _149_pen__ut_fond_a },
    //  { "fire", _149_pen__ut_fire_a },
      { "fish", _149_pen__ut_fish_a },
      { "fuse", _149_pen__ut_fuse_a },
      { "mint", _149_pen__ut_mint_a },
      { "mull", _149_pen__ut_mull_a },
      { "nest", _149_pen__ut_nest_a },
      { "peek", _149_pen__ut_peek_a },
      { "play", _149_pen__ut_play_a },
      { "rest", _149_pen__ut_rest_a },
      { "toss", _149_pen__ut_toss_a },
      { "wrap", _149_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _149_pen__ut_a[] = 
    { {"burn", u3wfu_burn},
      {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _149_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _149_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif

static u3j_core _149_pen_d[] =
  { { "hex", 0, _149_hex_d },

    { "cell", _149_pen_cell_a },
    { "comb", _149_pen_comb_a },
    { "cons", _149_pen_cons_a },
    { "core", _149_pen_core_a },
    { "face", _149_pen_face_a },
    { "fitz", _149_pen_fitz_a },
    { "flan", _149_pen_flan_a },
    { "flip", _149_pen_flip_a },
    { "flor", _149_pen_flor_a },
    { "fork", _149_pen_fork_a },
    { "hike", _149_pen_hike_a },
    { "look", _149_pen_look_a },

    // { "ap", _149_pen__ap_a },
    // { "al", _149_pen__al_a },
    { "ut", _149_pen__ut_a, _149_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _149_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _149_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _149_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _149_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _149_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _149_qua__po_d[] =
      { { "ind", _149_qua__po_ind_a },
        { "ins", _149_qua__po_ins_a },
        { "tod", _149_qua__po_tod_a },
        { "tos", _149_qua__po_tos_a },
        {}
      };

  static u3j_harm _149_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _149_qua__bend_d[] =
      { { "fun", _149_qua__bend_fun_a },
        {}
      };

  static u3j_harm _149_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _149_qua__cold_d[] =
      { { "fun", _149_qua__cold_fun_a },
        {}
      };

  static u3j_harm _149_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _149_qua__cook_d[] =
      { { "fun", _149_qua__cook_fun_a },
        {}
      };

  static u3j_harm _149_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _149_qua__comp_d[] =
      { { "fun", _149_qua__comp_fun_a },
        {}
      };

  static u3j_harm _149_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _149_qua__easy_d[] =
      { { "fun", _149_qua__easy_fun_a },
        {}
      };

  static u3j_harm _149_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _149_qua__glue_d[] =
      { { "fun", _149_qua__glue_fun_a },
        {}
      };

  static u3j_harm _149_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _149_qua__here_d[] =
      { { "fun", _149_qua__here_fun_a },
        {}
      };

  static u3j_harm _149_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _149_qua__just_d[] =
      { { "fun", _149_qua__just_fun_a },
        {}
      };

  static u3j_harm _149_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _149_qua__mask_d[] =
      { { "fun", _149_qua__mask_fun_a },
        {}
      };

  static u3j_harm _149_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _149_qua__shim_d[] =
      { { "fun", _149_qua__shim_fun_a },
        {}
      };

  static u3j_harm _149_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _149_qua__stag_d[] =
      { { "fun", _149_qua__stag_fun_a },
        {}
      };

  static u3j_harm _149_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _149_qua__stew_d[] =
      { { "fun", _149_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _149_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _149_qua__stir_d[] =
      { { "fun", _149_qua__stir_fun_a },
        {}
      };

  static u3j_harm _149_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _149_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _149_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _149_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _149_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _149_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _149_qua_d[] =
  { { "pen", 0, _149_pen_d },

    { "po", 0, _149_qua__po_d },

    { "trip", _149_qua_trip_a },

    { "bend", 0, _149_qua__bend_d },
    { "cold", 0, _149_qua__cold_d },
    { "comp", 0, _149_qua__comp_d },
    { "cook", 0, _149_qua__cook_d },
    { "easy", 0, _149_qua__easy_d },
    { "glue", 0, _149_qua__glue_d },
    { "here", 0, _149_qua__here_d },
    { "just", 0, _149_qua__just_d },
    { "mask", 0, _149_qua__mask_d },
    { "shim", 0, _149_qua__shim_d },
    { "stag", 0, _149_qua__stag_d },
    { "stew", 0, _149_qua__stew_d },
    { "stir", 0, _149_qua__stir_d },

    { "pfix", _149_qua_pfix_a },
    { "plug", _149_qua_plug_a },
    { "pose", _149_qua_pose_a },
    { "sfix", _149_qua_sfix_a },

    { "mink", _149_qua_mink_a },
    { "mule", _149_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _149_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _149_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _149_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _149_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _149_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _149_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _149_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _149_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _149_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _149_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _149_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _149_tri__rd_d[] =
    { { "add", _149_tri__rd_add_a },
      { "sub", _149_tri__rd_sub_a },
      { "mul", _149_tri__rd_mul_a },
      { "div", _149_tri__rd_div_a },
      { "sqt", _149_tri__rd_sqt_a },
      { "fma", _149_tri__rd_fma_a },
      { "lth", _149_tri__rd_lth_a },
      { "lte", _149_tri__rd_lte_a },
      { "equ", _149_tri__rd_equ_a },
      { "gte", _149_tri__rd_gte_a },
      { "gth", _149_tri__rd_gth_a },
      {}
    };

    static u3j_harm _149_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _149_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _149_tri__cofl_d[] =
    { { "drg", _149_tri__cofl__drg_a },
      { "lug", _149_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _149_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _149_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _149_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _149_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _149_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _149_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _149_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _149_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _149_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _149_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _149_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _149_tri__rs_d[] =
    { { "add", _149_tri__rs_add_a },
      { "sub", _149_tri__rs_sub_a },
      { "mul", _149_tri__rs_mul_a },
      { "div", _149_tri__rs_div_a },
      { "sqt", _149_tri__rs_sqt_a },
      { "fma", _149_tri__rs_fma_a },
      { "lth", _149_tri__rs_lth_a },
      { "lte", _149_tri__rs_lte_a },
      { "equ", _149_tri__rs_equ_a },
      { "gte", _149_tri__rs_gte_a },
      { "gth", _149_tri__rs_gth_a },
      {}
    };

    static u3j_harm _149_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _149_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _149_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _149_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _149_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _149_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _149_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _149_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _149_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _149_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _149_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _149_tri__rq_d[] =
    { { "add", _149_tri__rq_add_a },
      { "sub", _149_tri__rq_sub_a },
      { "mul", _149_tri__rq_mul_a },
      { "div", _149_tri__rq_div_a },
      { "sqt", _149_tri__rq_sqt_a },
      { "fma", _149_tri__rq_fma_a },
      { "lth", _149_tri__rq_lth_a },
      { "lte", _149_tri__rq_lte_a },
      { "equ", _149_tri__rq_equ_a },
      { "gte", _149_tri__rq_gte_a },
      { "gth", _149_tri__rq_gth_a },
      {}
    };

    static u3j_harm _149_tri__aesc_en_a[] = {{".2", u3wea_en}, {}};
    static u3j_harm _149_tri__aesc_de_a[] = {{".2", u3wea_de}, {}};
  static u3j_core _149_tri__aesc_d[] =
    { { "en", _149_tri__aesc_en_a },
      { "de", _149_tri__aesc_de_a },
      {}
    };

    static u3j_harm _149_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _149_tri__og_d[] =
    { { "raw", _149_tri__og_raw_a },
      {}
    };

  static u3j_harm _149_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _149_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _149_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _149_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _149_tri_d[] =
  { { "qua", 0, _149_qua_d },

    { "cofl", 0, _149_tri__cofl_d },
    { "rd", 0, _149_tri__rd_d },
    { "rs", 0, _149_tri__rs_d },
    { "rq", 0, _149_tri__rq_d },
    { "og", 0, _149_tri__og_d },
    { "shax", _149_tri_shax_a },
    { "shay", _149_tri_shay_a },
    { "shas", _149_tri_shas_a },
    { "shal", _149_tri_shal_a },
    { "aesc", 0, _149_tri__aesc_d },

    {}
  };

/* layer two
*/
  static u3j_harm _149_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _149_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _149_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _149_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _149_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _149_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _149_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _149_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _149_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _149_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _149_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _149_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _149_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _149_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _149_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _149_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _149_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _149_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _149_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _149_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static u3j_harm _149_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _149_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _149_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _149_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _149_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _149_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _149_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _149_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _149_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};

  static u3j_harm _149_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _149_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _149_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};

  static u3j_harm _149_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};

  static u3j_harm _149_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _149_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _149_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _149_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};

  static u3j_harm _149_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _149_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};

    static u3j_harm _149_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _149_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _149_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _149_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _149_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _149_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _149_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _149_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _149_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _149_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _149_two__in_d[] =
    { { "bif", _149_two__in_bif_a },
      { "dif", _149_two__in_dif_a },
      { "gas", _149_two__in_gas_a },
      { "has", _149_two__in_has_a },
      { "mer", _149_two__in_mer_a },
      // { "int", _149_two__in_int_a },
      { "put", _149_two__in_put_a },
      { "tap", _149_two__in_tap_a },
      { "wyt", _149_two__in_wyt_a },
      { "uni", _149_two__in_uni_a },
      {}
    };

    static u3j_harm _149_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _149_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _149_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _149_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _149_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _149_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _149_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _149_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _149_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _149_two__by_d[] =
    { { "bif", _149_two__by_bif_a },
      { "dif", _149_two__by_dif_a },
      { "gas", _149_two__by_gas_a },
      { "get", _149_two__by_get_a },
      { "has", _149_two__by_has_a },
      // { "int", _149_two__by_int_a },
      { "put", _149_two__by_put_a },
      { "tap", _149_two__by_tap_a },
      // { "uni", _149_two__by_uni_a },
      {}
    };

  static u3j_harm _149_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _149_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _149_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _149_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _149_two_d[] =
  { { "tri", 0, _149_tri_d },
    { "flop", _149_two_flop_a },
    { "lent", _149_two_lent_a },
    { "levy", _149_two_levy_a },
    { "lien", _149_two_lien_a },
    { "murn", _149_two_murn_a },
    { "need", _149_two_need_a },
    { "reap", _149_two_reap_a },
    { "reel", _149_two_reel_a },
    { "roll", _149_two_roll_a },
    { "skid", _149_two_skid_a },
    { "skim", _149_two_skim_a },
    { "skip", _149_two_skip_a },
    { "scag", _149_two_scag_a },
    { "slag", _149_two_slag_a },
    { "snag", _149_two_snag_a },
//  { "sort", _149_two_sort_a },
    { "turn", _149_two_turn_a },
    { "weld", _149_two_weld_a },

    { "bex", _149_two_bex_a },
    { "xeb", _149_two_xeb_a },
    { "can", _149_two_can_a },
    { "cat", _149_two_cat_a },
    { "cut", _149_two_cut_a },
    { "end", _149_two_end_a },
    { "lsh", _149_two_lsh_a },
    { "met", _149_two_met_a },
    { "rap", _149_two_rap_a },
    { "rip", _149_two_rip_a },
    { "rsh", _149_two_rsh_a },

    { "con", _149_two_con_a },
    { "dis", _149_two_dis_a },
    { "mix", _149_two_mix_a },

    { "mug", _149_two_mug_a },

    { "dor", _149_two_dor_a },
    { "gor", _149_two_gor_a },
    { "hor", _149_two_hor_a },
    { "vor", _149_two_vor_a },

    { "pow", _149_two_pow_a },
    { "sqt", _149_two_sqt_a },

    { "by", 0, _149_two__by_d },
    { "in", 0, _149_two__in_d },

    { "cue", _149_two_cue_a },
    { "jam", _149_two_jam_a },
    { "mat", _149_two_mat_a },
    { "rub", _149_two_rub_a },
  };


/* layer one
*/
  static u3j_harm _149_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _149_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _149_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _149_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _149_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _149_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _149_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _149_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _149_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _149_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _149_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _149_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _149_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _149_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _149_one_d[] =
  { { "two", 0, _149_two_d },
    { "add", _149_one_add_a },
    { "dec", _149_one_dec_a },
    { "div", _149_one_div_a },
    { "dvr", _149_one_dvr_a },
    { "gte", _149_one_gte_a },
    { "gth", _149_one_gth_a },
    { "lte", _149_one_lte_a },
    { "lth", _149_one_lth_a },
    { "mod", _149_one_mod_a },
    { "mul", _149_one_mul_a },
    { "sub", _149_one_sub_a },

    { "cap", _149_one_cap_a },
    { "mas", _149_one_mas_a },
    { "peg", _149_one_peg_a },

    {}
  };
static u3j_core _k149_d[] =
  { { "one", 0, _149_one_d },
    {}
  };

/*************** 150*/

  static u3j_harm _150_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _150_hex_down_d[] =
      { { "mark", _150_hex_down_mark_a },
        {}
      };

  static u3j_harm _150_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _150_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _150_hex_lune_a[] = {{".2", u3we_lune}, {}};

static u3j_core _150_hex_d[] =
  { { "down", 0, _150_hex_down_d },
    { "lore", _150_hex_lore_a },
    { "loss", _150_hex_loss_a },
    { "lune", _150_hex_lune_a },
    {}
  };

/* layer five
*/
  static u3j_harm _150_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static u3j_harm _150_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static u3j_harm _150_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static u3j_harm _150_pen_core_a[] = {{".2", u3wf_core}, {}};
  static u3j_harm _150_pen_face_a[] = {{".2", u3wf_face}, {}};
  static u3j_harm _150_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static u3j_harm _150_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static u3j_harm _150_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static u3j_harm _150_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static u3j_harm _150_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static u3j_harm _150_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static u3j_harm _150_pen_look_a[] = {{".2", u3wf_look}, {}};

    static u3j_harm _150_pen__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
    static u3j_harm _150_pen__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
    static u3j_harm _150_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    // static u3j_harm _150_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _150_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static u3j_harm _150_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static u3j_harm _150_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static u3j_harm _150_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static u3j_harm _150_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static u3j_harm _150_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static u3j_harm _150_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static u3j_harm _150_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static u3j_harm _150_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static u3j_harm _150_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static u3j_harm _150_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
  static u3j_core _150_pen__ut_d[] =
    { 
      { "burn", _150_pen__ut_burn_a },
      { "conk", _150_pen__ut_conk_a },
      { "crop", _150_pen__ut_crop_a },
      { "fond", _150_pen__ut_fond_a },
    //  { "fire", _150_pen__ut_fire_a },
      { "fish", _150_pen__ut_fish_a },
      { "fuse", _150_pen__ut_fuse_a },
      { "mint", _150_pen__ut_mint_a },
      { "mull", _150_pen__ut_mull_a },
      { "nest", _150_pen__ut_nest_a },
      { "peek", _150_pen__ut_peek_a },
      { "play", _150_pen__ut_play_a },
      { "rest", _150_pen__ut_rest_a },
      { "toss", _150_pen__ut_toss_a },
      { "wrap", _150_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _150_pen__ut_a[] = 
    { {"burn", u3wfu_burn},
      {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _150_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _150_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif

static u3j_core _150_pen_d[] =
  { { "hex", 0, _150_hex_d },

    { "cell", _150_pen_cell_a },
    { "comb", _150_pen_comb_a },
    { "cons", _150_pen_cons_a },
    { "core", _150_pen_core_a },
    { "face", _150_pen_face_a },
    { "fitz", _150_pen_fitz_a },
    { "flan", _150_pen_flan_a },
    { "flip", _150_pen_flip_a },
    { "flor", _150_pen_flor_a },
    { "fork", _150_pen_fork_a },
    { "hike", _150_pen_hike_a },
    { "look", _150_pen_look_a },

    // { "ap", _150_pen__ap_a },
    // { "al", _150_pen__al_a },
    { "ut", _150_pen__ut_a, _150_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _150_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _150_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _150_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _150_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _150_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _150_qua__po_d[] =
      { { "ind", _150_qua__po_ind_a },
        { "ins", _150_qua__po_ins_a },
        { "tod", _150_qua__po_tod_a },
        { "tos", _150_qua__po_tos_a },
        {}
      };

  static u3j_harm _150_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _150_qua__bend_d[] =
      { { "fun", _150_qua__bend_fun_a },
        {}
      };

  static u3j_harm _150_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _150_qua__cold_d[] =
      { { "fun", _150_qua__cold_fun_a },
        {}
      };

  static u3j_harm _150_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _150_qua__cook_d[] =
      { { "fun", _150_qua__cook_fun_a },
        {}
      };

  static u3j_harm _150_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _150_qua__comp_d[] =
      { { "fun", _150_qua__comp_fun_a },
        {}
      };

  static u3j_harm _150_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _150_qua__easy_d[] =
      { { "fun", _150_qua__easy_fun_a },
        {}
      };

  static u3j_harm _150_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _150_qua__glue_d[] =
      { { "fun", _150_qua__glue_fun_a },
        {}
      };

  static u3j_harm _150_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _150_qua__here_d[] =
      { { "fun", _150_qua__here_fun_a },
        {}
      };

  static u3j_harm _150_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _150_qua__just_d[] =
      { { "fun", _150_qua__just_fun_a },
        {}
      };

  static u3j_harm _150_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _150_qua__mask_d[] =
      { { "fun", _150_qua__mask_fun_a },
        {}
      };

  static u3j_harm _150_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _150_qua__shim_d[] =
      { { "fun", _150_qua__shim_fun_a },
        {}
      };

  static u3j_harm _150_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _150_qua__stag_d[] =
      { { "fun", _150_qua__stag_fun_a },
        {}
      };

  static u3j_harm _150_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _150_qua__stew_d[] =
      { { "fun", _150_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _150_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _150_qua__stir_d[] =
      { { "fun", _150_qua__stir_fun_a },
        {}
      };

  static u3j_harm _150_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _150_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _150_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _150_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _150_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _150_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _150_qua_d[] =
  { { "pen", 0, _150_pen_d },

    { "po", 0, _150_qua__po_d },

    { "trip", _150_qua_trip_a },

    { "bend", 0, _150_qua__bend_d },
    { "cold", 0, _150_qua__cold_d },
    { "comp", 0, _150_qua__comp_d },
    { "cook", 0, _150_qua__cook_d },
    { "easy", 0, _150_qua__easy_d },
    { "glue", 0, _150_qua__glue_d },
    { "here", 0, _150_qua__here_d },
    { "just", 0, _150_qua__just_d },
    { "mask", 0, _150_qua__mask_d },
    { "shim", 0, _150_qua__shim_d },
    { "stag", 0, _150_qua__stag_d },
    { "stew", 0, _150_qua__stew_d },
    { "stir", 0, _150_qua__stir_d },

    { "pfix", _150_qua_pfix_a },
    { "plug", _150_qua_plug_a },
    { "pose", _150_qua_pose_a },
    { "sfix", _150_qua_sfix_a },

    { "mink", _150_qua_mink_a },
    { "mule", _150_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _150_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _150_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _150_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _150_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _150_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _150_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _150_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _150_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _150_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _150_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _150_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _150_tri__rd_d[] =
    { { "add", _150_tri__rd_add_a },
      { "sub", _150_tri__rd_sub_a },
      { "mul", _150_tri__rd_mul_a },
      { "div", _150_tri__rd_div_a },
      { "sqt", _150_tri__rd_sqt_a },
      { "fma", _150_tri__rd_fma_a },
      { "lth", _150_tri__rd_lth_a },
      { "lte", _150_tri__rd_lte_a },
      { "equ", _150_tri__rd_equ_a },
      { "gte", _150_tri__rd_gte_a },
      { "gth", _150_tri__rd_gth_a },
      {}
    };

    static u3j_harm _150_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _150_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _150_tri__cofl_d[] =
    { { "drg", _150_tri__cofl__drg_a },
      { "lug", _150_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _150_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _150_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _150_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _150_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _150_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _150_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _150_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _150_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _150_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _150_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _150_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _150_tri__rs_d[] =
    { { "add", _150_tri__rs_add_a },
      { "sub", _150_tri__rs_sub_a },
      { "mul", _150_tri__rs_mul_a },
      { "div", _150_tri__rs_div_a },
      { "sqt", _150_tri__rs_sqt_a },
      { "fma", _150_tri__rs_fma_a },
      { "lth", _150_tri__rs_lth_a },
      { "lte", _150_tri__rs_lte_a },
      { "equ", _150_tri__rs_equ_a },
      { "gte", _150_tri__rs_gte_a },
      { "gth", _150_tri__rs_gth_a },
      {}
    };

    static u3j_harm _150_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _150_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _150_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _150_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _150_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _150_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _150_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _150_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _150_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _150_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _150_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _150_tri__rq_d[] =
    { { "add", _150_tri__rq_add_a },
      { "sub", _150_tri__rq_sub_a },
      { "mul", _150_tri__rq_mul_a },
      { "div", _150_tri__rq_div_a },
      { "sqt", _150_tri__rq_sqt_a },
      { "fma", _150_tri__rq_fma_a },
      { "lth", _150_tri__rq_lth_a },
      { "lte", _150_tri__rq_lte_a },
      { "equ", _150_tri__rq_equ_a },
      { "gte", _150_tri__rq_gte_a },
      { "gth", _150_tri__rq_gth_a },
      {}
    };

    static u3j_harm _150_tri__aesc_en_a[] = {{".2", u3wea_en}, {}};
    static u3j_harm _150_tri__aesc_de_a[] = {{".2", u3wea_de}, {}};
  static u3j_core _150_tri__aesc_d[] =
    { { "en", _150_tri__aesc_en_a },
      { "de", _150_tri__aesc_de_a },
      {}
    };

    static u3j_harm _150_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _150_tri__og_d[] =
    { { "raw", _150_tri__og_raw_a },
      {}
    };

  static u3j_harm _150_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _150_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _150_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _150_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _150_tri_d[] =
  { { "qua", 0, _150_qua_d },

    { "cofl", 0, _150_tri__cofl_d },
    { "rd", 0, _150_tri__rd_d },
    { "rs", 0, _150_tri__rs_d },
    { "rq", 0, _150_tri__rq_d },
    { "og", 0, _150_tri__og_d },
    { "shax", _150_tri_shax_a },
    { "shay", _150_tri_shay_a },
    { "shas", _150_tri_shas_a },
    { "shal", _150_tri_shal_a },
    { "aesc", 0, _150_tri__aesc_d },

    {}
  };

/* layer two
*/
  static u3j_harm _150_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _150_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _150_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _150_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _150_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _150_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _150_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _150_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _150_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _150_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _150_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _150_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _150_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _150_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _150_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _150_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _150_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _150_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _150_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _150_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static u3j_harm _150_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _150_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _150_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _150_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _150_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _150_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _150_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _150_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _150_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};

  static u3j_harm _150_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _150_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _150_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};

  static u3j_harm _150_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};

  static u3j_harm _150_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _150_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _150_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _150_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};

  static u3j_harm _150_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _150_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};

    static u3j_harm _150_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _150_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _150_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _150_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _150_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _150_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _150_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _150_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _150_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _150_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _150_two__in_d[] =
    { { "bif", _150_two__in_bif_a },
      { "dif", _150_two__in_dif_a },
      { "gas", _150_two__in_gas_a },
      { "has", _150_two__in_has_a },
      { "mer", _150_two__in_mer_a },
      // { "int", _150_two__in_int_a },
      { "put", _150_two__in_put_a },
      { "tap", _150_two__in_tap_a },
      { "wyt", _150_two__in_wyt_a },
      { "uni", _150_two__in_uni_a },
      {}
    };

    static u3j_harm _150_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _150_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _150_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _150_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _150_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _150_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _150_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _150_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _150_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _150_two__by_d[] =
    { { "bif", _150_two__by_bif_a },
      { "dif", _150_two__by_dif_a },
      { "gas", _150_two__by_gas_a },
      { "get", _150_two__by_get_a },
      { "has", _150_two__by_has_a },
      // { "int", _150_two__by_int_a },
      { "put", _150_two__by_put_a },
      { "tap", _150_two__by_tap_a },
      // { "uni", _150_two__by_uni_a },
      {}
    };

  static u3j_harm _150_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _150_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _150_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _150_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _150_two_d[] =
  { { "tri", 0, _150_tri_d },
    { "flop", _150_two_flop_a },
    { "lent", _150_two_lent_a },
    { "levy", _150_two_levy_a },
    { "lien", _150_two_lien_a },
    { "murn", _150_two_murn_a },
    { "need", _150_two_need_a },
    { "reap", _150_two_reap_a },
    { "reel", _150_two_reel_a },
    { "roll", _150_two_roll_a },
    { "skid", _150_two_skid_a },
    { "skim", _150_two_skim_a },
    { "skip", _150_two_skip_a },
    { "scag", _150_two_scag_a },
    { "slag", _150_two_slag_a },
    { "snag", _150_two_snag_a },
//  { "sort", _150_two_sort_a },
    { "turn", _150_two_turn_a },
    { "weld", _150_two_weld_a },

    { "bex", _150_two_bex_a },
    { "xeb", _150_two_xeb_a },
    { "can", _150_two_can_a },
    { "cat", _150_two_cat_a },
    { "cut", _150_two_cut_a },
    { "end", _150_two_end_a },
    { "lsh", _150_two_lsh_a },
    { "met", _150_two_met_a },
    { "rap", _150_two_rap_a },
    { "rip", _150_two_rip_a },
    { "rsh", _150_two_rsh_a },

    { "con", _150_two_con_a },
    { "dis", _150_two_dis_a },
    { "mix", _150_two_mix_a },

    { "mug", _150_two_mug_a },

    { "dor", _150_two_dor_a },
    { "gor", _150_two_gor_a },
    { "hor", _150_two_hor_a },
    { "vor", _150_two_vor_a },

    { "pow", _150_two_pow_a },
    { "sqt", _150_two_sqt_a },

    { "by", 0, _150_two__by_d },
    { "in", 0, _150_two__in_d },

    { "cue", _150_two_cue_a },
    { "jam", _150_two_jam_a },
    { "mat", _150_two_mat_a },
    { "rub", _150_two_rub_a },
  };


/* layer one
*/
  static u3j_harm _150_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _150_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _150_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _150_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _150_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _150_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _150_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _150_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _150_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _150_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _150_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _150_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _150_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _150_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _150_one_d[] =
  { { "two", 0, _150_two_d },
    { "add", _150_one_add_a },
    { "dec", _150_one_dec_a },
    { "div", _150_one_div_a },
    { "dvr", _150_one_dvr_a },
    { "gte", _150_one_gte_a },
    { "gth", _150_one_gth_a },
    { "lte", _150_one_lte_a },
    { "lth", _150_one_lth_a },
    { "mod", _150_one_mod_a },
    { "mul", _150_one_mul_a },
    { "sub", _150_one_sub_a },

    { "cap", _150_one_cap_a },
    { "mas", _150_one_mas_a },
    { "peg", _150_one_peg_a },

    {}
  };
static u3j_core _k150_d[] =
  { { "one", 0, _150_one_d },
    {}
  };


static u3j_core _d[] = {
  { "k149", 0, _k149_d},
  { "k150", 0, _k150_d},
  {}
};

u3j_dash 
u3j_Dash = {
  _d,
  0,
  0
};
