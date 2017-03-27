/* j/tree.c
*/
#include "all.h"

  static u3j_harm _145_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _145_hex_down_d[] =
      { { "mark", _145_hex_down_mark_a },
        {}
      };

  static u3j_harm _145_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _145_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _145_hex_lune_a[] = {{".2", u3we_lune}, {}};

static u3j_core _145_hex_d[] =
  { { "down", 0, _145_hex_down_d },
    { "lore", _145_hex_lore_a },
    { "loss", _145_hex_loss_a },
    { "lune", _145_hex_lune_a },
    {}
  };

/* layer five
*/
  static u3j_harm _145_pen_cell_a[] = {{".2", u3wz_cell}, {}};
  static u3j_harm _145_pen_comb_a[] = {{".2", u3wz_comb}, {}};
  static u3j_harm _145_pen_cons_a[] = {{".2", u3wz_cons}, {}};
  static u3j_harm _145_pen_core_a[] = {{".2", u3wz_core}, {}};
  static u3j_harm _145_pen_help_a[] = {{".2", u3wz_help}, {}};
  static u3j_harm _145_pen_face_a[] = {{".2", u3wz_face}, {}};
  static u3j_harm _145_pen_fitz_a[] = {{".2", u3wz_fitz}, {}};
  static u3j_harm _145_pen_flan_a[] = {{".2", u3wz_flan}, {}};
  static u3j_harm _145_pen_flip_a[] = {{".2", u3wz_flip}, {}};
  static u3j_harm _145_pen_flor_a[] = {{".2", u3wz_flor}, {}};
  static u3j_harm _145_pen_fork_a[] = {{".2", u3wz_fork}, {}};
  static u3j_harm _145_pen_hike_a[] = {{".2", u3wz_hike}, {}};
  static u3j_harm _145_pen_look_a[] = {{".2", u3wz_look}, {}};
  static u3j_harm _145_pen_loot_a[] = {{".2", u3wz_loot}, {}};

    static u3j_harm _145_pen__ut_burn_a[] = {{".2", u3wzu_burn}, {}};
    static u3j_harm _145_pen__ut_conk_a[] = {{".2", u3wzu_conk}, {}};
    static u3j_harm _145_pen__ut_crop_a[] = {{".2", u3wzu_crop}, {}};
    // static u3j_harm _145_pen__ut_fire_a[] = {{".2", u3wzu_fire}, {}};
    static u3j_harm _145_pen__ut_fond_a[] = {{".2", u3wzu_fond}, {}};
    static u3j_harm _145_pen__ut_fish_a[] = {{".2", u3wzu_fish}, {}};
    static u3j_harm _145_pen__ut_fuse_a[] = {{".2", u3wzu_fuse}, {}};
    static u3j_harm _145_pen__ut_mint_a[] = {{".2", u3wzu_mint}, {}};
    static u3j_harm _145_pen__ut_mull_a[] = {{".2", u3wzu_mull}, {}};
    static u3j_harm _145_pen__ut_nest_a[] = {{".2", u3wzu_nest}, {}};
    static u3j_harm _145_pen__ut_peek_a[] = {{".2", u3wzu_peek}, {}};
    static u3j_harm _145_pen__ut_play_a[] = {{".2", u3wzu_play}, {}};
    static u3j_harm _145_pen__ut_rest_a[] = {{".2", u3wzu_rest}, {}};
    static u3j_harm _145_pen__ut_toss_a[] = {{".2", u3wzu_toss}, {}};
    static u3j_harm _145_pen__ut_wrap_a[] = {{".2", u3wzu_wrap}, {}};
  static u3j_core _145_pen__ut_d[] =
    { 
      { "burn", _145_pen__ut_burn_a },
      { "conk", _145_pen__ut_conk_a },
      { "crop", _145_pen__ut_crop_a },
      { "fond", _145_pen__ut_fond_a },
    //  { "fire", _145_pen__ut_fire_a },
      { "fish", _145_pen__ut_fish_a },
      { "fuse", _145_pen__ut_fuse_a },
      { "mint", _145_pen__ut_mint_a },
      { "mull", _145_pen__ut_mull_a },
      { "nest", _145_pen__ut_nest_a },
      { "peek", _145_pen__ut_peek_a },
      { "play", _145_pen__ut_play_a },
      { "rest", _145_pen__ut_rest_a },
      { "toss", _145_pen__ut_toss_a },
      { "wrap", _145_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _145_pen__ut_a[] = 
    { {"burn", u3wfu_burn},
      {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _145_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _145_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif

static u3j_core _145_pen_d[] =
  { { "hex", 0, _145_hex_d },

    { "cell", _145_pen_cell_a },
    { "comb", _145_pen_comb_a },
    { "cons", _145_pen_cons_a },
    { "core", _145_pen_core_a },
    { "face", _145_pen_face_a },
    { "fitz", _145_pen_fitz_a },
    { "flan", _145_pen_flan_a },
    { "flip", _145_pen_flip_a },
    { "flor", _145_pen_flor_a },
    { "fork", _145_pen_fork_a },
    { "help", _145_pen_help_a },
    { "hike", _145_pen_hike_a },
    { "look", _145_pen_look_a },
    { "loot", _145_pen_loot_a },

    // { "ap", _145_pen__ap_a },
    // { "al", _145_pen__al_a },
    { "ut", _145_pen__ut_a, _145_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _145_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _145_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _145_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _145_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _145_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _145_qua__po_d[] =
      { { "ind", _145_qua__po_ind_a },
        { "ins", _145_qua__po_ins_a },
        { "tod", _145_qua__po_tod_a },
        { "tos", _145_qua__po_tos_a },
        {}
      };

  static u3j_harm _145_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _145_qua__bend_d[] =
      { { "fun", _145_qua__bend_fun_a },
        {}
      };

  static u3j_harm _145_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _145_qua__cold_d[] =
      { { "fun", _145_qua__cold_fun_a },
        {}
      };

  static u3j_harm _145_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _145_qua__cook_d[] =
      { { "fun", _145_qua__cook_fun_a },
        {}
      };

  static u3j_harm _145_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _145_qua__comp_d[] =
      { { "fun", _145_qua__comp_fun_a },
        {}
      };

  static u3j_harm _145_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _145_qua__easy_d[] =
      { { "fun", _145_qua__easy_fun_a },
        {}
      };

  static u3j_harm _145_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _145_qua__glue_d[] =
      { { "fun", _145_qua__glue_fun_a },
        {}
      };

  static u3j_harm _145_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _145_qua__here_d[] =
      { { "fun", _145_qua__here_fun_a },
        {}
      };

  static u3j_harm _145_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _145_qua__just_d[] =
      { { "fun", _145_qua__just_fun_a },
        {}
      };

  static u3j_harm _145_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _145_qua__mask_d[] =
      { { "fun", _145_qua__mask_fun_a },
        {}
      };

  static u3j_harm _145_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _145_qua__shim_d[] =
      { { "fun", _145_qua__shim_fun_a },
        {}
      };

  static u3j_harm _145_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _145_qua__stag_d[] =
      { { "fun", _145_qua__stag_fun_a },
        {}
      };

  static u3j_harm _145_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _145_qua__stew_d[] =
      { { "fun", _145_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _145_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _145_qua__stir_d[] =
      { { "fun", _145_qua__stir_fun_a },
        {}
      };

  static u3j_harm _145_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _145_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _145_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _145_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _145_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _145_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _145_qua_d[] =
  { { "pen", 0, _145_pen_d },

    { "po", 0, _145_qua__po_d },

    { "trip", _145_qua_trip_a },

    { "bend", 0, _145_qua__bend_d },
    { "cold", 0, _145_qua__cold_d },
    { "comp", 0, _145_qua__comp_d },
    { "cook", 0, _145_qua__cook_d },
    { "easy", 0, _145_qua__easy_d },
    { "glue", 0, _145_qua__glue_d },
    { "here", 0, _145_qua__here_d },
    { "just", 0, _145_qua__just_d },
    { "mask", 0, _145_qua__mask_d },
    { "shim", 0, _145_qua__shim_d },
    { "stag", 0, _145_qua__stag_d },
    { "stew", 0, _145_qua__stew_d },
    { "stir", 0, _145_qua__stir_d },

    { "pfix", _145_qua_pfix_a },
    { "plug", _145_qua_plug_a },
    { "pose", _145_qua_pose_a },
    { "sfix", _145_qua_sfix_a },

    { "mink", _145_qua_mink_a },
    { "mule", _145_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _145_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _145_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _145_tri__cofl_d[] =
    { { "drg", _145_tri__cofl__drg_a },
      { "lug", _145_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _145_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _145_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _145_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _145_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _145_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _145_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _145_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _145_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _145_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _145_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _145_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _145_tri__rd_d[] =
    { { "add", _145_tri__rd_add_a },
      { "sub", _145_tri__rd_sub_a },
      { "mul", _145_tri__rd_mul_a },
      { "div", _145_tri__rd_div_a },
      { "sqt", _145_tri__rd_sqt_a },
      { "fma", _145_tri__rd_fma_a },
      { "lth", _145_tri__rd_lth_a },
      { "lte", _145_tri__rd_lte_a },
      { "equ", _145_tri__rd_equ_a },
      { "gte", _145_tri__rd_gte_a },
      { "gth", _145_tri__rd_gth_a },
      {}
    };
    static u3j_harm _145_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _145_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _145_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _145_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _145_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _145_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _145_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _145_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _145_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _145_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _145_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _145_tri__rs_d[] =
    { { "add", _145_tri__rs_add_a },
      { "sub", _145_tri__rs_sub_a },
      { "mul", _145_tri__rs_mul_a },
      { "div", _145_tri__rs_div_a },
      { "sqt", _145_tri__rs_sqt_a },
      { "fma", _145_tri__rs_fma_a },
      { "lth", _145_tri__rs_lth_a },
      { "lte", _145_tri__rs_lte_a },
      { "equ", _145_tri__rs_equ_a },
      { "gte", _145_tri__rs_gte_a },
      { "gth", _145_tri__rs_gth_a },
      {}
    };

    static u3j_harm _145_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _145_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _145_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _145_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _145_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _145_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _145_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _145_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _145_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _145_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _145_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _145_tri__rq_d[] =
    { { "add", _145_tri__rq_add_a },
      { "sub", _145_tri__rq_sub_a },
      { "mul", _145_tri__rq_mul_a },
      { "div", _145_tri__rq_div_a },
      { "sqt", _145_tri__rq_sqt_a },
      { "fma", _145_tri__rq_fma_a },
      { "lth", _145_tri__rq_lth_a },
      { "lte", _145_tri__rq_lte_a },
      { "equ", _145_tri__rq_equ_a },
      { "gte", _145_tri__rq_gte_a },
      { "gth", _145_tri__rq_gth_a },
      {}
    };

    static u3j_harm _145_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static u3j_harm _145_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static u3j_harm _145_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static u3j_harm _145_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static u3j_harm _145_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static u3j_harm _145_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static u3j_harm _145_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static u3j_harm _145_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static u3j_harm _145_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static u3j_harm _145_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static u3j_harm _145_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static u3j_core _145_tri__rh_d[] =
    { { "add", _145_tri__rh_add_a },
      { "sub", _145_tri__rh_sub_a },
      { "mul", _145_tri__rh_mul_a },
      { "div", _145_tri__rh_div_a },
      { "sqt", _145_tri__rh_sqt_a },
      { "fma", _145_tri__rh_fma_a },
      { "lth", _145_tri__rh_lth_a },
      { "lte", _145_tri__rh_lte_a },
      { "equ", _145_tri__rh_equ_a },
      { "gte", _145_tri__rh_gte_a },
      { "gth", _145_tri__rh_gth_a },
      {}
    };

    static u3j_harm _145_tri__aesc_en_a[] = {{".2", u3wea_en}, {}};
    static u3j_harm _145_tri__aesc_de_a[] = {{".2", u3wea_de}, {}};
  static u3j_core _145_tri__aesc_d[] =
    { { "en", _145_tri__aesc_en_a },
      { "de", _145_tri__aesc_de_a },
      {}
    };

    static u3j_harm _145_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _145_tri__og_d[] =
    { { "raw", _145_tri__og_raw_a },
      {}
    };

  static u3j_harm _145_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _145_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _145_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _145_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _145_tri_d[] =
  { { "qua", 0, _145_qua_d },

    { "cofl", 0, _145_tri__cofl_d },
    { "rd", 0, _145_tri__rd_d },
    { "rs", 0, _145_tri__rs_d },
    { "rq", 0, _145_tri__rq_d },
    { "rh", 0, _145_tri__rh_d },
    { "og", 0, _145_tri__og_d },
    { "shax", _145_tri_shax_a },
    { "shay", _145_tri_shay_a },
    { "shas", _145_tri_shas_a },
    { "shal", _145_tri_shal_a },
    { "aesc", 0, _145_tri__aesc_d },

    {}
  };

/* layer two
*/
  static u3j_harm _145_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _145_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _145_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _145_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _145_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _145_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _145_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _145_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _145_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _145_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _145_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _145_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _145_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _145_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _145_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _145_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _145_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _145_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _145_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _145_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static u3j_harm _145_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _145_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _145_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _145_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _145_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _145_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _145_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _145_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _145_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};

  static u3j_harm _145_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _145_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _145_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};

  static u3j_harm _145_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};

  static u3j_harm _145_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _145_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _145_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _145_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};

  static u3j_harm _145_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _145_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};

    static u3j_harm _145_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _145_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _145_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _145_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _145_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _145_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _145_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _145_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _145_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _145_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _145_two__in_d[] =
    { { "bif", _145_two__in_bif_a },
      { "dif", _145_two__in_dif_a },
      { "gas", _145_two__in_gas_a },
      { "has", _145_two__in_has_a },
      { "mer", _145_two__in_mer_a },
      // { "int", _145_two__in_int_a },
      { "put", _145_two__in_put_a },
      { "tap", _145_two__in_tap_a },
      { "wyt", _145_two__in_wyt_a },
      { "uni", _145_two__in_uni_a },
      {}
    };

    static u3j_harm _145_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _145_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _145_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _145_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _145_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _145_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _145_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _145_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _145_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _145_two__by_d[] =
    { { "bif", _145_two__by_bif_a },
      { "dif", _145_two__by_dif_a },
      { "gas", _145_two__by_gas_a },
      { "get", _145_two__by_get_a },
      { "has", _145_two__by_has_a },
      // { "int", _145_two__by_int_a },
      { "put", _145_two__by_put_a },
      { "tap", _145_two__by_tap_a },
      // { "uni", _145_two__by_uni_a },
      {}
    };

  static u3j_harm _145_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _145_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _145_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _145_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _145_two_d[] =
  { { "tri", 0, _145_tri_d },
    { "flop", _145_two_flop_a },
    { "lent", _145_two_lent_a },
    { "levy", _145_two_levy_a },
    { "lien", _145_two_lien_a },
    { "murn", _145_two_murn_a },
    { "need", _145_two_need_a },
    { "reap", _145_two_reap_a },
    { "reel", _145_two_reel_a },
    { "roll", _145_two_roll_a },
    { "skid", _145_two_skid_a },
    { "skim", _145_two_skim_a },
    { "skip", _145_two_skip_a },
    { "scag", _145_two_scag_a },
    { "slag", _145_two_slag_a },
    { "snag", _145_two_snag_a },
//  { "sort", _145_two_sort_a },
    { "turn", _145_two_turn_a },
    { "weld", _145_two_weld_a },

    { "bex", _145_two_bex_a },
    { "xeb", _145_two_xeb_a },
    { "can", _145_two_can_a },
    { "cat", _145_two_cat_a },
    { "cut", _145_two_cut_a },
    { "end", _145_two_end_a },
    { "lsh", _145_two_lsh_a },
    { "met", _145_two_met_a },
    { "rap", _145_two_rap_a },
    { "rip", _145_two_rip_a },
    { "rsh", _145_two_rsh_a },

    { "con", _145_two_con_a },
    { "dis", _145_two_dis_a },
    { "mix", _145_two_mix_a },

    { "mug", _145_two_mug_a },

    { "dor", _145_two_dor_a },
    { "gor", _145_two_gor_a },
    { "hor", _145_two_hor_a },
    { "vor", _145_two_vor_a },

    { "pow", _145_two_pow_a },
    { "sqt", _145_two_sqt_a },

    { "by", 0, _145_two__by_d },
    { "in", 0, _145_two__in_d },

    { "cue", _145_two_cue_a },
    { "jam", _145_two_jam_a },
    { "mat", _145_two_mat_a },
    { "rub", _145_two_rub_a },
  };


/* layer one
*/
  static u3j_harm _145_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _145_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _145_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _145_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _145_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _145_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _145_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _145_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _145_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _145_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _145_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _145_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _145_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _145_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _145_one_d[] =
  { { "two", 0, _145_two_d },
    { "add", _145_one_add_a },
    { "dec", _145_one_dec_a },
    { "div", _145_one_div_a },
    { "dvr", _145_one_dvr_a },
    { "gte", _145_one_gte_a },
    { "gth", _145_one_gth_a },
    { "lte", _145_one_lte_a },
    { "lth", _145_one_lth_a },
    { "mod", _145_one_mod_a },
    { "mul", _145_one_mul_a },
    { "sub", _145_one_sub_a },

    { "cap", _145_one_cap_a },
    { "mas", _145_one_mas_a },
    { "peg", _145_one_peg_a },

    {}
  };
static u3j_core _k145_d[] =
  { { "one", 0, _145_one_d },
    {}
  };

  static u3j_harm _146_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _146_hex_down_d[] =
      { { "mark", _146_hex_down_mark_a },
        {}
      };

  static u3j_harm _146_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _146_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _146_hex_lune_a[] = {{".2", u3we_lune}, {}};

static u3j_core _146_hex_d[] =
  { { "down", 0, _146_hex_down_d },
    { "lore", _146_hex_lore_a },
    { "loss", _146_hex_loss_a },
    { "lune", _146_hex_lune_a },
    {}
  };

/* layer five
*/
  static u3j_harm _146_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static u3j_harm _146_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static u3j_harm _146_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static u3j_harm _146_pen_core_a[] = {{".2", u3wf_core}, {}};
  static u3j_harm _146_pen_help_a[] = {{".2", u3wf_help}, {}};
  static u3j_harm _146_pen_face_a[] = {{".2", u3wf_face}, {}};
  static u3j_harm _146_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static u3j_harm _146_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static u3j_harm _146_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static u3j_harm _146_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static u3j_harm _146_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static u3j_harm _146_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static u3j_harm _146_pen_look_a[] = {{".2", u3wf_look}, {}};
  static u3j_harm _146_pen_loot_a[] = {{".2", u3wf_loot}, {}};

    static u3j_harm _146_pen__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
    static u3j_harm _146_pen__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
    static u3j_harm _146_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    // static u3j_harm _146_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _146_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static u3j_harm _146_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static u3j_harm _146_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static u3j_harm _146_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static u3j_harm _146_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static u3j_harm _146_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static u3j_harm _146_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static u3j_harm _146_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static u3j_harm _146_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static u3j_harm _146_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static u3j_harm _146_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
  static u3j_core _146_pen__ut_d[] =
    { 
      { "burn", _146_pen__ut_burn_a },
      { "conk", _146_pen__ut_conk_a },
      { "crop", _146_pen__ut_crop_a },
      { "fond", _146_pen__ut_fond_a },
    //  { "fire", _146_pen__ut_fire_a },
      { "fish", _146_pen__ut_fish_a },
      { "fuse", _146_pen__ut_fuse_a },
      { "mint", _146_pen__ut_mint_a },
      { "mull", _146_pen__ut_mull_a },
      { "nest", _146_pen__ut_nest_a },
      { "peek", _146_pen__ut_peek_a },
      { "play", _146_pen__ut_play_a },
      { "rest", _146_pen__ut_rest_a },
      { "toss", _146_pen__ut_toss_a },
      { "wrap", _146_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _146_pen__ut_a[] = 
    { {"burn", u3wfu_burn},
      {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _146_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _146_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif

static u3j_core _146_pen_d[] =
  { { "hex", 0, _146_hex_d },

    { "cell", _146_pen_cell_a },
    { "comb", _146_pen_comb_a },
    { "cons", _146_pen_cons_a },
    { "core", _146_pen_core_a },
    { "face", _146_pen_face_a },
    { "fitz", _146_pen_fitz_a },
    { "flan", _146_pen_flan_a },
    { "flip", _146_pen_flip_a },
    { "flor", _146_pen_flor_a },
    { "fork", _146_pen_fork_a },
    { "help", _146_pen_help_a },
    { "hike", _146_pen_hike_a },
    { "look", _146_pen_look_a },
    { "loot", _146_pen_loot_a },

    // { "ap", _146_pen__ap_a },
    // { "al", _146_pen__al_a },
    { "ut", _146_pen__ut_a, _146_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _146_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _146_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _146_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _146_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _146_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _146_qua__po_d[] =
      { { "ind", _146_qua__po_ind_a },
        { "ins", _146_qua__po_ins_a },
        { "tod", _146_qua__po_tod_a },
        { "tos", _146_qua__po_tos_a },
        {}
      };

  static u3j_harm _146_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _146_qua__bend_d[] =
      { { "fun", _146_qua__bend_fun_a },
        {}
      };

  static u3j_harm _146_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _146_qua__cold_d[] =
      { { "fun", _146_qua__cold_fun_a },
        {}
      };

  static u3j_harm _146_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _146_qua__cook_d[] =
      { { "fun", _146_qua__cook_fun_a },
        {}
      };

  static u3j_harm _146_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _146_qua__comp_d[] =
      { { "fun", _146_qua__comp_fun_a },
        {}
      };

  static u3j_harm _146_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _146_qua__easy_d[] =
      { { "fun", _146_qua__easy_fun_a },
        {}
      };

  static u3j_harm _146_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _146_qua__glue_d[] =
      { { "fun", _146_qua__glue_fun_a },
        {}
      };

  static u3j_harm _146_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _146_qua__here_d[] =
      { { "fun", _146_qua__here_fun_a },
        {}
      };

  static u3j_harm _146_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _146_qua__just_d[] =
      { { "fun", _146_qua__just_fun_a },
        {}
      };

  static u3j_harm _146_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _146_qua__mask_d[] =
      { { "fun", _146_qua__mask_fun_a },
        {}
      };

  static u3j_harm _146_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _146_qua__shim_d[] =
      { { "fun", _146_qua__shim_fun_a },
        {}
      };

  static u3j_harm _146_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _146_qua__stag_d[] =
      { { "fun", _146_qua__stag_fun_a },
        {}
      };

  static u3j_harm _146_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _146_qua__stew_d[] =
      { { "fun", _146_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _146_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _146_qua__stir_d[] =
      { { "fun", _146_qua__stir_fun_a },
        {}
      };

  static u3j_harm _146_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _146_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _146_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _146_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _146_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _146_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _146_qua_d[] =
  { { "pen", 0, _146_pen_d },

    { "po", 0, _146_qua__po_d },

    { "trip", _146_qua_trip_a },

    { "bend", 0, _146_qua__bend_d },
    { "cold", 0, _146_qua__cold_d },
    { "comp", 0, _146_qua__comp_d },
    { "cook", 0, _146_qua__cook_d },
    { "easy", 0, _146_qua__easy_d },
    { "glue", 0, _146_qua__glue_d },
    { "here", 0, _146_qua__here_d },
    { "just", 0, _146_qua__just_d },
    { "mask", 0, _146_qua__mask_d },
    { "shim", 0, _146_qua__shim_d },
    { "stag", 0, _146_qua__stag_d },
    { "stew", 0, _146_qua__stew_d },
    { "stir", 0, _146_qua__stir_d },

    { "pfix", _146_qua_pfix_a },
    { "plug", _146_qua_plug_a },
    { "pose", _146_qua_pose_a },
    { "sfix", _146_qua_sfix_a },

    { "mink", _146_qua_mink_a },
    { "mule", _146_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _146_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _146_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _146_tri__cofl_d[] =
    { { "drg", _146_tri__cofl__drg_a },
      { "lug", _146_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _146_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _146_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _146_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _146_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _146_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _146_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _146_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _146_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _146_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _146_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _146_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _146_tri__rd_d[] =
    { { "add", _146_tri__rd_add_a },
      { "sub", _146_tri__rd_sub_a },
      { "mul", _146_tri__rd_mul_a },
      { "div", _146_tri__rd_div_a },
      { "sqt", _146_tri__rd_sqt_a },
      { "fma", _146_tri__rd_fma_a },
      { "lth", _146_tri__rd_lth_a },
      { "lte", _146_tri__rd_lte_a },
      { "equ", _146_tri__rd_equ_a },
      { "gte", _146_tri__rd_gte_a },
      { "gth", _146_tri__rd_gth_a },
      {}
    };
    static u3j_harm _146_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _146_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _146_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _146_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _146_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _146_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _146_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _146_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _146_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _146_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _146_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _146_tri__rs_d[] =
    { { "add", _146_tri__rs_add_a },
      { "sub", _146_tri__rs_sub_a },
      { "mul", _146_tri__rs_mul_a },
      { "div", _146_tri__rs_div_a },
      { "sqt", _146_tri__rs_sqt_a },
      { "fma", _146_tri__rs_fma_a },
      { "lth", _146_tri__rs_lth_a },
      { "lte", _146_tri__rs_lte_a },
      { "equ", _146_tri__rs_equ_a },
      { "gte", _146_tri__rs_gte_a },
      { "gth", _146_tri__rs_gth_a },
      {}
    };

    static u3j_harm _146_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _146_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _146_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _146_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _146_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _146_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _146_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _146_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _146_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _146_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _146_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _146_tri__rq_d[] =
    { { "add", _146_tri__rq_add_a },
      { "sub", _146_tri__rq_sub_a },
      { "mul", _146_tri__rq_mul_a },
      { "div", _146_tri__rq_div_a },
      { "sqt", _146_tri__rq_sqt_a },
      { "fma", _146_tri__rq_fma_a },
      { "lth", _146_tri__rq_lth_a },
      { "lte", _146_tri__rq_lte_a },
      { "equ", _146_tri__rq_equ_a },
      { "gte", _146_tri__rq_gte_a },
      { "gth", _146_tri__rq_gth_a },
      {}
    };

    static u3j_harm _146_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static u3j_harm _146_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static u3j_harm _146_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static u3j_harm _146_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static u3j_harm _146_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static u3j_harm _146_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static u3j_harm _146_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static u3j_harm _146_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static u3j_harm _146_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static u3j_harm _146_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static u3j_harm _146_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static u3j_core _146_tri__rh_d[] =
    { { "add", _146_tri__rh_add_a },
      { "sub", _146_tri__rh_sub_a },
      { "mul", _146_tri__rh_mul_a },
      { "div", _146_tri__rh_div_a },
      { "sqt", _146_tri__rh_sqt_a },
      { "fma", _146_tri__rh_fma_a },
      { "lth", _146_tri__rh_lth_a },
      { "lte", _146_tri__rh_lte_a },
      { "equ", _146_tri__rh_equ_a },
      { "gte", _146_tri__rh_gte_a },
      { "gth", _146_tri__rh_gth_a },
      {}
    };

    static u3j_harm _146_tri__aesc_en_a[] = {{".2", u3wea_en}, {}};
    static u3j_harm _146_tri__aesc_de_a[] = {{".2", u3wea_de}, {}};
  static u3j_core _146_tri__aesc_d[] =
    { { "en", _146_tri__aesc_en_a },
      { "de", _146_tri__aesc_de_a },
      {}
    };

    static u3j_harm _146_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _146_tri__og_d[] =
    { { "raw", _146_tri__og_raw_a },
      {}
    };

  static u3j_harm _146_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _146_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _146_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _146_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _146_tri_d[] =
  { { "qua", 0, _146_qua_d },

    { "cofl", 0, _146_tri__cofl_d },
    { "rd", 0, _146_tri__rd_d },
    { "rs", 0, _146_tri__rs_d },
    { "rq", 0, _146_tri__rq_d },
    { "rh", 0, _146_tri__rh_d },
    { "og", 0, _146_tri__og_d },
    { "shax", _146_tri_shax_a },
    { "shay", _146_tri_shay_a },
    { "shas", _146_tri_shas_a },
    { "shal", _146_tri_shal_a },
    { "aesc", 0, _146_tri__aesc_d },

    {}
  };

/* layer two
*/
  static u3j_harm _146_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _146_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _146_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _146_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _146_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _146_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _146_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _146_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _146_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _146_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _146_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _146_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _146_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _146_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _146_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _146_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _146_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _146_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _146_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _146_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static u3j_harm _146_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _146_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _146_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _146_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _146_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _146_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _146_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _146_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _146_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};

  static u3j_harm _146_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _146_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _146_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};

  static u3j_harm _146_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};

  static u3j_harm _146_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _146_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _146_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _146_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};

  static u3j_harm _146_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _146_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};

    static u3j_harm _146_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _146_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _146_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _146_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _146_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _146_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _146_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _146_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _146_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _146_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _146_two__in_d[] =
    { { "bif", _146_two__in_bif_a },
      { "dif", _146_two__in_dif_a },
      { "gas", _146_two__in_gas_a },
      { "has", _146_two__in_has_a },
      { "mer", _146_two__in_mer_a },
      // { "int", _146_two__in_int_a },
      { "put", _146_two__in_put_a },
      { "tap", _146_two__in_tap_a },
      { "wyt", _146_two__in_wyt_a },
      { "uni", _146_two__in_uni_a },
      {}
    };

    static u3j_harm _146_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _146_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _146_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _146_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _146_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _146_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _146_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _146_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _146_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _146_two__by_d[] =
    { { "bif", _146_two__by_bif_a },
      { "dif", _146_two__by_dif_a },
      { "gas", _146_two__by_gas_a },
      { "get", _146_two__by_get_a },
      { "has", _146_two__by_has_a },
      // { "int", _146_two__by_int_a },
      { "put", _146_two__by_put_a },
      { "tap", _146_two__by_tap_a },
      // { "uni", _146_two__by_uni_a },
      {}
    };

  static u3j_harm _146_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _146_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _146_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _146_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _146_two_d[] =
  { { "tri", 0, _146_tri_d },
    { "flop", _146_two_flop_a },
    { "lent", _146_two_lent_a },
    { "levy", _146_two_levy_a },
    { "lien", _146_two_lien_a },
    { "murn", _146_two_murn_a },
    { "need", _146_two_need_a },
    { "reap", _146_two_reap_a },
    { "reel", _146_two_reel_a },
    { "roll", _146_two_roll_a },
    { "skid", _146_two_skid_a },
    { "skim", _146_two_skim_a },
    { "skip", _146_two_skip_a },
    { "scag", _146_two_scag_a },
    { "slag", _146_two_slag_a },
    { "snag", _146_two_snag_a },
//  { "sort", _146_two_sort_a },
    { "turn", _146_two_turn_a },
    { "weld", _146_two_weld_a },

    { "bex", _146_two_bex_a },
    { "xeb", _146_two_xeb_a },
    { "can", _146_two_can_a },
    { "cat", _146_two_cat_a },
    { "cut", _146_two_cut_a },
    { "end", _146_two_end_a },
    { "lsh", _146_two_lsh_a },
    { "met", _146_two_met_a },
    { "rap", _146_two_rap_a },
    { "rip", _146_two_rip_a },
    { "rsh", _146_two_rsh_a },

    { "con", _146_two_con_a },
    { "dis", _146_two_dis_a },
    { "mix", _146_two_mix_a },

    { "mug", _146_two_mug_a },

    { "dor", _146_two_dor_a },
    { "gor", _146_two_gor_a },
    { "hor", _146_two_hor_a },
    { "vor", _146_two_vor_a },

    { "pow", _146_two_pow_a },
    { "sqt", _146_two_sqt_a },

    { "by", 0, _146_two__by_d },
    { "in", 0, _146_two__in_d },

    { "cue", _146_two_cue_a },
    { "jam", _146_two_jam_a },
    { "mat", _146_two_mat_a },
    { "rub", _146_two_rub_a },
  };


/* layer one
*/
  static u3j_harm _146_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _146_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _146_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _146_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _146_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _146_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _146_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _146_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _146_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _146_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _146_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _146_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _146_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _146_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _146_one_d[] =
  { { "two", 0, _146_two_d },
    { "add", _146_one_add_a },
    { "dec", _146_one_dec_a },
    { "div", _146_one_div_a },
    { "dvr", _146_one_dvr_a },
    { "gte", _146_one_gte_a },
    { "gth", _146_one_gth_a },
    { "lte", _146_one_lte_a },
    { "lth", _146_one_lth_a },
    { "mod", _146_one_mod_a },
    { "mul", _146_one_mul_a },
    { "sub", _146_one_sub_a },

    { "cap", _146_one_cap_a },
    { "mas", _146_one_mas_a },
    { "peg", _146_one_peg_a },

    {}
  };
static u3j_core _k146_d[] =
  { { "one", 0, _146_one_d },
    {}
  };

static u3j_core _d[] = {
  { "k145", 0, _k145_d},
  { "k146", 0, _k146_d},
  {}
};

u3j_dash 
u3j_Dash = {
  _d,
  0,
  0
};
