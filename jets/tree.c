/* j/tree.c
*/
#include "all.h"

  static u3j_harm _141_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
  static u3j_harm _141_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
  static u3j_harm _141_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
  static u3j_harm _141_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
  static u3j_harm _141_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
  static u3j_harm _141_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};

  static u3j_harm _141_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
  static u3j_harm _141_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
  static u3j_harm _141_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
  static u3j_harm _141_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
  static u3j_harm _141_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
  static u3j_harm _141_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};

  static u3j_core _141_hex_aes_ecba_d[] =
    { { "en", _141_hex_aes_ecba_en_a },
      { "de", _141_hex_aes_ecba_de_a },
      {}
    };

  static u3j_core _141_hex_aes_ecbb_d[] =
    { { "en", _141_hex_aes_ecbb_en_a },
      { "de", _141_hex_aes_ecbb_de_a },
      {}
    };

  static u3j_core _141_hex_aes_ecbc_d[] =
    { { "en", _141_hex_aes_ecbc_en_a },
      { "de", _141_hex_aes_ecbc_de_a },
      {}
    };

  static u3j_core _141_hex_aes_cbca_d[] =
    { { "en", _141_hex_aes_cbca_en_a },
      { "de", _141_hex_aes_cbca_de_a },
      {}
    };

  static u3j_core _141_hex_aes_cbcb_d[] =
    { { "en", _141_hex_aes_cbcb_en_a },
      { "de", _141_hex_aes_cbcb_de_a },
      {}
    };

  static u3j_core _141_hex_aes_cbcc_d[] =
    { { "en", _141_hex_aes_cbcc_en_a },
      { "de", _141_hex_aes_cbcc_de_a },
      {}
    };

  static u3j_core _141_hex_aes_d[] =
    { { "ecba", 0, _141_hex_aes_ecba_d },
      { "ecbb", 0, _141_hex_aes_ecbb_d },
      { "ecbc", 0, _141_hex_aes_ecbc_d },
      { "cbca", 0, _141_hex_aes_cbca_d },
      { "cbcb", 0, _141_hex_aes_cbcb_d },
      { "cbcc", 0, _141_hex_aes_cbcc_d },
      {}
    };

  static u3j_harm _141_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _141_hex_down_d[] =
      { { "mark", _141_hex_down_mark_a },
        {}
      };

  static u3j_harm _141_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _141_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _141_hex_lune_a[] = {{".2", u3we_lune}, {}};

  static u3j_harm _141_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
  static u3j_harm _141_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
  static u3j_harm _141_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
  static u3j_harm _141_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};

  static u3j_core _141_hex_coed__ed_d[] =
    { { "sign", _141_hex_coed__ed_sign_a },
      { "puck", _141_hex_coed__ed_puck_a },
      { "veri", _141_hex_coed__ed_veri_a },
      { "shar", _141_hex_coed__ed_shar_a },
      {}
    };
static u3j_core _141_hex_coed_d[] =
  { { "ed", 0, _141_hex_coed__ed_d },
    {}
  };

  static u3j_harm _141_hex_hmac_a[] = {{".2", u3we_hmac}, {}};

  static u3j_core _141_hex_hmac_d[] =
    { { "hmac", _141_hex_hmac_a },
      {}
    };

  static u3j_harm _141_hex_argon2_a[] = {{".2", u3we_argon2}, {}};

  static u3j_core _141_hex_argon_d[] =
    { { "argon2", _141_hex_argon2_a },
      {}
    };

  static u3j_harm _141_hex_secp_make_a[] = {{".2", u3we_make, c3y}, {}};
  static u3j_harm _141_hex_secp_sign_a[] = {{".2", u3we_sign, c3y}, {}};
  static u3j_harm _141_hex_secp_reco_a[] = {{".2", u3we_reco, c3y}, {}};

  static u3j_core _141_hex_secp_secp_helper_d[] =
    { { "make-k",            _141_hex_secp_make_a },
      { "ecdsa-raw-sign",    _141_hex_secp_sign_a },
      { "ecdsa-raw-recover", _141_hex_secp_reco_a },
      {}
    };

  static u3j_core _141_hex_secp_secp_d[] =
    { { "helper", 0,   _141_hex_secp_secp_helper_d },
      {}
    };

  static u3j_core _141_hex_secp_d[] =
    { { "secp", 0,   _141_hex_secp_secp_d },
      {}
    };

  static u3j_harm _141_hex_blake2b_a[] = {{".2", u3we_blake, c3y}, {}};

  static u3j_core _141_hex_blake_d[] =
    { { "blake2b",  _141_hex_blake2b_a },
      {}
    };

  static u3j_harm _141_hex_ripemd_160_a[] = {{".2", u3we_ripe, c3y}, {}};

  static u3j_core _141_hex_ripe_d[] =
    { { "ripemd160",  _141_hex_ripemd_160_a },
      {}
    };


static u3j_core _141_hex_d[] =
  { { "down", 0, _141_hex_down_d },
    { "lore", _141_hex_lore_a },
    { "loss", _141_hex_loss_a },
    { "lune", _141_hex_lune_a },
    { "coed", 0, _141_hex_coed_d },
    { "aes", 0, _141_hex_aes_d },
    { "hmac", 0, _141_hex_hmac_d },
    { "argon", 0, _141_hex_argon_d },
    { "blake2b", 0, _141_hex_blake_d },
    { "ripemd", 0, _141_hex_ripe_d },
    { "secp", 0, _141_hex_secp_d },
    {}
  };

/* layer five
*/
  static u3j_harm _141_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static u3j_harm _141_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static u3j_harm _141_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static u3j_harm _141_pen_core_a[] = {{".2", u3wf_core}, {}};
  static u3j_harm _141_pen_face_a[] = {{".2", u3wf_face}, {}};
  static u3j_harm _141_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static u3j_harm _141_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static u3j_harm _141_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static u3j_harm _141_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static u3j_harm _141_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static u3j_harm _141_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static u3j_harm _141_pen_look_a[] = {{".2", u3wf_look}, {}};
  static u3j_harm _141_pen_loot_a[] = {{".2", u3wf_loot}, {}};

    static u3j_harm _141_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    // static u3j_harm _141_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _141_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static u3j_harm _141_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static u3j_harm _141_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static u3j_harm _141_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static u3j_harm _141_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static u3j_harm _141_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static u3j_harm _141_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static u3j_harm _141_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static u3j_harm _141_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static u3j_harm _141_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static u3j_harm _141_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
  static u3j_core _141_pen__ut_d[] =
    { 
      { "crop", _141_pen__ut_crop_a },
      { "fond", _141_pen__ut_fond_a },
    //  { "fire", _141_pen__ut_fire_a },
      { "fish", _141_pen__ut_fish_a },
      { "fuse", _141_pen__ut_fuse_a },
      { "mint", _141_pen__ut_mint_a },
      { "mull", _141_pen__ut_mull_a },
      { "nest", _141_pen__ut_nest_a },
      { "peek", _141_pen__ut_peek_a },
      { "play", _141_pen__ut_play_a },
      { "rest", _141_pen__ut_rest_a },
      { "toss", _141_pen__ut_toss_a },
      { "wrap", _141_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _141_pen__ut_a[] = 
    { {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _141_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _141_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif

static u3j_core _141_pen_d[] =
  { { "hex", 0, _141_hex_d },

    { "cell", _141_pen_cell_a },
    { "comb", _141_pen_comb_a },
    { "cons", _141_pen_cons_a },
    { "core", _141_pen_core_a },
    { "face", _141_pen_face_a },
    { "fitz", _141_pen_fitz_a },
    { "flan", _141_pen_flan_a },
    { "flip", _141_pen_flip_a },
    { "flor", _141_pen_flor_a },
    { "fork", _141_pen_fork_a },
    { "hike", _141_pen_hike_a },
    { "look", _141_pen_look_a },
    { "loot", _141_pen_loot_a },

    // { "ap", _141_pen__ap_a },
    // { "al", _141_pen__al_a },
    { "ut", _141_pen__ut_a, _141_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _141_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _141_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _141_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _141_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _141_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _141_qua__po_d[] =
      { { "ind", _141_qua__po_ind_a },
        { "ins", _141_qua__po_ins_a },
        { "tod", _141_qua__po_tod_a },
        { "tos", _141_qua__po_tos_a },
        {}
      };

  static u3j_harm _141_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _141_qua__bend_d[] =
      { { "fun", _141_qua__bend_fun_a },
        {}
      };

  static u3j_harm _141_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _141_qua__cold_d[] =
      { { "fun", _141_qua__cold_fun_a },
        {}
      };

  static u3j_harm _141_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _141_qua__cook_d[] =
      { { "fun", _141_qua__cook_fun_a },
        {}
      };

  static u3j_harm _141_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _141_qua__comp_d[] =
      { { "fun", _141_qua__comp_fun_a },
        {}
      };

  static u3j_harm _141_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _141_qua__easy_d[] =
      { { "fun", _141_qua__easy_fun_a },
        {}
      };

  static u3j_harm _141_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _141_qua__glue_d[] =
      { { "fun", _141_qua__glue_fun_a },
        {}
      };

  static u3j_harm _141_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _141_qua__here_d[] =
      { { "fun", _141_qua__here_fun_a },
        {}
      };

  static u3j_harm _141_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _141_qua__just_d[] =
      { { "fun", _141_qua__just_fun_a },
        {}
      };

  static u3j_harm _141_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _141_qua__mask_d[] =
      { { "fun", _141_qua__mask_fun_a },
        {}
      };

  static u3j_harm _141_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _141_qua__shim_d[] =
      { { "fun", _141_qua__shim_fun_a },
        {}
      };

  static u3j_harm _141_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _141_qua__stag_d[] =
      { { "fun", _141_qua__stag_fun_a },
        {}
      };

  static u3j_harm _141_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _141_qua__stew_d[] =
      { { "fun", _141_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _141_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _141_qua__stir_d[] =
      { { "fun", _141_qua__stir_fun_a },
        {}
      };

  static u3j_harm _141_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _141_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _141_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _141_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _141_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _141_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _141_qua_d[] =
  { { "pen", 0, _141_pen_d },

    { "po", 0, _141_qua__po_d },

    { "trip", _141_qua_trip_a },

    { "bend", 0, _141_qua__bend_d },
    { "cold", 0, _141_qua__cold_d },
    { "comp", 0, _141_qua__comp_d },
    { "cook", 0, _141_qua__cook_d },
    { "easy", 0, _141_qua__easy_d },
    { "glue", 0, _141_qua__glue_d },
    { "here", 0, _141_qua__here_d },
    { "just", 0, _141_qua__just_d },
    { "mask", 0, _141_qua__mask_d },
    { "shim", 0, _141_qua__shim_d },
    { "stag", 0, _141_qua__stag_d },
    { "stew", 0, _141_qua__stew_d },
    { "stir", 0, _141_qua__stir_d },

    { "pfix", _141_qua_pfix_a },
    { "plug", _141_qua_plug_a },
    { "pose", _141_qua_pose_a },
    { "sfix", _141_qua_sfix_a },

    { "mink", _141_qua_mink_a },
    { "mule", _141_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _141_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _141_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _141_tri__cofl_d[] =
    { { "drg", _141_tri__cofl__drg_a },
      { "lug", _141_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _141_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _141_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _141_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _141_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _141_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _141_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _141_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _141_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _141_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _141_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _141_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _141_tri__rd_d[] =
    { { "add", _141_tri__rd_add_a },
      { "sub", _141_tri__rd_sub_a },
      { "mul", _141_tri__rd_mul_a },
      { "div", _141_tri__rd_div_a },
      { "sqt", _141_tri__rd_sqt_a },
      { "fma", _141_tri__rd_fma_a },
      { "lth", _141_tri__rd_lth_a },
      { "lte", _141_tri__rd_lte_a },
      { "equ", _141_tri__rd_equ_a },
      { "gte", _141_tri__rd_gte_a },
      { "gth", _141_tri__rd_gth_a },
      {}
    };
    static u3j_harm _141_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _141_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _141_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _141_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _141_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _141_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _141_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _141_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _141_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _141_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _141_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _141_tri__rs_d[] =
    { { "add", _141_tri__rs_add_a },
      { "sub", _141_tri__rs_sub_a },
      { "mul", _141_tri__rs_mul_a },
      { "div", _141_tri__rs_div_a },
      { "sqt", _141_tri__rs_sqt_a },
      { "fma", _141_tri__rs_fma_a },
      { "lth", _141_tri__rs_lth_a },
      { "lte", _141_tri__rs_lte_a },
      { "equ", _141_tri__rs_equ_a },
      { "gte", _141_tri__rs_gte_a },
      { "gth", _141_tri__rs_gth_a },
      {}
    };

    static u3j_harm _141_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _141_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _141_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _141_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _141_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _141_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _141_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _141_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _141_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _141_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _141_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _141_tri__rq_d[] =
    { { "add", _141_tri__rq_add_a },
      { "sub", _141_tri__rq_sub_a },
      { "mul", _141_tri__rq_mul_a },
      { "div", _141_tri__rq_div_a },
      { "sqt", _141_tri__rq_sqt_a },
      { "fma", _141_tri__rq_fma_a },
      { "lth", _141_tri__rq_lth_a },
      { "lte", _141_tri__rq_lte_a },
      { "equ", _141_tri__rq_equ_a },
      { "gte", _141_tri__rq_gte_a },
      { "gth", _141_tri__rq_gth_a },
      {}
    };

    static u3j_harm _141_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static u3j_harm _141_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static u3j_harm _141_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static u3j_harm _141_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static u3j_harm _141_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static u3j_harm _141_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static u3j_harm _141_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static u3j_harm _141_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static u3j_harm _141_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static u3j_harm _141_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static u3j_harm _141_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static u3j_core _141_tri__rh_d[] =
    { { "add", _141_tri__rh_add_a },
      { "sub", _141_tri__rh_sub_a },
      { "mul", _141_tri__rh_mul_a },
      { "div", _141_tri__rh_div_a },
      { "sqt", _141_tri__rh_sqt_a },
      { "fma", _141_tri__rh_fma_a },
      { "lth", _141_tri__rh_lth_a },
      { "lte", _141_tri__rh_lte_a },
      { "equ", _141_tri__rh_equ_a },
      { "gte", _141_tri__rh_gte_a },
      { "gth", _141_tri__rh_gth_a },
      {}
    };

    static u3j_harm _141_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _141_tri__og_d[] =
    { { "raw", _141_tri__og_raw_a },
      {}
    };

  static u3j_harm _141_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _141_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _141_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _141_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _141_tri_d[] =
  { { "qua", 0, _141_qua_d },

    { "cofl", 0, _141_tri__cofl_d },
    { "rd", 0, _141_tri__rd_d },
    { "rs", 0, _141_tri__rs_d },
    { "rq", 0, _141_tri__rq_d },
    { "rh", 0, _141_tri__rh_d },
    { "og", 0, _141_tri__og_d },
    { "shax", _141_tri_shax_a },
    { "shay", _141_tri_shay_a },
    { "shas", _141_tri_shas_a },
    { "shal", _141_tri_shal_a },
    {}
  };

/* layer two
*/
  static u3j_harm _141_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _141_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _141_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _141_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _141_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _141_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _141_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _141_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _141_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _141_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _141_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _141_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _141_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _141_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _141_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _141_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _141_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _141_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _141_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _141_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _141_two_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _141_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _141_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _141_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _141_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _141_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _141_two_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _141_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _141_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _141_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _141_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _141_two_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
  static u3j_harm _141_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _141_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
  static u3j_harm _141_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
  static u3j_harm _141_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
  static u3j_harm _141_two_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _141_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _141_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _141_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
  static u3j_harm _141_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _141_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
  static u3j_harm _141_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
  static u3j_harm _141_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
  static u3j_harm _141_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};



    static u3j_harm _141_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _141_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _141_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _141_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _141_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _141_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _141_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _141_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _141_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _141_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _141_two__in_d[] =
    { { "bif", _141_two__in_bif_a },
      { "dif", _141_two__in_dif_a },
      { "gas", _141_two__in_gas_a },
      { "has", _141_two__in_has_a },
      { "mer", _141_two__in_mer_a },
      // { "int", _141_two__in_int_a },
      { "put", _141_two__in_put_a },
      { "tap", _141_two__in_tap_a },
      { "wyt", _141_two__in_wyt_a },
      { "uni", _141_two__in_uni_a },
      {}
    };

    static u3j_harm _141_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _141_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _141_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _141_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _141_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _141_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _141_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _141_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _141_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _141_two__by_d[] =
    { { "bif", _141_two__by_bif_a },
      { "dif", _141_two__by_dif_a },
      { "gas", _141_two__by_gas_a },
      { "get", _141_two__by_get_a },
      { "has", _141_two__by_has_a },
      // { "int", _141_two__by_int_a },
      { "put", _141_two__by_put_a },
      { "tap", _141_two__by_tap_a },
      // { "uni", _141_two__by_uni_a },
      {}
    };

  static u3j_harm _141_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _141_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _141_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _141_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _141_two_d[] =
  { { "tri", 0, _141_tri_d },
    { "flop", _141_two_flop_a },
    { "lent", _141_two_lent_a },
    { "levy", _141_two_levy_a },
    { "lien", _141_two_lien_a },
    { "murn", _141_two_murn_a },
    { "need", _141_two_need_a },
    { "reap", _141_two_reap_a },
    { "reel", _141_two_reel_a },
    { "roll", _141_two_roll_a },
    { "skid", _141_two_skid_a },
    { "skim", _141_two_skim_a },
    { "skip", _141_two_skip_a },
    { "scag", _141_two_scag_a },
    { "slag", _141_two_slag_a },
    { "snag", _141_two_snag_a },
//  { "sort", _141_two_sort_a },
    { "turn", _141_two_turn_a },
    { "weld", _141_two_weld_a },

    { "bex", _141_two_bex_a },
    { "cat", _141_two_cat_a },
    { "can", _141_two_can_a },
    { "cap", _141_two_cap_a },
    { "con", _141_two_con_a },
    { "cue", _141_two_cue_a },
    { "cut", _141_two_cut_a },
    { "dis", _141_two_dis_a },
    { "dor", _141_two_dor_a },
    { "dvr", _141_two_dvr_a },
    { "end", _141_two_end_a },
    { "gor", _141_two_gor_a },
    { "hor", _141_two_hor_a },
    { "jam", _141_two_jam_a },
    { "lsh", _141_two_lsh_a },
    { "mas", _141_two_mas_a },
    { "mat", _141_two_mat_a },
    { "met", _141_two_met_a },
    { "mix", _141_two_mix_a },
    { "mug", _141_two_mug_a },
    { "muk", _141_two_muk_a },
    { "rap", _141_two_rap_a },
    { "rep", _141_two_rep_a },
    { "rip", _141_two_rip_a },
    { "rsh", _141_two_rsh_a },
    { "rub", _141_two_rub_a },
    { "peg", _141_two_peg_a },
    { "pow", _141_two_pow_a },
    { "sqt", _141_two_sqt_a },
    { "vor", _141_two_vor_a },
    { "xeb", _141_two_xeb_a },

    { "by", 0, _141_two__by_d },
    { "in", 0, _141_two__in_d },
  };


/* layer one
*/
  static u3j_harm _141_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _141_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _141_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _141_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _141_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _141_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _141_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _141_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _141_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _141_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _141_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _141_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _141_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _141_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _141_one_d[] =
  { { "two", 0, _141_two_d },
    { "add", _141_one_add_a },
    { "dec", _141_one_dec_a },
    { "div", _141_one_div_a },
    { "dvr", _141_one_dvr_a },
    { "gte", _141_one_gte_a },
    { "gth", _141_one_gth_a },
    { "lte", _141_one_lte_a },
    { "lth", _141_one_lth_a },
    { "mod", _141_one_mod_a },
    { "mul", _141_one_mul_a },
    { "sub", _141_one_sub_a },

    { "cap", _141_one_cap_a },
    { "mas", _141_one_mas_a },
    { "peg", _141_one_peg_a },

    {}
  };
u3j_core _k141_d[] =
  { { "one", 0, _141_one_d },
    {}
  };

  static u3j_harm _142_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
  static u3j_harm _142_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
  static u3j_harm _142_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
  static u3j_harm _142_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
  static u3j_harm _142_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
  static u3j_harm _142_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};

  static u3j_harm _142_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
  static u3j_harm _142_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
  static u3j_harm _142_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
  static u3j_harm _142_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
  static u3j_harm _142_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
  static u3j_harm _142_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};

  static u3j_core _142_hex_aes_ecba_d[] =
    { { "en", _142_hex_aes_ecba_en_a },
      { "de", _142_hex_aes_ecba_de_a },
      {}
    };

  static u3j_core _142_hex_aes_ecbb_d[] =
    { { "en", _142_hex_aes_ecbb_en_a },
      { "de", _142_hex_aes_ecbb_de_a },
      {}
    };

  static u3j_core _142_hex_aes_ecbc_d[] =
    { { "en", _142_hex_aes_ecbc_en_a },
      { "de", _142_hex_aes_ecbc_de_a },
      {}
    };

  static u3j_core _142_hex_aes_cbca_d[] =
    { { "en", _142_hex_aes_cbca_en_a },
      { "de", _142_hex_aes_cbca_de_a },
      {}
    };

  static u3j_core _142_hex_aes_cbcb_d[] =
    { { "en", _142_hex_aes_cbcb_en_a },
      { "de", _142_hex_aes_cbcb_de_a },
      {}
    };

  static u3j_core _142_hex_aes_cbcc_d[] =
    { { "en", _142_hex_aes_cbcc_en_a },
      { "de", _142_hex_aes_cbcc_de_a },
      {}
    };

  static u3j_core _142_hex_aes_d[] =
    { { "ecba", 0, _142_hex_aes_ecba_d },
      { "ecbb", 0, _142_hex_aes_ecbb_d },
      { "ecbc", 0, _142_hex_aes_ecbc_d },
      { "cbca", 0, _142_hex_aes_cbca_d },
      { "cbcb", 0, _142_hex_aes_cbcb_d },
      { "cbcc", 0, _142_hex_aes_cbcc_d },
      {}
    };

  static u3j_harm _142_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _142_hex_down_d[] =
      { { "mark", _142_hex_down_mark_a },
        {}
      };

  static u3j_harm _142_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _142_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _142_hex_lune_a[] = {{".2", u3we_lune}, {}};

  static u3j_harm _142_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
  static u3j_harm _142_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
  static u3j_harm _142_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
  static u3j_harm _142_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};

  static u3j_core _142_hex_coed__ed_d[] =
    { { "sign", _142_hex_coed__ed_sign_a },
      { "puck", _142_hex_coed__ed_puck_a },
      { "veri", _142_hex_coed__ed_veri_a },
      { "shar", _142_hex_coed__ed_shar_a },
      {}
    };
static u3j_core _142_hex_coed_d[] =
  { { "ed", 0, _142_hex_coed__ed_d },
    {}
  };

static u3j_core _142_hex_d[] =
  { { "down", 0, _142_hex_down_d },
    { "lore", _142_hex_lore_a },
    { "loss", _142_hex_loss_a },
    { "lune", _142_hex_lune_a },
    { "coed", 0, _142_hex_coed_d },
    { "aes", 0, _142_hex_aes_d },
    {}
  };

/* layer five
*/
  static u3j_harm _142_pen_cell_a[] = {{".2", u3wz_cell}, {}};
  static u3j_harm _142_pen_comb_a[] = {{".2", u3wz_comb}, {}};
  static u3j_harm _142_pen_cons_a[] = {{".2", u3wz_cons}, {}};
  static u3j_harm _142_pen_core_a[] = {{".2", u3wz_core}, {}};
  static u3j_harm _142_pen_face_a[] = {{".2", u3wz_face}, {}};
  static u3j_harm _142_pen_fitz_a[] = {{".2", u3wz_fitz}, {}};
  static u3j_harm _142_pen_flan_a[] = {{".2", u3wz_flan}, {}};
  static u3j_harm _142_pen_flip_a[] = {{".2", u3wz_flip}, {}};
  static u3j_harm _142_pen_flor_a[] = {{".2", u3wz_flor}, {}};
  static u3j_harm _142_pen_fork_a[] = {{".2", u3wz_fork}, {}};
  static u3j_harm _142_pen_hike_a[] = {{".2", u3wz_hike}, {}};
  static u3j_harm _142_pen_look_a[] = {{".2", u3wz_look}, {}};
  static u3j_harm _142_pen_loot_a[] = {{".2", u3wz_loot}, {}};

    static u3j_harm _142_pen__ut_crop_a[] = {{".2", u3wzu_crop}, {}};
    // static u3j_harm _142_pen__ut_fire_a[] = {{".2", u3wzu_fire}, {}};
    static u3j_harm _142_pen__ut_fond_a[] = {{".2", u3wzu_fond}, {}};
    static u3j_harm _142_pen__ut_fish_a[] = {{".2", u3wzu_fish}, {}};
    static u3j_harm _142_pen__ut_fuse_a[] = {{".2", u3wzu_fuse}, {}};
    static u3j_harm _142_pen__ut_mint_a[] = {{".2", u3wzu_mint}, {}};
    static u3j_harm _142_pen__ut_mull_a[] = {{".2", u3wzu_mull}, {}};
    static u3j_harm _142_pen__ut_nest_a[] = {{".2", u3wzu_nest}, {}};
    static u3j_harm _142_pen__ut_peek_a[] = {{".2", u3wzu_peek}, {}};
    static u3j_harm _142_pen__ut_play_a[] = {{".2", u3wzu_play}, {}};
    static u3j_harm _142_pen__ut_rest_a[] = {{".2", u3wzu_rest}, {}};
    static u3j_harm _142_pen__ut_toss_a[] = {{".2", u3wzu_toss}, {}};
    static u3j_harm _142_pen__ut_wrap_a[] = {{".2", u3wzu_wrap}, {}};
  static u3j_core _142_pen__ut_d[] =
    { 
      { "crop", _142_pen__ut_crop_a },
      { "fond", _142_pen__ut_fond_a },
    //  { "fire", _142_pen__ut_fire_a },
      { "fish", _142_pen__ut_fish_a },
      { "fuse", _142_pen__ut_fuse_a },
      { "mint", _142_pen__ut_mint_a },
      { "mull", _142_pen__ut_mull_a },
      { "nest", _142_pen__ut_nest_a },
      { "peek", _142_pen__ut_peek_a },
      { "play", _142_pen__ut_play_a },
      { "rest", _142_pen__ut_rest_a },
      { "toss", _142_pen__ut_toss_a },
      { "wrap", _142_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _142_pen__ut_a[] = 
    { {"repo", u3wzu_repo},
      {}
    };

static u3j_core _142_pen_d[] =
  { { "hex", 0, _142_hex_d },

    { "cell", _142_pen_cell_a },
    { "comb", _142_pen_comb_a },
    { "cons", _142_pen_cons_a },
    { "core", _142_pen_core_a },
    { "face", _142_pen_face_a },
    { "fitz", _142_pen_fitz_a },
    { "flan", _142_pen_flan_a },
    { "flip", _142_pen_flip_a },
    { "flor", _142_pen_flor_a },
    { "fork", _142_pen_fork_a },
    { "hike", _142_pen_hike_a },
    { "look", _142_pen_look_a },
    { "loot", _142_pen_loot_a },

    // { "ap", _142_pen__ap_a },
    // { "al", _142_pen__al_a },
    { "ut", _142_pen__ut_a, _142_pen__ut_d },

    {}
  };

/* layer four
*/
  static u3j_harm _142_qua_trip_a[] = {{".2", u3we_trip}, {}};

  static u3j_harm _142_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _142_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _142_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _142_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _142_qua__po_d[] =
      { { "ind", _142_qua__po_ind_a },
        { "ins", _142_qua__po_ins_a },
        { "tod", _142_qua__po_tod_a },
        { "tos", _142_qua__po_tos_a },
        {}
      };

  static u3j_harm _142_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static u3j_core _142_qua__bend_d[] =
      { { "fun", _142_qua__bend_fun_a },
        {}
      };

  static u3j_harm _142_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static u3j_core _142_qua__cold_d[] =
      { { "fun", _142_qua__cold_fun_a },
        {}
      };

  static u3j_harm _142_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static u3j_core _142_qua__cook_d[] =
      { { "fun", _142_qua__cook_fun_a },
        {}
      };

  static u3j_harm _142_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static u3j_core _142_qua__comp_d[] =
      { { "fun", _142_qua__comp_fun_a },
        {}
      };

  static u3j_harm _142_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static u3j_core _142_qua__easy_d[] =
      { { "fun", _142_qua__easy_fun_a },
        {}
      };

  static u3j_harm _142_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _142_qua__glue_d[] =
      { { "fun", _142_qua__glue_fun_a },
        {}
      };

  static u3j_harm _142_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static u3j_core _142_qua__here_d[] =
      { { "fun", _142_qua__here_fun_a },
        {}
      };

  static u3j_harm _142_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static u3j_core _142_qua__just_d[] =
      { { "fun", _142_qua__just_fun_a },
        {}
      };

  static u3j_harm _142_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static u3j_core _142_qua__mask_d[] =
      { { "fun", _142_qua__mask_fun_a },
        {}
      };

  static u3j_harm _142_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static u3j_core _142_qua__shim_d[] =
      { { "fun", _142_qua__shim_fun_a },
        {}
      };

  static u3j_harm _142_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static u3j_core _142_qua__stag_d[] =
      { { "fun", _142_qua__stag_fun_a },
        {}
      };

  static u3j_harm _142_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static u3j_core _142_qua__stew_d[] =
      { { "fun", _142_qua__stew_fun_a },
        {}
      };
 
  static u3j_harm _142_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static u3j_core _142_qua__stir_d[] =
      { { "fun", _142_qua__stir_fun_a },
        {}
      };

  static u3j_harm _142_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _142_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static u3j_harm _142_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static u3j_harm _142_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _142_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _142_qua_mule_a[] = {{".2", u3we_mule}, {}};

static u3j_core _142_qua_d[] =
  { { "pen", 0, _142_pen_d },

    { "po", 0, _142_qua__po_d },

    { "trip", _142_qua_trip_a },

    { "bend", 0, _142_qua__bend_d },
    { "cold", 0, _142_qua__cold_d },
    { "comp", 0, _142_qua__comp_d },
    { "cook", 0, _142_qua__cook_d },
    { "easy", 0, _142_qua__easy_d },
    { "glue", 0, _142_qua__glue_d },
    { "here", 0, _142_qua__here_d },
    { "just", 0, _142_qua__just_d },
    { "mask", 0, _142_qua__mask_d },
    { "shim", 0, _142_qua__shim_d },
    { "stag", 0, _142_qua__stag_d },
    { "stew", 0, _142_qua__stew_d },
    { "stir", 0, _142_qua__stir_d },

    { "pfix", _142_qua_pfix_a },
    { "plug", _142_qua_plug_a },
    { "pose", _142_qua_pose_a },
    { "sfix", _142_qua_sfix_a },

    { "mink", _142_qua_mink_a },
    { "mule", _142_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _142_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _142_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _142_tri__cofl_d[] =
    { { "drg", _142_tri__cofl__drg_a },
      { "lug", _142_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _142_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _142_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _142_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _142_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _142_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _142_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _142_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _142_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _142_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _142_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _142_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _142_tri__rd_d[] =
    { { "add", _142_tri__rd_add_a },
      { "sub", _142_tri__rd_sub_a },
      { "mul", _142_tri__rd_mul_a },
      { "div", _142_tri__rd_div_a },
      { "sqt", _142_tri__rd_sqt_a },
      { "fma", _142_tri__rd_fma_a },
      { "lth", _142_tri__rd_lth_a },
      { "lte", _142_tri__rd_lte_a },
      { "equ", _142_tri__rd_equ_a },
      { "gte", _142_tri__rd_gte_a },
      { "gth", _142_tri__rd_gth_a },
      {}
    };
    static u3j_harm _142_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _142_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _142_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _142_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _142_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _142_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _142_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _142_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _142_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _142_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _142_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _142_tri__rs_d[] =
    { { "add", _142_tri__rs_add_a },
      { "sub", _142_tri__rs_sub_a },
      { "mul", _142_tri__rs_mul_a },
      { "div", _142_tri__rs_div_a },
      { "sqt", _142_tri__rs_sqt_a },
      { "fma", _142_tri__rs_fma_a },
      { "lth", _142_tri__rs_lth_a },
      { "lte", _142_tri__rs_lte_a },
      { "equ", _142_tri__rs_equ_a },
      { "gte", _142_tri__rs_gte_a },
      { "gth", _142_tri__rs_gth_a },
      {}
    };

    static u3j_harm _142_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _142_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _142_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _142_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _142_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _142_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _142_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _142_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _142_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _142_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _142_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _142_tri__rq_d[] =
    { { "add", _142_tri__rq_add_a },
      { "sub", _142_tri__rq_sub_a },
      { "mul", _142_tri__rq_mul_a },
      { "div", _142_tri__rq_div_a },
      { "sqt", _142_tri__rq_sqt_a },
      { "fma", _142_tri__rq_fma_a },
      { "lth", _142_tri__rq_lth_a },
      { "lte", _142_tri__rq_lte_a },
      { "equ", _142_tri__rq_equ_a },
      { "gte", _142_tri__rq_gte_a },
      { "gth", _142_tri__rq_gth_a },
      {}
    };

    static u3j_harm _142_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static u3j_harm _142_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static u3j_harm _142_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static u3j_harm _142_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static u3j_harm _142_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static u3j_harm _142_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static u3j_harm _142_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static u3j_harm _142_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static u3j_harm _142_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static u3j_harm _142_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static u3j_harm _142_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static u3j_core _142_tri__rh_d[] =
    { { "add", _142_tri__rh_add_a },
      { "sub", _142_tri__rh_sub_a },
      { "mul", _142_tri__rh_mul_a },
      { "div", _142_tri__rh_div_a },
      { "sqt", _142_tri__rh_sqt_a },
      { "fma", _142_tri__rh_fma_a },
      { "lth", _142_tri__rh_lth_a },
      { "lte", _142_tri__rh_lte_a },
      { "equ", _142_tri__rh_equ_a },
      { "gte", _142_tri__rh_gte_a },
      { "gth", _142_tri__rh_gth_a },
      {}
    };

    static u3j_harm _142_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static u3j_core _142_tri__og_d[] =
    { { "raw", _142_tri__og_raw_a },
      {}
    };
   
    static u3j_harm _142_tri__sha_sha1_a[] = {{".2", u3we_sha1}, {}};
  static u3j_core _142_tri__sha_d[] =
    { { "sha1", _142_tri__sha_sha1_a },
      {}
    };

  static u3j_harm _142_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static u3j_harm _142_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static u3j_harm _142_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static u3j_harm _142_tri_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_core _142_tri_d[] =
  { { "qua", 0, _142_qua_d },

    { "cofl", 0, _142_tri__cofl_d },
    { "rd", 0, _142_tri__rd_d },
    { "rs", 0, _142_tri__rs_d },
    { "rq", 0, _142_tri__rq_d },
    { "rh", 0, _142_tri__rh_d },
    { "og", 0, _142_tri__og_d },
    { "sha", 0, _142_tri__sha_d },
    { "shax", _142_tri_shax_a },
    { "shay", _142_tri_shay_a },
    { "shas", _142_tri_shas_a },
    { "shal", _142_tri_shal_a },
    {}
  };

/* layer two
*/
  static u3j_harm _142_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static u3j_harm _142_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static u3j_harm _142_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static u3j_harm _142_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static u3j_harm _142_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static u3j_harm _142_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static u3j_harm _142_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static u3j_harm _142_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static u3j_harm _142_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static u3j_harm _142_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static u3j_harm _142_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static u3j_harm _142_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static u3j_harm _142_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static u3j_harm _142_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static u3j_harm _142_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  // static u3j_harm _142_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _142_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static u3j_harm _142_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

  static u3j_harm _142_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static u3j_harm _142_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static u3j_harm _142_two_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _142_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static u3j_harm _142_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static u3j_harm _142_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static u3j_harm _142_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static u3j_harm _142_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static u3j_harm _142_two_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _142_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static u3j_harm _142_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static u3j_harm _142_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static u3j_harm _142_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static u3j_harm _142_two_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
  static u3j_harm _142_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static u3j_harm _142_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
  static u3j_harm _142_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
  static u3j_harm _142_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
  static u3j_harm _142_two_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _142_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static u3j_harm _142_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static u3j_harm _142_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
  static u3j_harm _142_two_rev_a[] = {{".2", u3wc_rev, c3y}, {}};
  static u3j_harm _142_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static u3j_harm _142_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
  static u3j_harm _142_two_swp_a[] = {{".2", u3wc_swp, c3y}, {}};
  static u3j_harm _142_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
  static u3j_harm _142_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
  static u3j_harm _142_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};



    static u3j_harm _142_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static u3j_harm _142_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static u3j_harm _142_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static u3j_harm _142_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static u3j_harm _142_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    // static u3j_harm _142_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _142_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static u3j_harm _142_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static u3j_harm _142_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static u3j_harm _142_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static u3j_core _142_two__in_d[] =
    { { "bif", _142_two__in_bif_a },
      { "dif", _142_two__in_dif_a },
      { "gas", _142_two__in_gas_a },
      { "has", _142_two__in_has_a },
      { "mer", _142_two__in_mer_a },
      // { "int", _142_two__in_int_a },
      { "put", _142_two__in_put_a },
      { "tap", _142_two__in_tap_a },
      { "wyt", _142_two__in_wyt_a },
      { "uni", _142_two__in_uni_a },
      {}
    };

    static u3j_harm _142_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static u3j_harm _142_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static u3j_harm _142_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static u3j_harm _142_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static u3j_harm _142_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    // static u3j_harm _142_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _142_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static u3j_harm _142_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    // static u3j_harm _142_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static u3j_core _142_two__by_d[] =
    { { "bif", _142_two__by_bif_a },
      { "dif", _142_two__by_dif_a },
      { "gas", _142_two__by_gas_a },
      { "get", _142_two__by_get_a },
      { "has", _142_two__by_has_a },
      // { "int", _142_two__by_int_a },
      { "put", _142_two__by_put_a },
      { "tap", _142_two__by_tap_a },
      // { "uni", _142_two__by_uni_a },
      {}
    };

  static u3j_harm _142_two_cue_a[] = {{".2", u3we_cue}, {}};
  static u3j_harm _142_two_jam_a[] = {{".2", u3we_jam}, {}};
  static u3j_harm _142_two_mat_a[] = {{".2", u3we_mat}, {}};
  static u3j_harm _142_two_rub_a[] = {{".2", u3we_rub}, {}};

static u3j_core _142_two_d[] =
  { { "tri", 0, _142_tri_d },
    { "flop", _142_two_flop_a },
    { "lent", _142_two_lent_a },
    { "levy", _142_two_levy_a },
    { "lien", _142_two_lien_a },
    { "murn", _142_two_murn_a },
    { "need", _142_two_need_a },
    { "reap", _142_two_reap_a },
    { "reel", _142_two_reel_a },
    { "roll", _142_two_roll_a },
    { "skid", _142_two_skid_a },
    { "skim", _142_two_skim_a },
    { "skip", _142_two_skip_a },
    { "scag", _142_two_scag_a },
    { "slag", _142_two_slag_a },
    { "snag", _142_two_snag_a },
//  { "sort", _142_two_sort_a },
    { "turn", _142_two_turn_a },
    { "weld", _142_two_weld_a },

    { "bex", _142_two_bex_a },
    { "cat", _142_two_cat_a },
    { "can", _142_two_can_a },
    { "cap", _142_two_cap_a },
    { "con", _142_two_con_a },
    { "cue", _142_two_cue_a },
    { "cut", _142_two_cut_a },
    { "dis", _142_two_dis_a },
    { "dor", _142_two_dor_a },
    { "dvr", _142_two_dvr_a },
    { "end", _142_two_end_a },
    { "gor", _142_two_gor_a },
    { "hor", _142_two_hor_a },
    { "jam", _142_two_jam_a },
    { "lsh", _142_two_lsh_a },
    { "mas", _142_two_mas_a },
    { "mat", _142_two_mat_a },
    { "met", _142_two_met_a },
    { "mix", _142_two_mix_a },
    { "mug", _142_two_mug_a },
    { "muk", _142_two_muk_a },
    { "rap", _142_two_rap_a },
    { "rep", _142_two_rep_a },
    { "rev", _142_two_rev_a },
    { "rip", _142_two_rip_a },
    { "rsh", _142_two_rsh_a },
    { "swp", _142_two_swp_a },
    { "rub", _142_two_rub_a },
    { "peg", _142_two_peg_a },
    { "pow", _142_two_pow_a },
    { "sqt", _142_two_sqt_a },
    { "vor", _142_two_vor_a },
    { "xeb", _142_two_xeb_a },

    { "by", 0, _142_two__by_d },
    { "in", 0, _142_two__in_d },
  };


/* layer one
*/
  static u3j_harm _142_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static u3j_harm _142_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static u3j_harm _142_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static u3j_harm _142_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static u3j_harm _142_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static u3j_harm _142_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static u3j_harm _142_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static u3j_harm _142_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static u3j_harm _142_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static u3j_harm _142_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static u3j_harm _142_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

  static u3j_harm _142_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static u3j_harm _142_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static u3j_harm _142_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};

static u3j_core _142_one_d[] =
  { { "two", 0, _142_two_d },
    { "add", _142_one_add_a },
    { "dec", _142_one_dec_a },
    { "div", _142_one_div_a },
    { "dvr", _142_one_dvr_a },
    { "gte", _142_one_gte_a },
    { "gth", _142_one_gth_a },
    { "lte", _142_one_lte_a },
    { "lth", _142_one_lth_a },
    { "mod", _142_one_mod_a },
    { "mul", _142_one_mul_a },
    { "sub", _142_one_sub_a },

    { "cap", _142_one_cap_a },
    { "mas", _142_one_mas_a },
    { "peg", _142_one_peg_a },

    {}
  };

u3j_core _k142_d[] =
  { { "one", 0, _142_one_d },
    {}
  };

static u3j_core _d[] = {
  { "k141", 0, _k141_d},
  { "k142", 0, _k142_d},
  {}
};

u3j_dash 
u3j_Dash = {
  _d,
  0,
  0
};
