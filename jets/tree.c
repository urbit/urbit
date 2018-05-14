/* j/tree.c
*/
#include "all.h"

  static u3j_harm _143_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
  static u3j_harm _143_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
  static u3j_harm _143_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
  static u3j_harm _143_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
  static u3j_harm _143_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
  static u3j_harm _143_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};

  static u3j_harm _143_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
  static u3j_harm _143_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
  static u3j_harm _143_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
  static u3j_harm _143_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
  static u3j_harm _143_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
  static u3j_harm _143_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};

  static u3j_core _143_hex_aes_ecba_d[] =
    { { "en", 7, _143_hex_aes_ecba_en_a },
      { "de", 7, _143_hex_aes_ecba_de_a },
      {}
    };

  static u3j_core _143_hex_aes_ecbb_d[] =
    { { "en", 7, _143_hex_aes_ecbb_en_a },
      { "de", 7, _143_hex_aes_ecbb_de_a },
      {}
    };

  static u3j_core _143_hex_aes_ecbc_d[] =
    { { "en", 7, _143_hex_aes_ecbc_en_a },
      { "de", 7, _143_hex_aes_ecbc_de_a },
      {}
    };

  static u3j_core _143_hex_aes_cbca_d[] =
    { { "en", 7, _143_hex_aes_cbca_en_a },
      { "de", 7, _143_hex_aes_cbca_de_a },
      {}
    };

  static u3j_core _143_hex_aes_cbcb_d[] =
    { { "en", 7, _143_hex_aes_cbcb_en_a },
      { "de", 7, _143_hex_aes_cbcb_de_a },
      {}
    };

  static u3j_core _143_hex_aes_cbcc_d[] =
    { { "en", 7, _143_hex_aes_cbcc_en_a },
      { "de", 7, _143_hex_aes_cbcc_de_a },
      {}
    };

  static u3j_core _143_hex_aes_d[] =
    { { "ecba", 7, 0, _143_hex_aes_ecba_d },
      { "ecbb", 7, 0, _143_hex_aes_ecbb_d },
      { "ecbc", 7, 0, _143_hex_aes_ecbc_d },
      { "cbca", 7, 0, _143_hex_aes_cbca_d },
      { "cbcb", 7, 0, _143_hex_aes_cbcb_d },
      { "cbcc", 7, 0, _143_hex_aes_cbcc_d },
      {}
    };

  static u3j_harm _143_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static u3j_core _143_hex_down_d[] =
      { { "mark", 7, _143_hex_down_mark_a },
        {}
      };

  static u3j_harm _143_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static u3j_harm _143_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static u3j_harm _143_hex_lune_a[] = {{".2", u3we_lune}, {}};

  static u3j_harm _143_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
  static u3j_harm _143_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
  static u3j_harm _143_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
  static u3j_harm _143_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};

  static u3j_core _143_hex_coed__ed_d[] =
    { { "sign", 7, _143_hex_coed__ed_sign_a },
      { "puck", 7, _143_hex_coed__ed_puck_a },
      { "veri", 7, _143_hex_coed__ed_veri_a },
      { "shar", 7, _143_hex_coed__ed_shar_a },
      {}
    };
static u3j_core _143_hex_coed_d[] =
  { { "ed", 3, 0, _143_hex_coed__ed_d },
    {}
  };

static c3_c* _143_hex_ha[] = {
  "2fd207b3",
  0,
};
static u3j_core _143_hex_d[] =
  { { "down", 31337, 0, _143_hex_down_d },
    { "lore", 7, _143_hex_lore_a },
    { "loss", 7, _143_hex_loss_a },
    { "lune", 7, _143_hex_lune_a },
    { "coed", 31337, 0, _143_hex_coed_d },
    { "aes", 31337, 0, _143_hex_aes_d },
    {}
  };

/* layer five
*/
  static u3j_harm _143_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static c3_c* _143_pen_cell_ha[] = {
    "6bfc1bca",
    0,
  };
  static u3j_harm _143_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static u3j_harm _143_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static u3j_harm _143_pen_core_a[] = {{".2", u3wf_core}, {}};
  static c3_c* _143_pen_core_ha[] = {
    "5c37f17b",
    0,
  };
  static u3j_harm _143_pen_help_a[] = {{".2", u3wf_help}, {}};
  static u3j_harm _143_pen_face_a[] = {{".2", u3wf_face}, {}};
  static c3_c* _143_pen_face_ha[] = {
    "742c24f0",
    0,
  };
  static u3j_harm _143_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static u3j_harm _143_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static u3j_harm _143_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static u3j_harm _143_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static u3j_harm _143_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static c3_c* _143_pen_fork_ha[] = {
    "6c3239f5",
    0,
  };

  static u3j_harm _143_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static c3_c* _143_pen_hike_ha[] = {
    "65731f61",
    0,
  };
  static u3j_harm _143_pen_look_a[] = {{".2", u3wf_look}, {}};
  static u3j_harm _143_pen_loot_a[] = {{".2", u3wf_loot}, {}};

    static u3j_harm _143_pen__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
    static u3j_harm _143_pen__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
    static u3j_harm _143_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    // static u3j_harm _143_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _143_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static c3_c* _143_pen__ut_fond_ha[] = {
      "32d3327a",
      0,
    };
    static u3j_harm _143_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static u3j_harm _143_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static u3j_harm _143_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static c3_c* _143_pen__ut_mint_ha[] = {
      "48723fb5",
      0,
    };
    static u3j_harm _143_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static u3j_harm _143_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static c3_c* _143_pen__ut_nest_ha[] = {
      "4512a8f4",
      0,
    };
    static u3j_harm _143_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static c3_c* _143_pen__ut_peek_ha[] = {
      "3c631b7d",
      0,
    };
    static u3j_harm _143_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static c3_c* _143_pen__ut_play_ha[] = {
      "e276a30",
      0,
    };
    static u3j_harm _143_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static u3j_harm _143_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static c3_c* _143_pen__ut_toss_ha[] = {
      "54362dd",
      0,
    };
    static u3j_harm _143_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};

  static c3_c* _143_pen__ut_ha[] = {
    "c2cda56",
    0,
  };
  static u3j_hood _143_pen__ut_ho[] = {
    { "fan",  28, c3n },
    { "rib",  58, c3n },
    { "vet", 118, c3n },
    { "fab", 119, c3n },

    { "blow",   49131 },
    { "burn",    3051 },
    { "busk", 0x5ff57 },
    { "buss",      94 },
    { "crop",       4 },
    { "duck",   48087 },
    { "dune",    1524 },
    { "dunk",     763 },
    { "epla",   12283 },
    { "emin",    5998 },
    { "emul",      86 },
    { "felt",   24046 },
    { "fine",    3004 },
    { "fire",     700 },
    { "fish",    1498 },
    { "fond",      92 },
    { "fund",    1402 },
    { "funk", 0x1757c, c3y, 31 },
    { "fuse",    1534 },
    { "gain",     748 },
    { "lose",      22 },
    { "mint", 0x17fd4 },
    { "moot",    2807 },
    { "mull",   11995 },
    { "nest",     380 },
    { "peel",    5999 },
    { "play",   12022 },
    { "peek",    5996 },
    { "repo",    3050 },
    { "rest",     382 },
    { "tack",     351 },
    { "toss",   24042 },
    { "wrap",    6143 },
    {},
  };
  static u3j_core _143_pen__ut_d[] =
    { 
      { "burn", 7, _143_pen__ut_burn_a },
      { "conk", 7, _143_pen__ut_conk_a },
      { "crop", 7, _143_pen__ut_crop_a },
      { "fond", 7, _143_pen__ut_fond_a, 0, _143_pen__ut_fond_ha },
    //  { "fire", 7, _143_pen__ut_fire_a },
      { "fish", 7, _143_pen__ut_fish_a },
      { "fuse", 7, _143_pen__ut_fuse_a },
      { "mint", 7, _143_pen__ut_mint_a, 0, _143_pen__ut_mint_ha },
      { "mull", 7, _143_pen__ut_mull_a },
      { "nest", 7, _143_pen__ut_nest_a, 0, _143_pen__ut_nest_ha },
      { "peek", 7, _143_pen__ut_peek_a, 0, _143_pen__ut_peek_ha },
      { "play", 7, _143_pen__ut_play_a, 0, _143_pen__ut_play_ha },
      { "rest", 7, _143_pen__ut_rest_a },
      { "toss", 7, _143_pen__ut_toss_a, 0, _143_pen__ut_toss_ha },
      { "wrap", 7, _143_pen__ut_wrap_a },
      {}
    };
  static u3j_harm _143_pen__ut_a[] = 
    { {"burn", u3wfu_burn},
      {"repo", u3wfu_repo},
      {}
    };

#if 0
  static u3j_harm _143_pen__ap_a[] = 
    { {"open", u3wfp_open},
      {"rake", u3wfp_rake},
      {}
    };
  static u3j_harm _143_pen__al_a[] = 
    { {"bunt", u3wfl_bunt},
      {"whip", u3wfl_whip},
      {}
    };
#endif


static c3_c* _143_pen_ha[] = {
  "4fcd534d",
  0,
};
static u3j_hood _143_pen_ho[] = {
  { "al", 350   },
  { "ap", 24412 },
  { "ut", 11262 },
  {},
};
static u3j_core _143_pen_d[] =
  { { "hex", 3, 0, _143_hex_d, _143_hex_ha },

    { "cell", 7, _143_pen_cell_a, 0, _143_pen_cell_ha },
    { "comb", 7, _143_pen_comb_a },
    { "cons", 7, _143_pen_cons_a },
    { "core", 7, _143_pen_core_a, 0, _143_pen_core_ha },
    { "face", 7, _143_pen_face_a, 0, _143_pen_face_ha },
    { "fitz", 7, _143_pen_fitz_a },
    { "flan", 7, _143_pen_flan_a },
    { "flip", 7, _143_pen_flip_a },
    { "flor", 7, _143_pen_flor_a },
    { "fork", 7, _143_pen_fork_a, 0, _143_pen_fork_ha },
    { "help", 7, _143_pen_help_a },
    { "hike", 7, _143_pen_hike_a, 0, _143_pen_hike_ha },
    { "look", 7, _143_pen_look_a },
    { "loot", 7, _143_pen_loot_a },

    // { "ap", _143_pen__ap_a },
    // { "al", _143_pen__al_a },
    { "ut", 15, _143_pen__ut_a, _143_pen__ut_d, _143_pen__ut_ha, _143_pen__ut_ho },
    {}
  };

/* layer four
*/
  static u3j_harm _143_qua_trip_a[] = {{".2", u3we_trip}, {}};
  static c3_c* _143_qua_trip_ha[] = {
    "2988e186",
    0,
  };

  static u3j_harm _143_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static u3j_harm _143_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static u3j_harm _143_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static u3j_harm _143_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
    static u3j_core _143_qua__po_d[] =
      { { "ind", 7, _143_qua__po_ind_a },
        { "ins", 7, _143_qua__po_ins_a },
        { "tod", 7, _143_qua__po_tod_a },
        { "tos", 7, _143_qua__po_tos_a },
        {}
      };

  static u3j_harm _143_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static c3_c* _143_qua__bend_ha[] = {
      "36e4e2fb",
      0,
    };
    static c3_c* _143_qua__bend_fun_ha[] = {
      "708e40b",
      0,
    };
    static u3j_core _143_qua__bend_d[] =
      { { "fun", 7, _143_qua__bend_fun_a, 0, _143_qua__bend_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static c3_c* _143_qua__cold_ha[] = {
      "7881e709",
      0,
    };
    static c3_c* _143_qua__cold_fun_ha[] = {
      "7a6d3682",
      0,
    };
    static u3j_core _143_qua__cold_d[] =
      { { "fun", 7, _143_qua__cold_fun_a, 0, _143_qua__cold_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static c3_c* _143_qua__cook_ha[] = {
      "7b58b2c3",
      0,
    };
    static c3_c* _143_qua__cook_fun_ha[] = {
      "725a3063",
      0,
    };
    static u3j_core _143_qua__cook_d[] =
      { { "fun", 7, _143_qua__cook_fun_a, 0, _143_qua__cook_fun_ha },
        {}
      };

  static u3j_harm _143_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static c3_c* _143_qua__comp_ha[] = {
      "ff1d7ca",
      0,
    };
    static c3_c* _143_qua__comp_fun_ha[] = {
      "5a71faf5",
      0,
    };
    static u3j_core _143_qua__comp_d[] =
      { { "fun", 7, _143_qua__comp_fun_a, 0, _143_qua__comp_fun_ha },
        {}
      };

  static u3j_harm _143_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static c3_c* _143_qua__easy_ha[] = {
      "7c0caf1b",
      0,
    };
    static c3_c* _143_qua__easy_fun_ha[] = {
      "13cb0749",
      0,
    };
    static u3j_core _143_qua__easy_d[] =
      { { "fun", 7, _143_qua__easy_fun_a, 0, _143_qua__easy_fun_ha },
        {}
      };

  static u3j_harm _143_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static u3j_core _143_qua__glue_d[] =
      { { "fun", 7, _143_qua__glue_fun_a },
        {}
      };

  static u3j_harm _143_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static c3_c* _143_qua__here_ha[] = {
      "6392963e",
      0,
    };
    static c3_c* _143_qua__here_fun_ha[] = {
      "72522b7d",
      0,
    };
    static u3j_core _143_qua__here_d[] =
      { { "fun", 7, _143_qua__here_fun_a, 0, _143_qua__here_fun_ha },
        {}
      };

  static u3j_harm _143_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static c3_c* _143_qua__just_ha[] = {
      "295a2fbe",
      0,
    };
    static c3_c* _143_qua__just_fun_ha[] = {
      "50d2f368",
      0,
    };
    static u3j_core _143_qua__just_d[] =
      { { "fun", 7, _143_qua__just_fun_a, 0, _143_qua__just_fun_ha },
        {}
      };

  static u3j_harm _143_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static c3_c* _143_qua__mask_ha[] = {
      "25f3cc5e",
      0,
    };
    static c3_c* _143_qua__mask_fun_ha[] = {
      "5fb25be8",
      0,
    };
    static u3j_core _143_qua__mask_d[] =
      { { "fun", 7, _143_qua__mask_fun_a, 0, _143_qua__mask_fun_ha },
        {}
      };

  static u3j_harm _143_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static c3_c* _143_qua__shim_ha[] = {
      "53e64202",
      0,
    };
    static c3_c* _143_qua__shim_fun_ha[] = {
      "61c4cfac",
      0,
    };
    static u3j_core _143_qua__shim_d[] =
      { { "fun", 7, _143_qua__shim_fun_a, 0, _143_qua__shim_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static c3_c* _143_qua__stag_ha[] = {
      "50e7e0d9",
      0,
    };
    static c3_c* _143_qua__stag_fun_ha[] = {
      "5347a0fe",
      0,
    };
    static u3j_core _143_qua__stag_d[] =
      { { "fun", 7, _143_qua__stag_fun_a, 0, _143_qua__stag_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static c3_c* _143_qua__stew_ha[] = {
      "23a21615",
      0,
    };
    static c3_c* _143_qua__stew_fun_ha[] = {
      "74d4c941",
      0,
    };
    static u3j_core _143_qua__stew_d[] =
      { { "fun", 31, _143_qua__stew_fun_a, 0, _143_qua__stew_fun_ha },
        {}
      };
 
  static u3j_harm _143_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static c3_c* _143_qua__stir_ha[] = {
      "1e2b7159",
      0,
    };
    static c3_c* _143_qua__stir_fun_ha[] = {
      "261f565a",
      0,
    };
    static u3j_core _143_qua__stir_d[] =
      { { "fun", 7, _143_qua__stir_fun_a, 0, _143_qua__stir_fun_ha },
        {}
      };

  static u3j_harm _143_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _143_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static c3_c* _143_qua__plug_ha[] = {
    "11b6a2f8",
    0,
  };
  static u3j_harm _143_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static c3_c* _143_qua__pose_ha[] = {
    "6fb235ed",
    0,
  };
  static u3j_harm _143_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _143_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static u3j_harm _143_qua_mule_a[] = {{".2", u3we_mule}, {}};

static c3_c* _143_qua_ha[] = {
  "7f2af46e",
  0,
};
static u3j_hood _143_qua_ho[] = {
  { "mute", 0x17dfc },
  { "show", 0x2fbbaba },
  {},
};
static u3j_core _143_qua_d[] =
  { { "pen", 3, 0, _143_pen_d, _143_pen_ha, _143_pen_ho },

    { "po", 7, 0, _143_qua__po_d },

    { "trip", 7, _143_qua_trip_a, 0, _143_qua_trip_ha },

    { "bend", 7, 0, _143_qua__bend_d, _143_qua__bend_ha },
    { "cold", 7, 0, _143_qua__cold_d, _143_qua__cold_ha },
    { "comp", 7, 0, _143_qua__comp_d, _143_qua__comp_ha },
    { "cook", 7, 0, _143_qua__cook_d, _143_qua__cook_ha },
    { "easy", 7, 0, _143_qua__easy_d, _143_qua__easy_ha },
    { "glue", 7, 0, _143_qua__glue_d },
    { "here", 7, 0, _143_qua__here_d, _143_qua__here_ha },
    { "just", 7, 0, _143_qua__just_d, _143_qua__just_ha },
    { "mask", 7, 0, _143_qua__mask_d, _143_qua__mask_ha },
    { "shim", 7, 0, _143_qua__shim_d, _143_qua__shim_ha },
    { "stag", 7, 0, _143_qua__stag_d, _143_qua__stag_ha },
    { "stew", 7, 0, _143_qua__stew_d, _143_qua__stew_ha },
    { "stir", 7, 0, _143_qua__stir_d, _143_qua__stir_ha },

    { "pfix", 7, _143_qua_pfix_a },
    { "plug", 7, _143_qua_plug_a, 0, _143_qua__plug_ha },
    { "pose", 7, _143_qua_pose_a, 0, _143_qua__pose_ha },
    { "sfix", 7, _143_qua_sfix_a },

    { "mink", 7, _143_qua_mink_a },
    { "mule", 7, _143_qua_mule_a },
    {}
  };

/* layer three
*/
    static u3j_harm _143_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static u3j_harm _143_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static u3j_core _143_tri__cofl_d[] =
    { { "drg", 7, _143_tri__cofl__drg_a },
      { "lug", 7, _143_tri__cofl__lug_a },
      {}
    };

    static u3j_harm _143_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static u3j_harm _143_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static u3j_harm _143_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static u3j_harm _143_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static u3j_harm _143_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static u3j_harm _143_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static u3j_harm _143_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static u3j_harm _143_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static u3j_harm _143_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static u3j_harm _143_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static u3j_harm _143_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static u3j_core _143_tri__rd_d[] =
    { { "add", 7, _143_tri__rd_add_a },
      { "sub", 7, _143_tri__rd_sub_a },
      { "mul", 7, _143_tri__rd_mul_a },
      { "div", 7, _143_tri__rd_div_a },
      { "sqt", 7, _143_tri__rd_sqt_a },
      { "fma", 7, _143_tri__rd_fma_a },
      { "lth", 7, _143_tri__rd_lth_a },
      { "lte", 7, _143_tri__rd_lte_a },
      { "equ", 7, _143_tri__rd_equ_a },
      { "gte", 7, _143_tri__rd_gte_a },
      { "gth", 7, _143_tri__rd_gth_a },
      {}
    };
    static u3j_harm _143_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static u3j_harm _143_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static u3j_harm _143_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static u3j_harm _143_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static u3j_harm _143_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static u3j_harm _143_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static u3j_harm _143_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static u3j_harm _143_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static u3j_harm _143_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static u3j_harm _143_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static u3j_harm _143_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static u3j_core _143_tri__rs_d[] =
    { { "add", 7, _143_tri__rs_add_a },
      { "sub", 7, _143_tri__rs_sub_a },
      { "mul", 7, _143_tri__rs_mul_a },
      { "div", 7, _143_tri__rs_div_a },
      { "sqt", 7, _143_tri__rs_sqt_a },
      { "fma", 7, _143_tri__rs_fma_a },
      { "lth", 7, _143_tri__rs_lth_a },
      { "lte", 7, _143_tri__rs_lte_a },
      { "equ", 7, _143_tri__rs_equ_a },
      { "gte", 7, _143_tri__rs_gte_a },
      { "gth", 7, _143_tri__rs_gth_a },
      {}
    };

    static u3j_harm _143_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static u3j_harm _143_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static u3j_harm _143_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static u3j_harm _143_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static u3j_harm _143_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static u3j_harm _143_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static u3j_harm _143_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static u3j_harm _143_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static u3j_harm _143_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static u3j_harm _143_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static u3j_harm _143_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static u3j_core _143_tri__rq_d[] =
    { { "add", 7, _143_tri__rq_add_a },
      { "sub", 7, _143_tri__rq_sub_a },
      { "mul", 7, _143_tri__rq_mul_a },
      { "div", 7, _143_tri__rq_div_a },
      { "sqt", 7, _143_tri__rq_sqt_a },
      { "fma", 7, _143_tri__rq_fma_a },
      { "lth", 7, _143_tri__rq_lth_a },
      { "lte", 7, _143_tri__rq_lte_a },
      { "equ", 7, _143_tri__rq_equ_a },
      { "gte", 7, _143_tri__rq_gte_a },
      { "gth", 7, _143_tri__rq_gth_a },
      {}
    };

    static u3j_harm _143_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static u3j_harm _143_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static u3j_harm _143_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static u3j_harm _143_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static u3j_harm _143_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static u3j_harm _143_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static u3j_harm _143_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static u3j_harm _143_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static u3j_harm _143_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static u3j_harm _143_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static u3j_harm _143_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static u3j_core _143_tri__rh_d[] =
    { { "add", 7, _143_tri__rh_add_a },
      { "sub", 7, _143_tri__rh_sub_a },
      { "mul", 7, _143_tri__rh_mul_a },
      { "div", 7, _143_tri__rh_div_a },
      { "sqt", 7, _143_tri__rh_sqt_a },
      { "fma", 7, _143_tri__rh_fma_a },
      { "lth", 7, _143_tri__rh_lth_a },
      { "lte", 7, _143_tri__rh_lte_a },
      { "equ", 7, _143_tri__rh_equ_a },
      { "gte", 7, _143_tri__rh_gte_a },
      { "gth", 7, _143_tri__rh_gth_a },
      {}
    };

    static u3j_harm _143_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
    static c3_c* _143_tri__og_raw_ha[] = {
      "2b65a131",
      0,
    };

  static c3_c* _143_tri__og_ha[] = {
    "65c7f2b6",
    0,
  };
  static u3j_core _143_tri__og_d[] =
    { { "raw", 7, _143_tri__og_raw_a, 0, _143_tri__og_raw_ha },
      {}
    };

  static u3j_harm _143_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static c3_c* _143_tri_shax_ha[] = {
    "2d81b3eb",
    0,
  };
  static u3j_harm _143_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static c3_c* _143_tri_shay_ha[] = {
    "3081f15b",
    0,
  };
/* XX: no hint
  static u3j_harm _143_tri_shas_a[] = {{".2", u3we_shas}, {}};
  static c3_c* _143_tri_shas_ha[] = {
    "",
    0,
  };
*/
  static u3j_harm _143_tri_shal_a[] = {{".2", u3we_shal}, {}};
  static c3_c* _143_tri_shal_ha[] = {
    "63ea0b00",
    0,
  };

static c3_c* _143_tri_ha[] = {
  "6cc318e2",
  0,
};
static u3j_core _143_tri_d[] =
  { { "qua", 3, 0, _143_qua_d, _143_qua_ha, _143_qua_ho },
    { "cofl", 7, 0, _143_tri__cofl_d },
    { "rd", 7, 0, _143_tri__rd_d },
    { "rs", 7, 0, _143_tri__rs_d },
    { "rq", 7, 0, _143_tri__rq_d },
    { "rh", 7, 0, _143_tri__rh_d },
    { "og", 7, 0, _143_tri__og_d, _143_tri__og_ha },
    { "shax", 7, _143_tri_shax_a, 0, _143_tri_shax_ha },
    { "shay", 7, _143_tri_shay_a, 0, _143_tri_shay_ha },
//    { "shas", 7, _143_tri_shas_a, 0, _143_tri_shas_ha },
    { "shal", 7, _143_tri_shal_a, 0, _143_tri_shal_ha },
    {}
  };

/* layer two
*/
  static u3j_harm _143_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
  static c3_c* _143_two_flop_ha[] = {
    "41e43e64",
    0,
  };
  static u3j_harm _143_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static c3_c* _143_two_lent_ha[] = {
    "1dfd0931",
    0,
  };
  static u3j_harm _143_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static c3_c* _143_two_levy_ha[] = {
    "35460246",
    0,
  };
  static u3j_harm _143_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static c3_c* _143_two_lien_ha[] = {
    "26fe83d7",
    0,
  };
  static u3j_harm _143_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static c3_c* _143_two_murn_ha[] = {
    "7267235b",
    0,
  };
/* XX: no hint
  static u3j_harm _143_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
  static c3_c* _143_two_need_ha[] = {
    "",
    0,
  };
*/
  static u3j_harm _143_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
  static c3_c* _143_two_reap_ha[] = {
    "1808a81d",
    0,
  };
  static u3j_harm _143_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static c3_c* _143_two_reel_ha[] = {
    "660c4ee9",
    0,
  };
  static u3j_harm _143_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static c3_c* _143_two_roll_ha[] = {
    "3c16e592",
    0,
  };
  static u3j_harm _143_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static c3_c* _143_two_skid_ha[] = {
    "3dc45cc",
    0,
  };
  static u3j_harm _143_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static c3_c* _143_two_skim_ha[] = {
    "36892702",
    0,
  };
  static u3j_harm _143_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static c3_c* _143_two_skip_ha[] = {
    "580714ad",
    0,
  };
  static u3j_harm _143_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static c3_c* _143_two_scag_ha[] = {
    "6d492573",
    0,
  };
  static u3j_harm _143_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static c3_c* _143_two_slag_ha[] = {
    "6a9fe551",
    0,
  };
  static u3j_harm _143_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  static c3_c* _143_two_snag_ha[] = {
    "da6440d",
    0,
  };
  // static u3j_harm _143_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _143_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static c3_c* _143_two_turn_ha[] = {
    "6edb42",
    0,
  };
  static u3j_harm _143_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
  static c3_c* _143_two_weld_ha[] = {
    "752d9421",
    0,
  };

  static u3j_harm _143_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static c3_c* _143_two_bex_ha[] = {
    "6c223bad",
    0,
  };
  static u3j_harm _143_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static c3_c* _143_two_can_ha[] = {
    "6039dee0",
    0,
  };
  static u3j_harm _143_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static c3_c* _143_two_cat_ha[] = {
    "408a97e8",
    0,
  };
  static u3j_harm _143_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static c3_c* _143_two_con_ha[] = {
    "49ff881",
    0,
  };
  static u3j_harm _143_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static c3_c* _143_two_cut_ha[] = {
    "2c8d7897",
    0,
  };
  static u3j_harm _143_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static c3_c* _143_two_dis_ha[] = {
    "135fb298",
    0,
  };
  static u3j_harm _143_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static c3_c* _143_two_dor_ha[] = {
    "5c0d99f7",
    0,
  };
  static u3j_harm _143_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static c3_c* _143_two_end_ha[] = {
    "3fb28e44",
    0,
  };
  static u3j_harm _143_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static c3_c* _143_two_gor_ha[] = {
    "17eba2b9",
    0,
  };
  static u3j_harm _143_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static c3_c* _143_two_hor_ha[] = {
    "4a7e5d16",
    0,
  };
  static u3j_harm _143_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static c3_c* _143_two_lsh_ha[] = {
    "41b3a4c7",
    0,
  };
  static u3j_harm _143_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static c3_c* _143_two_met_ha[] = {
    "51509f40",
    0,
  };
  static u3j_harm _143_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
  static c3_c* _143_two_mix_ha[] = {
    "76bbeb8a",
    0,
  };
  static u3j_harm _143_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
  static c3_c* _143_two_mug_ha[] = {
    "11404a2",
    0,
  };
  static u3j_harm _143_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
  static c3_c* _143_two_muk_ha[] = {
    "482e58af",
    0,
  };
  static u3j_harm _143_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static c3_c* _143_two_pow_ha[] = {
    "4a70ddfe",
    0,
  };
  static u3j_harm _143_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static c3_c* _143_two_rap_ha[] = {
    "7a6875bb",
    0,
  };
  static u3j_harm _143_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
  static c3_c* _143_two_rep_ha[] = {
    "53e80eb3",
    0,
  };
  static u3j_harm _143_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static c3_c* _143_two_rip_ha[] = {
    "54970200",
    0,
  };
  static u3j_harm _143_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
  static c3_c* _143_two_rsh_ha[] = {
    "7c0c8df2",
    0,
  };
  static u3j_harm _143_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
  static c3_c* _143_two_sqt_ha[] = {
    "50929e0b",
    0,
  };
  static u3j_harm _143_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
  static c3_c* _143_two_vor_ha[] = {
    "2b05b267",
    0,
  };
  static u3j_harm _143_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static c3_c* _143_two_xeb_ha[] = {
    "5e2bcea8",
    0,
  };

    static u3j_harm _143_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static c3_c* _143_two__in_bif_ha[] = {
      "16e17df9",
      0,
    };
    static u3j_harm _143_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static c3_c* _143_two__in_dif_ha[] = {
      "4ced9091",
      0,
    };
    static u3j_harm _143_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static c3_c* _143_two__in_gas_ha[] = {
      "4dbdbab4",
      0,
    };
    static u3j_harm _143_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static c3_c* _143_two__in_has_ha[] = {
      "39a46ced",
      0,
    };
/* XX: mer doesn't exist any more?
    static u3j_harm _143_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
    static c3_c* _143_two__in_mer_ha[] = {
      "",
      0,
    };
*/
    // static u3j_harm _143_two__in_int_a[] = {{".2", u3wdi_int}, {}};
    static u3j_harm _143_two__in_put_a[] = {{".2", u3wdi_put}, {}};
    static c3_c* _143_two__in_put_ha[] = {
      "57cf34a9",
      0,
    };
    static u3j_harm _143_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static c3_c* _143_two__in_tap_ha[] = {
      "70881f1a",
      0,
    };
    static u3j_harm _143_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static c3_c* _143_two__in_wyt_ha[] = {
      "71887a13",
      0,
    };
    static u3j_harm _143_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
    static c3_c* _143_two__in_uni_ha[] = {
      "26e34913",
      0,
    };

  static c3_c* _143_two__in_ha[] = {
    "51074d19",
    0,
  };
  static u3j_core _143_two__in_d[] =
    { { "bif", 7, _143_two__in_bif_a, 0, _143_two__in_bif_ha },
      { "dif", 7, _143_two__in_dif_a, 0, _143_two__in_dif_ha },
      { "gas", 7, _143_two__in_gas_a, 0, _143_two__in_gas_ha },
      { "has", 7, _143_two__in_has_a, 0, _143_two__in_has_ha },
      // { "mer", 7, _143_two__in_mer_a },
      // { "int", 7, _143_two__in_int_a },
      { "put", 7, _143_two__in_put_a, 0, _143_two__in_put_ha },
      { "tap", 7, _143_two__in_tap_a, 0, _143_two__in_tap_ha },
      { "wyt", 3, _143_two__in_wyt_a, 0, _143_two__in_wyt_ha },
      { "uni", 7, _143_two__in_uni_a, 0, _143_two__in_uni_ha },
      {}
    };

    static u3j_harm _143_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
    static c3_c* _143_two__by_bif_ha[] = {
      "4e6aedd2",
      0,
    };
    static u3j_harm _143_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static c3_c* _143_two__by_dif_ha[] = {
      "1388fad7",
      0,
    };
    static u3j_harm _143_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static c3_c* _143_two__by_gas_ha[] = {
      "5a8a7e7d",
      0,
    };
    static u3j_harm _143_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static c3_c* _143_two__by_get_ha[] = {
      "ece3b4d",
      0,
    };
    static u3j_harm _143_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    static c3_c* _143_two__by_has_ha[] = {
      "373d538d",
      0,
    };
    // static u3j_harm _143_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _143_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static c3_c* _143_two__by_put_ha[] = {
      "2c511da5",
      0,
    };
    static u3j_harm _143_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    static c3_c* _143_two__by_tap_ha[] = {
      "70881f1a",
      0,
    };
    // static u3j_harm _143_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};

  static c3_c* _143_two__by_ha[] = {
    "76d3df01",
    0,
  };
  static u3j_core _143_two__by_d[] =
    { { "bif", 7, _143_two__by_bif_a, 0, _143_two__by_bif_ha },
      { "dif", 7, _143_two__by_dif_a, 0, _143_two__by_dif_ha },
      { "gas", 7, _143_two__by_gas_a, 0, _143_two__by_gas_ha },
      { "get", 7, _143_two__by_get_a, 0, _143_two__by_get_ha },
      { "has", 7, _143_two__by_has_a, 0, _143_two__by_has_ha },
      // { "int", 7, _143_two__by_int_a },
      { "put", 7, _143_two__by_put_a, 0, _143_two__by_put_ha },
      { "tap", 7, _143_two__by_tap_a, 0, _143_two__by_tap_ha },
      // { "uni", _143_two__by_uni_a },
      {}
    };

  static u3j_harm _143_two_cue_a[] = {{".2", u3we_cue}, {}};
  static c3_c* _143_two_cue_ha[] = {
    "6f20da6b",
    0,
  };
  static u3j_harm _143_two_jam_a[] = {{".2", u3we_jam}, {}};
  static c3_c* _143_two_jam_ha[] = {
    "4103997f",
    0,
  };
  static u3j_harm _143_two_mat_a[] = {{".2", u3we_mat}, {}};
  static c3_c* _143_two_mat_ha[] = {
    "1a2c351b",
    0,
  };
  static u3j_harm _143_two_rub_a[] = {{".2", u3we_rub}, {}};
  static c3_c* _143_two_rub_ha[] = {
    "56b1242e",
    0,
  };

static c3_c* _143_two_ha[] = {
  "613c4d6d",
  0,
};
static u3j_core _143_two_d[] =
  { { "tri", 3, 0, _143_tri_d, _143_tri_ha },
    { "flop", 7, _143_two_flop_a, 0, _143_two_flop_ha },
    { "lent", 7, _143_two_lent_a, 0, _143_two_lent_ha },
    { "levy", 7, _143_two_levy_a, 0, _143_two_levy_ha },
    { "lien", 7, _143_two_lien_a, 0, _143_two_lien_ha },
    { "murn", 7, _143_two_murn_a, 0, _143_two_murn_ha },
//    { "need", 7, _143_two_need_a, 0, _143_two_need_ha },
    { "reap", 7, _143_two_reap_a, 0, _143_two_reap_ha },
    { "reel", 7, _143_two_reel_a, 0, _143_two_reel_ha },
    { "roll", 7, _143_two_roll_a, 0, _143_two_roll_ha },
    { "skid", 7, _143_two_skid_a, 0, _143_two_skid_ha },
    { "skim", 7, _143_two_skim_a, 0, _143_two_skim_ha },
    { "skip", 7, _143_two_skip_a, 0, _143_two_skip_ha },
    { "scag", 7, _143_two_scag_a, 0, _143_two_scag_ha },
    { "slag", 7, _143_two_slag_a, 0, _143_two_slag_ha },
    { "snag", 7, _143_two_snag_a, 0, _143_two_snag_ha },
//  { "sort", 7, _143_two_sort_a },
    { "turn", 7, _143_two_turn_a, 0, _143_two_turn_ha },
    { "weld", 7, _143_two_weld_a, 0, _143_two_weld_ha },

    { "bex", 7, _143_two_bex_a, 0, _143_two_bex_ha },
    { "cat", 7, _143_two_cat_a, 0, _143_two_cat_ha },
    { "can", 7, _143_two_can_a, 0, _143_two_can_ha },
    { "con", 7, _143_two_con_a, 0, _143_two_con_ha },
    { "cue", 7, _143_two_cue_a, 0, _143_two_cue_ha },
    { "cut", 7, _143_two_cut_a, 0, _143_two_cut_ha },
    { "dis", 7, _143_two_dis_a, 0, _143_two_dis_ha },
    { "dor", 7, _143_two_dor_a, 0, _143_two_dor_ha },
    { "end", 7, _143_two_end_a, 0, _143_two_end_ha },
    { "gor", 7, _143_two_gor_a, 0, _143_two_gor_ha },
    { "hor", 7, _143_two_hor_a, 0, _143_two_hor_ha },
    { "jam", 7, _143_two_jam_a, 0, _143_two_jam_ha },
    { "lsh", 7, _143_two_lsh_a, 0, _143_two_lsh_ha },
    { "mat", 7, _143_two_mat_a, 0, _143_two_mat_ha },
    { "met", 7, _143_two_met_a, 0, _143_two_met_ha },
    { "mix", 7, _143_two_mix_a, 0, _143_two_mix_ha },
    { "mug", 7, _143_two_mug_a, 0, _143_two_mug_ha },
    { "muk", 59, _143_two_muk_a, 0, _143_two_muk_ha },
    { "rap", 7, _143_two_rap_a, 0, _143_two_rap_ha },
    { "rep", 7, _143_two_rep_a, 0, _143_two_rep_ha },
    { "rip", 7, _143_two_rip_a, 0, _143_two_rip_ha },
    { "rsh", 7, _143_two_rsh_a, 0, _143_two_rsh_ha },
    { "rub", 7, _143_two_rub_a, 0, _143_two_rub_ha },
    { "pow", 7, _143_two_pow_a, 0, _143_two_pow_ha },
    { "sqt", 7, _143_two_sqt_a, 0, _143_two_sqt_ha },
    { "vor", 7, _143_two_vor_a, 0, _143_two_vor_ha },
    { "xeb", 7, _143_two_xeb_a, 0, _143_two_xeb_ha },

    { "by", 7, 0, _143_two__by_d, _143_two__by_ha },
    { "in", 7, 0, _143_two__in_d, _143_two__in_ha },
    {},
  };


/* layer one
*/
  static u3j_harm _143_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
  static c3_c* _143_one_add_ha[] = {
    "68a25eb6",
    0,
  };
  static u3j_harm _143_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static c3_c* _143_one_dec_ha[] = {
    "ce60540",
    0,
  };
  static u3j_harm _143_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static c3_c* _143_one_div_ha[] = {
    "3457adb3",
    0,
  };
  static u3j_harm _143_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static c3_c* _143_one_dvr_ha[] = {
    "20cdcad0",
    0,
  };
  static u3j_harm _143_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static c3_c* _143_one_gte_ha[] = {
    "8542678",
    0,
  };
  static u3j_harm _143_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static c3_c* _143_one_gth_ha[] = {
    "7555a49a",
    0,
  };
  static u3j_harm _143_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static c3_c* _143_one_lte_ha[] = {
    "6fc5ac17",
    0,
  };
  static u3j_harm _143_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static c3_c* _143_one_lth_ha[] = {
    "ef85ae3",
    0,
  };
  static u3j_harm _143_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static c3_c* _143_one_mod_ha[] = {
    "1ccddd83",
    0,
  };
  static u3j_harm _143_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static c3_c* _143_one_mul_ha[] = {
    "679b5099",
    0,
  };
  static u3j_harm _143_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
  static c3_c* _143_one_sub_ha[] = {
    "5e25e8d4",
    0,
  };
  static u3j_harm _143_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static c3_c* _143_one_cap_ha[] = {
    "7d28acf4",
    0,
  };
  static u3j_harm _143_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static c3_c* _143_one_peg_ha[] = {
    "4ecc0289",
    0,
  };
  static u3j_harm _143_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
  static c3_c* _143_one_mas_ha[] = {
    "611f2aab",
    0,
  };

static c3_c* _143_one_ha[] = {
  "c4b5bd8",
  0,
};
static u3j_core _143_one_d[] =
  { { "two", 3, 0, _143_two_d, _143_two_ha },
    { "add", 7, _143_one_add_a, 0, _143_one_add_ha },
    { "dec", 7, _143_one_dec_a, 0, _143_one_dec_ha },
    { "div", 7, _143_one_div_a, 0, _143_one_div_ha },
    { "dvr", 7, _143_one_dvr_a, 0, _143_one_dvr_ha },
    { "gte", 7, _143_one_gte_a, 0, _143_one_gte_ha },
    { "gth", 7, _143_one_gth_a, 0, _143_one_gth_ha },
    { "lte", 7, _143_one_lte_a, 0, _143_one_lte_ha },
    { "lth", 7, _143_one_lth_a, 0, _143_one_lth_ha },
    { "mod", 7, _143_one_mod_a, 0, _143_one_mod_ha },
    { "mul", 7, _143_one_mul_a, 0, _143_one_mul_ha },
    { "sub", 7, _143_one_sub_a, 0, _143_one_sub_ha },

    { "cap", 7, _143_one_cap_a, 0, _143_one_cap_ha },
    { "mas", 7, _143_one_mas_a, 0, _143_one_mas_ha },
    { "peg", 7, _143_one_peg_a, 0, _143_one_peg_ha },

    {}
  };

static c3_c* _k143_ha[] = {
  "13e199f7",
  0
};
u3j_core _k143_d[] =
  { { "one", 3, 0, _143_one_d, _143_one_ha },
    {}
  };

static u3j_core _d[] = {
  { "k143", 0, 0, _k143_d, _k143_ha, 0, (u3j_core*) 143, 0},
  {}
};

u3j_dash 
u3j_Dash = {
  _d,
  0,
  0
};
