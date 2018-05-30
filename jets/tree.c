/* j/tree.c
*/
#include "all.h"

  static u3j_harm _143_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
  static c3_c* _143_hex_aes_ecba_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
  static c3_c* _143_hex_aes_ecba_de_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
  static c3_c* _143_hex_aes_ecbb_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
  static c3_c* _143_hex_aes_ecbb_de_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
  static c3_c* _143_hex_aes_ecbc_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};
  static c3_c* _143_hex_aes_ecbc_de_ha[] = {
    0,
  };

  static u3j_harm _143_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
  static c3_c* _143_hex_aes_cbca_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
  static c3_c* _143_hex_aes_cbca_de_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
  static c3_c* _143_hex_aes_cbcb_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
  static c3_c* _143_hex_aes_cbcb_de_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
  static c3_c* _143_hex_aes_cbcc_en_ha[] = {
    0,
  };
  static u3j_harm _143_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};
  static c3_c* _143_hex_aes_cbcc_de_ha[] = {
    0,
  };

  static c3_c* _143_hex_aes_ecba_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_ecba_d[] =
    { { "en", 7, _143_hex_aes_ecba_en_a, 0, _143_hex_aes_ecba_en_ha },
      { "de", 7, _143_hex_aes_ecba_de_a, 0, _143_hex_aes_ecba_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_ecbb_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_ecbb_d[] =
    { { "en", 7, _143_hex_aes_ecbb_en_a, 0, _143_hex_aes_ecbb_en_ha },
      { "de", 7, _143_hex_aes_ecbb_de_a, 0, _143_hex_aes_ecbb_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_ecbc_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_ecbc_d[] =
    { { "en", 7, _143_hex_aes_ecbc_en_a, 0, _143_hex_aes_ecbc_en_ha },
      { "de", 7, _143_hex_aes_ecbc_de_a, 0, _143_hex_aes_ecbc_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_cbca_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_cbca_d[] =
    { { "en", 7, _143_hex_aes_cbca_en_a, 0, _143_hex_aes_cbca_en_ha },
      { "de", 7, _143_hex_aes_cbca_de_a, 0, _143_hex_aes_cbca_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_cbcb_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_cbcb_d[] =
    { { "en", 7, _143_hex_aes_cbcb_en_a, 0, _143_hex_aes_cbcb_en_ha },
      { "de", 7, _143_hex_aes_cbcb_de_a, 0, _143_hex_aes_cbcb_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_cbcc_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_cbcc_d[] =
    { { "en", 7, _143_hex_aes_cbcc_en_a, 0, _143_hex_aes_cbcc_en_ha },
      { "de", 7, _143_hex_aes_cbcc_de_a, 0, _143_hex_aes_cbcc_de_ha },
      {}
    };

  static c3_c* _143_hex_aes_ha[] = {
    0,
  };
  static u3j_core _143_hex_aes_d[] =
    { { "ecba", 7, 0, _143_hex_aes_ecba_d, _143_hex_aes_ecba_ha },
      { "ecbb", 7, 0, _143_hex_aes_ecbb_d, _143_hex_aes_ecbb_ha },
      { "ecbc", 7, 0, _143_hex_aes_ecbc_d, _143_hex_aes_ecbc_ha },
      { "cbca", 7, 0, _143_hex_aes_cbca_d, _143_hex_aes_cbca_ha },
      { "cbcb", 7, 0, _143_hex_aes_cbcb_d, _143_hex_aes_cbcb_ha },
      { "cbcc", 7, 0, _143_hex_aes_cbcc_d, _143_hex_aes_cbcc_ha },
      {}
    };

  static u3j_harm _143_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
    static c3_c* _143_hex_down_mark_ha[] = {
      0,
    };

    static c3_c* _143_hex_down_ha[] = {
      0,
    };
    static u3j_core _143_hex_down_d[] =
      { { "mark", 7, _143_hex_down_mark_a, 0, _143_hex_down_mark_ha },
        {}
      };

  static u3j_harm _143_hex_lore_a[] = {{".2", u3we_lore}, {}};
  static c3_c* _143_hex_lore_ha[] = {
    0,
  };
  static u3j_harm _143_hex_loss_a[] = {{".2", u3we_loss}, {}};
  static c3_c* _143_hex_loss_ha[] = {
    0,
  };
  static u3j_harm _143_hex_lune_a[] = {{".2", u3we_lune}, {}};
  static c3_c* _143_hex_lune_ha[] = {
    0,
  };

  static u3j_harm _143_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
  static c3_c* _143_hex_coed__ed_puck_ha[] = {
    0,
  };
  static u3j_harm _143_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
  static c3_c* _143_hex_coed__ed_sign_ha[] = {
    0,
  };
  static u3j_harm _143_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
  static c3_c* _143_hex_coed__ed_veri_ha[] = {
    0,
  };
  static u3j_harm _143_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};
  static c3_c* _143_hex_coed__ed_shar_ha[] = {
    0,
  };

  static c3_c* _143_hex_coed__ed_ha[] = {
    0,
  };
  static u3j_core _143_hex_coed__ed_d[] =
    { { "sign", 7, _143_hex_coed__ed_sign_a, 0, _143_hex_coed__ed_sign_ha },
      { "puck", 7, _143_hex_coed__ed_puck_a, 0, _143_hex_coed__ed_puck_ha },
      { "veri", 7, _143_hex_coed__ed_veri_a, 0, _143_hex_coed__ed_veri_ha },
      { "shar", 7, _143_hex_coed__ed_shar_a, 0, _143_hex_coed__ed_shar_ha },
      {}
    };

static c3_c* _143_hex_coed_ha[] = {
  0,
};
static u3j_core _143_hex_coed_d[] =
  { { "ed", 3, 0, _143_hex_coed__ed_d, _143_hex_coed__ed_ha },
    {}
  };

static c3_c* _143_hex_ha[] = {
  0,
};
static u3j_core _143_hex_d[] =
  { { "down", 8063, 0, _143_hex_down_d, _143_hex_down_ha },

    { "lore",   63, _143_hex_lore_a, 0, _143_hex_lore_ha },
    { "loss",   63, _143_hex_loss_a, 0, _143_hex_loss_ha },
    { "lune",  127, _143_hex_lune_a, 0, _143_hex_lune_ha },

    { "coed",   63, 0, _143_hex_coed_d, _143_hex_coed_ha },
    { "aes",    31, 0, _143_hex_aes_d, _143_hex_aes_ha },
    {}
  };

/* layer five
*/
  static u3j_harm _143_pen_cell_a[] = {{".2", u3wf_cell}, {}};
  static c3_c* _143_pen_cell_ha[] = {
    0,
  };
  static u3j_harm _143_pen_comb_a[] = {{".2", u3wf_comb}, {}};
  static c3_c* _143_pen_comb_ha[] = {
    0,
  };
  static u3j_harm _143_pen_cons_a[] = {{".2", u3wf_cons}, {}};
  static c3_c* _143_pen_cons_ha[] = {
    0,
  };
  static u3j_harm _143_pen_core_a[] = {{".2", u3wf_core}, {}};
  static c3_c* _143_pen_core_ha[] = {
    0,
  };
  static u3j_harm _143_pen_help_a[] = {{".2", u3wf_help}, {}};
  static c3_c* _143_pen_help_ha[] = {
    0,
  };
  static u3j_harm _143_pen_face_a[] = {{".2", u3wf_face}, {}};
  static c3_c* _143_pen_face_ha[] = {
    0,
  };
  static u3j_harm _143_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
  static c3_c* _143_pen_fitz_ha[] = {
    0,
  };
  static u3j_harm _143_pen_flan_a[] = {{".2", u3wf_flan}, {}};
  static c3_c* _143_pen_flan_ha[] = {
    0,
  };
  static u3j_harm _143_pen_flip_a[] = {{".2", u3wf_flip}, {}};
  static c3_c* _143_pen_flip_ha[] = {
    0,
  };
  static u3j_harm _143_pen_flor_a[] = {{".2", u3wf_flor}, {}};
  static c3_c* _143_pen_flor_ha[] = {
    0,
  };
  static u3j_harm _143_pen_fork_a[] = {{".2", u3wf_fork}, {}};
  static c3_c* _143_pen_fork_ha[] = {
    0,
  };

  static u3j_harm _143_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static c3_c* _143_pen_hike_ha[] = {
    0,
  };
  static u3j_harm _143_pen_look_a[] = {{".2", u3wf_look}, {}};
  static c3_c* _143_pen_look_ha[] = {
    0,
  };
  static u3j_harm _143_pen_loot_a[] = {{".2", u3wf_loot}, {}};
  static c3_c* _143_pen_loot_ha[] = {
    0,
  };

    //static u3j_harm _143_pen__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
    //static u3j_harm _143_pen__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
    static u3j_harm _143_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
    static c3_c* _143_pen__ut_crop_ha[] = {
      0,
    };
    // static u3j_harm _143_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _143_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static c3_c* _143_pen__ut_fond_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static c3_c* _143_pen__ut_fish_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static c3_c* _143_pen__ut_fuse_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static c3_c* _143_pen__ut_mint_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static c3_c* _143_pen__ut_mull_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static c3_c* _143_pen__ut_nest_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static c3_c* _143_pen__ut_peek_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static c3_c* _143_pen__ut_play_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static c3_c* _143_pen__ut_rest_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static c3_c* _143_pen__ut_toss_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
    static c3_c* _143_pen__ut_wrap_ha[] = {
      0,
    };

  static c3_c* _143_pen__ut_ha[] = {
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
  //    { "burn", 7, _143_pen__ut_burn_a },
  //    { "conk", 7, _143_pen__ut_conk_a },
      { "crop", 7, _143_pen__ut_crop_a, 0, _143_pen__ut_crop_ha },
      { "fond", 7, _143_pen__ut_fond_a, 0, _143_pen__ut_fond_ha },
    //  { "fire", 7, _143_pen__ut_fire_a },
      { "fish", 7, _143_pen__ut_fish_a, 0, _143_pen__ut_fish_ha },
      { "fuse", 7, _143_pen__ut_fuse_a, 0, _143_pen__ut_fuse_ha },
      { "mint", 7, _143_pen__ut_mint_a, 0, _143_pen__ut_mint_ha },
      { "mull", 7, _143_pen__ut_mull_a, 0, _143_pen__ut_mull_ha },
      { "nest", 7, _143_pen__ut_nest_a, 0, _143_pen__ut_nest_ha },
      { "peek", 7, _143_pen__ut_peek_a, 0, _143_pen__ut_peek_ha },
      { "play", 7, _143_pen__ut_play_a, 0, _143_pen__ut_play_ha },
      { "rest", 7, _143_pen__ut_rest_a, 0, _143_pen__ut_rest_ha },
      { "toss", 7, _143_pen__ut_toss_a, 0, _143_pen__ut_toss_ha },
      { "wrap", 7, _143_pen__ut_wrap_a, 0, _143_pen__ut_wrap_ha },
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
  "93e4cad5f01e16a173f31810ee5ab530a8a760010eb7cc9b451880456f32267e",
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
    { "comb", 7, _143_pen_comb_a, 0, _143_pen_comb_ha },
    { "cons", 7, _143_pen_cons_a, 0, _143_pen_cons_ha },
    { "core", 7, _143_pen_core_a, 0, _143_pen_core_ha },
    { "face", 7, _143_pen_face_a, 0, _143_pen_face_ha },
    { "fitz", 7, _143_pen_fitz_a, 0, _143_pen_fitz_ha },
    { "flan", 7, _143_pen_flan_a, 0, _143_pen_flan_ha },
    { "flip", 7, _143_pen_flip_a, 0, _143_pen_flip_ha },
    { "flor", 7, _143_pen_flor_a, 0, _143_pen_flor_ha },
    { "fork", 7, _143_pen_fork_a, 0, _143_pen_fork_ha },
    { "help", 7, _143_pen_help_a, 0, _143_pen_help_ha },
    { "hike", 7, _143_pen_hike_a, 0, _143_pen_hike_ha },
    { "look", 7, _143_pen_look_a, 0, _143_pen_look_ha },
    { "loot", 7, _143_pen_loot_a, 0, _143_pen_loot_ha },

    // { "ap", _143_pen__ap_a },
    // { "al", _143_pen__al_a },
    { "ut", 15, _143_pen__ut_a, _143_pen__ut_d, _143_pen__ut_ha, _143_pen__ut_ho },
    {}
  };

/* layer four
*/
  static u3j_harm _143_qua_trip_a[] = {{".2", u3we_trip}, {}};
  static c3_c* _143_qua_trip_ha[] = {
    "335ad28e7924ee9bdf29050bf2d7b4eebf434dfaaa0f704a689f065eb5c703b0",
    0,
  };

  static u3j_harm _143_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static c3_c* _143_qua__po_ind_ha[] = {
    0,
  };
  static u3j_harm _143_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static c3_c* _143_qua__po_ins_ha[] = {
    0,
  };
  static u3j_harm _143_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static c3_c* _143_qua__po_tod_ha[] = {
    0,
  };
  static u3j_harm _143_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
  static c3_c* _143_qua__po_tos_ha[] = {
    0,
  };

    static c3_c* _143_qua__po_ha[] = {
      0,
    };
    static u3j_core _143_qua__po_d[] =
      { { "ind", 7, _143_qua__po_ind_a, 0, _143_qua__po_ind_ha },
        { "ins", 7, _143_qua__po_ins_a, 0, _143_qua__po_ins_ha },
        { "tod", 7, _143_qua__po_tod_a, 0, _143_qua__po_tod_ha },
        { "tos", 7, _143_qua__po_tos_a, 0, _143_qua__po_tos_ha },
        {}
      };

  static u3j_harm _143_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
    static c3_c* _143_qua__bend_ha[] = {
      0,
    };
    static c3_c* _143_qua__bend_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__bend_d[] =
      { { "fun", 7, _143_qua__bend_fun_a, 0, _143_qua__bend_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static c3_c* _143_qua__cold_ha[] = {
      0,
    };
    static c3_c* _143_qua__cold_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__cold_d[] =
      { { "fun", 7, _143_qua__cold_fun_a, 0, _143_qua__cold_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static c3_c* _143_qua__cook_ha[] = {
      0,
    };
    static c3_c* _143_qua__cook_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__cook_d[] =
      { { "fun", 7, _143_qua__cook_fun_a, 0, _143_qua__cook_fun_ha },
        {}
      };

  static u3j_harm _143_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static c3_c* _143_qua__comp_ha[] = {
      0,
    };
    static c3_c* _143_qua__comp_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__comp_d[] =
      { { "fun", 7, _143_qua__comp_fun_a, 0, _143_qua__comp_fun_ha },
        {}
      };

  static u3j_harm _143_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static c3_c* _143_qua__easy_ha[] = {
      "bbdf198f84a73d758c1ccc5608159c6a1bfa72495fcd963e7327b9ccc26eded3",
      0,
    };
    static c3_c* _143_qua__easy_fun_ha[] = {
      "b4b36293b7e9bfcc9029283fe8b770967073ef18d45f0eb4b90141e9c087ffec",
      0,
    };
    static u3j_core _143_qua__easy_d[] =
      { { "fun", 7, _143_qua__easy_fun_a, 0, _143_qua__easy_fun_ha },
        {}
      };

  static u3j_harm _143_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static c3_c* _143_qua__glue_ha[] = {
      0,
    };
    static c3_c* _143_qua__glue_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__glue_d[] =
      { { "fun", 7, _143_qua__glue_fun_a, 0, _143_qua__glue_fun_ha },
        {}
      };

  static u3j_harm _143_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static c3_c* _143_qua__here_ha[] = {
      0,
    };
    static c3_c* _143_qua__here_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__here_d[] =
      { { "fun", 7, _143_qua__here_fun_a, 0, _143_qua__here_fun_ha },
        {}
      };

  static u3j_harm _143_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static c3_c* _143_qua__just_ha[] = {
      0,
    };
    static c3_c* _143_qua__just_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__just_d[] =
      { { "fun", 7, _143_qua__just_fun_a, 0, _143_qua__just_fun_ha },
        {}
      };

  static u3j_harm _143_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static c3_c* _143_qua__mask_ha[] = {
      0,
    };
    static c3_c* _143_qua__mask_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__mask_d[] =
      { { "fun", 7, _143_qua__mask_fun_a, 0, _143_qua__mask_fun_ha },
        {}
      };

  static u3j_harm _143_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static c3_c* _143_qua__shim_ha[] = {
      0,
    };
    static c3_c* _143_qua__shim_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__shim_d[] =
      { { "fun", 7, _143_qua__shim_fun_a, 0, _143_qua__shim_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static c3_c* _143_qua__stag_ha[] = {
      0,
    };
    static c3_c* _143_qua__stag_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__stag_d[] =
      { { "fun", 7, _143_qua__stag_fun_a, 0, _143_qua__stag_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static c3_c* _143_qua__stew_ha[] = {
      0,
    };
    static c3_c* _143_qua__stew_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__stew_d[] =
      { { "fun", 31, _143_qua__stew_fun_a, 0, _143_qua__stew_fun_ha },
        {}
      };
 
  static u3j_harm _143_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static c3_c* _143_qua__stir_ha[] = {
      0,
    };
    static c3_c* _143_qua__stir_fun_ha[] = {
      0,
    };
    static u3j_core _143_qua__stir_d[] =
      { { "fun", 7, _143_qua__stir_fun_a, 0, _143_qua__stir_fun_ha },
        {}
      };

  // static u3j_harm _143_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _143_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static c3_c* _143_qua__plug_ha[] = {
    0,
  };
  static u3j_harm _143_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static c3_c* _143_qua__pose_ha[] = {
    0,
  };
  //static u3j_harm _143_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _143_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static c3_c* _143_qua_mink_ha[] = {
    0,
  };
  static u3j_harm _143_qua_mule_a[] = {{".2", u3we_mule}, {}};
  static c3_c* _143_qua_mule_ha[] = {
    0,
  };

static c3_c* _143_qua_ha[] = {
  "a8e494c95b4f2d7e74100c8aa9d0797ff6a3a9b5e6bd3dfbc0ebe56c2e46b499",
  0,
};
static u3j_hood _143_qua_ho[] = {
  { "mute", 0x17dfc },
  { "show", 0x2fbbaba },
  {},
};
static u3j_core _143_qua_d[] =
  { { "pen", 3, 0, _143_pen_d, _143_pen_ha, _143_pen_ho },

    { "po", 7, 0, _143_qua__po_d, _143_qua__po_ha },

    { "trip", 7, _143_qua_trip_a, 0, _143_qua_trip_ha },

    { "bend", 7, 0, _143_qua__bend_d, _143_qua__bend_ha },
    { "cold", 7, 0, _143_qua__cold_d, _143_qua__cold_ha },
    { "comp", 7, 0, _143_qua__comp_d, _143_qua__comp_ha },
    { "cook", 7, 0, _143_qua__cook_d, _143_qua__cook_ha },
    { "easy", 7, 0, _143_qua__easy_d, _143_qua__easy_ha },
    { "glue", 7, 0, _143_qua__glue_d, _143_qua__glue_ha },
    { "here", 7, 0, _143_qua__here_d, _143_qua__here_ha },
    { "just", 7, 0, _143_qua__just_d, _143_qua__just_ha },
    { "mask", 7, 0, _143_qua__mask_d, _143_qua__mask_ha },
    { "shim", 7, 0, _143_qua__shim_d, _143_qua__shim_ha },
    { "stag", 7, 0, _143_qua__stag_d, _143_qua__stag_ha },
    { "stew", 7, 0, _143_qua__stew_d, _143_qua__stew_ha },
    { "stir", 7, 0, _143_qua__stir_d, _143_qua__stir_ha },

    // { "pfix", 7, _143_qua_pfix_a },
    { "plug", 7, _143_qua_plug_a, 0, _143_qua__plug_ha },
    { "pose", 7, _143_qua_pose_a, 0, _143_qua__pose_ha },
    // { "sfix", 7, _143_qua_sfix_a },

    { "mink", 7, _143_qua_mink_a, 0, _143_qua_mink_ha },
    { "mule", 7, _143_qua_mule_a, 0, _143_qua_mule_ha },
    {}
  };

/* layer three
*/
    static u3j_harm _143_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
    static c3_c* _143_tri__cofl__drg_ha[] = {
      0,
    };
    static u3j_harm _143_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
    static c3_c* _143_tri__cofl__lug_ha[] = {
      0,
    };

  static c3_c* _143_tri__cofl_ha[] = {
    0,
  };
  static u3j_core _143_tri__cofl_d[] =
    { { "drg", 7, _143_tri__cofl__drg_a, 0, _143_tri__cofl__drg_ha },
      { "lug", 7, _143_tri__cofl__lug_a, 0, _143_tri__cofl__lug_ha },
      {}
    };

    static u3j_harm _143_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
    static c3_c* _143_tri__rd_add_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
    static c3_c* _143_tri__rd_sub_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
    static c3_c* _143_tri__rd_mul_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
    static c3_c* _143_tri__rd_div_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
    static c3_c* _143_tri__rd_sqt_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
    static c3_c* _143_tri__rd_fma_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
    static c3_c* _143_tri__rd_lth_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
    static c3_c* _143_tri__rd_lte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
    static c3_c* _143_tri__rd_equ_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
    static c3_c* _143_tri__rd_gte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
    static c3_c* _143_tri__rd_gth_ha[] = {
      0,
    };

  static c3_c* _143_tri__rd_ha[] = {
    0,
  };
  static u3j_core _143_tri__rd_d[] =
    { { "add", 7, _143_tri__rd_add_a, 0, _143_tri__rd_add_ha },
      { "sub", 7, _143_tri__rd_sub_a, 0, _143_tri__rd_sub_ha },
      { "mul", 7, _143_tri__rd_mul_a, 0, _143_tri__rd_mul_ha },
      { "div", 7, _143_tri__rd_div_a, 0, _143_tri__rd_div_ha },
      { "sqt", 7, _143_tri__rd_sqt_a, 0, _143_tri__rd_sqt_ha },
      { "fma", 7, _143_tri__rd_fma_a, 0, _143_tri__rd_fma_ha },
      { "lth", 7, _143_tri__rd_lth_a, 0, _143_tri__rd_lth_ha },
      { "lte", 7, _143_tri__rd_lte_a, 0, _143_tri__rd_lte_ha },
      { "equ", 7, _143_tri__rd_equ_a, 0, _143_tri__rd_equ_ha },
      { "gte", 7, _143_tri__rd_gte_a, 0, _143_tri__rd_gte_ha },
      { "gth", 7, _143_tri__rd_gth_a, 0, _143_tri__rd_gth_ha },
      {}
    };

    static u3j_harm _143_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
    static c3_c* _143_tri__rs_add_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
    static c3_c* _143_tri__rs_sub_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
    static c3_c* _143_tri__rs_mul_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
    static c3_c* _143_tri__rs_div_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
    static c3_c* _143_tri__rs_sqt_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
    static c3_c* _143_tri__rs_fma_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
    static c3_c* _143_tri__rs_lth_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
    static c3_c* _143_tri__rs_lte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
    static c3_c* _143_tri__rs_equ_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
    static c3_c* _143_tri__rs_gte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
    static c3_c* _143_tri__rs_gth_ha[] = {
      0,
    };

  static c3_c* _143_tri__rs_ha[] = {
    0,
  };
  static u3j_core _143_tri__rs_d[] =
    { { "add", 7, _143_tri__rs_add_a, 0, _143_tri__rs_add_ha },
      { "sub", 7, _143_tri__rs_sub_a, 0, _143_tri__rs_sub_ha },
      { "mul", 7, _143_tri__rs_mul_a, 0, _143_tri__rs_mul_ha },
      { "div", 7, _143_tri__rs_div_a, 0, _143_tri__rs_div_ha },
      { "sqt", 7, _143_tri__rs_sqt_a, 0, _143_tri__rs_sqt_ha },
      { "fma", 7, _143_tri__rs_fma_a, 0, _143_tri__rs_fma_ha },
      { "lth", 7, _143_tri__rs_lth_a, 0, _143_tri__rs_lth_ha },
      { "lte", 7, _143_tri__rs_lte_a, 0, _143_tri__rs_lte_ha },
      { "equ", 7, _143_tri__rs_equ_a, 0, _143_tri__rs_equ_ha },
      { "gte", 7, _143_tri__rs_gte_a, 0, _143_tri__rs_gte_ha },
      { "gth", 7, _143_tri__rs_gth_a, 0, _143_tri__rs_gth_ha },
      {}
    };

    static u3j_harm _143_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
    static c3_c* _143_tri__rq_add_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
    static c3_c* _143_tri__rq_sub_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
    static c3_c* _143_tri__rq_mul_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
    static c3_c* _143_tri__rq_div_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
    static c3_c* _143_tri__rq_sqt_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
    static c3_c* _143_tri__rq_fma_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
    static c3_c* _143_tri__rq_lth_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
    static c3_c* _143_tri__rq_lte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
    static c3_c* _143_tri__rq_equ_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
    static c3_c* _143_tri__rq_gte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
    static c3_c* _143_tri__rq_gth_ha[] = {
      0,
    };

  static c3_c* _143_tri__rq_ha[] = {
    0,
  };
  static u3j_core _143_tri__rq_d[] =
    { { "add", 7, _143_tri__rq_add_a, 0, _143_tri__rq_add_ha },
      { "sub", 7, _143_tri__rq_sub_a, 0, _143_tri__rq_sub_ha },
      { "mul", 7, _143_tri__rq_mul_a, 0, _143_tri__rq_mul_ha },
      { "div", 7, _143_tri__rq_div_a, 0, _143_tri__rq_div_ha },
      { "sqt", 7, _143_tri__rq_sqt_a, 0, _143_tri__rq_sqt_ha },
      { "fma", 7, _143_tri__rq_fma_a, 0, _143_tri__rq_fma_ha },
      { "lth", 7, _143_tri__rq_lth_a, 0, _143_tri__rq_lth_ha },
      { "lte", 7, _143_tri__rq_lte_a, 0, _143_tri__rq_lte_ha },
      { "equ", 7, _143_tri__rq_equ_a, 0, _143_tri__rq_equ_ha },
      { "gte", 7, _143_tri__rq_gte_a, 0, _143_tri__rq_gte_ha },
      { "gth", 7, _143_tri__rq_gth_a, 0, _143_tri__rq_gth_ha },
      {}
    };

    static u3j_harm _143_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
    static c3_c* _143_tri__rh_add_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
    static c3_c* _143_tri__rh_sub_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
    static c3_c* _143_tri__rh_mul_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
    static c3_c* _143_tri__rh_div_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
    static c3_c* _143_tri__rh_sqt_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
    static c3_c* _143_tri__rh_fma_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
    static c3_c* _143_tri__rh_lth_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
    static c3_c* _143_tri__rh_lte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
    static c3_c* _143_tri__rh_equ_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
    static c3_c* _143_tri__rh_gte_ha[] = {
      0,
    };
    static u3j_harm _143_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
    static c3_c* _143_tri__rh_gth_ha[] = {
      0,
    };

  static c3_c* _143_tri__rh_ha[] = {
    0,
  };
  static u3j_core _143_tri__rh_d[] =
    { { "add", 7, _143_tri__rh_add_a, 0, _143_tri__rh_add_ha },
      { "sub", 7, _143_tri__rh_sub_a, 0, _143_tri__rh_sub_ha },
      { "mul", 7, _143_tri__rh_mul_a, 0, _143_tri__rh_mul_ha },
      { "div", 7, _143_tri__rh_div_a, 0, _143_tri__rh_div_ha },
      { "sqt", 7, _143_tri__rh_sqt_a, 0, _143_tri__rh_sqt_ha },
      { "fma", 7, _143_tri__rh_fma_a, 0, _143_tri__rh_fma_ha },
      { "lth", 7, _143_tri__rh_lth_a, 0, _143_tri__rh_lth_ha },
      { "lte", 7, _143_tri__rh_lte_a, 0, _143_tri__rh_lte_ha },
      { "equ", 7, _143_tri__rh_equ_a, 0, _143_tri__rh_equ_ha },
      { "gte", 7, _143_tri__rh_gte_a, 0, _143_tri__rh_gte_ha },
      { "gth", 7, _143_tri__rh_gth_a, 0, _143_tri__rh_gth_ha },
      {}
    };

    static u3j_harm _143_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
    static c3_c* _143_tri__og_raw_ha[] = {
      0,
    };

  static c3_c* _143_tri__og_ha[] = {
    0,
  };
  static u3j_core _143_tri__og_d[] =
    { { "raw", 7, _143_tri__og_raw_a, 0, _143_tri__og_raw_ha },
      {}
    };

  static u3j_harm _143_tri_shax_a[] = {{".2", u3we_shax}, {}};
  static c3_c* _143_tri_shax_ha[] = {
    0,
  };
  static u3j_harm _143_tri_shay_a[] = {{".2", u3we_shay}, {}};
  static c3_c* _143_tri_shay_ha[] = {
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
    0,
  };

static c3_c* _143_tri_ha[] = {
  "3532a5d4a01664989f527f0c42399d5a8e6e6e32f2542cde39da918d3b9ec41a",
  0,
};
static u3j_core _143_tri_d[] =
  { { "qua",  3, 0, _143_qua_d, _143_qua_ha, _143_qua_ho },
    { "cofl", 7, 0, _143_tri__cofl_d, _143_tri__cofl_ha },
    { "rd",   7, 0, _143_tri__rd_d, _143_tri__rd_ha },
    { "rs",   7, 0, _143_tri__rs_d, _143_tri__rs_ha },
    { "rq",   7, 0, _143_tri__rq_d, _143_tri__rq_ha },
    { "rh",   7, 0, _143_tri__rh_d, _143_tri__rh_ha },
    { "og",   7, 0, _143_tri__og_d, _143_tri__og_ha },

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
    0,
  };
  static u3j_harm _143_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static c3_c* _143_two_lent_ha[] = {
    0,
  };
  static u3j_harm _143_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static c3_c* _143_two_levy_ha[] = {
    0,
  };
  static u3j_harm _143_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static c3_c* _143_two_lien_ha[] = {
    0,
  };
  static u3j_harm _143_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static c3_c* _143_two_murn_ha[] = {
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
    0,
  };
  static u3j_harm _143_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
  static c3_c* _143_two_reel_ha[] = {
    0,
  };
  static u3j_harm _143_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static c3_c* _143_two_roll_ha[] = {
    0,
  };
  static u3j_harm _143_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static c3_c* _143_two_skid_ha[] = {
    0,
  };
  static u3j_harm _143_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static c3_c* _143_two_skim_ha[] = {
    0,
  };
  static u3j_harm _143_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static c3_c* _143_two_skip_ha[] = {
    0,
  };
  static u3j_harm _143_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static c3_c* _143_two_scag_ha[] = {
    0,
  };
  static u3j_harm _143_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static c3_c* _143_two_slag_ha[] = {
    0,
  };
  static u3j_harm _143_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  static c3_c* _143_two_snag_ha[] = {
    0,
  };
  // static u3j_harm _143_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _143_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static c3_c* _143_two_turn_ha[] = {
    0,
  };
  static u3j_harm _143_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
  static c3_c* _143_two_weld_ha[] = {
    0,
  };

  static u3j_harm _143_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static c3_c* _143_two_bex_ha[] = {
    0,
  };
  static u3j_harm _143_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static c3_c* _143_two_can_ha[] = {
    0,
  };
  static u3j_harm _143_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static c3_c* _143_two_cat_ha[] = {
    0,
  };
  static u3j_harm _143_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static c3_c* _143_two_con_ha[] = {
    0,
  };
  static u3j_harm _143_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static c3_c* _143_two_cut_ha[] = {
    0,
  };
  static u3j_harm _143_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static c3_c* _143_two_dis_ha[] = {
    0,
  };
  static u3j_harm _143_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static c3_c* _143_two_dor_ha[] = {
    0,
  };
  static u3j_harm _143_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static c3_c* _143_two_end_ha[] = {
    0,
  };
  static u3j_harm _143_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static c3_c* _143_two_gor_ha[] = {
    0,
  };
  static u3j_harm _143_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static c3_c* _143_two_hor_ha[] = {
    0,
  };
  static u3j_harm _143_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static c3_c* _143_two_lsh_ha[] = {
    0,
  };
  static u3j_harm _143_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static c3_c* _143_two_met_ha[] = {
    0,
  };
  static u3j_harm _143_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
  static c3_c* _143_two_mix_ha[] = {
    0,
  };
  static u3j_harm _143_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
  static c3_c* _143_two_mug_ha[] = {
    0,
  };
  static u3j_harm _143_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
  static c3_c* _143_two_muk_ha[] = {
    0,
  };
  static u3j_harm _143_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static c3_c* _143_two_pow_ha[] = {
    0,
  };
  static u3j_harm _143_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static c3_c* _143_two_rap_ha[] = {
    0,
  };
  static u3j_harm _143_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
  static c3_c* _143_two_rep_ha[] = {
    0,
  };
  static u3j_harm _143_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static c3_c* _143_two_rip_ha[] = {
    0,
  };
  static u3j_harm _143_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
  static c3_c* _143_two_rsh_ha[] = {
    0,
  };
  static u3j_harm _143_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
  static c3_c* _143_two_sqt_ha[] = {
    0,
  };
  static u3j_harm _143_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
  static c3_c* _143_two_vor_ha[] = {
    0,
  };
  static u3j_harm _143_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
  static c3_c* _143_two_xeb_ha[] = {
    0,
  };

    static u3j_harm _143_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
    static c3_c* _143_two__in_bif_ha[] = {
      0,
    };
    static u3j_harm _143_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
    static c3_c* _143_two__in_dif_ha[] = {
      0,
    };
    static u3j_harm _143_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
    static c3_c* _143_two__in_gas_ha[] = {
      0,
    };
    static u3j_harm _143_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static c3_c* _143_two__in_has_ha[] = {
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
      0,
    };
    static u3j_harm _143_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static c3_c* _143_two__in_tap_ha[] = {
      0,
    };
    static u3j_harm _143_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static c3_c* _143_two__in_wyt_ha[] = {
      0,
    };
    static u3j_harm _143_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
    static c3_c* _143_two__in_uni_ha[] = {
      0,
    };

  static c3_c* _143_two__in_ha[] = {
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
      0,
    };
    static u3j_harm _143_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
    static c3_c* _143_two__by_dif_ha[] = {
      0,
    };
    static u3j_harm _143_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
    static c3_c* _143_two__by_gas_ha[] = {
      0,
    };
    static u3j_harm _143_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static c3_c* _143_two__by_get_ha[] = {
      0,
    };
    static u3j_harm _143_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    static c3_c* _143_two__by_has_ha[] = {
      0,
    };
    // static u3j_harm _143_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _143_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static c3_c* _143_two__by_put_ha[] = {
      0,
    };
    static u3j_harm _143_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    static c3_c* _143_two__by_tap_ha[] = {
      0,
    };
    // static u3j_harm _143_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};

  static c3_c* _143_two__by_ha[] = {
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
    0,
  };
  static u3j_harm _143_two_jam_a[] = {{".2", u3we_jam}, {}};
  static c3_c* _143_two_jam_ha[] = {
    0,
  };
  static u3j_harm _143_two_mat_a[] = {{".2", u3we_mat}, {}};
  static c3_c* _143_two_mat_ha[] = {
    0,
  };
  static u3j_harm _143_two_rub_a[] = {{".2", u3we_rub}, {}};
  static c3_c* _143_two_rub_ha[] = {
    0,
  };

static c3_c* _143_two_ha[] = {
  "aa20f6b35462ffcf6903536717dccd9f9a77f856ba9eee3a8cd5e39440a479a9",
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
    0,
  };
  static u3j_harm _143_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static c3_c* _143_one_dec_ha[] = {
    0,
  };
  static u3j_harm _143_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static c3_c* _143_one_div_ha[] = {
    0,
  };
  static u3j_harm _143_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static c3_c* _143_one_dvr_ha[] = {
    0,
  };
  static u3j_harm _143_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static c3_c* _143_one_gte_ha[] = {
    0,
  };
  static u3j_harm _143_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static c3_c* _143_one_gth_ha[] = {
    0,
  };
  static u3j_harm _143_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static c3_c* _143_one_lte_ha[] = {
    0,
  };
  static u3j_harm _143_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static c3_c* _143_one_lth_ha[] = {
    0,
  };
  static u3j_harm _143_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static c3_c* _143_one_mod_ha[] = {
    0,
  };
  static u3j_harm _143_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static c3_c* _143_one_mul_ha[] = {
    0,
  };
  static u3j_harm _143_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
  static c3_c* _143_one_sub_ha[] = {
    0,
  };
  static u3j_harm _143_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static c3_c* _143_one_cap_ha[] = {
    0,
  };
  static u3j_harm _143_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static c3_c* _143_one_peg_ha[] = {
    0,
  };
  static u3j_harm _143_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
  static c3_c* _143_one_mas_ha[] = {
    0,
  };

static c3_c* _143_one_ha[] = {
  "f04034ab0335ab53292f74127065b609b8b99bd9905f51a8254ef4580b84aaf3",
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
  "11ef831e540bfe588cde998b55e7d8cd58bb35898774b19f9164328c5cb3c637",
  0,
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
