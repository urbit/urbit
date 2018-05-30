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
    "2abdf1dd2b9e30333451a49c7a5890ab27c77603d4210aa4d1b8016d7bb730a7",
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
    "1327f1c0b31be786c5185086186814bbb6d8b44bc8ad26e651ad027e0e62844d",
    0,
  };
  static u3j_harm _143_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
  static c3_c* _143_hex_coed__ed_sign_ha[] = {
    "18c4367cbc8990c70610c6d43a42ab69693952997b4ff58eb675f1f14249bd0b",
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
    "aa922a96a00ba63a328b9f67c814e0cd006b830481834101de4b92e36533914c",
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
  "84efc127c57ffa5ee61c12737816ef42e067fd64f8262f3df93481d8c905b2bf",
  0,
};
static u3j_core _143_hex_coed_d[] =
  { { "ed", 3, 0, _143_hex_coed__ed_d, _143_hex_coed__ed_ha },
    {}
  };

static c3_c* _143_hex_ha[] = {
  "9cf8c9378dbb910e1130eb5aa30d12573943388613018c05a32f3d7d1d6c5ca2",
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
    "1f3485acbf3f0159facb1e450bdcb67604fef3ebac979b7b78e254c2d4a9f81a",
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
    "0b222d63259aba485c1ccede583507b6bba49bcceed4a89ad76f3bd9a37913bb",
    0,
  };
  static u3j_harm _143_pen_help_a[] = {{".2", u3wf_help}, {}};
  static c3_c* _143_pen_help_ha[] = {
    "fdf6593bc217b85d5ad2d9554af56d8c967f31215dc54a2195a1a131b876866b",
    0,
  };
  static u3j_harm _143_pen_face_a[] = {{".2", u3wf_face}, {}};
  static c3_c* _143_pen_face_ha[] = {
    "327dd91a9bdb8b70bd275d2ddeb688de0d9723d62605fe78f9557f77a9cbf8c9",
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
    "4b7999c53d8a82a5bab869ee9675c979138b8c9ad1cfaa44b0d67dec5cb56966",
    0,
  };

  static u3j_harm _143_pen_hike_a[] = {{".2", u3wf_hike}, {}};
  static c3_c* _143_pen_hike_ha[] = {
    "7503dc65fe3164b3e95f81b51afd64ab615e26516b60df95241244e2b793d024",
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
      "7a0dc306776c8603556558009749aa0dd6d6077994d949fc29e5386ee1fb0ca7",
      0,
    };
    // static u3j_harm _143_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
    static u3j_harm _143_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
    static c3_c* _143_pen__ut_fond_ha[] = {
      "e6f0ac3a903d7462a998093ee8772648dfa5f349c1a92c722c47236173780584",
      0,
    };
    static u3j_harm _143_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
    static c3_c* _143_pen__ut_fish_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
    static c3_c* _143_pen__ut_fuse_ha[] = {
      "492e29ce02a6440977dcb189d5bd892c4fed9f8c586a83f6f1a7973cf0c26c83",
      0,
    };
    static u3j_harm _143_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
    static c3_c* _143_pen__ut_mint_ha[] = {
      "fd9a27632b1c345625a3965cde12371bd444c3e0132dfa6d663aa82b9392dd1b",
      0,
    };
    static u3j_harm _143_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
    static c3_c* _143_pen__ut_mull_ha[] = {
      "0edf5ab7ce7fc7893ced2184df6b7b218dc371dad43e4f545281fedea272bbfd",
      0,
    };
    static u3j_harm _143_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
    static c3_c* _143_pen__ut_nest_ha[] = {
      "3e74b022bc38641cedc3dbb9a78384d4325d9227b1517c83411b83f2ea79f048",
      0,
    };
    static u3j_harm _143_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
    static c3_c* _143_pen__ut_peek_ha[] = {
      "acdf38ab8c65c15fd4d84f1d63607b6870cc030cd696f6197a7f815bcd87b4ec",
      0,
    };
    static u3j_harm _143_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
    static c3_c* _143_pen__ut_play_ha[] = {
      "2a2495a5b697aee9e5c66e08cf7a1808c29984b397f06fb82d30e4315d57aff2",
      0,
    };
    static u3j_harm _143_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
    static c3_c* _143_pen__ut_rest_ha[] = {
      0,
    };
    static u3j_harm _143_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
    static c3_c* _143_pen__ut_toss_ha[] = {
      "08d3c163f0f3e375595761129b07a7de81314fc0d5c864d343b920f5af2d3e8a",
      0,
    };
    static u3j_harm _143_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
    static c3_c* _143_pen__ut_wrap_ha[] = {
      0,
    };

  static c3_c* _143_pen__ut_ha[] = {
    "353a299bfb10d1e68b34dd5c2950a26efdfb9ed07af221232341158590f5e755",
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
  "b5e8b2005ffc86fc51f84be2042eed1bd45eddf51dd4e4e5d3a97290eb22ba8a",
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
    "5c85bf1a1e5cf0049d758c0549a4c5738e8064160f11692ce85c56f77a40430f",
    0,
  };

  static u3j_harm _143_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
  static c3_c* _143_qua__po_ind_ha[] = {
    "ac8d03331537ca664a44ea42427f944078d57f669568315843aad6c4038113a9",
    0,
  };
  static u3j_harm _143_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
  static c3_c* _143_qua__po_ins_ha[] = {
    "8a116a57268df234f621370d9aa1983ca15bbb49bba2e8384a8b90430f3b325a",
    0,
  };
  static u3j_harm _143_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
  static c3_c* _143_qua__po_tod_ha[] = {
    "a57c4f9d5678f8e46590603da2e385f6f5d9f62997e911933dcb1dea3de343b4",
    0,
  };
  static u3j_harm _143_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
  static c3_c* _143_qua__po_tos_ha[] = {
    "fec59aa7f960f453d931e21fa17f877e0d1d58a98891277aa6372c42874936e3",
    0,
  };

    static c3_c* _143_qua__po_ha[] = {
      "7aea092b4dc4ed511aeb60b9669319c64516120113ffd3cf18edf55044957679",
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
      "0a0fa420f15155cc780d3bee1b614f7dba587ea12ad3d216f8a9bcd06c6a549f",
      0,
    };
    static c3_c* _143_qua__bend_fun_ha[] = {
      "27906ec8a1d10c15e976245064ea5b0868608c92f58c7cf60afcd3fae0193b25",
      0,
    };
    static u3j_core _143_qua__bend_d[] =
      { { "fun", 7, _143_qua__bend_fun_a, 0, _143_qua__bend_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
    static c3_c* _143_qua__cold_ha[] = {
      "b3eeada7d2d7ce063830b7cbc9f7d36b2a2b9101e9c36a7b03eaebf2d1598389",
      0,
    };
    static c3_c* _143_qua__cold_fun_ha[] = {
      "b628559bb6eb765bd439454c582bd8adb9595075aed10b157e2fa7b4193248ab",
      0,
    };
    static u3j_core _143_qua__cold_d[] =
      { { "fun", 7, _143_qua__cold_fun_a, 0, _143_qua__cold_fun_ha },
        {}
      };

  static u3j_harm _143_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
    static c3_c* _143_qua__cook_ha[] = {
      "762985e18fe1c7007c6ad51445e449d24b1cec693294c8edc89da3bbb4f9f441",
      0,
    };
    static c3_c* _143_qua__cook_fun_ha[] = {
      "9b23b8aeadf056cee3f092468f70211b453ba4bdefc9229021800055bd47ea61",
      0,
    };
    static u3j_core _143_qua__cook_d[] =
      { { "fun", 7, _143_qua__cook_fun_a, 0, _143_qua__cook_fun_ha },
        {}
      };

  static u3j_harm _143_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
    static c3_c* _143_qua__comp_ha[] = {
      "eb085cbf01982751994cbaafc9ed7efe80758682af6d67aab646a972239401a0",
      0,
    };
    static c3_c* _143_qua__comp_fun_ha[] = {
      "e7d29e21ffd7e0d89a3408ef78ab5350516896c671e8fc240726f0d64fbad29b",
      0,
    };
    static u3j_core _143_qua__comp_d[] =
      { { "fun", 7, _143_qua__comp_fun_a, 0, _143_qua__comp_fun_ha },
        {}
      };

  static u3j_harm _143_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
    static c3_c* _143_qua__easy_ha[] = {
      "a30bd1a1006a06ca6cd6f01909f8745272d65739944425c1fd9682a6bed44999",
      0,
    };
    static c3_c* _143_qua__easy_fun_ha[] = {
      "4bbbc43ece463d961e572301d0824d3e3cab3ba09ec2756cbefae63ee106044b",
      0,
    };
    static u3j_core _143_qua__easy_d[] =
      { { "fun", 7, _143_qua__easy_fun_a, 0, _143_qua__easy_fun_ha },
        {}
      };

  static u3j_harm _143_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
    static c3_c* _143_qua__glue_ha[] = {
      "0b4722d874b19c25fad7f5630b8e46a46ea86d54e1db1600d7ebebf373dc44a5",
      0,
    };
    static c3_c* _143_qua__glue_fun_ha[] = {
      "ef2b7e80475e0e6aca6a8248a40829c382b262213594f6b9db1e990b5c3792af",
      0,
    };
    static u3j_core _143_qua__glue_d[] =
      { { "fun", 7, _143_qua__glue_fun_a, 0, _143_qua__glue_fun_ha },
        {}
      };

  static u3j_harm _143_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
    static c3_c* _143_qua__here_ha[] = {
      "1f7fe1031a68621b04bacad1bf3a39a8358e74f41db12969d7f39ddabe9eab1a",
      0,
    };
    static c3_c* _143_qua__here_fun_ha[] = {
      "25df3d2c1a0d9b809beaec9662a0f17c4f7762250c7675a7315216b97bb8767f",
      0,
    };
    static u3j_core _143_qua__here_d[] =
      { { "fun", 7, _143_qua__here_fun_a, 0, _143_qua__here_fun_ha },
        {}
      };

  static u3j_harm _143_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
    static c3_c* _143_qua__just_ha[] = {
      "48aa6959752752a63c2dd6d5064312755d57415df3fa7fbfc038466f5aa6874c",
      0,
    };
    static c3_c* _143_qua__just_fun_ha[] = {
      "287a8ed9a6b7bb26be684ac6ef095433f6d2ca5eaa6e71c3113553c8c897860b",
      0,
    };
    static u3j_core _143_qua__just_d[] =
      { { "fun", 7, _143_qua__just_fun_a, 0, _143_qua__just_fun_ha },
        {}
      };

  static u3j_harm _143_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
    static c3_c* _143_qua__mask_ha[] = {
      "cd12b6930b293cb898713870d9048ec4c598ef018427173f2c1a3a92a356dcb5",
      0,
    };
    static c3_c* _143_qua__mask_fun_ha[] = {
      "cf9868ba0b5eb8555846bf9436fb2aaf508d8cb283e10574f85d41fc0d1b6162",
      0,
    };
    static u3j_core _143_qua__mask_d[] =
      { { "fun", 7, _143_qua__mask_fun_a, 0, _143_qua__mask_fun_ha },
        {}
      };

  static u3j_harm _143_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
    static c3_c* _143_qua__shim_ha[] = {
      "0c204d6f7e7f105a5c2b64f77fa0bfb0ba6246a293868dcc63dbed00aa1c159d",
      0,
    };
    static c3_c* _143_qua__shim_fun_ha[] = {
      "3954cabbffe0576646976f16cf4333d72b636b7b5e1de6b4689da6d950530715",
      0,
    };
    static u3j_core _143_qua__shim_d[] =
      { { "fun", 7, _143_qua__shim_fun_a, 0, _143_qua__shim_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
    static c3_c* _143_qua__stag_ha[] = {
      "b0cf7747e734c4565467fa2702c37efcfd0ae19c59bd92f115a32d2f3200fedf",
      0,
    };
    static c3_c* _143_qua__stag_fun_ha[] = {
      "a2c40563eafd02090845b3250009239e98356f987908d3c4491a95007a433a02",
      0,
    };
    static u3j_core _143_qua__stag_d[] =
      { { "fun", 7, _143_qua__stag_fun_a, 0, _143_qua__stag_fun_ha },
        {}
      };

  static u3j_harm _143_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
    static c3_c* _143_qua__stew_ha[] = {
      "c444fd202534a0026defd2ee2ff283fee2a5d52ca4f4075bd0290164ae59cddd",
      0,
    };
    static c3_c* _143_qua__stew_fun_ha[] = {
      "673727e5a6ca9414042cf72f794b8fb4c014111aaf43d056554e0f5c5a429967",
      0,
    };
    static u3j_core _143_qua__stew_d[] =
      { { "fun", 31, _143_qua__stew_fun_a, 0, _143_qua__stew_fun_ha },
        {}
      };
 
  static u3j_harm _143_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
    static c3_c* _143_qua__stir_ha[] = {
      "9dd5a6197336769883ca246dab71cc7ea9997d355172891607f0cc6720b73e39",
      0,
    };
    static c3_c* _143_qua__stir_fun_ha[] = {
      "13f5792ea114749b242fe03ea6012cff3a8e435f06033df059c398023c8f706e",
      0,
    };
    static u3j_core _143_qua__stir_d[] =
      { { "fun", 7, _143_qua__stir_fun_a, 0, _143_qua__stir_fun_ha },
        {}
      };

  // static u3j_harm _143_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
  static u3j_harm _143_qua_plug_a[] = {{".2", u3we_plug}, {}};
  static c3_c* _143_qua__plug_ha[] = {
    "13088844b6585a6fbfc46de42d49dda8830e4db2cc90b12451f761064c9536e1",
    0,
  };
  static u3j_harm _143_qua_pose_a[] = {{".2", u3we_pose}, {}};
  static c3_c* _143_qua__pose_ha[] = {
    "7a80993302f8966d651a56fe46fd88e30a02d13d2f7ac5d472e6c6a66332e066",
    0,
  };
  //static u3j_harm _143_qua_sfix_a[] = {{".2", u3we_sfix}, {}};

  static u3j_harm _143_qua_mink_a[] = {{".2", u3we_mink}, {}};
  static c3_c* _143_qua_mink_ha[] = {
    "22517aa2c9e97f80de47307cc8d26ced480b10104a189bdbd478d842fa04d2fc",
    0,
  };
  static u3j_harm _143_qua_mule_a[] = {{".2", u3we_mule}, {}};
  static c3_c* _143_qua_mule_ha[] = {
    "690c6603a5d64e3bd15df17d7c736ac8a7acbab3c869211cc133fda65506a4a3",
    0,
  };

static c3_c* _143_qua_ha[] = {
  "5b402d501f23c2965a4cf270a6c7cad8898d1b25e9679ea419dbf6663edace4e",
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
    "6fd600b0b160128df4dc4cdd119a415e22ed864c7d90e985f960af4814d8ec02",
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
    "379203099b3e98165af1cdcc224e85da97889d8883c7732f8ba2cc71abcec1ac",
    0,
  };

static c3_c* _143_tri_ha[] = {
  "e19e164d1591dda79642169f894a6129d9effc76668a0218046ea512291b99e2",
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
    "facc483d8c2911821dffb6a36dabbacab99dd2e4141e37ddb3e19c42de5a99e3",
    0,
  };
  static u3j_harm _143_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
  static c3_c* _143_two_lent_ha[] = {
    "ecf53a78c22d98e2dfb7dbe44169a6d6665d167821696f89c1c3176aa79bcc0b",
    0,
  };
  static u3j_harm _143_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
  static c3_c* _143_two_levy_ha[] = {
    "8f02ef06c48cc4fb63648546b3f805d19401ed30d83976b04983c2f44c81f3f5",
    0,
  };
  static u3j_harm _143_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
  static c3_c* _143_two_lien_ha[] = {
    0,
  };
  static u3j_harm _143_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
  static c3_c* _143_two_murn_ha[] = {
    "7c675855df3726786a9678309b0aec910227e77d73bc4a9b75152ff75d2ff995",
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
    "d2a4bd27dad0931e082629bb64a7144b4dd5aa53cab045dfaceae54ff6e3ab3b",
    0,
  };
  static u3j_harm _143_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
  static c3_c* _143_two_roll_ha[] = {
    "44e12087e29a5e025e45589a0ef923fe6df8b8f49d735af4167c8eb5ab68080e",
    0,
  };
  static u3j_harm _143_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
  static c3_c* _143_two_skid_ha[] = {
    "60f95e823597e3385f7472edec19d90c85aeffc4b8bcbcf1d54b365ce6c5aa23",
    0,
  };
  static u3j_harm _143_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
  static c3_c* _143_two_skim_ha[] = {
    "de6ee398b8264fe4e77ed63007a7b5f92dc81d1613da7d6e31003f5fa74ee12b",
    0,
  };
  static u3j_harm _143_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
  static c3_c* _143_two_skip_ha[] = {
    "90e9162bb117cb9a68939d6910eb854663cd6aff03c530dd9654628480469cfc",
    0,
  };
  static u3j_harm _143_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
  static c3_c* _143_two_scag_ha[] = {
    "c910ee62801c9113c9f689515defdf8b159b35f593eee8e396b4b5e4017c7b40",
    0,
  };
  static u3j_harm _143_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
  static c3_c* _143_two_slag_ha[] = {
    "95b4e2050be824e44f4baaa951d1cc191f116a846711cd7329c9cbd88f965a09",
    0,
  };
  static u3j_harm _143_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
  static c3_c* _143_two_snag_ha[] = {
    "d6d4dbda4f29b3cde0a3d2bc3c59df4f1eddcc22810bfbf3130f0a1e14fe2c80",
    0,
  };
  // static u3j_harm _143_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
  static u3j_harm _143_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
  static c3_c* _143_two_turn_ha[] = {
    "7ca8d0d1e9b5fc3ab9cce9ceba83a170a7298be4a92b58b58354585ce71c75c9",
    0,
  };
  static u3j_harm _143_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
  static c3_c* _143_two_weld_ha[] = {
    "82fb71bd085c6657ce1eecc399143e35588fb10c02db6b7ab2bf2a0adabff49e",
    0,
  };

  static u3j_harm _143_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
  static c3_c* _143_two_bex_ha[] = {
    "c9738c81e31bd813c9b53c2a99c426f67769cb55ebd0186b48b482cc22115ccd",
    0,
  };
  static u3j_harm _143_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
  static c3_c* _143_two_can_ha[] = {
    "7c8a43f690431660885ecf336f567ba405466404bbdf4057da0eb8b2f56f295c",
    0,
  };
  static u3j_harm _143_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
  static c3_c* _143_two_cat_ha[] = {
    "82ce75876bb5c44812c78264598d61acf4679b401a9a12130acb2d5f20beb9d5",
    0,
  };
  static u3j_harm _143_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
  static c3_c* _143_two_con_ha[] = {
    "f78cb0e4959ae6b4449a0dc7d3cdc0629718bb7a153838c1777f91e8000feea9",
    0,
  };
  static u3j_harm _143_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
  static c3_c* _143_two_cut_ha[] = {
    "b55a0ffd9320c44b5050ea6103abf2da309d7ee2c86d9463cdf0ad45e21bdbb9",
    0,
  };
  static u3j_harm _143_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
  static c3_c* _143_two_dis_ha[] = {
    "a64330428923afdf18b8a5e2a4b6cb8370cf07428055539d43e4cc98f6a3996f",
    0,
  };
  static u3j_harm _143_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
  static c3_c* _143_two_dor_ha[] = {
    0,
  };
  static u3j_harm _143_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
  static c3_c* _143_two_end_ha[] = {
    "e24495cc46efa7cf294477301c5fe35a289e503a104c01eca92edb692881bb49",
    0,
  };
  static u3j_harm _143_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
  static c3_c* _143_two_gor_ha[] = {
    "30ff6eb815ffa5ed1ba120143906d2de4bb8095099b616d35641a46f21646a84",
    0,
  };
  static u3j_harm _143_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
  static c3_c* _143_two_hor_ha[] = {
    "7d65051bf64017bac1199427ec4338e898b8522b8263e7e82e24dc7aec534a6f",
    0,
  };
  static u3j_harm _143_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
  static c3_c* _143_two_lsh_ha[] = {
    "67d04cf1cc99459994d7b157771cdb2918c7c416cf3aebf8fb1600167e136563",
    0,
  };
  static u3j_harm _143_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
  static c3_c* _143_two_met_ha[] = {
    "87dc9967fe71315464bf2d626d68641c1ff2eb08e765e182e8c64054e7e31d09",
    0,
  };
  static u3j_harm _143_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
  static c3_c* _143_two_mix_ha[] = {
    "7cd98d76ba4af2461bc5fad2e0ccb7ca65161aca7205945709cd8a04961778a5",
    0,
  };
  static u3j_harm _143_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
  static c3_c* _143_two_mug_ha[] = {
    0,
  };
  static u3j_harm _143_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
  static c3_c* _143_two_muk_ha[] = {
    "d10798c3330555c586b63dec9e55aabc8fb06f1dacc0a4eccddb73a8fc32fa32",
    0,
  };
  static u3j_harm _143_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
  static c3_c* _143_two_pow_ha[] = {
    0,
  };
  static u3j_harm _143_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
  static c3_c* _143_two_rap_ha[] = {
    "92a44a93ee7824201abddf126339a1ecd3b9057f73d2788043693e712a41cdcd",
    0,
  };
  static u3j_harm _143_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
  static c3_c* _143_two_rep_ha[] = {
    0,
  };
  static u3j_harm _143_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
  static c3_c* _143_two_rip_ha[] = {
    "8c4888f617dc007aad1d661fcbff97d3a4539e3aa56a9c36cd550b637cf3d976",
    0,
  };
  static u3j_harm _143_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
  static c3_c* _143_two_rsh_ha[] = {
    "4f9ed2377189bbbd4c1f6b50d99e3a8b3609a4e4dc35f294e01bcd5a5445cec6",
    0,
  };
  static u3j_harm _143_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
  static c3_c* _143_two_sqt_ha[] = {
    0,
  };
  static u3j_harm _143_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
  static c3_c* _143_two_vor_ha[] = {
    "929c18cae2ce59b33e39d7d9108e4f22a4b0fb346d626aeeb50fe35a38db18ee",
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
      "bed276f4863f42ab18dd56609b739ab858c5880b330b1d8db9a0ee630a1b0ef3",
      0,
    };
    static u3j_harm _143_two__in_has_a[] = {{".2", u3wdi_has}, {}};
    static c3_c* _143_two__in_has_ha[] = {
      "9bf14a279421b5f09a7ee0864082a2d8e9149e18112574ceaceeb436c49c03af",
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
      "ff02e2bf371279c4658420d0b973bd981d297222702ed59174d936c0aca00142",
      0,
    };
    static u3j_harm _143_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
    static c3_c* _143_two__in_tap_ha[] = {
      "2ce39c34e5306fccf83db8035bdeb0e50d254d6f62017369546916519d58a49b",
      0,
    };
    static u3j_harm _143_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
    static c3_c* _143_two__in_wyt_ha[] = {
      "9ad9a550c586148279ea523f58bbe3360821fe3b0d1b93bdd0f87bfd51c80bd3",
      0,
    };
    static u3j_harm _143_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
    static c3_c* _143_two__in_uni_ha[] = {
      "36c5a475b7e6a1c991b206e0e40e8e2b439c6ef4a01ae210bdcbc1d18bcd779a",
      0,
    };

  static c3_c* _143_two__in_ha[] = {
    "8fe005605f2949e9004684e261ec821dfa2974f2d802d9dc0812b17f975594d1",
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
      "f5e0ed0062a29e1f6215d7d2a707933b04fdc1e69c762a06fdd50f491ce360d8",
      0,
    };
    static u3j_harm _143_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
    static c3_c* _143_two__by_get_ha[] = {
      "8bc3d77b5044e488ee8ee0b80fca18f53a2116282f603211c4ee2e8e58a08623",
      0,
    };
    static u3j_harm _143_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
    static c3_c* _143_two__by_has_ha[] = {
      "2483675ead9aa11d3454fa22c1c7be9464261bbd20eb8981b68ac149a595bd0d",
      0,
    };
    // static u3j_harm _143_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
    static u3j_harm _143_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
    static c3_c* _143_two__by_put_ha[] = {
      "a6019be8da4c18a336b4a1aeb1d3c4956764a54f3c2383417ba1747707bdd9f0",
      0,
    };
    static u3j_harm _143_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
    static c3_c* _143_two__by_tap_ha[] = {
      "2ce39c34e5306fccf83db8035bdeb0e50d254d6f62017369546916519d58a49b",
      0,
    };
    // static u3j_harm _143_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};

  static c3_c* _143_two__by_ha[] = {
    "ea6f795e84e7a982595e3713241181072e8c2254c122c9bec6c176a44d74be38",
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
    "4fdc5aff57adf3b0665dbf3f7cae00c92b0470ee74f79c054e63ed9b2c2a3761",
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
  "961d066b817b1dcac58e175d2bc49745ff096b39a9f81ae7c50bd4f69ed60673",
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
    "f56080814115fa58aa5b4c21948bd5f36587cf8b24a66b64d95665c7b34adbf3",
    0,
  };
  static u3j_harm _143_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
  static c3_c* _143_one_dec_ha[] = {
    "036cac68adadd083df7a426ea1ee974f84823e3d3a840fec31c7610f19688fe7",
    0,
  };
  static u3j_harm _143_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
  static c3_c* _143_one_div_ha[] = {
    "b9bdb2e64a1f585509a5b3883f8d7dba1471fea38ff316cf7b2b970ba60def49",
    0,
  };
  static u3j_harm _143_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
  static c3_c* _143_one_dvr_ha[] = {
    0,
  };
  static u3j_harm _143_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
  static c3_c* _143_one_gte_ha[] = {
    "4fa8535a1650ca96d16e17a9f4e77b779fe017f4585a6397370c4251057c439c",
    0,
  };
  static u3j_harm _143_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
  static c3_c* _143_one_gth_ha[] = {
    "042b58314829f4d8af8747b330e3c10a4d02ed574e99b996a9cc46c4a53588a1",
    0,
  };
  static u3j_harm _143_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
  static c3_c* _143_one_lte_ha[] = {
    "892702a3bf38377ad877b43effe6e9cc428d398beb0793bcefae7c5d06d10477",
    0,
  };
  static u3j_harm _143_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
  static c3_c* _143_one_lth_ha[] = {
   "637fcd910ab0e253640efa6a473b5fec809fb8ab9326dab987f9b361b1378ffa",
    0,
  };
  static u3j_harm _143_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
  static c3_c* _143_one_mod_ha[] = {
    "175121db3c63288c435b860d6e793f9155b19ae5a9b0659f19ce05826c131588",
    0,
  };
  static u3j_harm _143_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
  static c3_c* _143_one_mul_ha[] = {
    "5417b2b3c5fdf39f4717e37bd9f833262ca6ae893d691f5e1f0962fd90ed3a6e",
    0,
  };
  static u3j_harm _143_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
  static c3_c* _143_one_sub_ha[] = {
    "448497a0aecbf1a0858b307415adad729db64a55f42c9f57085b8e561557f71c",
    0,
  };
  static u3j_harm _143_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
  static c3_c* _143_one_cap_ha[] = {
    "60a1602892f3295d380d51ab567b128c6cfc740b154f40c465b4bc4eec49eec9",
    0,
  };
  static u3j_harm _143_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
  static c3_c* _143_one_peg_ha[] = {
    "520eb037f25f74439b80cb3de5081c4b6dcdee3e929d2c7803d79abe8fe3e7c1",
    0,
  };
  static u3j_harm _143_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
  static c3_c* _143_one_mas_ha[] = {
    "350a2bf08b62e53e3911ea3b0587f871c80625fc1fb969ce794618910a9ef11d",
    0,
  };

static c3_c* _143_one_ha[] = {
  "694fc75229f41568a9679c8606c61c881c4e2662c481ca3f0cea0c682a42e1d7",
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
  "9b82a903093c077afb3f0b9d4e95e1a9c9789d1ca605b57bbacf79857e3d5c52",
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
