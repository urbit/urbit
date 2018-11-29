/* j/tree.c
*/
#include "all.h"

static u3j_harm _141_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
static c3_c* _141_hex_aes_ecba_en_ha[] = {0};
static u3j_harm _141_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
static c3_c* _141_hex_aes_ecba_de_ha[] = {0};
static u3j_harm _141_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
static c3_c* _141_hex_aes_ecbb_en_ha[] = {0};
static u3j_harm _141_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
static c3_c* _141_hex_aes_ecbb_de_ha[] = {0};
static u3j_harm _141_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
static c3_c* _141_hex_aes_ecbc_en_ha[] = {0};
static u3j_harm _141_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};
static c3_c* _141_hex_aes_ecbc_de_ha[] = {0};

static u3j_harm _141_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
static c3_c* _141_hex_aes_cbca_en_ha[] = {0};
static u3j_harm _141_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
static c3_c* _141_hex_aes_cbca_de_ha[] = {0};
static u3j_harm _141_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
static c3_c* _141_hex_aes_cbcb_en_ha[] = {0};
static u3j_harm _141_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
static c3_c* _141_hex_aes_cbcb_de_ha[] = {0};
static u3j_harm _141_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
static c3_c* _141_hex_aes_cbcc_en_ha[] = {0};
static u3j_harm _141_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};
static c3_c* _141_hex_aes_cbcc_de_ha[] = {0};

static u3j_core _141_hex_aes_ecba_d[] =
  { { "en", 7, _141_hex_aes_ecba_en_a, 0, _141_hex_aes_ecba_en_ha },
    { "de", 7, _141_hex_aes_ecba_de_a, 0, _141_hex_aes_ecba_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecba_ha[] = {0};

static u3j_core _141_hex_aes_ecbb_d[] =
  { { "en", 7, _141_hex_aes_ecbb_en_a, 0, _141_hex_aes_ecbb_en_ha },
    { "de", 7, _141_hex_aes_ecbb_de_a, 0, _141_hex_aes_ecbb_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecbb_ha[] = {0};

static u3j_core _141_hex_aes_ecbc_d[] =
  { { "en", 7, _141_hex_aes_ecbc_en_a, 0, _141_hex_aes_ecbc_en_ha },
    { "de", 7, _141_hex_aes_ecbc_de_a, 0, _141_hex_aes_ecbc_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecbc_ha[] = {0};

static u3j_core _141_hex_aes_cbca_d[] =
  { { "en", 7, _141_hex_aes_cbca_en_a, 0, _141_hex_aes_cbca_en_ha },
    { "de", 7, _141_hex_aes_cbca_de_a, 0, _141_hex_aes_cbca_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbca_ha[] = {0};

static u3j_core _141_hex_aes_cbcb_d[] =
  { { "en", 7, _141_hex_aes_cbcb_en_a, 0, _141_hex_aes_cbcb_en_ha },
    { "de", 7, _141_hex_aes_cbcb_de_a, 0, _141_hex_aes_cbcb_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbcb_ha[] = {0};

static u3j_core _141_hex_aes_cbcc_d[] =
  { { "en", 7, _141_hex_aes_cbcc_en_a, 0, _141_hex_aes_cbcc_en_ha },
    { "de", 7, _141_hex_aes_cbcc_de_a, 0, _141_hex_aes_cbcc_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbcc_ha[] = {0};

static u3j_core _141_hex_aes_d[] =
  { { "ecba", 7, 0, _141_hex_aes_ecba_d, _141_hex_aes_ecba_ha },
    { "ecbb", 7, 0, _141_hex_aes_ecbb_d, _141_hex_aes_ecbb_ha },
    { "ecbc", 7, 0, _141_hex_aes_ecbc_d, _141_hex_aes_ecbc_ha },
    { "cbca", 7, 0, _141_hex_aes_cbca_d, _141_hex_aes_cbca_ha },
    { "cbcb", 7, 0, _141_hex_aes_cbcb_d, _141_hex_aes_cbcb_ha },
    { "cbcc", 7, 0, _141_hex_aes_cbcc_d, _141_hex_aes_cbcc_ha },
    {}
  };
static c3_c* _141_hex_aes_ha[] = {0};

static u3j_harm _141_hex_down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
static c3_c* _141_hex_down_mark_ha[] = {0};

  static u3j_core _141_hex_down_d[] =
    { { "mark", 7, _141_hex_down_mark_a, 0, _141_hex_down_mark_ha },
      {}
    };
static c3_c* _141_hex_down_ha[] = {0};

static u3j_harm _141_hex_lore_a[] = {{".2", u3we_lore}, {}};
static c3_c* _141_hex_lore_ha[] = {0};
static u3j_harm _141_hex_loss_a[] = {{".2", u3we_loss}, {}};
static c3_c* _141_hex_loss_ha[] = {0};
static u3j_harm _141_hex_lune_a[] = {{".2", u3we_lune}, {}};
static c3_c* _141_hex_lune_ha[] = {0};

static u3j_harm _141_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
static c3_c* _141_hex_coed__ed_puck_ha[] = {0};
static u3j_harm _141_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
static c3_c* _141_hex_coed__ed_sign_ha[] = {0};
static u3j_harm _141_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
static c3_c* _141_hex_coed__ed_veri_ha[] = {0};
static u3j_harm _141_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};
static c3_c* _141_hex_coed__ed_shar_ha[] = {0};

static u3j_core _141_hex_coed__ed_d[] =
  { { "sign", 7, _141_hex_coed__ed_sign_a, 0, _141_hex_coed__ed_sign_ha },
    { "puck", 7, _141_hex_coed__ed_puck_a, 0, _141_hex_coed__ed_puck_ha },
    { "veri", 7, _141_hex_coed__ed_veri_a, 0, _141_hex_coed__ed_veri_ha },
    { "shar", 7, _141_hex_coed__ed_shar_a, 0, _141_hex_coed__ed_shar_ha },
    {}
  };
static c3_c* _141_hex_coed__ed_ha[] = {0};

static u3j_core _141_hex_coed_d[] =
{ { "ed", 3, 0, _141_hex_coed__ed_d, _141_hex_coed__ed_ha },
  {}
};
static c3_c* _141_hex_coed_ha[] = {0};

  static u3j_harm _141_hex_hmac_hmac_a[] = {{".2", u3we_hmac}, {}};
  static c3_c* _141_hex_hmac_hmac_ha[] = {0};
static u3j_core _141_hex_hmac_d[] =
  { { "hmac", 7, _141_hex_hmac_hmac_a, 0, _141_hex_hmac_hmac_ha },
    {}
  };
static c3_c* _141_hex_hmac_ha[] = {0};

  static u3j_harm _141_hex_argon2_a[] = {{".2", u3we_argon2}, {}};
  static c3_c* _141_hex_argon2_ha[] = {0};
static u3j_core _141_hex_argon_d[] =
  { { "argon2", 7, _141_hex_argon2_a, 0, _141_hex_argon2_ha },
    {}
  };
static c3_c* _141_hex_argon_ha[] = {0};

  static u3j_harm _141_hex_secp_make_a[] = {{".2", u3we_make, c3y}, {}};
  static c3_c* _141_hex_secp_make_ha[] = {0};
  static u3j_harm _141_hex_secp_sign_a[] = {{".2", u3we_sign, c3y}, {}};
  static c3_c* _141_hex_secp_sign_ha[] = {0};
  static u3j_harm _141_hex_secp_reco_a[] = {{".2", u3we_reco, c3y}, {}};
  static c3_c* _141_hex_secp_reco_ha[] = {0};
static u3j_core _141_hex_secp_secp_helper_d[] =
  { { "make-k",            7, _141_hex_secp_make_a, 0, _141_hex_secp_make_ha },
    { "ecdsa-raw-sign",    7, _141_hex_secp_sign_a, 0, _141_hex_secp_sign_ha },
    { "ecdsa-raw-recover", 7, _141_hex_secp_reco_a, 0, _141_hex_secp_reco_ha },
    {}
  };
static c3_c* _141_hex_secp_secp_helper_ha[] = {0};

static u3j_core _141_hex_secp_secp_d[] =
  { { "helper", 7, 0, _141_hex_secp_secp_helper_d, _141_hex_secp_secp_helper_ha },
    {}
  };
static c3_c* _141_hex_secp_secp_ha[] = {0};

static u3j_core _141_hex_secp_d[] =
  { { "secp", 7, 0, _141_hex_secp_secp_d, _141_hex_secp_secp_ha },
    {}
  };
static c3_c* _141_hex_secp_ha[] = {0};

  static u3j_harm _141_hex_blake2b_a[] = {{".2", u3we_blake, c3y}, {}};
  static c3_c* _141_hex_blake2b_ha[] = {0};
static u3j_core _141_hex_blake_d[] =
  { { "blake2b", 7, _141_hex_blake2b_a, 0, _141_hex_blake2b_ha },
    {}
  };
static c3_c* _141_hex_blake_ha[] = {0};

  static u3j_harm _141_hex_ripemd_160_a[] = {{".2", u3we_ripe, c3y}, {}};
  static c3_c* _141_hex_ripemd_160_ha[] = {0};
static u3j_core _141_hex_ripe_d[] =
  { { "ripemd160", 7, _141_hex_ripemd_160_a, 0, _141_hex_ripemd_160_ha },
    {}
  };
static c3_c* _141_hex_ripe_ha[] = {0};


static u3j_core _141_hex_d[] =
{ { "down", 8063, 0, _141_hex_down_d, _141_hex_down_ha },

  { "lore",   63, _141_hex_lore_a, 0, _141_hex_lore_ha },
  { "loss",   63, _141_hex_loss_a, 0, _141_hex_loss_ha },
  { "lune",  127, _141_hex_lune_a, 0, _141_hex_lune_ha },

  { "coed",   63, 0, _141_hex_coed_d, _141_hex_coed_ha },
  { "aes",    31, 0, _141_hex_aes_d, _141_hex_aes_ha },

  { "hmac",   31, 0, _141_hex_hmac_d,  _141_hex_hmac_ha  },
  { "argon",  15, 0, _141_hex_argon_d, _141_hex_argon_ha },
  { "blake",  15, 0, _141_hex_blake_d, _141_hex_blake_ha },
  { "ripemd", 15, 0, _141_hex_ripe_d,  _141_hex_ripe_ha  },
  { "secp",   15, 0, _141_hex_secp_d,  _141_hex_secp_ha  },
  {}
};
static c3_c* _141_hex_ha[] = {0};

/* layer five
*/
static u3j_harm _141_pen_cell_a[] = {{".2", u3wf_cell}, {}};
static c3_c* _141_pen_cell_ha[] = {0};
static u3j_harm _141_pen_comb_a[] = {{".2", u3wf_comb}, {}};
static c3_c* _141_pen_comb_ha[] = {0};
static u3j_harm _141_pen_cons_a[] = {{".2", u3wf_cons}, {}};
static c3_c* _141_pen_cons_ha[] = {0};
static u3j_harm _141_pen_core_a[] = {{".2", u3wf_core}, {}};
static c3_c* _141_pen_core_ha[] = {0};
static u3j_harm _141_pen_face_a[] = {{".2", u3wf_face}, {}};
static c3_c* _141_pen_face_ha[] = {0};
static u3j_harm _141_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
static c3_c* _141_pen_fitz_ha[] = {0};
static u3j_harm _141_pen_flan_a[] = {{".2", u3wf_flan}, {}};
static c3_c* _141_pen_flan_ha[] = {0};
static u3j_harm _141_pen_flip_a[] = {{".2", u3wf_flip}, {}};
static c3_c* _141_pen_flip_ha[] = {0};
static u3j_harm _141_pen_flor_a[] = {{".2", u3wf_flor}, {}};
static c3_c* _141_pen_flor_ha[] = {0};
static u3j_harm _141_pen_fork_a[] = {{".2", u3wf_fork}, {}};
static c3_c* _141_pen_fork_ha[] = {0};

// hike disabled while implementing edit
// static u3j_harm _141_pen_hike_a[] = {{".2", u3wf_hike}, {}};
// static c3_c* _141_pen_hike_ha[] = {0};

static u3j_harm _141_pen_look_a[] = {{".2", u3wf_look}, {}};
static c3_c* _141_pen_look_ha[] = {0};
static u3j_harm _141_pen_loot_a[] = {{".2", u3wf_loot}, {}};
static c3_c* _141_pen_loot_ha[] = {0};

  static u3j_harm _141_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
  static c3_c* _141_pen__ut_crop_ha[] = {0};
  static u3j_harm _141_pen__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
  static c3_c* _141_pen__ut_fire_ha[] = {0};
  static u3j_harm _141_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
  static c3_c* _141_pen__ut_fond_ha[] = {0};
  static u3j_harm _141_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
  static c3_c* _141_pen__ut_fish_ha[] = {0};
  static u3j_harm _141_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
  static c3_c* _141_pen__ut_fuse_ha[] = {0};
  static u3j_harm _141_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
  static c3_c* _141_pen__ut_mint_ha[] = {0};
  static u3j_harm _141_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
  static c3_c* _141_pen__ut_mull_ha[] = {0};
  static u3j_harm _141_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
  static c3_c* _141_pen__ut_nest_ha[] = {0};
  static u3j_harm _141_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
  static c3_c* _141_pen__ut_peek_ha[] = {0};
  static u3j_harm _141_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
  static c3_c* _141_pen__ut_play_ha[] = {0};
  static u3j_harm _141_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
  static c3_c* _141_pen__ut_rest_ha[] = {0};
  static u3j_harm _141_pen__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
  static c3_c* _141_pen__ut_toss_ha[] = {0};
  static u3j_harm _141_pen__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};
  static c3_c* _141_pen__ut_wrap_ha[] = {0};

static u3j_core _141_pen__ut_d[] =
  {
    { "crop", 7, _141_pen__ut_crop_a, 0, _141_pen__ut_crop_ha },
    { "fond", 7, _141_pen__ut_fond_a, 0, _141_pen__ut_fond_ha },
    { "fire", 7, _141_pen__ut_fire_a, 0, _141_pen__ut_fire_ha },
    { "fish", 7, _141_pen__ut_fish_a, 0, _141_pen__ut_fish_ha },
    { "fuse", 7, _141_pen__ut_fuse_a, 0, _141_pen__ut_fuse_ha },
    { "mint", 7, _141_pen__ut_mint_a, 0, _141_pen__ut_mint_ha },
    { "mull", 7, _141_pen__ut_mull_a, 0, _141_pen__ut_mull_ha },
    { "nest", 7, _141_pen__ut_nest_a, 0, _141_pen__ut_nest_ha },
    { "peek", 7, _141_pen__ut_peek_a, 0, _141_pen__ut_peek_ha },
    { "play", 7, _141_pen__ut_play_a, 0, _141_pen__ut_play_ha },
    { "rest", 7, _141_pen__ut_rest_a, 0, _141_pen__ut_rest_ha },
    { "toss", 7, _141_pen__ut_toss_a, 0, _141_pen__ut_toss_ha },
    { "wrap", 7, _141_pen__ut_wrap_a, 0, _141_pen__ut_wrap_ha },
    {}
  };
static u3j_harm _141_pen__ut_a[] =
  { {"repo", u3wfu_repo},
    {}
  };

static c3_c* _141_pen__ut_ha[] = {0};

static u3j_hood _141_pen__ut_ho[] = {
  // %ar     ar
  { "fan",  28, c3n },
  { "rib",  58, c3n },
  { "vet", 118, c3n },
  { "fab", 119, c3n },

  { "blow",   49131 },
  // %burp   burp
  { "busk", 0x5ff57 },
  { "buss",      94 },
  { "crop",       4 },
  { "duck",   48087 },
  { "dune",    1524 },
  { "dunk",     763 },
  { "epla",   12283 },
  { "emin",    5998 },
  { "emul",      86 },
  // %feel   feel
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
  // %mile   mile
  // %mine   mine
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

// XX figure out why this is disabled
#if 0
static u3j_harm _141_pen__ap_a[] =
  { {"open", u3wfp_open},
    {"rake", u3wfp_rake},
    {}
  };
static c3_c* _141_pen__ap_ha[] = {0};

static u3j_harm _141_pen__al_a[] =
  { {"bunt", u3wfl_bunt},
    {"whip", u3wfl_whip},
    {}
  };
static c3_c* _141_pen__al_ha[] = {0};
#endif

static u3j_core _141_pen_d[] =
{ { "hex", 3, 0, _141_hex_d, _141_hex_ha },

  { "cell", 7, _141_pen_cell_a, 0, _141_pen_cell_ha },
  { "comb", 7, _141_pen_comb_a, 0, _141_pen_comb_ha },
  { "cons", 7, _141_pen_cons_a, 0, _141_pen_cons_ha },
  { "core", 7, _141_pen_core_a, 0, _141_pen_core_ha },
  { "face", 7, _141_pen_face_a, 0, _141_pen_face_ha },
  { "fitz", 7, _141_pen_fitz_a, 0, _141_pen_fitz_ha },
  { "flan", 7, _141_pen_flan_a, 0, _141_pen_flan_ha },
  { "flip", 7, _141_pen_flip_a, 0, _141_pen_flip_ha },
  { "flor", 7, _141_pen_flor_a, 0, _141_pen_flor_ha },
  { "fork", 7, _141_pen_fork_a, 0, _141_pen_fork_ha },
  // { "hike", 7, _141_pen_hike_a, 0, _141_pen_hike_ha },
  { "look", 7, _141_pen_look_a, 0, _141_pen_look_ha },
  { "loot", 7, _141_pen_loot_a, 0, _141_pen_loot_ha },

  // { "ap", 7, _141_pen__ap_a, 0, _141_pen__ap_ha },
  // { "al", 7, _141_pen__al_a, 0, _141_pen__al_ha },
  { "ut", 15, _141_pen__ut_a, _141_pen__ut_d, _141_pen__ut_ha, _141_pen__ut_ho },
  {}
};
static c3_c* _141_pen_ha[] = {0};

static u3j_hood _141_pen_ho[] = {
  { "ap", 24412 },
  { "ut", 11262 },
  {},
};

/* layer four
*/
static u3j_harm _141_qua_trip_a[] = {{".2", u3we_trip}, {}};
static c3_c* _141_qua_trip_ha[] = {0};

static u3j_harm _141_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
static c3_c* _141_qua__po_ind_ha[] = {0};
static u3j_harm _141_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
static c3_c* _141_qua__po_ins_ha[] = {0};
static u3j_harm _141_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
static c3_c* _141_qua__po_tod_ha[] = {0};
static u3j_harm _141_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
static c3_c* _141_qua__po_tos_ha[] = {0};
  static u3j_core _141_qua__po_d[] =
    { { "ind", 7, _141_qua__po_ind_a, 0, _141_qua__po_ind_ha },
      { "ins", 7, _141_qua__po_ins_a, 0, _141_qua__po_ins_ha },
      { "tod", 7, _141_qua__po_tod_a, 0, _141_qua__po_tod_ha },
      { "tos", 7, _141_qua__po_tos_a, 0, _141_qua__po_tos_ha },
      {}
    };
  static c3_c* _141_qua__po_ha[] = {0};

static u3j_harm _141_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
static c3_c* _141_qua__bend_fun_ha[] = {0};
  static u3j_core _141_qua__bend_d[] =
    { { "fun", 7, _141_qua__bend_fun_a, 0, _141_qua__bend_fun_ha },
      {}
    };
  static c3_c* _141_qua__bend_ha[] = {0};

static u3j_harm _141_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
static c3_c* _141_qua__cold_fun_ha[] = {0};
  static u3j_core _141_qua__cold_d[] =
    { { "fun", 7, _141_qua__cold_fun_a, 0, _141_qua__cold_fun_ha },
      {}
    };
  static c3_c* _141_qua__cold_ha[] = {0};

static u3j_harm _141_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
static c3_c* _141_qua__cook_fun_ha[] = {0};
  static u3j_core _141_qua__cook_d[] =
    { { "fun", 7, _141_qua__cook_fun_a, 0, _141_qua__cook_fun_ha },
      {}
    };
  static c3_c* _141_qua__cook_ha[] = {0};

static u3j_harm _141_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
static c3_c* _141_qua__comp_fun_ha[] = {0};
  static u3j_core _141_qua__comp_d[] =
    { { "fun", 7, _141_qua__comp_fun_a, 0, _141_qua__comp_fun_ha },
      {}
    };
  static c3_c* _141_qua__comp_ha[] = {0};

static u3j_harm _141_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
static c3_c* _141_qua__easy_fun_ha[] = {0};
  static u3j_core _141_qua__easy_d[] =
    { { "fun", 7, _141_qua__easy_fun_a, 0, _141_qua__easy_fun_ha },
      {}
    };
  static c3_c* _141_qua__easy_ha[] = {0};

static u3j_harm _141_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
static c3_c* _141_qua__glue_fun_ha[] = {0};
  static u3j_core _141_qua__glue_d[] =
    { { "fun", 7, _141_qua__glue_fun_a, 0, _141_qua__glue_fun_ha },
      {}
    };
  static c3_c* _141_qua__glue_ha[] = {0};

static u3j_harm _141_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
static c3_c* _141_qua__here_fun_ha[] = {0};
  static u3j_core _141_qua__here_d[] =
    { { "fun", 7, _141_qua__here_fun_a, 0, _141_qua__here_fun_ha },
      {}
    };
  static c3_c* _141_qua__here_ha[] = {0};

static u3j_harm _141_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
static c3_c* _141_qua__just_fun_ha[] = {0};
  static u3j_core _141_qua__just_d[] =
    { { "fun", 7, _141_qua__just_fun_a, 0, _141_qua__just_fun_ha },
      {}
    };
  static c3_c* _141_qua__just_ha[] = {0};

static u3j_harm _141_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
static c3_c* _141_qua__mask_fun_ha[] = {0};
  static u3j_core _141_qua__mask_d[] =
    { { "fun", 7, _141_qua__mask_fun_a, 0, _141_qua__mask_fun_ha },
      {}
    };
  static c3_c* _141_qua__mask_ha[] = {0};

static u3j_harm _141_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
static c3_c* _141_qua__shim_fun_ha[] = {0};
  static u3j_core _141_qua__shim_d[] =
    { { "fun", 7, _141_qua__shim_fun_a, 0, _141_qua__shim_fun_ha },
      {}
    };
  static c3_c* _141_qua__shim_ha[] = {0};

static u3j_harm _141_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
static c3_c* _141_qua__stag_fun_ha[] = {0};
  static u3j_core _141_qua__stag_d[] =
    { { "fun", 7, _141_qua__stag_fun_a, 0, _141_qua__stag_fun_ha },
      {}
    };
  static c3_c* _141_qua__stag_ha[] = {0};

static u3j_harm _141_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
static c3_c* _141_qua__stew_fun_ha[] = {0};
  static u3j_core _141_qua__stew_d[] =
    { { "fun", 7, _141_qua__stew_fun_a, 0, _141_qua__stew_fun_ha },
      {}
    };
  static c3_c* _141_qua__stew_ha[] = {0};

static u3j_harm _141_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
static c3_c* _141_qua__stir_fun_ha[] = {0};
  static u3j_core _141_qua__stir_d[] =
    { { "fun", 7, _141_qua__stir_fun_a, 0, _141_qua__stir_fun_ha },
      {}
    };
  static c3_c* _141_qua__stir_ha[] = {0};

// duplicate core re sfix
// static u3j_harm _141_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
// static c3_c* _141_qua_pfix_ha[] = {0};

static u3j_harm _141_qua_plug_a[] = {{".2", u3we_plug}, {}};
static c3_c* _141_qua_plug_ha[] = {0};
static u3j_harm _141_qua_pose_a[] = {{".2", u3we_pose}, {}};
static c3_c* _141_qua_pose_ha[] = {0};

// duplicate core re pfix
// static u3j_harm _141_qua_sfix_a[] = {{".2", u3we_sfix}, {}};
// static c3_c* _141_qua_sfix_ha[] = {0};

static u3j_harm _141_qua_mink_a[] = {{".2", u3we_mink}, {}};
static c3_c* _141_qua_mink_ha[] = {0};
static u3j_harm _141_qua_mule_a[] = {{".2", u3we_mule}, {}};
static c3_c* _141_qua_mule_ha[] = {0};

static u3j_core _141_qua_d[] =
{ { "pen", 3, 0, _141_pen_d, _141_pen_ha, _141_pen_ho },

  { "po", 7, 0, _141_qua__po_d, _141_qua__po_ha },

  { "trip", 7, _141_qua_trip_a, 0, _141_qua_trip_ha },

  { "bend", 7, 0, _141_qua__bend_d, _141_qua__bend_ha },
  { "cold", 7, 0, _141_qua__cold_d, _141_qua__cold_ha },
  { "comp", 7, 0, _141_qua__comp_d, _141_qua__comp_ha },
  { "cook", 7, 0, _141_qua__cook_d, _141_qua__cook_ha },
  { "easy", 7, 0, _141_qua__easy_d, _141_qua__easy_ha },
  { "glue", 7, 0, _141_qua__glue_d, _141_qua__glue_ha },
  { "here", 7, 0, _141_qua__here_d, _141_qua__here_ha },
  { "just", 7, 0, _141_qua__just_d, _141_qua__just_ha },
  { "mask", 7, 0, _141_qua__mask_d, _141_qua__mask_ha },
  { "shim", 7, 0, _141_qua__shim_d, _141_qua__shim_ha },
  { "stag", 7, 0, _141_qua__stag_d, _141_qua__stag_ha },
  { "stew", 7, 0, _141_qua__stew_d, _141_qua__stew_ha },
  { "stir", 7, 0, _141_qua__stir_d, _141_qua__stir_ha },

  // { "pfix", 7, _141_qua_pfix_a, 0, _141_qua_pfix_ha },
  { "plug", 7, _141_qua_plug_a, 0, _141_qua_plug_ha },
  { "pose", 7, _141_qua_pose_a, 0, _141_qua_pose_ha },
  // { "sfix", 7, _141_qua_sfix_a, 0, _141_qua_sfix_ha },

  { "mink", 7, _141_qua_mink_a, 0, _141_qua_mink_ha },
  { "mule", 7, _141_qua_mule_a, 0, _141_qua_mule_ha },
  {}
};
static c3_c* _141_qua_ha[] = {0};

static u3j_hood _141_qua_ho[] = {
  { "mute", 0x17dfc },
  { "show", 0x2fbbaba },
  {},
};

/* layer three
*/
  static u3j_harm _141_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
  static c3_c* _141_tri__cofl__drg_ha[] = {0};
  static u3j_harm _141_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static c3_c* _141_tri__cofl__lug_ha[] = {0};
static u3j_core _141_tri__cofl_d[] =
  { { "drg", 7, _141_tri__cofl__drg_a, 0, _141_tri__cofl__drg_ha },
    { "lug", 7, _141_tri__cofl__lug_a, 0, _141_tri__cofl__lug_ha },
    {}
  };
static c3_c* _141_tri__cofl_ha[] = {0};

  static u3j_harm _141_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
  static c3_c* _141_tri__rd_add_ha[] = {0};
  static u3j_harm _141_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
  static c3_c* _141_tri__rd_sub_ha[] = {0};
  static u3j_harm _141_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
  static c3_c* _141_tri__rd_mul_ha[] = {0};
  static u3j_harm _141_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
  static c3_c* _141_tri__rd_div_ha[] = {0};
  static u3j_harm _141_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
  static c3_c* _141_tri__rd_sqt_ha[] = {0};
  static u3j_harm _141_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
  static c3_c* _141_tri__rd_fma_ha[] = {0};
  static u3j_harm _141_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
  static c3_c* _141_tri__rd_lth_ha[] = {0};
  static u3j_harm _141_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
  static c3_c* _141_tri__rd_lte_ha[] = {0};
  static u3j_harm _141_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
  static c3_c* _141_tri__rd_equ_ha[] = {0};
  static u3j_harm _141_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
  static c3_c* _141_tri__rd_gte_ha[] = {0};
  static u3j_harm _141_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static c3_c* _141_tri__rd_gth_ha[] = {0};
static u3j_core _141_tri__rd_d[] =
  { { "add", 7, _141_tri__rd_add_a, 0, _141_tri__rd_add_ha },
    { "sub", 7, _141_tri__rd_sub_a, 0, _141_tri__rd_sub_ha },
    { "mul", 7, _141_tri__rd_mul_a, 0, _141_tri__rd_mul_ha },
    { "div", 7, _141_tri__rd_div_a, 0, _141_tri__rd_div_ha },
    { "sqt", 7, _141_tri__rd_sqt_a, 0, _141_tri__rd_sqt_ha },
    { "fma", 7, _141_tri__rd_fma_a, 0, _141_tri__rd_fma_ha },
    { "lth", 7, _141_tri__rd_lth_a, 0, _141_tri__rd_lth_ha },
    { "lte", 7, _141_tri__rd_lte_a, 0, _141_tri__rd_lte_ha },
    { "equ", 7, _141_tri__rd_equ_a, 0, _141_tri__rd_equ_ha },
    { "gte", 7, _141_tri__rd_gte_a, 0, _141_tri__rd_gte_ha },
    { "gth", 7, _141_tri__rd_gth_a, 0, _141_tri__rd_gth_ha },
    {}
  };
static c3_c* _141_tri__rd_ha[] = {0};

  static u3j_harm _141_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
  static c3_c* _141_tri__rs_add_ha[] = {0};
  static u3j_harm _141_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
  static c3_c* _141_tri__rs_sub_ha[] = {0};
  static u3j_harm _141_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
  static c3_c* _141_tri__rs_mul_ha[] = {0};
  static u3j_harm _141_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
  static c3_c* _141_tri__rs_div_ha[] = {0};
  static u3j_harm _141_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
  static c3_c* _141_tri__rs_sqt_ha[] = {0};
  static u3j_harm _141_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
  static c3_c* _141_tri__rs_fma_ha[] = {0};
  static u3j_harm _141_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
  static c3_c* _141_tri__rs_lth_ha[] = {0};
  static u3j_harm _141_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
  static c3_c* _141_tri__rs_lte_ha[] = {0};
  static u3j_harm _141_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
  static c3_c* _141_tri__rs_equ_ha[] = {0};
  static u3j_harm _141_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
  static c3_c* _141_tri__rs_gte_ha[] = {0};
  static u3j_harm _141_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static c3_c* _141_tri__rs_gth_ha[] = {0};
static u3j_core _141_tri__rs_d[] =
  { { "add", 7, _141_tri__rs_add_a, 0, _141_tri__rs_add_ha },
    { "sub", 7, _141_tri__rs_sub_a, 0, _141_tri__rs_sub_ha },
    { "mul", 7, _141_tri__rs_mul_a, 0, _141_tri__rs_mul_ha },
    { "div", 7, _141_tri__rs_div_a, 0, _141_tri__rs_div_ha },
    { "sqt", 7, _141_tri__rs_sqt_a, 0, _141_tri__rs_sqt_ha },
    { "fma", 7, _141_tri__rs_fma_a, 0, _141_tri__rs_fma_ha },
    { "lth", 7, _141_tri__rs_lth_a, 0, _141_tri__rs_lth_ha },
    { "lte", 7, _141_tri__rs_lte_a, 0, _141_tri__rs_lte_ha },
    { "equ", 7, _141_tri__rs_equ_a, 0, _141_tri__rs_equ_ha },
    { "gte", 7, _141_tri__rs_gte_a, 0, _141_tri__rs_gte_ha },
    { "gth", 7, _141_tri__rs_gth_a, 0, _141_tri__rs_gth_ha },
    {}
  };
  static c3_c* _141_tri__rs_ha[] = {0};

  static u3j_harm _141_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
  static c3_c* _141_tri__rq_add_ha[] = {0};
  static u3j_harm _141_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
  static c3_c* _141_tri__rq_sub_ha[] = {0};
  static u3j_harm _141_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
  static c3_c* _141_tri__rq_mul_ha[] = {0};
  static u3j_harm _141_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
  static c3_c* _141_tri__rq_div_ha[] = {0};
  static u3j_harm _141_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
  static c3_c* _141_tri__rq_sqt_ha[] = {0};
  static u3j_harm _141_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
  static c3_c* _141_tri__rq_fma_ha[] = {0};
  static u3j_harm _141_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
  static c3_c* _141_tri__rq_lth_ha[] = {0};
  static u3j_harm _141_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
  static c3_c* _141_tri__rq_lte_ha[] = {0};
  static u3j_harm _141_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
  static c3_c* _141_tri__rq_equ_ha[] = {0};
  static u3j_harm _141_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
  static c3_c* _141_tri__rq_gte_ha[] = {0};
  static u3j_harm _141_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static c3_c* _141_tri__rq_gth_ha[] = {0};
static u3j_core _141_tri__rq_d[] =
  { { "add", 7, _141_tri__rq_add_a, 0, _141_tri__rq_add_ha },
    { "sub", 7, _141_tri__rq_sub_a, 0, _141_tri__rq_sub_ha },
    { "mul", 7, _141_tri__rq_mul_a, 0, _141_tri__rq_mul_ha },
    { "div", 7, _141_tri__rq_div_a, 0, _141_tri__rq_div_ha },
    { "sqt", 7, _141_tri__rq_sqt_a, 0, _141_tri__rq_sqt_ha },
    { "fma", 7, _141_tri__rq_fma_a, 0, _141_tri__rq_fma_ha },
    { "lth", 7, _141_tri__rq_lth_a, 0, _141_tri__rq_lth_ha },
    { "lte", 7, _141_tri__rq_lte_a, 0, _141_tri__rq_lte_ha },
    { "equ", 7, _141_tri__rq_equ_a, 0, _141_tri__rq_equ_ha },
    { "gte", 7, _141_tri__rq_gte_a, 0, _141_tri__rq_gte_ha },
    { "gth", 7, _141_tri__rq_gth_a, 0, _141_tri__rq_gth_ha },
    {}
  };
static c3_c* _141_tri__rq_ha[] = {0};

  static u3j_harm _141_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
  static c3_c* _141_tri__rh_add_ha[] = {0};
  static u3j_harm _141_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
  static c3_c* _141_tri__rh_sub_ha[] = {0};
  static u3j_harm _141_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
  static c3_c* _141_tri__rh_mul_ha[] = {0};
  static u3j_harm _141_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
  static c3_c* _141_tri__rh_div_ha[] = {0};
  static u3j_harm _141_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
  static c3_c* _141_tri__rh_sqt_ha[] = {0};
  static u3j_harm _141_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
  static c3_c* _141_tri__rh_fma_ha[] = {0};
  static u3j_harm _141_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
  static c3_c* _141_tri__rh_lth_ha[] = {0};
  static u3j_harm _141_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
  static c3_c* _141_tri__rh_lte_ha[] = {0};
  static u3j_harm _141_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
  static c3_c* _141_tri__rh_equ_ha[] = {0};
  static u3j_harm _141_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
  static c3_c* _141_tri__rh_gte_ha[] = {0};
  static u3j_harm _141_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static c3_c* _141_tri__rh_gth_ha[] = {0};
static u3j_core _141_tri__rh_d[] =
  { { "add", 7, _141_tri__rh_add_a, 0, _141_tri__rh_add_ha },
    { "sub", 7, _141_tri__rh_sub_a, 0, _141_tri__rh_sub_ha },
    { "mul", 7, _141_tri__rh_mul_a, 0, _141_tri__rh_mul_ha },
    { "div", 7, _141_tri__rh_div_a, 0, _141_tri__rh_div_ha },
    { "sqt", 7, _141_tri__rh_sqt_a, 0, _141_tri__rh_sqt_ha },
    { "fma", 7, _141_tri__rh_fma_a, 0, _141_tri__rh_fma_ha },
    { "lth", 7, _141_tri__rh_lth_a, 0, _141_tri__rh_lth_ha },
    { "lte", 7, _141_tri__rh_lte_a, 0, _141_tri__rh_lte_ha },
    { "equ", 7, _141_tri__rh_equ_a, 0, _141_tri__rh_equ_ha },
    { "gte", 7, _141_tri__rh_gte_a, 0, _141_tri__rh_gte_ha },
    { "gth", 7, _141_tri__rh_gth_a, 0, _141_tri__rh_gth_ha },
    {}
  };
static c3_c* _141_tri__rh_ha[] = {0};

  static u3j_harm _141_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static c3_c* _141_tri__og_raw_ha[] = {0};
static u3j_core _141_tri__og_d[] =
  { { "raw", 7, _141_tri__og_raw_a, 0, _141_tri__og_raw_ha },
    {}
  };
static c3_c* _141_tri__og_ha[] = {0};

  static u3j_harm _141_tri__sha_sha1_a[] = {{".2", u3we_sha1}, {}};
  static c3_c* _141_tri__sha_sha1_ha[] = {0};
static u3j_core _141_tri__sha_d[] =
  { { "sha1", 7, _141_tri__sha_sha1_a, 0, _141_tri__sha_sha1_ha },
    {}
  };
static c3_c* _141_tri__sha_ha[] = {0};

static u3j_harm _141_tri_shax_a[] = {{".2", u3we_shax}, {}};
static c3_c* _141_tri_shax_ha[] = {0};
static u3j_harm _141_tri_shay_a[] = {{".2", u3we_shay}, {}};
static c3_c* _141_tri_shay_ha[] = {0};
static u3j_harm _141_tri_shas_a[] = {{".2", u3we_shas}, {}};
static c3_c* _141_tri_shas_ha[] = {0};
static u3j_harm _141_tri_shal_a[] = {{".2", u3we_shal}, {}};
static c3_c* _141_tri_shal_ha[] = {0};

static u3j_core _141_tri_d[] =
{ { "qua",  3, 0, _141_qua_d, _141_qua_ha, _141_qua_ho },
  { "cofl", 7, 0, _141_tri__cofl_d, _141_tri__cofl_ha },
  { "rd",   7, 0, _141_tri__rd_d, _141_tri__rd_ha },
  { "rs",   7, 0, _141_tri__rs_d, _141_tri__rs_ha },
  { "rq",   7, 0, _141_tri__rq_d, _141_tri__rq_ha },
  { "rh",   7, 0, _141_tri__rh_d, _141_tri__rh_ha },
  { "og",   7, 0, _141_tri__og_d, _141_tri__og_ha },

  { "sha",  7, 0, _141_tri__sha_d, _141_tri__sha_ha },
  { "shax", 7, _141_tri_shax_a, 0, _141_tri_shax_ha },
  { "shay", 7, _141_tri_shay_a, 0, _141_tri_shay_ha },
  { "shas", 7, _141_tri_shas_a, 0, _141_tri_shas_ha },
  { "shal", 7, _141_tri_shal_a, 0, _141_tri_shal_ha },
  {}
};
static c3_c* _141_tri_ha[] = {0};

/* layer two
*/
static u3j_harm _141_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
static c3_c* _141_two_flop_ha[] = {0};
static u3j_harm _141_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
static c3_c* _141_two_lent_ha[] = {0};
static u3j_harm _141_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
static c3_c* _141_two_levy_ha[] = {0};
static u3j_harm _141_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
static c3_c* _141_two_lien_ha[] = {0};
static u3j_harm _141_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
static c3_c* _141_two_murn_ha[] = {0};
static u3j_harm _141_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
static c3_c* _141_two_need_ha[] = {0};
static u3j_harm _141_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
static c3_c* _141_two_reap_ha[] = {0};
static u3j_harm _141_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
static c3_c* _141_two_reel_ha[] = {0};
static u3j_harm _141_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
static c3_c* _141_two_roll_ha[] = {0};
static u3j_harm _141_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
static c3_c* _141_two_skid_ha[] = {0};
static u3j_harm _141_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
static c3_c* _141_two_skim_ha[] = {0};
static u3j_harm _141_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
static c3_c* _141_two_skip_ha[] = {0};
static u3j_harm _141_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
static c3_c* _141_two_scag_ha[] = {0};
static u3j_harm _141_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
static c3_c* _141_two_slag_ha[] = {0};
static u3j_harm _141_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
static c3_c* _141_two_snag_ha[] = {0};

// https://github.com/urbit/urbit/issues/387
// static u3j_harm _141_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
// static c3_c* _141_two_sort_ha[] = {0};

static u3j_harm _141_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
static c3_c* _141_two_turn_ha[] = {0};
static u3j_harm _141_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
static c3_c* _141_two_weld_ha[] = {0};

static u3j_harm _141_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
static c3_c* _141_two_bex_ha[] = {0};
static u3j_harm _141_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
static c3_c* _141_two_can_ha[] = {0};
static u3j_harm _141_two_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static c3_c* _141_two_cap_ha[] = {0};
static u3j_harm _141_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
static c3_c* _141_two_cat_ha[] = {0};
static u3j_harm _141_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
static c3_c* _141_two_con_ha[] = {0};
static u3j_harm _141_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
static c3_c* _141_two_cut_ha[] = {0};
static u3j_harm _141_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
static c3_c* _141_two_dis_ha[] = {0};
static u3j_harm _141_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
static c3_c* _141_two_dor_ha[] = {0};
static u3j_harm _141_two_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static c3_c* _141_two_dvr_ha[] = {0};
static u3j_harm _141_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
static c3_c* _141_two_end_ha[] = {0};
static u3j_harm _141_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
static c3_c* _141_two_gor_ha[] = {0};
static u3j_harm _141_two_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
static c3_c* _141_two_hor_ha[] = {0};
static u3j_harm _141_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
static c3_c* _141_two_lsh_ha[] = {0};
static u3j_harm _141_two_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static c3_c* _141_two_mas_ha[] = {0};
static u3j_harm _141_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
static c3_c* _141_two_met_ha[] = {0};
static u3j_harm _141_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
static c3_c* _141_two_mix_ha[] = {0};
static u3j_harm _141_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
static c3_c* _141_two_mug_ha[] = {0};
static u3j_harm _141_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
static c3_c* _141_two_muk_ha[] = {0};
static u3j_harm _141_two_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static c3_c* _141_two_peg_ha[] = {0};
static u3j_harm _141_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
static c3_c* _141_two_pow_ha[] = {0};
static u3j_harm _141_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
static c3_c* _141_two_rap_ha[] = {0};
static u3j_harm _141_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
static c3_c* _141_two_rep_ha[] = {0};
static u3j_harm _141_two_rev_a[] = {{".2", u3wc_rev, c3y}, {}};
static c3_c* _141_two_rev_ha[] = {0};
static u3j_harm _141_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
static c3_c* _141_two_rip_ha[] = {0};
static u3j_harm _141_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
static c3_c* _141_two_rsh_ha[] = {0};
static u3j_harm _141_two_swp_a[] = {{".2", u3wc_swp, c3y}, {}};
static c3_c* _141_two_swp_ha[] = {0};
static u3j_harm _141_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
static c3_c* _141_two_sqt_ha[] = {0};
static u3j_harm _141_two_vor_a[] = {{".2", u3wc_vor, c3y}, {}};
static c3_c* _141_two_vor_ha[] = {0};
static u3j_harm _141_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
static c3_c* _141_two_xeb_ha[] = {0};

  static u3j_harm _141_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
  static c3_c* _141_two__in_bif_ha[] = {0};
  static u3j_harm _141_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
  static c3_c* _141_two__in_dif_ha[] = {0};
  static u3j_harm _141_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
  static c3_c* _141_two__in_gas_ha[] = {0};
  static u3j_harm _141_two__in_has_a[] = {{".2", u3wdi_has}, {}};
  static c3_c* _141_two__in_has_ha[] = {0};
  static u3j_harm _141_two__in_mer_a[] = {{".2", u3wdi_mer}, {}};
  static c3_c* _141_two__in_mer_ha[] = {0};

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__in_int_a[] = {{".2", u3wdi_int}, {}};
  // static c3_c* _141_two__in_int_ha[] = {0};

  static u3j_harm _141_two__in_put_a[] = {{".2", u3wdi_put}, {}};
  static c3_c* _141_two__in_put_ha[] = {0};
  static u3j_harm _141_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
  static c3_c* _141_two__in_tap_ha[] = {0};
  static u3j_harm _141_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
  static c3_c* _141_two__in_wyt_ha[] = {0};
  static u3j_harm _141_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static c3_c* _141_two__in_uni_ha[] = {0};
static u3j_core _141_two__in_d[] =
  { { "bif", 7, _141_two__in_bif_a, 0, _141_two__in_bif_ha },
    { "dif", 7, _141_two__in_dif_a, 0, _141_two__in_dif_ha },
    { "gas", 7, _141_two__in_gas_a, 0, _141_two__in_gas_ha },
    { "has", 7, _141_two__in_has_a, 0, _141_two__in_has_ha },
    { "mer", 7, _141_two__in_mer_a, 0, _141_two__in_mer_ha },
    // { "int", 7, _141_two__in_int_a, 0, _141_two__in_int_ha },
    { "put", 7, _141_two__in_put_a, 0, _141_two__in_put_ha },
    { "tap", 7, _141_two__in_tap_a, 0, _141_two__in_tap_ha },
    { "wyt", 7, _141_two__in_wyt_a, 0, _141_two__in_wyt_ha },
    { "uni", 7, _141_two__in_uni_a, 0, _141_two__in_uni_ha },
    {}
  };
static c3_c* _141_two__in_ha[] = {0};

  static u3j_harm _141_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
  static c3_c* _141_two__by_bif_ha[] = {0};
  static u3j_harm _141_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
  static c3_c* _141_two__by_dif_ha[] = {0};
  static u3j_harm _141_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
  static c3_c* _141_two__by_gas_ha[] = {0};
  static u3j_harm _141_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
  static c3_c* _141_two__by_get_ha[] = {0};
  static u3j_harm _141_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
  static c3_c* _141_two__by_has_ha[] = {0};

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
  // static c3_c* _141_two__by_int_ha[] = {0};

  static u3j_harm _141_two__by_jab_a[] = {{".2", u3wdb_jab, c3y}, {}};
  static c3_c* _141_two__by_jab_ha[] = {0};
  static u3j_harm _141_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
  static c3_c* _141_two__by_put_ha[] = {0};
  static u3j_harm _141_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
  static c3_c* _141_two__by_tap_ha[] = {0};

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  // static c3_c* _141_two__by_uni_ha[] = {0};

static u3j_core _141_two__by_d[] =
  { { "bif", 7, _141_two__by_bif_a, 0, _141_two__by_bif_ha },
    { "dif", 7, _141_two__by_dif_a, 0, _141_two__by_dif_ha },
    { "gas", 7, _141_two__by_gas_a, 0, _141_two__by_gas_ha },
    { "get", 7, _141_two__by_get_a, 0, _141_two__by_get_ha },
    { "has", 7, _141_two__by_has_a, 0, _141_two__by_has_ha },
    // { "int", 7, _141_two__by_int_a, 0, _141_two__by_int_ha },
    { "jab", 7, _141_two__by_jab_a, 0, _141_two__by_jab_ha },
    { "put", 7, _141_two__by_put_a, 0, _141_two__by_put_ha },
    { "tap", 7, _141_two__by_tap_a, 0, _141_two__by_tap_ha },
    // { "uni", 7, _141_two__by_uni_a, 0, _141_two__by_uni_ha },
    {}
  };
static c3_c* _141_two__by_ha[] = {0};

static u3j_harm _141_two_cue_a[] = {{".2", u3we_cue}, {}};
static c3_c* _141_two_cue_ha[] = {0};
static u3j_harm _141_two_jam_a[] = {{".2", u3we_jam}, {}};
static c3_c* _141_two_jam_ha[] = {0};
static u3j_harm _141_two_mat_a[] = {{".2", u3we_mat}, {}};
static c3_c* _141_two_mat_ha[] = {0};
static u3j_harm _141_two_rub_a[] = {{".2", u3we_rub}, {}};
static c3_c* _141_two_rub_ha[] = {0};

static u3j_core _141_two_d[] =
{ { "tri",  3, 0, _141_tri_d, _141_tri_ha },

  { "flop", 7, _141_two_flop_a, 0, _141_two_flop_ha },
  { "lent", 7, _141_two_lent_a, 0, _141_two_lent_ha },
  { "levy", 7, _141_two_levy_a, 0, _141_two_levy_ha },
  { "lien", 7, _141_two_lien_a, 0, _141_two_lien_ha },
  { "murn", 7, _141_two_murn_a, 0, _141_two_murn_ha },
  { "need", 7, _141_two_need_a, 0, _141_two_need_ha },
  { "reap", 7, _141_two_reap_a, 0, _141_two_reap_ha },
  { "reel", 7, _141_two_reel_a, 0, _141_two_reel_ha },
  { "roll", 7, _141_two_roll_a, 0, _141_two_roll_ha },
  { "skid", 7, _141_two_skid_a, 0, _141_two_skid_ha },
  { "skim", 7, _141_two_skim_a, 0, _141_two_skim_ha },
  { "skip", 7, _141_two_skip_a, 0, _141_two_skip_ha },
  { "scag", 7, _141_two_scag_a, 0, _141_two_scag_ha },
  { "slag", 7, _141_two_slag_a, 0, _141_two_slag_ha },
  { "snag", 7, _141_two_snag_a, 0, _141_two_snag_ha },
  // { "sort", 7, _141_two_sort_a, 0, _141_two_sort_ha },
  { "turn", 7, _141_two_turn_a, 0, _141_two_turn_ha },
  { "weld", 7, _141_two_weld_a, 0, _141_two_weld_ha },

  { "bex", 7, _141_two_bex_a, 0, _141_two_bex_ha },
  { "cat", 7, _141_two_cat_a, 0, _141_two_cat_ha },
  { "can", 7, _141_two_can_a, 0, _141_two_can_ha },
  { "cap", 7, _141_two_cap_a, 0, _141_two_cap_ha },
  { "con", 7, _141_two_con_a, 0, _141_two_con_ha },
  { "cue", 7, _141_two_cue_a, 0, _141_two_cue_ha },
  { "cut", 7, _141_two_cut_a, 0, _141_two_cut_ha },
  { "dis", 7, _141_two_dis_a, 0, _141_two_dis_ha },
  { "dor", 7, _141_two_dor_a, 0, _141_two_dor_ha },
  { "dvr", 7, _141_two_dvr_a, 0, _141_two_dvr_ha },
  { "end", 7, _141_two_end_a, 0, _141_two_end_ha },
  { "gor", 7, _141_two_gor_a, 0, _141_two_gor_ha },
  { "hor", 7, _141_two_hor_a, 0, _141_two_hor_ha },
  { "jam", 7, _141_two_jam_a, 0, _141_two_jam_ha },
  { "lsh", 7, _141_two_lsh_a, 0, _141_two_lsh_ha },
  { "mas", 7, _141_two_mas_a, 0, _141_two_mas_ha },
  { "mat", 7, _141_two_mat_a, 0, _141_two_mat_ha },
  { "met", 7, _141_two_met_a, 0, _141_two_met_ha },
  { "mix", 7, _141_two_mix_a, 0, _141_two_mix_ha },
  { "mug", 7, _141_two_mug_a, 0, _141_two_mug_ha },
  { "muk", 59, _141_two_muk_a, 0, _141_two_muk_ha },
  { "rap", 7, _141_two_rap_a, 0, _141_two_rap_ha },
  { "rep", 7, _141_two_rep_a, 0, _141_two_rep_ha },
  { "rev", 7, _141_two_rev_a, 0, _141_two_rev_ha },
  { "rip", 7, _141_two_rip_a, 0, _141_two_rip_ha },
  { "rsh", 7, _141_two_rsh_a, 0, _141_two_rsh_ha },
  { "swp", 7, _141_two_swp_a, 0, _141_two_swp_ha },
  { "rub", 7, _141_two_rub_a, 0, _141_two_rub_ha },
  { "peg", 7, _141_two_peg_a, 0, _141_two_peg_ha },
  { "pow", 7, _141_two_pow_a, 0, _141_two_pow_ha },
  { "sqt", 7, _141_two_sqt_a, 0, _141_two_sqt_ha },
  { "vor", 7, _141_two_vor_a, 0, _141_two_vor_ha },
  { "xeb", 7, _141_two_xeb_a, 0, _141_two_xeb_ha },

  { "by",  7, 0, _141_two__by_d, _141_two__by_ha },
  { "in",  7, 0, _141_two__in_d, _141_two__in_ha },
  {}
};
static c3_c* _141_two_ha[] = {0};

/* layer one
*/
static u3j_harm _141_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
static c3_c* _141_one_add_ha[] = {0};
static u3j_harm _141_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
static c3_c* _141_one_dec_ha[] = {0};
static u3j_harm _141_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
static c3_c* _141_one_div_ha[] = {0};
static u3j_harm _141_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static c3_c* _141_one_dvr_ha[] = {0};
static u3j_harm _141_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
static c3_c* _141_one_gte_ha[] = {0};
static u3j_harm _141_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
static c3_c* _141_one_gth_ha[] = {0};
static u3j_harm _141_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
static c3_c* _141_one_lte_ha[] = {0};
static u3j_harm _141_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
static c3_c* _141_one_lth_ha[] = {0};
static u3j_harm _141_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
static c3_c* _141_one_mod_ha[] = {0};
static u3j_harm _141_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
static c3_c* _141_one_mul_ha[] = {0};
static u3j_harm _141_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
static c3_c* _141_one_sub_ha[] = {0};

static u3j_harm _141_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static c3_c* _141_one_cap_ha[] = {0};
static u3j_harm _141_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static c3_c* _141_one_peg_ha[] = {0};
static u3j_harm _141_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static c3_c* _141_one_mas_ha[] = {0};

static u3j_core _141_one_d[] =
{ { "two", 3, 0, _141_two_d, _141_two_ha },

  { "add", 7, _141_one_add_a, 0, _141_one_add_ha },
  { "dec", 7, _141_one_dec_a, 0, _141_one_dec_ha },
  { "div", 7, _141_one_div_a, 0, _141_one_div_ha },
  { "dvr", 7, _141_one_dvr_a, 0, _141_one_dvr_ha },
  { "gte", 7, _141_one_gte_a, 0, _141_one_gte_ha },
  { "gth", 7, _141_one_gth_a, 0, _141_one_gth_ha },
  { "lte", 7, _141_one_lte_a, 0, _141_one_lte_ha },
  { "lth", 7, _141_one_lth_a, 0, _141_one_lth_ha },
  { "mod", 7, _141_one_mod_a, 0, _141_one_mod_ha },
  { "mul", 7, _141_one_mul_a, 0, _141_one_mul_ha },
  { "sub", 7, _141_one_sub_a, 0, _141_one_sub_ha },

  { "cap", 7, _141_one_cap_a, 0, _141_one_cap_ha },
  { "mas", 7, _141_one_mas_a, 0, _141_one_mas_ha },
  { "peg", 7, _141_one_peg_a, 0, _141_one_peg_ha },
  {}
};
static c3_c* _141_one_ha[] = {0};

u3j_core _k141_d[] =
{ { "one", 3, 0, _141_one_d, _141_one_ha },
  {}
};
static c3_c* _k141_ha[] = {0};

static u3j_core _d[] = {
  { "k141", 0, 0, _k141_d, _k141_ha, 0, (u3j_core*) 141, 0 },
  {}
};

u3j_dash
u3j_Dash = {
  _d,
  0,
  0
};
