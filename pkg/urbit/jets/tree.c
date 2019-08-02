/*
  To generate the hashes, take the sha256 of the jammed battery. For example:

  ```
  > `@ux`(shax (jam -:ripn))
  0x2759.a693.1e9e.f9a5.2c8e.ee43.1088.43d9.4d39.32a6.b04f.86cb.6ba1.5553.4329.3a28
  ```

  Becomes:

  ```
  2759a6931e9ef9a52c8eee43108843d94d3932a6b04f86cb6ba1555343293a28
  ```
*/

#include "all.h"

static u3j_harm _141_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
static c3_c* _141_hex_aes_ecba_en_ha[] = {
  "a4eaaead7ffeb213cf8d611f20d7be4786b34f31a27f41c77538125992107c2d",
  0
};
static u3j_harm _141_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
static c3_c* _141_hex_aes_ecba_de_ha[] = {
  "f34036da1666cd2a19be04684f44486fe5e90cbab96d5288e19c4e2bad6e07dd",
  0
};
static u3j_harm _141_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
static c3_c* _141_hex_aes_ecbb_en_ha[] = {
  "44678df63ff8c63be64266d3c06c4a27efbe99f21f078ed3698d3d45fae1807f",
  0
};
static u3j_harm _141_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
static c3_c* _141_hex_aes_ecbb_de_ha[] = {
  "81d2f078236aecaecfebd1d0f69fad64dcada7a7478f50c97ecf7d43a5b48f0c",
  0
};
static u3j_harm _141_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
static c3_c* _141_hex_aes_ecbc_en_ha[] = {
  "ecc3da3bdd3381476eb826cdfb1b839b19e550bb3b4798c8cf7c308efa897c79",
  0
};
static u3j_harm _141_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};
static c3_c* _141_hex_aes_ecbc_de_ha[] = {
  "62e93207275b261a281ad76250408da7cfc6eb6e87bc4b3fd5d99bbf8acc58bd",
  0
};

static u3j_harm _141_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
static c3_c* _141_hex_aes_cbca_en_ha[] = {
  "6b9c992891931f63d0db62d7b28436f88fa6a5cad9920740e94b531cdbf6a7ea",
  0
};
static u3j_harm _141_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
static c3_c* _141_hex_aes_cbca_de_ha[] = {
  "7ddd8a076c15ff737f9e3a0b91bc531c4f7f4a40733fb23e3a3dde98be86bb63",
  0
};
static u3j_harm _141_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
static c3_c* _141_hex_aes_cbcb_en_ha[] = {
  "449ea4600038a80c659705342f6f855a683ad933543679c8f37239e4e438b0d1",
  0
};
static u3j_harm _141_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
static c3_c* _141_hex_aes_cbcb_de_ha[] = {
  "7c26d5e55854c26ddfd3c42c4ae90b496cb81b67bb86eacfb6a5f3328bd6404b",
  0
};
static u3j_harm _141_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
static c3_c* _141_hex_aes_cbcc_en_ha[] = {
  "5c1a99a2a95cef482951a833dfe1d567f0c3ba41db8250baa2c34e7465fd6ee9",
  0
};
static u3j_harm _141_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};
static c3_c* _141_hex_aes_cbcc_de_ha[] = {
  "b9d521b4d5e1d9387b34bbf5ca38f4d52ba86952ea54490dad7e2670183c572b",
  0
};

static u3j_core _141_hex_aes_ecba_d[] =
  { { "en", 7, _141_hex_aes_ecba_en_a, 0, _141_hex_aes_ecba_en_ha },
    { "de", 7, _141_hex_aes_ecba_de_a, 0, _141_hex_aes_ecba_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecba_ha[] = {
  "95a46cbd493f303080f31b9b376df4c981cee336223bd6cffa7c971d38c2749b",
  0
};

static u3j_core _141_hex_aes_ecbb_d[] =
  { { "en", 7, _141_hex_aes_ecbb_en_a, 0, _141_hex_aes_ecbb_en_ha },
    { "de", 7, _141_hex_aes_ecbb_de_a, 0, _141_hex_aes_ecbb_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecbb_ha[] = {
  "6d9488a29d64e307bbce89400bc13420e0ea52a158715cae4f663536ed0a9a58",
  0
};

static u3j_core _141_hex_aes_ecbc_d[] =
  { { "en", 7, _141_hex_aes_ecbc_en_a, 0, _141_hex_aes_ecbc_en_ha },
    { "de", 7, _141_hex_aes_ecbc_de_a, 0, _141_hex_aes_ecbc_de_ha },
    {}
  };
static c3_c* _141_hex_aes_ecbc_ha[] = {
  "6c998edf14a8ca78ef1c03c31804662422b424187741c7f9ea8fa721de7b5bcb",
  0
};

static u3j_core _141_hex_aes_cbca_d[] =
  { { "en", 7, _141_hex_aes_cbca_en_a, 0, _141_hex_aes_cbca_en_ha },
    { "de", 7, _141_hex_aes_cbca_de_a, 0, _141_hex_aes_cbca_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbca_ha[] = {
  "59b5e7a31d01156e1c1c9332ce2ef57211b4f2ce27854bc2fe901cffc30fd93d",
  0
};

static u3j_core _141_hex_aes_cbcb_d[] =
  { { "en", 7, _141_hex_aes_cbcb_en_a, 0, _141_hex_aes_cbcb_en_ha },
    { "de", 7, _141_hex_aes_cbcb_de_a, 0, _141_hex_aes_cbcb_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbcb_ha[] = {
  "b7dd467d0920c5eaf9703af6c5c4a04f419ba010e75035072109d49dbcb1983c",
  0
};

static u3j_core _141_hex_aes_cbcc_d[] =
  { { "en", 7, _141_hex_aes_cbcc_en_a, 0, _141_hex_aes_cbcc_en_ha },
    { "de", 7, _141_hex_aes_cbcc_de_a, 0, _141_hex_aes_cbcc_de_ha },
    {}
  };
static c3_c* _141_hex_aes_cbcc_ha[] = {
  "703d019a7e12ca9c2836707f60cdb8d32b61b20c720e438d84d3a787f20e99f5",
  0
};

static u3j_core _141_hex_aes_d[] =
  { { "ecba", 7, 0, _141_hex_aes_ecba_d, _141_hex_aes_ecba_ha },
    { "ecbb", 7, 0, _141_hex_aes_ecbb_d, _141_hex_aes_ecbb_ha },
    { "ecbc", 7, 0, _141_hex_aes_ecbc_d, _141_hex_aes_ecbc_ha },
    { "cbca", 7, 0, _141_hex_aes_cbca_d, _141_hex_aes_cbca_ha },
    { "cbcb", 7, 0, _141_hex_aes_cbcb_d, _141_hex_aes_cbcb_ha },
    { "cbcc", 7, 0, _141_hex_aes_cbcc_d, _141_hex_aes_cbcc_ha },
    {}
  };
static c3_c* _141_hex_aes_ha[] = {
  "a5340a7ffcb8adac8085317094b9bd6bc4eb0a52badbbfb138e9ff3ce8b49a97",
  0
};

static u3j_harm _141_hex_lore_a[] = {{".2", u3we_lore}, {}};
static c3_c* _141_hex_lore_ha[] = {
  "19b13cfea49fd14aafbb20b8b888ba454f809c3f50a7cfeebd43f87336fe052d",
  0
};
static u3j_harm _141_hex_loss_a[] = {{".2", u3we_loss}, {}};
static c3_c* _141_hex_loss_ha[] = {
  "6c4fe849ec8520e847c09804c056aa0c5c890553e53f07c00b6e1f158e6deb8f",
  0
};
static u3j_harm _141_hex_lune_a[] = {{".2", u3we_lune}, {}};
static c3_c* _141_hex_lune_ha[] = {
  "417472f35b885fe6dd0715e78fd0920cb59f68b738aadc9768e73bc5efa0e570",
  0
};

static u3j_harm _141_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
static c3_c* _141_hex_coed__ed_puck_ha[] = {
  "540b16bba2321015feeb401cd65150d2050188de57041fd9d3d1ac8902cc1e63",
  0
};
static u3j_harm _141_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
static c3_c* _141_hex_coed__ed_sign_ha[] = {
  "de2e5ebf5bdb96e24e05a231b4eac0e0f803984c69948c16cd0e2397aa5dabc1",
  0
};
static u3j_harm _141_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
static c3_c* _141_hex_coed__ed_veri_ha[] = {
  "a0fa913b3a823e67ae3d6f416d623c9ff692a324deffd80d057020bbac91d223",
  0
};
static u3j_harm _141_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};
static c3_c* _141_hex_coed__ed_shar_ha[] = {
  "2115b6722bf59ebac897791293eeb7fe0a83e73b1e57d4a098d52af0948cb7b4",
  0
};

static u3j_harm _141_hex_coed__ed_point_add_a[] =
    {{".2", u3wee_point_add}, {}};

static u3j_harm _141_hex_coed__ed_scalarmult_a[] =
    {{".2", u3wee_scalarmult}, {}};

static u3j_harm _141_hex_coed__ed_scalarmult_base_a[] =
    {{".2", u3wee_scalarmult_base}, {}};

static u3j_harm _141_hex_coed__ed_add_scalarmult_scalarmult_base_a[] =
    {{".2", u3wee_add_scalarmult_scalarmult_base}, {}};

static u3j_harm _141_hex_coed__ed_add_double_scalarmult_a[] =
    {{".2", u3wee_add_double_scalarmult}, {}};

static u3j_core _141_hex_coed__ed_d[] =
  { { "sign", 7, _141_hex_coed__ed_sign_a, 0, _141_hex_coed__ed_sign_ha },
    { "puck", 7, _141_hex_coed__ed_puck_a, 0, _141_hex_coed__ed_puck_ha },
    { "veri", 7, _141_hex_coed__ed_veri_a, 0, _141_hex_coed__ed_veri_ha },
    { "shar", 7, _141_hex_coed__ed_shar_a, 0, _141_hex_coed__ed_shar_ha },
    { "point-add", 7, _141_hex_coed__ed_point_add_a, 0, 0 },
    { "scalarmult", 7, _141_hex_coed__ed_scalarmult_a, 0, 0 },
    { "scalarmult-base", 7, _141_hex_coed__ed_scalarmult_base_a, 0, 0 },
    { "add-scalarmult-scalarmult-base", 7,
      _141_hex_coed__ed_add_scalarmult_scalarmult_base_a, 0, 0 },
    { "add-double-scalarmult", 7,
      _141_hex_coed__ed_add_double_scalarmult_a, 0, 0 },
    {}
  };
static c3_c* _141_hex_coed__ed_ha[] = {
  "33223ee36ddc84831eff43939b035afe00bb23c7ba1475cbeadb24954216b814",
  0
};

static u3j_core _141_hex_coed_d[] =
{ { "ed", 3, 0, _141_hex_coed__ed_d, _141_hex_coed__ed_ha },
  {}
};
static c3_c* _141_hex_coed_ha[] = {
  "1f68bb8e3214132195e1183e61b05bccff19808df3cbdaeb6c7fcce9cd27a24d",
  0
};

  static u3j_harm _141_hex_hmac_hmac_a[] = {{".2", u3we_hmac}, {}};
  static c3_c* _141_hex_hmac_hmac_ha[] = {
    "41a3eb915ac8105751d5bc7ac309a21410896a82e129d3314cc5be300f2660db",
    0
  };
static u3j_core _141_hex_hmac_d[] =
  { { "hmac", 7, _141_hex_hmac_hmac_a, 0, _141_hex_hmac_hmac_ha },
    {}
  };
static c3_c* _141_hex_hmac_ha[] = {
  "c6cacf4657372591769ccb9b686be4c16d7dbe0d815f4b8d9e81ddc97c36b770",
  0
};

  static u3j_harm _141_hex_argon2_a[] = {{".2", u3we_argon2}, {}};
  static c3_c* _141_hex_argon2_ha[] = {
    "ef21e4f9108b5f2e6831145df4c21e0d44152abcd0f575532894d406425c04c9",
    0
  };
static u3j_core _141_hex_argon_d[] =
  { { "argon2", 511, _141_hex_argon2_a, 0, _141_hex_argon2_ha },
    {}
  };
static c3_c* _141_hex_argon_ha[] = {
  "7d8acf91db0262d485641547db6cc9ab4ef260c393fc7f12336fab393263056a",
  0
};

  static u3j_harm _141_hex_secp_make_a[] = {{".2", u3we_make, c3y}, {}};
  static c3_c* _141_hex_secp_make_ha[] = {
    "171cae298e8f73b6b77c72f957d9d7afd495ed1ca7d78fe9d5f869ea2203bada",
    0
  };
  static u3j_harm _141_hex_secp_sign_a[] = {{".2", u3we_sign, c3y}, {}};
  static c3_c* _141_hex_secp_sign_ha[] = {
    "aac58cd537481d41fc4d941a7a0ed247552d64af6c9dce71e0d74c39384e2d60",
    0
  };
  static u3j_harm _141_hex_secp_reco_a[] = {{".2", u3we_reco, c3y}, {}};
  static c3_c* _141_hex_secp_reco_ha[] = {
    "390d4cd3a04817b6436035a6fa77fe3008008afa164db732c8f4d5c52954fbee",
    0
  };
static u3j_core _141_hex_secp_secp_helper_d[] =
  { { "make-k",            7, _141_hex_secp_make_a, 0, _141_hex_secp_make_ha },
    { "ecdsa-raw-sign",    7, _141_hex_secp_sign_a, 0, _141_hex_secp_sign_ha },
    { "ecdsa-raw-recover", 7, _141_hex_secp_reco_a, 0, _141_hex_secp_reco_ha },
    {}
  };
static c3_c* _141_hex_secp_secp_helper_ha[] = {
  "24175b141f1efc2e2de00c39a2b70cf3491f2b82371e0e15f63dfb6d2d86eac5",
  0
};

static u3j_core _141_hex_secp_secp_d[] =
  { { "helper", 15, 0, _141_hex_secp_secp_helper_d, _141_hex_secp_secp_helper_ha },
    {}
  };
static c3_c* _141_hex_secp_secp_ha[] = {
  "42f57966a293fdadbce8b0cc2108039f1f7fafe0b12f1fec52b2d1937a8347d7",
  0
};

static u3j_core _141_hex_secp_d[] =
  { { "secp", 7, 0, _141_hex_secp_secp_d, _141_hex_secp_secp_ha },
    {}
  };
static c3_c* _141_hex_secp_ha[] = {
  "e153a8c88f04bfed03dc882f560f912eaf3f5e3911f55dbb054519c2e1b4d778",
  0
};

  static u3j_harm _141_hex_blake2b_a[] = {{".2", u3we_blake, c3y}, {}};
  static c3_c* _141_hex_blake2b_ha[] = {
    "affddbd9861660e0381edf82c88da18e18d2dd0aa0f430f9d8661c5a57e13cb5",
    0
  };
static u3j_core _141_hex_blake_d[] =
  { { "blake2b", 7, _141_hex_blake2b_a, 0, _141_hex_blake2b_ha },
    {}
  };
static c3_c* _141_hex_blake_ha[] = {
  "3a63284428b509489233513a0d6b13f705a67c5bed4354a64ef09054529a7c35",
  0
};

  static u3j_harm _141_hex_ripemd_160_a[] = {{".2", u3we_ripe, c3y}, {}};
  static c3_c* _141_hex_ripemd_160_ha[] = {
    "c918e263c56723986b6a5ba4a994199ec2afe12df42b2efa497e1b51f572ce13",
    0
  };
static u3j_core _141_hex_ripe_d[] =
  { { "ripemd160", 7, _141_hex_ripemd_160_a, 0, _141_hex_ripemd_160_ha },
    {}
  };
static c3_c* _141_hex_ripe_ha[] = {
  "fe7e2579d5053dead2f5ce27e0aa6bda1f9a84684db45349af38fe5bc827613a",
  0
};


static u3j_core _141_hex_d[] =
{ { "lore",   63, _141_hex_lore_a, 0, _141_hex_lore_ha },
  { "loss",   63, _141_hex_loss_a, 0, _141_hex_loss_ha },
  { "lune",  127, _141_hex_lune_a, 0, _141_hex_lune_ha },

  { "coed",   63, 0, _141_hex_coed_d, _141_hex_coed_ha },
  { "aes",    31, 0, _141_hex_aes_d, _141_hex_aes_ha },

  { "hmac",   63, 0, _141_hex_hmac_d,  _141_hex_hmac_ha  },
  { "argon",  31, 0, _141_hex_argon_d, _141_hex_argon_ha },
  { "blake",  31, 0, _141_hex_blake_d, _141_hex_blake_ha },
  { "ripemd", 31, 0, _141_hex_ripe_d,  _141_hex_ripe_ha  },
  { "secp",   31, 0, _141_hex_secp_d,  _141_hex_secp_ha  },
  {}
};
static c3_c* _141_hex_ha[] = {
  "b3352eada800d6c9db030ac128262e8286c245162b2ab2b317c43dc39f3e152d",
  0
};

/* layer five
*/
static u3j_harm _141_pen_cell_a[] = {{".2", u3wf_cell}, {}};
static c3_c* _141_pen_cell_ha[] = {
  "411649e69ff5c5d4a2976b300d213b99af3de724cec0e95f48404b808fc4f428",
  0
};
static u3j_harm _141_pen_comb_a[] = {{".2", u3wf_comb}, {}};
static c3_c* _141_pen_comb_ha[] = {
  "137e940853b2f823bac751069f7dd3e81367bda77037afe1c3cb4d0cd26982db",
  0
};
static u3j_harm _141_pen_cons_a[] = {{".2", u3wf_cons}, {}};
static c3_c* _141_pen_cons_ha[] = {
  "b698cc6bc49ea0473e344c784075e99a433b4c5738f90fc58ab17c3eaa44b2e9",
  0
};
static u3j_harm _141_pen_core_a[] = {{".2", u3wf_core}, {}};
static c3_c* _141_pen_core_ha[] = {
  "1180e9371cf3465783ef192f9a7f580cd90533f5b52b77b456287f1d580a3535",
  0
};
static u3j_harm _141_pen_face_a[] = {{".2", u3wf_face}, {}};
static c3_c* _141_pen_face_ha[] = {
  "a184c44d57f5c94b84a3258b05bf891f3c96a4a4bbff3a8934d11cad0efa81d8",
  0
};
static u3j_harm _141_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
static c3_c* _141_pen_fitz_ha[] = {
  "31ebe9b8ece572a90c8e8d6b8b334f445c010b92c0ce83380d0fd6ad21b014af",
  0
};
static u3j_harm _141_pen_flan_a[] = {{".2", u3wf_flan}, {}};
static c3_c* _141_pen_flan_ha[] = {
  "cc00cb9373b0274af4e17d7acd77f65d8a2fa886e422c949c12d9d9e7cb3525b",
  0
};
static u3j_harm _141_pen_flip_a[] = {{".2", u3wf_flip}, {}};
static c3_c* _141_pen_flip_ha[] = {
  "6e97fab9d039e715a30af5da93ef97389babfdcae7ef87655d278e77a1af0f0c",
  0
};
static u3j_harm _141_pen_flor_a[] = {{".2", u3wf_flor}, {}};
static c3_c* _141_pen_flor_ha[] = {
  "ab5360aacf0c9a325727e90e1caea9c42f5d94ccc248c9e1f253b0922b4c4e63",
  0
};
static u3j_harm _141_pen_fork_a[] = {{".2", u3wf_fork}, {}};
static c3_c* _141_pen_fork_ha[] = {
  "000af0f7a46f669c66b4f5d2de1d28544f093b579d93c16e41e717c3c40d1823",
  0
};

// hike disabled while implementing edit
// static u3j_harm _141_pen_hike_a[] = {{".2", u3wf_hike}, {}};
// static c3_c* _141_pen_hike_ha[] = {0};

static u3j_harm _141_pen_look_a[] = {{".2", u3wf_look}, {}};
static c3_c* _141_pen_look_ha[] = {
  "fdda2166a2b9e1a9bda6ab375dd6fb6c610e18f54636a5e89896b45fd0a7169b",
  0
};
static u3j_harm _141_pen_loot_a[] = {{".2", u3wf_loot}, {}};
static c3_c* _141_pen_loot_ha[] = {
  "be73de8944cd05c117fa698523940fd0a6a2a2286c56d8586ae35034d0a32200",
  0
};

  static u3j_harm _141_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
  static c3_c* _141_pen__ut_crop_ha[] = {
    "d83e5e47f712870aba815d79943d287cbefdc00640409464b30bf755115d4a1a",
    0
  };
  static u3j_harm _141_pen__ut_fond_a[] = {{".2", u3wfu_fond}, {}};
  static c3_c* _141_pen__ut_fond_ha[] = {
    "0da0cc79c938eb06515a5cc24a17b82cd60a50c0f1a02e2c68e5d1cf71c96054",
    0
  };
  static u3j_harm _141_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
  static c3_c* _141_pen__ut_fish_ha[] = {
    "2fd315436f48351002d9aa8c137649ca95b01fd57dba09db53d7235f84a284bf",
    0
  };
  static u3j_harm _141_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
  static c3_c* _141_pen__ut_fuse_ha[] = {
    "43d8bfdf9255f548bb58d9975bac273e2dcebe5ae98bd7e466b6fff6ff43a944",
    0
  };
  static u3j_harm _141_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
  static c3_c* _141_pen__ut_mint_ha[] = {
    "43a06316365bcd14a94f8ed1f3fe5a8f61d1da5bea989296a192b62a966fca11",
    0
  };
  static u3j_harm _141_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
  static c3_c* _141_pen__ut_mull_ha[] = {
    "9fe555b3f9ad666f04194037437d71ee98f6b884f7aacc46a11ad27407cb7e8e",
    0
  };
  static u3j_harm _141_pen__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
      static u3j_harm _141_pen__ut_nest_dext_a[] = {{".2", u3wfu_nest_dext}, {}};
    static u3j_core _141_pen__ut_nest_in_d[] =
      {
        // TODO: compute hash
        { "nest-dext", 3, _141_pen__ut_nest_dext_a, 0, 0 },
        {}
      };
  static u3j_core _141_pen__ut_nest_d[] =
    {
      // TODO: Compute hash for nest-in.
      { "nest-in", 7, 0, _141_pen__ut_nest_in_d, 0 },
      {}
    };
  static c3_c* _141_pen__ut_nest_ha[] = {
    "a55b695cdba00e7fa8f2c74a332880c877701555b2909b5b3028aea5839d62cd",
    0
  };
  static u3j_harm _141_pen__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
  static c3_c* _141_pen__ut_peek_ha[] = {
    "904ff7359e89d1886f884c4409f104269cdb8dfb4683f116ff00bc98a4720df7",
    0
  };
  static u3j_harm _141_pen__ut_play_a[] = {{".2", u3wfu_play}, {}};
  static c3_c* _141_pen__ut_play_ha[] = {
    "bdc5c072632f7133b4c64c465b1b214d7465b0c1163842b121b7369aba1b9b03",
    0
  };
  static u3j_harm _141_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
  static c3_c* _141_pen__ut_rest_ha[] = {
    "2e2d15f3efca0a4bf8ce08cca48c54d1d5a7204e2b0525137f59c3e7b037d2fd",
    0
  };

static u3j_core _141_pen__ut_d[] =
  {
    { "crop", 7, _141_pen__ut_crop_a, 0, _141_pen__ut_crop_ha },
    { "fond", 7, _141_pen__ut_fond_a, 0, _141_pen__ut_fond_ha },
    { "fish", 7, _141_pen__ut_fish_a, 0, _141_pen__ut_fish_ha },
    { "fuse", 7, _141_pen__ut_fuse_a, 0, _141_pen__ut_fuse_ha },
    { "mint", 7, _141_pen__ut_mint_a, 0, _141_pen__ut_mint_ha },
    { "mull", 7, _141_pen__ut_mull_a, 0, _141_pen__ut_mull_ha },
    { "nest", 7, _141_pen__ut_nest_a, _141_pen__ut_nest_d, _141_pen__ut_nest_ha },
    { "peek", 7, _141_pen__ut_peek_a, 0, _141_pen__ut_peek_ha },
    { "play", 7, _141_pen__ut_play_a, 0, _141_pen__ut_play_ha },
    { "rest", 7, _141_pen__ut_rest_a, 0, _141_pen__ut_rest_ha },
    {}
  };

static c3_c* _141_pen__ut_ha[] = {
  "479d0051e5fabe291e4cded603a071fce0f10734503638fd7d30e9c6d799969c",
  0
};

static u3j_hood _141_pen__ut_ho[] = {
  { "ar",     12282 },
  { "fan",  28, c3n },
  { "rib",  58, c3n },
  { "vet", 118, c3n },
  { "fab", 119, c3n },

  { "blow",    6015 },
  { "burp",     342 },
  { "busk",    1373 },
  { "buss",     374 },
  { "crop",    1494 },
  { "duck",    1524 },
  { "dune",    5982 },
  { "dunk",    3066 },
  { "epla",   12206 },
  { "emin",    1534 },
  { "emul",    6134 },
  { "feel",    1502 },
  { "felt",      94 },
  { "fine",   49086 },
  { "fire",       4 },
  { "fish",    6006 },
  { "fond",   12283 },
  { "fund",    6014 },
  //  XX +funk is not part of +ut, and this hook appears to be unused
  //  remove from here and the +ut hint
  //
  { "funk", 0xbefafa, c3y, 31 },
  { "fuse",   24021 },
  { "gain",     380 },
  { "lose", 0x2fefe },
  { "mile",     382 },
  { "mine",     372 },
  { "mint",   49083 },
  { "moot", 0x2feff },
  { "mull",   24020 },
  { "nest",      92 },
  { "peel",    1526 },
  { "play",    3006 },
  { "peek",    1532 },
  { "repo",      22 },
  { "rest",    6102 },
  { "tack",    6007 },
  { "toss",   24540 },
  { "wrap",    6140 },
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
{ { "hex", 7, 0, _141_hex_d, _141_hex_ha },

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
  { "ut", 15, 0, _141_pen__ut_d, _141_pen__ut_ha, _141_pen__ut_ho },
  {}
};
static c3_c* _141_pen_ha[] = {
  "5197c4be0f72a57d77ecda9f3976ba06cd22648751f434f40162f6759688b725",
  0
};

static u3j_hood _141_pen_ho[] = {
  { "ap", 86 },
  { "ut", 342 },
  {},
};

/* layer four
*/
static u3j_harm _141_qua_trip_a[] = {{".2", u3we_trip}, {}};
static c3_c* _141_qua_trip_ha[] = {
  "2f4df71315caaab44495ebd6b0c541484cb76d26d4caa306207a33876b09509c",
  0
};

static u3j_harm _141_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
static c3_c* _141_qua__po_ind_ha[] = {
  "95bbe9867dbbd1b9ce12671d64cf7b1dee8d987c6770955a83c73291c4537a61",
  0
};
static u3j_harm _141_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
static c3_c* _141_qua__po_ins_ha[] = {
  "aae783fb258dff7f8ade49756e01f96a2d2100411a88a886732270dcf9f174f0",
  0
};
static u3j_harm _141_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
static c3_c* _141_qua__po_tod_ha[] = {
  "c69fdde3a83159207e1e838e960fe48e809fc9eb296300ee85169aadf126339c",
  0
};
static u3j_harm _141_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
static c3_c* _141_qua__po_tos_ha[] = {
  "eba705fc6e46193f4a4f3e20c37f06141d0b8eae0f8db6e6c7a53659803a3f04",
  0
};
  static u3j_core _141_qua__po_d[] =
    { { "ind", 7, _141_qua__po_ind_a, 0, _141_qua__po_ind_ha },
      { "ins", 7, _141_qua__po_ins_a, 0, _141_qua__po_ins_ha },
      { "tod", 7, _141_qua__po_tod_a, 0, _141_qua__po_tod_ha },
      { "tos", 7, _141_qua__po_tos_a, 0, _141_qua__po_tos_ha },
      {}
    };
  static c3_c* _141_qua__po_ha[] = {
    "6ca8581f72f693ae465e658fd58e8cb7d705927f67254bcc95a449df9c9f7d1b",
    0
  };

static u3j_harm _141_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
static c3_c* _141_qua__bend_fun_ha[] = {
  "6a560ff29ece25d1f02a60a500feeb6288ec4d51b27b759fb8066abdce74ddbb",
  0
};
  static u3j_core _141_qua__bend_d[] =
    { { "fun", 7, _141_qua__bend_fun_a, 0, _141_qua__bend_fun_ha },
      {}
    };
  static c3_c* _141_qua__bend_ha[] = {
    "f9d15e37e625cec2d505532a2d83a7eba9dd9afb339d0a4e83613f5eca2e6c88",
    0
  };

static u3j_harm _141_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
static c3_c* _141_qua__cold_fun_ha[] = {
  "ad5a0ab7405be9ffda0a9dd34580c6039f6bbb0301920fb2df0c31be3c72c58e",
  0
};
  static u3j_core _141_qua__cold_d[] =
    { { "fun", 7, _141_qua__cold_fun_a, 0, _141_qua__cold_fun_ha },
      {}
    };
  static c3_c* _141_qua__cold_ha[] = {
    "06c49d8dab6cb057e3ae1d1164e96950944ea48e54ad72347239ea434f1d4b9c",
    0
  };

static u3j_harm _141_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
static c3_c* _141_qua__cook_fun_ha[] = {
  "f2a45612ad9c279b723334ab0915d3ed3ece7727309968c2555f45e0668eeb27",
  0
};
  static u3j_core _141_qua__cook_d[] =
    { { "fun", 7, _141_qua__cook_fun_a, 0, _141_qua__cook_fun_ha },
      {}
    };
  static c3_c* _141_qua__cook_ha[] = {
    "3ccd46dd21828d7be11f8c093536e305b1df982393a69c40ea73f63a574b3bb1",
    0
  };

static u3j_harm _141_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
static c3_c* _141_qua__comp_fun_ha[] = {
  "60e5ff2cea860d80f8af65e1323518f58a081b93b49ad7351a1ea1dfe87380e4",
  0
};
  static u3j_core _141_qua__comp_d[] =
    { { "fun", 7, _141_qua__comp_fun_a, 0, _141_qua__comp_fun_ha },
      {}
    };
  static c3_c* _141_qua__comp_ha[] = {
    "4960a20da1b43ff953b86ba08cbdeb9a4468af8bfd27fcde12f39f1449fdee9c",
    0
  };

static u3j_harm _141_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
static c3_c* _141_qua__easy_fun_ha[] = {
  "4bbbc43ece463d961e572301d0824d3e3cab3ba09ec2756cbefae63ee106044b",
  0
};
  static u3j_core _141_qua__easy_d[] =
    { { "fun", 7, _141_qua__easy_fun_a, 0, _141_qua__easy_fun_ha },
      {}
    };
  static c3_c* _141_qua__easy_ha[] = {
    "fdcf833943e24323deb6b071497498933ce6c4ba7d4742f2752b6ddb7fb9634e",
    0
  };

static u3j_harm _141_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
static c3_c* _141_qua__glue_fun_ha[] = {
  "7a4b978b56658b5c93fc79cb9394e3b46b9f02428a3668958de05e326c512d6b",
  0
};
  static u3j_core _141_qua__glue_d[] =
    { { "fun", 7, _141_qua__glue_fun_a, 0, _141_qua__glue_fun_ha },
      {}
    };
  static c3_c* _141_qua__glue_ha[] = {
    "9510f468b9c5d64f80a20d56d8cffd7a1ba0b6444aef9299f2d0a8c3e619b387",
    0
  };

static u3j_harm _141_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
static c3_c* _141_qua__here_fun_ha[] = {
  "47ec445fcfa89d266dae3c3590ed041d1b05f92d1bce360f232da5d496e4f2eb",
  0
};
  static u3j_core _141_qua__here_d[] =
    { { "fun", 7, _141_qua__here_fun_a, 0, _141_qua__here_fun_ha },
      {}
    };
  static c3_c* _141_qua__here_ha[] = {
    "4600093be5becba9a65a14b67624e3d9e4c66e0c97ba57b2bc271907eea32ffe",
    0
  };

static u3j_harm _141_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
static c3_c* _141_qua__just_fun_ha[] = {
  "2a77a5aec1b5394cd282f7ba3a6a0492906446e18f9569b15810c979ec5842de",
  0
};
  static u3j_core _141_qua__just_d[] =
    { { "fun", 7, _141_qua__just_fun_a, 0, _141_qua__just_fun_ha },
      {}
    };
  static c3_c* _141_qua__just_ha[] = {
    "3d3f00579c4b1d2707418eff6cdb037e95f5c499d986a3cdb535faa35bbf05a6",
    0
  };

static u3j_harm _141_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
static c3_c* _141_qua__mask_fun_ha[] = {
  "04bd9009b0c52c6140256c9694df4823e735733ed0c825b14a1f7bce1f8fc0f8",
  0
};
  static u3j_core _141_qua__mask_d[] =
    { { "fun", 7, _141_qua__mask_fun_a, 0, _141_qua__mask_fun_ha },
      {}
    };
  static c3_c* _141_qua__mask_ha[] = {
    "9ec051bb1101cfd7411795e30c43a0564b80c7d2ca68940ef54f9f6d332adac0",
    0
  };

static u3j_harm _141_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
static c3_c* _141_qua__shim_fun_ha[] = {
  "64c01dcfb9d3a66fbfcf6182770cf3421f1be08c693d61476d0dfc223fc6b762",
  0
};
  static u3j_core _141_qua__shim_d[] =
    { { "fun", 7, _141_qua__shim_fun_a, 0, _141_qua__shim_fun_ha },
      {}
    };
  static c3_c* _141_qua__shim_ha[] = {
    "838be7322079341d7f0135069d84212527a8dfefd7fab7cafa5e1d5d18406228",
    0
  };

static u3j_harm _141_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
static c3_c* _141_qua__stag_fun_ha[] = {
  "f76c7205c23e77809af793bc506f4727071fd029d234317fe78a7f65e2b7d6ea",
  0
};
  static u3j_core _141_qua__stag_d[] =
    { { "fun", 7, _141_qua__stag_fun_a, 0, _141_qua__stag_fun_ha },
      {}
    };
  static c3_c* _141_qua__stag_ha[] = {
    "fc978af18fb13dc0d77501b6f3eeb538b079b4345eb9195c3dd44c516e69424a",
    0
  };

static u3j_harm _141_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
static c3_c* _141_qua__stew_fun_ha[] = {
  "677c85d6ba46e134025f652d94049b3db54b576d32159707d16a4c0a56578951",
  0
};
  static u3j_core _141_qua__stew_d[] =
    { { "fun", 31, _141_qua__stew_fun_a, 0, _141_qua__stew_fun_ha },
      {}
    };
  static c3_c* _141_qua__stew_ha[] = {
    "11b3c70145eedcd888f4b3f51861e759da7405b0b93398b59cfb82d187b29889",
    0
  };

static u3j_harm _141_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
static c3_c* _141_qua__stir_fun_ha[] = {
  "ad1e756459e084cc7463c6e1ebdcef013fdfbe915c345657aee859622e311985",
  0
};
  static u3j_core _141_qua__stir_d[] =
    { { "fun", 7, _141_qua__stir_fun_a, 0, _141_qua__stir_fun_ha },
      {}
    };
  static c3_c* _141_qua__stir_ha[] = {
    "aa51d9e03f44ae821553dde66bf84e40e1b95867d5ccc51ed6f390771c97a708",
    0
  };

static u3j_harm _141_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
static c3_c* _141_qua_pfix_ha[] = {
  "f7019bccc8b3b04a969878ffed84c9eba4dfa60ee32f984119cacb0c2381656b",
  0
};

static u3j_harm _141_qua_plug_a[] = {{".2", u3we_plug}, {}};
static c3_c* _141_qua_plug_ha[] = {
  "dd5a5a82b572ebb3009f9e1dbb52d0a955273742b8e1f5be27885ca208a0c9c7",
  0
};
static u3j_harm _141_qua_pose_a[] = {{".2", u3we_pose}, {}};
static c3_c* _141_qua_pose_ha[] = {
  "8586df7438d5b37935f9fa1bc3ac6d8740f13fa9885f6b4f1acfe672e4e4cc33",
  0
};

static u3j_harm _141_qua_sfix_a[] = {{".2", u3we_sfix}, {}};
static c3_c* _141_qua_sfix_ha[] = {
  "00987ed37104b902c5264d4d013826d762bfa80a6b29cfe4b7fa61b1ddd9cfac",
  0
};

static u3j_harm _141_qua_mink_a[] = {{".2", u3we_mink}, {}};
static c3_c* _141_qua_mink_ha[] = {
  "fd66c7ed46e5440ea759e6ace2341e6170aec48c79de27ffff3d179d1b5e491e",
  0
};
static u3j_harm _141_qua_mule_a[] = {{".2", u3we_mule}, {}};
static c3_c* _141_qua_mule_ha[] = {
  "d54688d726565ddade7f2636741cad7209ea40fab28d3335555d8a02ff6001c4",
  0
};

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

  { "pfix", 7, _141_qua_pfix_a, 0, _141_qua_pfix_ha },
  { "plug", 7, _141_qua_plug_a, 0, _141_qua_plug_ha },
  { "pose", 7, _141_qua_pose_a, 0, _141_qua_pose_ha },
  { "sfix", 7, _141_qua_sfix_a, 0, _141_qua_sfix_ha },

  { "mink", 7, _141_qua_mink_a, 0, _141_qua_mink_ha },
  { "mule", 7, _141_qua_mule_a, 0, _141_qua_mule_ha },
  {}
};
static c3_c* _141_qua_ha[] = {
  "0efd1620ed40369f957d53d796c0bf497e0585100829e5f37ede41f6a841d0f8",
  0
};

static u3j_hood _141_qua_ho[] = {
  { "mute", 0x2fbabe },
  { "show", 24406 },
  {},
};

/* layer three
*/
  static u3j_harm _141_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
  static c3_c* _141_tri__cofl__drg_ha[] = {
    "6063adb8cac639f7b20d5e7700c8108266be04f99cce4434f906240b424bf36d",
    0
  };
  static u3j_harm _141_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static c3_c* _141_tri__cofl__lug_ha[] = {
    "f146a84731447e5c4b1e7b6e9331b33d1babed09bb0618e134c9535062154a87",
    0
  };
static u3j_core _141_tri__cofl_d[] =
  { { "drg", 7, _141_tri__cofl__drg_a, 0, _141_tri__cofl__drg_ha },
    { "lug", 7, _141_tri__cofl__lug_a, 0, _141_tri__cofl__lug_ha },
    {}
  };
static c3_c* _141_tri__cofl_ha[] = {
  "f320c5bf51db85f55b900f4160f7e0ab9ef267f43ddb698900de034c6e2600d5",
  0
};

  static u3j_harm _141_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
  static c3_c* _141_tri__rd_add_ha[] = {
    "90dfaaadb2878d6d89a808ce4199e5bb239fa981e1c2edf24dc54aa3fcab55a5",
    0
  };
  static u3j_harm _141_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
  static c3_c* _141_tri__rd_sub_ha[] = {
    "5898a2424ba815d66d83917953f01860e63207f4200a447f632d9a5cc77a8a9c",
    0
  };
  static u3j_harm _141_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
  static c3_c* _141_tri__rd_mul_ha[] = {
    "a3af44ef4cd89afe78f1088bddb7d56dfa7fc209153256557c98ff34b67976bc",
    0
  };
  static u3j_harm _141_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
  static c3_c* _141_tri__rd_div_ha[] = {
    "9be3b38b9b4b0b0cd0bb060529c8f439cb0589aa9c3528efc519fbcc6845e98d",
    0
  };
  static u3j_harm _141_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
  static c3_c* _141_tri__rd_sqt_ha[] = {
    "0413678ac8ec89c3425ba762cd24b2a8a455543ec9b5dd719524e6b834e0c99c",
    0
  };
  static u3j_harm _141_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
  static c3_c* _141_tri__rd_fma_ha[] = {
    "7c920238dd42fb645c057cc950ed7ece775d5f502a0faf6ef5d17d348e0fc3e3",
    0
  };
  static u3j_harm _141_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
  static c3_c* _141_tri__rd_lth_ha[] = {
    "a108f1ac15c1d4e2c86457c9afc97a97a5954003c709c3c19c722b37255bcba9",
    0
  };
  static u3j_harm _141_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
  static c3_c* _141_tri__rd_lte_ha[] = {
    "ef9a6b2c5cdd0d4de550a32679d242693131cdc3cf40ac914eadfa7d6d9f1bac",
    0
  };
  static u3j_harm _141_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
  static c3_c* _141_tri__rd_equ_ha[] = {
    "c93cdb951dca3b0ac61070780f95e1baa3718fe519d0268305c032cf14a21e39",
    0
  };
  static u3j_harm _141_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
  static c3_c* _141_tri__rd_gte_ha[] = {
    "6857077d97e2fc203b555dc20748c33d34b694d33c48f628543bea6491e722a6",
    0
  };
  static u3j_harm _141_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static c3_c* _141_tri__rd_gth_ha[] = {
    "87fd815913fa590c715d3a96ada5bae6298f3e7823af90ffcdf235f493c56330",
    0
  };
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
static c3_c* _141_tri__rd_ha[] = {
  "0afab285837f9c88faff3ac8f3f49b5e259da253e0505cd823a53d4e826261f7",
  0
};

  static u3j_harm _141_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
  static c3_c* _141_tri__rs_add_ha[] = {
    "b89a1e348628fd9b4fd520aabbf6c53e12be5bbf661b9094f7a9841be3f51de9",
    0
  };
  static u3j_harm _141_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
  static c3_c* _141_tri__rs_sub_ha[] = {
    "9d13b86d17908830f93b920e80e1a985105583597f6fb640f175c98052e357f1",
    0
  };
  static u3j_harm _141_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
  static c3_c* _141_tri__rs_mul_ha[] = {
    "5c61a9e335f6c139332523d871ca5773ab476365b623444370474fde08639061",
    0
  };
  static u3j_harm _141_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
  static c3_c* _141_tri__rs_div_ha[] = {
    "a7147a81be3ef5a1c0de6587edb2990e30057d7871d73632eabc8dc567e751c8",
    0
  };
  static u3j_harm _141_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
  static c3_c* _141_tri__rs_sqt_ha[] = {
    "c403bb48d78bcfa9cd4128b7f9eb362e71e5269ab6578fb0bb5e70b4244c5781",
    0
  };
  static u3j_harm _141_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
  static c3_c* _141_tri__rs_fma_ha[] = {
    "afc14914eb8f579a0c6620db69bc951329325747a8ef01c2d0e5feb29e9a9ddd",
    0
  };
  static u3j_harm _141_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
  static c3_c* _141_tri__rs_lth_ha[] = {
    "e7efa422def6b6dbd25b5f664462adb3895b8b45f9531877c9dbfe40a96612bf",
    0
  };
  static u3j_harm _141_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
  static c3_c* _141_tri__rs_lte_ha[] = {
    "b45e1ef16c47dc9b99865c7e75d4c2094c0e206ed538818a38da0adb7fbe2ce3",
    0
  };
  static u3j_harm _141_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
  static c3_c* _141_tri__rs_equ_ha[] = {
    "8c1178311ded837292c297380e48cf7e0bc4d83962dadcafda0c9ef9f20e39f2",
    0
  };
  static u3j_harm _141_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
  static c3_c* _141_tri__rs_gte_ha[] = {
    "50f5eb237b74e772eb6a3257441078461450cd4a25cf9bd97cb1a5e00f4ff4d2",
    0
  };
  static u3j_harm _141_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static c3_c* _141_tri__rs_gth_ha[] = {
    "505842ff486f0bb8fa63f04d2fd6f806dc760f9b4a12a3bf2d57da92f560785b",
    0
  };
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
  static c3_c* _141_tri__rs_ha[] = {
    "6c7027c5de34540a4b1548039e5423fb44727079af054b7135a3e961ec8dede8",
    0
  };

  static u3j_harm _141_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
  static c3_c* _141_tri__rq_add_ha[] = {
    "9c4c2a37550930605495401886d41fb9fbc2eba487e0ba845130fe88e4c52a01",
    0
  };
  static u3j_harm _141_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
  static c3_c* _141_tri__rq_sub_ha[] = {
    "f3b027090a1bb5af74234301facfbf64503b3e0599501ade12cb05aa158a79a3",
    0
  };
  static u3j_harm _141_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
  static c3_c* _141_tri__rq_mul_ha[] = {
    "15c03b1f3081514c4767d86297aaebf1d62af7f3437f30821f011b17ed769f3a",
    0
  };
  static u3j_harm _141_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
  static c3_c* _141_tri__rq_div_ha[] = {
    "094105c77e37e548ea1b8d49a132ab97d90a0ab5f329340400c381bcd44de347",
    0
  };
  static u3j_harm _141_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
  static c3_c* _141_tri__rq_sqt_ha[] = {
    "a031cd5b8e05f997323b0ca1ca9d2419401d7d2acc2da6fc6387fe57701b84b0",
    0
  };
  static u3j_harm _141_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
  static c3_c* _141_tri__rq_fma_ha[] = {
    "871664a9305808a671aacf1de0e83f8e951611033170d86370352afb363b79bc",
    0
  };
  static u3j_harm _141_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
  static c3_c* _141_tri__rq_lth_ha[] = {
    "b10822caa442c8e9f6b9eb52aac5796a42bed5ae0eef7fe49d4075b38bf78ddd",
    0
  };
  static u3j_harm _141_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
  static c3_c* _141_tri__rq_lte_ha[] = {
    "3316346be3e9464fbb5b0feff16cd2252086004229d08f8eaa320f9509d6a029",
    0
  };
  static u3j_harm _141_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
  static c3_c* _141_tri__rq_equ_ha[] = {
    "154be82a7b8ecf4571015d9ef6c0e90ffd8c3e8803441d9c2df5a4ea484ccf3b",
    0
  };
  static u3j_harm _141_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
  static c3_c* _141_tri__rq_gte_ha[] = {
    "edbe94f9cfefa89deef7f6c11f4ce8240fd93970dbd6130e632d0447452a612a",
    0
  };
  static u3j_harm _141_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static c3_c* _141_tri__rq_gth_ha[] = {
    "50e85936cfad61659ed1bfdab26fda2b2696571b9aa4b4c55dc5c0d919edc296",
    0
  };
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
static c3_c* _141_tri__rq_ha[] = {
  "b772cd5901a18dc91852fada74c2a1e4b1bbf4e7465451b0d8bdcc86d2288c22",
  0
};

  static u3j_harm _141_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
  static c3_c* _141_tri__rh_add_ha[] = {
    "7e2b600eced08d774800a6a3d82e18189db85010b870c26905ee38008d3d301e",
    0
  };
  static u3j_harm _141_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
  static c3_c* _141_tri__rh_sub_ha[] = {
    "827dfb41660cb4743a88d921b4185bd2000ee6f0708ec36ac8aba2e4e19c0875",
    0
  };
  static u3j_harm _141_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
  static c3_c* _141_tri__rh_mul_ha[] = {
    "ec2a010128aca0a6f74196f3de4fe7b6617fd810d3b19f7bf5878afb787e8b86",
    0
  };
  static u3j_harm _141_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
  static c3_c* _141_tri__rh_div_ha[] = {
    "067a3dafb0158bc2441729beb2c0934c6c6ccf0a9b5193db9c16f22b490b27c6",
    0
  };
  static u3j_harm _141_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
  static c3_c* _141_tri__rh_sqt_ha[] = {
    "3fdbba47626d91d41fcdf460ad38018c78b0233d7ec4d0fac406a8c5357a2384",
    0
  };
  static u3j_harm _141_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
  static c3_c* _141_tri__rh_fma_ha[] = {
    "ada2adf5a88ba61759219926aef950e72ae6926c6e10c30ecd0f9c99e79beca0",
    0
  };
  static u3j_harm _141_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
  static c3_c* _141_tri__rh_lth_ha[] = {
    "70c9bc0073d23371d8155c28795f5acbff1a9504a5a3881c8693aa7c1f3d35d4",
    0
  };
  static u3j_harm _141_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
  static c3_c* _141_tri__rh_lte_ha[] = {
    "6157e766050f9697c05b111ad2a582459a63a98b3b1ec70881f9ad943f951a0d",
    0
  };
  static u3j_harm _141_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
  static c3_c* _141_tri__rh_equ_ha[] = {
    "72664392e2a00137383aa5a55e947c5e95c7b3172f71a7238dc0d55050196884",
    0
  };
  static u3j_harm _141_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
  static c3_c* _141_tri__rh_gte_ha[] = {
    "145355b13712b9471031e755d0ab271907efdb9a287b1bacb2f1d0338d28dbf6",
    0
  };
  static u3j_harm _141_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static c3_c* _141_tri__rh_gth_ha[] = {
    "d53219ee10acdd291a1e2b34fa5f543c780a0301357e93cfebd466e556f9824e",
    0
  };
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
static c3_c* _141_tri__rh_ha[] = {
  "c38f8c0a7e2f1fccb52c459f60a30ec5d21635cafaf1aa120b70c1fa91cf7da5",
  0
};

  static u3j_harm _141_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static c3_c* _141_tri__og_raw_ha[] = {
    "280709dd8e0e720487dc9af267e0b44d096a59d7257b11a66d1d43f0608cfc3a",
    0
  };
static u3j_core _141_tri__og_d[] =
  { { "raw", 7, _141_tri__og_raw_a, 0, _141_tri__og_raw_ha },
    {}
  };
static c3_c* _141_tri__og_ha[] = {
  "6e39a44e0fc50378090e8c71f0cfac01d3ee07f11f5125f71619605d86b51676",
  0
};

  static u3j_harm _141_tri__sha_sha1_a[] = {{".2", u3we_sha1}, {}};
  static c3_c* _141_tri__sha_sha1_ha[] = {
    "20a18116548d3bfa459ae426d92a1c27535425b124d6a48ec1642945d27e5548",
    0
  };
static u3j_core _141_tri__sha_d[] =
  { { "sha1", 7, _141_tri__sha_sha1_a, 0, _141_tri__sha_sha1_ha },
    {}
  };
static c3_c* _141_tri__sha_ha[] = {
  "d3e4be4c3a39f94a51f675fd9a712bf1cfef9ac7ae6fc980160fc370a93bbf3b",
  0
};

static u3j_harm _141_tri_shax_a[] = {{".2", u3we_shax}, {}};
static c3_c* _141_tri_shax_ha[] = {
  "48ee5b29692df484bd1d0fd30ca01ea843f89f70fff8698a8f6af5c38639afe8",
  0
};
static u3j_harm _141_tri_shay_a[] = {{".2", u3we_shay}, {}};
static c3_c* _141_tri_shay_ha[] = {
  "02bcd048fca47fe895b5da5412cf1472eb09abbd2513de96d30a784f629410c9",
  0
};
static u3j_harm _141_tri_shas_a[] = {{".2", u3we_shas}, {}};
static c3_c* _141_tri_shas_ha[] = {
  "d6e39714b8e1a324be185a6d4f7a776f78eedd54becc820edbe53ce83f239e9b",
  0
};
static u3j_harm _141_tri_shal_a[] = {{".2", u3we_shal}, {}};
static c3_c* _141_tri_shal_ha[] = {
  "a9b750ed311b4fde51a51374cea35c6e0c4775908c9ad997ee470a003f086290",
  0
};

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
static c3_c* _141_tri_ha[] = {
  "6c8837fca8182e808dfd8019435663b584a79a6572e9b33f1c3f4afe0a86f6b9",
  0
};

/* layer two
*/
static u3j_harm _141_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
static c3_c* _141_two_flop_ha[] = {
  "73ac3be0119bcb822621de738f90975d98ce1ff3fb9a52853adc638271f61cd2",
  0
};
static u3j_harm _141_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
static c3_c* _141_two_lent_ha[] = {
  "1b98ab19350f6a6753ea4bd6daf4509a7c5681b7ac20c83204fe62846d46c2c3",
  0
};
static u3j_harm _141_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
static c3_c* _141_two_levy_ha[] = {
  "634f1f506b17b4b50e6902f6e21b290ffc5305d1546075cba745c9e195fcc56b",
  0
};
static u3j_harm _141_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
static c3_c* _141_two_lien_ha[] = {
  "2ffb70864f2be120b48869b27c614aadeed1390bde497d8940fe85b7861093ea",
  0
};
static u3j_harm _141_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
static c3_c* _141_two_murn_ha[] = {
  "e3ce526989bdb076849f594d6e2f72670d69e7d5d7d8b7bae464cf318a65f357",
  0
};
static u3j_harm _141_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
static c3_c* _141_two_need_ha[] = {
  "7bb1c43a5766a77fea1dc949121dd3f13529da62b726c76f34248047bc74f29f",
  0
};
static u3j_harm _141_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
static c3_c* _141_two_reap_ha[] = {
  "cf6bd10b97b418b67c645374712c768d9e7e9809c14ecf36a5c507e5fc4b4039",
  0
};
static u3j_harm _141_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
static c3_c* _141_two_reel_ha[] = {
  "36108d1ba09617cf62e739e0ff2dcf9286f322ca0e8faa3521ef127e9840eebf",
  0
};
static u3j_harm _141_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
static c3_c* _141_two_roll_ha[] = {
  "42abc6b3defd7c5eb8f6d14d57a14ba2a02d559907c03141c70a65e0803c01e5",
  0
};
static u3j_harm _141_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
static c3_c* _141_two_skid_ha[] = {
  "832366432a85005f9a9849d6de9a9045c8f9a591050519b06aec6a9a1a54c360",
  0
};
static u3j_harm _141_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
static c3_c* _141_two_skim_ha[] = {
  "ccbecb459b90d05ed6c1073859a58987bf9a479820b5550fa75f37b95f98a279",
  0
};
static u3j_harm _141_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
static c3_c* _141_two_skip_ha[] = {
  "873e3e4d6b8f16212911aa982065dd0d36b1f6e8834828d5eb5d59afa9da2384",
  0
};
static u3j_harm _141_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
static c3_c* _141_two_scag_ha[] = {
  "ddba868a28eb9655c9f6e06cfecb4ec9e9ff78277290579b9bb9b25339bb4ab9",
  0
};
static u3j_harm _141_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
static c3_c* _141_two_slag_ha[] = {
  "811f7f67de7ab3f33b85198a69b1bf344498cd4922b0273d918a48b803b7877d",
  0
};
static u3j_harm _141_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
static c3_c* _141_two_snag_ha[] = {
  "12a1d53541d4df9be60bda45f2dae6c6e6381f85edd4de062af23c6654860591",
  0
};
static u3j_harm _141_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
static c3_c* _141_two_sort_ha[] = {
  "f3f89553fc2eafd9702b9533b6dd405bae8056b4aa9674d5f12248d5a964149f",
  0
};
static u3j_harm _141_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
static c3_c* _141_two_turn_ha[] = {
  "cd4a292788acd440d6ace689f82fa999b342bb749585bc0e173098529bb75fb8",
  0
};
static u3j_harm _141_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
static c3_c* _141_two_weld_ha[] = {
  "d855628821d57392f575c5da000c7326eaaa19e08cda967a4772859269669df2",
  0
};

static u3j_harm _141_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
static c3_c* _141_two_bex_ha[] = {
  "ee7a095ea21b6438ec19ab235e73877b96108f0a14cae02cecbd8a48c44e70e3",
  0
};
static u3j_harm _141_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
static c3_c* _141_two_can_ha[] = {
  "5fe17c6d254a231e8c9ff94bc47f994c0c1bc202cc9fc2705faaf3fb351c78ec",
  0
};

//  XX appears to be a duplicate of _141_one_cap_a
//
static u3j_harm _141_two_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static c3_c* _141_two_cap_ha[] = {0};
static u3j_harm _141_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
static c3_c* _141_two_cat_ha[] = {
  "292d9fd88787d017fc1bfd743950d33143b8847212cad718b391a92ba725475a",
  0
};
static u3j_harm _141_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
static c3_c* _141_two_con_ha[] = {
  "4a5b1e559516a4208ac058371e045dcbe237dbc56a0a51f9cd4647c1efda5e5d",
  0
};
static u3j_harm _141_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
static c3_c* _141_two_cut_ha[] = {
  "c5892a89fb38f542b111240e882f02e0fdece4d91a90e5bf2d1f32c0a4770ffb",
  0
};
static u3j_harm _141_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
static c3_c* _141_two_dis_ha[] = {
  "a2e8b319b7b87d93572622b2b982d23c3f833b7fd652fc26ac8718153fbc0235",
  0
};
static u3j_harm _141_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
static c3_c* _141_two_dor_ha[] = {
  "277927a2e49e4d942e81ffc7740a71e68a7b732df886a9f84dc7d914be911879",
  0
};

//  XX appears to be a duplicate of _141_one_dvr_a
//
static u3j_harm _141_two_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static c3_c* _141_two_dvr_ha[] = {0};
static u3j_harm _141_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
static c3_c* _141_two_end_ha[] = {
  "45a0efc0c4ae4b93f554d480a9d2c52474d5ebd6b1b9b0ab888b9bee2117db55",
  0
};
static u3j_harm _141_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
static c3_c* _141_two_gor_ha[] = {
  "3ab7d6a56b8b347bd677a77ec43cda984d1eb869bab5c9bc2185f5c4a366703a",
  0
};
static u3j_harm _141_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
static c3_c* _141_two_lsh_ha[] = {
  "a93f01f1db5bcaf1973d01234bbcec8f8adf9d6402a8d715a1b13b70a140a428",
  0
};
//  XX appears to be a duplicate of _141_one_mas_a
//
static u3j_harm _141_two_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static c3_c* _141_two_mas_ha[] = {0};
static u3j_harm _141_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
static c3_c* _141_two_met_ha[] = {
  "6654d029fcee53f56439e35e824d955a1ec4081134916b0c5394941febb17b1e",
  0
};
static u3j_harm _141_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
static c3_c* _141_two_mix_ha[] = {
  "311a0350d86dac62f8f4b89c8fdf3ec61f14a4d66cc4cf59f9f548f806e4fe31",
  0
};
static u3j_harm _141_two_mor_a[] = {{".2", u3wc_mor, c3y}, {}};
static c3_c* _141_two_mor_ha[] = {
  "10ee585bfd1f9109535f09a57fd86e02522e9f019d05edfb70bcedf8b01521b8",
  0
};
static u3j_harm _141_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
static c3_c* _141_two_mug_ha[] = {
  "4ce008be48d5e609df8fa981bdce3d00722128aab1702573aa0c1a528477c3a7",
  0
};
static u3j_harm _141_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
static c3_c* _141_two_muk_ha[] = {
  "de425abca39f90204eee4b89958f4b1be21eada95754ffc37597bd76653a689d",
  0
};

//  XX appears to be a duplicate of _141_one_peg_a
//
static u3j_harm _141_two_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static c3_c* _141_two_peg_ha[] = {0};
static u3j_harm _141_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
static c3_c* _141_two_pow_ha[] = {
  "3bc8ad91db75395dc15a996ae7e8c2522f97d8f4b8037e23e2675061c5029792",
  0
};
static u3j_harm _141_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
static c3_c* _141_two_rap_ha[] = {
  "575b53509cddb0a58026f885bd0f53be371ba9f5720f09c4c28a2ba97f89ae99",
  0
};
static u3j_harm _141_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
static c3_c* _141_two_rep_ha[] = {
  "41c77539ac2d81936770a56791f19156c57e3faf46be3d3b7f4426d87a5a199b",
  0
};
static u3j_harm _141_two_rev_a[] = {{".2", u3wc_rev, c3y}, {}};
static c3_c* _141_two_rev_ha[] = {
  "e9cbd82073ced7b2b96a6ba0a4794c9f5dc90ddc362f9de5a65a1f2fa4fa9cd3",
  0
};
static u3j_harm _141_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
static c3_c* _141_two_rip_ha[] = {
  "e8e0b834aded0d2738bcf38a93bf373d412a51e0cee7f274277a6393e634a65e",
  0
};
static u3j_harm _141_two_ripn_a[] = {{".2", u3wc_ripn, c3y}, {}};
static c3_c* _141_two_ripn_ha[] = {
  "2759a6931e9ef9a52c8eee43108843d94d3932a6b04f86cb6ba1555343293a28",
  0
};
static u3j_harm _141_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
static c3_c* _141_two_rsh_ha[] = {
  "a401145b4c11ec8d17a729fe30f06c295865ffed1b970b0a788f0fec1ed0a703",
  0
};
static u3j_harm _141_two_swp_a[] = {{".2", u3wc_swp, c3y}, {}};
static c3_c* _141_two_swp_ha[] = {
  "f809ed11a87db6cef8944c7252d53cda1e030240ee52912c3843d56805ac17fa",
  0
};
static u3j_harm _141_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
static c3_c* _141_two_sqt_ha[] = {
  "fc28ff327ae69f55ccf257b69a1477b845552ef9e615e85718902c249bdeca6f",
  0
};
static u3j_harm _141_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
static c3_c* _141_two_xeb_ha[] = {
  "39501080d96580dab9086d3cbdf95356c0821897fd54a930a8cfe2684cf3c7de",
  0
};

  static u3j_harm _141_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
  static c3_c* _141_two__in_bif_ha[] = {
    "7ccbde61c80246056f6acfd8dc30f560af9e5abd44841c22ba0f49951dbc2f2a",
    0
  };
  static u3j_harm _141_two__in_del_a[] = {{".2", u3wdi_del}, {}};
  static c3_c* _141_two__in_del_ha[] = {
    "b03dc379cfa0b9eca24cf01d57cadd20f65c64311b5ee90732ec2def97c8a673",
    0
  };
  static u3j_harm _141_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
  static c3_c* _141_two__in_dif_ha[] = {
    "e4367b9e5d425687a18c98def65e36385d05b4e7ed5d30420807bf147fd5fabb",
    0
  };
  static u3j_harm _141_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
  static c3_c* _141_two__in_gas_ha[] = {
    "223a60a43a10f1f90a3b205ecfce8e17af1adfcf9dbf3cff9b8b1362656b1af1",
    0
  };
  static u3j_harm _141_two__in_has_a[] = {{".2", u3wdi_has}, {}};
  static c3_c* _141_two__in_has_ha[] = {
    "eebeebeaff243c5795575a468191474459c7b191fb575e1b96feb484fcbc19dc",
    0
  };

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__in_int_a[] = {{".2", u3wdi_int}, {}};
  // static c3_c* _141_two__in_int_ha[] = {0};

  static u3j_harm _141_two__in_put_a[] = {{".2", u3wdi_put}, {}};
  static c3_c* _141_two__in_put_ha[] = {
    "4a9fd615fecd2fd36485b3a2f24cdc13afc86f9a478362934b4654297496a03c",
    0
  };
  static u3j_harm _141_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
  static c3_c* _141_two__in_tap_ha[] = {
    "7dde59e2bd7684e785ce9787bc394571bd1216d7a62398c703447fc951c6b352",
    0
  };
  static u3j_harm _141_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
  static c3_c* _141_two__in_wyt_ha[] = {
    "fac9248ebd1defade9df695cd81f94355bebb271f85b164ff34658a5f45c71a0",
    0
  };
  static u3j_harm _141_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static c3_c* _141_two__in_uni_ha[] = {
    "8369d11970bfa09bd20c5b112a353fa10e8e64c9c081e3a5b17bcf3700127add",
    0
  };
static u3j_core _141_two__in_d[] =
  { { "bif", 7, _141_two__in_bif_a, 0, _141_two__in_bif_ha },
    { "del", 7, _141_two__in_del_a, 0, _141_two__in_del_ha },
    { "dif", 7, _141_two__in_dif_a, 0, _141_two__in_dif_ha },
    { "gas", 7, _141_two__in_gas_a, 0, _141_two__in_gas_ha },
    { "has", 7, _141_two__in_has_a, 0, _141_two__in_has_ha },
    // { "int", 7, _141_two__in_int_a, 0, _141_two__in_int_ha },
    { "put", 7, _141_two__in_put_a, 0, _141_two__in_put_ha },
    { "tap", 7, _141_two__in_tap_a, 0, _141_two__in_tap_ha },
    { "wyt", 3, _141_two__in_wyt_a, 0, _141_two__in_wyt_ha },
    { "uni", 7, _141_two__in_uni_a, 0, _141_two__in_uni_ha },
    {}
  };
static c3_c* _141_two__in_ha[] = {
  "abf20b11b7d7f9aa8cc7b4de01c15ec3aca3ea07ca09a461a3277fe24c640849",
  0
};

  static u3j_harm _141_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
  static c3_c* _141_two__by_bif_ha[] = {
    "09ce4cf00dd9b4f95d4d93a984ffab94cb99cb6017bb73531245ea4813855f4e",
    0
  };
  static u3j_harm _141_two__by_del_a[] = {{".2", u3wdb_del, c3y}, {}};
  static c3_c* _141_two__by_del_ha[] = {
    "c51c30a2c58c351d4c7cbc3f8276432140b74f3f2b3a76db4b46b189f5cd8cfe",
    0
  };
  static u3j_harm _141_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
  static c3_c* _141_two__by_dif_ha[] = {
    "f40cac6183410ea88c1d6dd43fd2b2c7fb6178bcbf9d5ceb4accf5e28a0c1103",
    0
  };
  static u3j_harm _141_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
  static c3_c* _141_two__by_gas_ha[] = {
    "43046602e0b9e568b09448cfe18527e2331f3393a2f32e485d9707a14c346698",
    0
  };
  static u3j_harm _141_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
  static c3_c* _141_two__by_get_ha[] = {
    "ce021b5e383d672ab43d771857239b6789a8cdb145a626799c77c748a2f7c918",
    0
  };
  static u3j_harm _141_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
  static c3_c* _141_two__by_has_ha[] = {
    "04ecc67ab25961bee1b7c9dbcf42965d16f32474b9bbdd2b286983f998e3957a",
    0
  };

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
  // static c3_c* _141_two__by_int_ha[] = {0};

  static u3j_harm _141_two__by_jab_a[] = {{".2", u3wdb_jab, c3y}, {}};
  static c3_c* _141_two__by_jab_ha[] = {
    "8bc992aefabd2e0f43c900f2c4f3b06cf330973774d8f43428049cc3b3cb5b94",
    0
  };
  static u3j_harm _141_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
  static c3_c* _141_two__by_put_ha[] = {
    "2cc9f005fde5314e9ad545286493a8c81b5c3b775d645ad82954f405d9414a32",
    0
  };
  static u3j_harm _141_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
  static c3_c* _141_two__by_tap_ha[] = {
    "7dde59e2bd7684e785ce9787bc394571bd1216d7a62398c703447fc951c6b352",
    0
  };

  // https://github.com/urbit/urbit/issues/328
  // static u3j_harm _141_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  // static c3_c* _141_two__by_uni_ha[] = {0};

static u3j_core _141_two__by_d[] =
  { { "bif", 7, _141_two__by_bif_a, 0, _141_two__by_bif_ha },
    { "del", 7, _141_two__by_del_a, 0, _141_two__by_del_ha },
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
static c3_c* _141_two__by_ha[] = {
  "2bb4c60da0ae916cd0aa596588bdd0f7070f0832e698526aac951fd55a4abbdc",
  0
};

static u3j_harm _141_two_cue_a[] = {{".2", u3we_cue}, {}};
static c3_c* _141_two_cue_ha[] = {
  "87acffeccdc6e1ce72d74e41f91c8f1d190f70e09ce755c6a487e0c951dcc139",
  0
};
static u3j_harm _141_two_jam_a[] = {{".2", u3we_jam}, {}};
static c3_c* _141_two_jam_ha[] = {
  "5c52fe8ebea73c478aaac344d06e9ff48c075be67c3a3cc77a57ef0143bd9219",
  0
};
static u3j_harm _141_two_mat_a[] = {{".2", u3we_mat}, {}};
static c3_c* _141_two_mat_ha[] = {
  "b5cd9fd1eded54fcb9bfd06af3c34460c1aa4cfc46f1ee9bd3f6476aa8fbb8c8",
  0
};
static u3j_harm _141_two_rub_a[] = {{".2", u3we_rub}, {}};
static c3_c* _141_two_rub_ha[] = {
  "87fcf40fb6fce8c3cb778373670d0682785ae650f785531db8ff69d431bc14c6",
  0
};

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
  { "sort", 7, _141_two_sort_a, 0, _141_two_sort_ha },
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
  { "jam", 7, _141_two_jam_a, 0, _141_two_jam_ha },
  { "lsh", 7, _141_two_lsh_a, 0, _141_two_lsh_ha },
  { "mas", 7, _141_two_mas_a, 0, _141_two_mas_ha },
  { "mat", 7, _141_two_mat_a, 0, _141_two_mat_ha },
  { "met", 7, _141_two_met_a, 0, _141_two_met_ha },
  { "mix", 7, _141_two_mix_a, 0, _141_two_mix_ha },
  { "mor", 7, _141_two_mor_a, 0, _141_two_mor_ha },
  { "mug", 7, _141_two_mug_a, 0, _141_two_mug_ha },
  { "muk", 59, _141_two_muk_a, 0, _141_two_muk_ha },
  { "rap", 7, _141_two_rap_a, 0, _141_two_rap_ha },
  { "rep", 7, _141_two_rep_a, 0, _141_two_rep_ha },
  { "rev", 7, _141_two_rev_a, 0, _141_two_rev_ha },
  { "rip", 7, _141_two_rip_a, 0, _141_two_rip_ha },
  { "ripn", 7, _141_two_ripn_a, 0, _141_two_ripn_ha },
  { "rsh", 7, _141_two_rsh_a, 0, _141_two_rsh_ha },
  { "swp", 7, _141_two_swp_a, 0, _141_two_swp_ha },
  { "rub", 7, _141_two_rub_a, 0, _141_two_rub_ha },
  { "peg", 7, _141_two_peg_a, 0, _141_two_peg_ha },
  { "pow", 7, _141_two_pow_a, 0, _141_two_pow_ha },
  { "sqt", 7, _141_two_sqt_a, 0, _141_two_sqt_ha },
  { "xeb", 7, _141_two_xeb_a, 0, _141_two_xeb_ha },

  { "by",  7, 0, _141_two__by_d, _141_two__by_ha },
  { "in",  7, 0, _141_two__in_d, _141_two__in_ha },
  {}
};
static c3_c* _141_two_ha[] = {
  "56cd63625015fb07de63ad95cbb90a303dd2e1e1efca4c93a1bc053e0b6a9f7a",
  0
};

/* layer one
*/
static u3j_harm _141_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
static c3_c* _141_one_add_ha[] = {
  "46407e27fe5d7c20b3ba25c02657c227b37217ddab8501b2d3b70b818aca7a44",
  0
};
static u3j_harm _141_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
static c3_c* _141_one_dec_ha[] = {
  "6345d28d34c62c4b4f9da98828574bc9060ff0869789968d9045d90faeb3580c",
  0
};
static u3j_harm _141_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
static c3_c* _141_one_div_ha[] = {
  "e3292e76feb274b9314e7693827de11e96677629c556b3a6c72cc15ebad45113",
  0
};
static u3j_harm _141_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static c3_c* _141_one_dvr_ha[] = {
  "fc259f46d770f82767163544f3662dfd45b1484a7bcffad396c7420651f092a4",
  0
};
static u3j_harm _141_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
static c3_c* _141_one_gte_ha[] = {
  "f3ff2c0fc1f386226183e8834cff87420a1206583f8710e1e75f0e34ed8df5fe",
  0
};
static u3j_harm _141_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
static c3_c* _141_one_gth_ha[] = {
  "62692d64c8166c7d48bb2a00713064846da9629a1dd2d924c3b15cfd18a5912a",
  0
};
static u3j_harm _141_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
static c3_c* _141_one_lte_ha[] = {
  "6ca61752aa27b453f28f20e12f652610d45695c3bd965190d5b4fa8b9daa518c",
  0
};
static u3j_harm _141_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
static c3_c* _141_one_lth_ha[] = {
  "39260325faffbbf5bd88c4abb3efb09c5a7e1deb81a2126498d6c0f49474955e",
  0
};
static u3j_harm _141_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
static c3_c* _141_one_mod_ha[] = {
  "374d2f3cd0ece33f680bd7103b99891d7dae03590f9eb9faac03a4a501f17038",
  0
};
static u3j_harm _141_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
static c3_c* _141_one_mul_ha[] = {
  "51e45dbea29cf65a5c26ead095a20eb12ba078840652c88b9c1997820e670bc6",
  0
};
static u3j_harm _141_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
static c3_c* _141_one_sub_ha[] = {
  "016695719ffe93c177e8a03afa5d29fc428ff596bb8962ace50f7706cd6e53a6",
  0
};

static u3j_harm _141_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static c3_c* _141_one_cap_ha[] = {
  "407e764ee978c712b81c9c3452932e0f7d33faeda36dfe99aaf81d543db16254",
  0
};
static u3j_harm _141_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static c3_c* _141_one_peg_ha[] = {
  "8b608d2d2e2eccec3e2fc8cd2d92fd69504c72b26581bb9cbfa4ff51f997251f",
  0
};
static u3j_harm _141_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static c3_c* _141_one_mas_ha[] = {
  "94bfb3ec6e032bf386349e9ae0784f37144e65692830d11a06fa89602e313f7f",
  0
};

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
static c3_c* _141_one_ha[] = {
  "3f22006efef06ab8171cfd03057dbcb1af325c16f03662a67119d4060b3b0f6a",
  0
};

u3j_core _k141_d[] =
{ { "one", 3, 0, _141_one_d, _141_one_ha },
  {}
};
static c3_c* _k141_ha[] = {
  "7768e2670a7d95397c0587f4d7834652602f70f0206efce6c3345c3f70dfc12a",
  0
};

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
