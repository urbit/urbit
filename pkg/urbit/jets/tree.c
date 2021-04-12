/*
  To generate the hashes, take the sha256 of the jammed battery. For example:

  ```
  > `@ux`(shax (jam -:rip))
  0x2759.a693.1e9e.f9a5.2c8e.ee43.1088.43d9.4d39.32a6.b04f.86cb.6ba1.5553.4329.3a28
  ```

  Becomes:

  ```
  2759a6931e9ef9a52c8eee43108843d94d3932a6b04f86cb6ba1555343293a28
  ```
*/

#include "all.h"

static u3j_harm _140_hex_mimes_base16_en_a[] = {{".2", u3we_en_base16}, {}};
static c3_c* _140_hex_mimes_base16_en_ha[] = {
  "669807766b6802719769fcbfe149d77fb352fcf0922afaf35dc4ab8c201d84e5",
  0
};

static u3j_harm _140_hex_mimes_base16_de_a[] = {{".2", u3we_de_base16}, {}};
static c3_c* _140_hex_mimes_base16_de_ha[] = {
  "f1e04d0f452f2783e17b3bd5bbbcf95a651624fe7a2aca28dd9a7feae1319734",
  0
};

static u3j_core _140_hex_mimes_base16_d[] =
  { { "en", 7, _140_hex_mimes_base16_en_a, 0, _140_hex_mimes_base16_en_ha },
    { "de", 7, _140_hex_mimes_base16_de_a, 0, _140_hex_mimes_base16_de_ha },
    {}
  };
static c3_c* _140_hex_mimes_base16_ha[] = {
  "c71bdcc8542fd49aa307f296ac097e300a714fb3b3d1e475426f0916fa61f12c",
  0
};

static u3j_core _140_hex_mimes_d[] =
  { { "base16", 3, 0, _140_hex_mimes_base16_d, _140_hex_mimes_base16_ha },
    {}
  };
static c3_c* _140_hex_mimes_ha[] = {
  "bca2510fd7172643812f9d224deb95ddd69400c990db3d8cfb517a7292e8f06e",
  0
};

static u3j_harm _140_hex_aes_ecba_en_a[] = {{".2", u3wea_ecba_en}, {}};
static c3_c* _140_hex_aes_ecba_en_ha[] = {
  "d7674ad72666a787580c52785c5d4d37ca462ba05e904efbeded5d1bd8b02b4b",
  0
};
static u3j_harm _140_hex_aes_ecba_de_a[] = {{".2", u3wea_ecba_de}, {}};
static c3_c* _140_hex_aes_ecba_de_ha[] = {
  "6e6599e93bea2e4297b621814bfb17a9b10849f920d50b14e808ef2e4fe62383",
  0
};
static u3j_harm _140_hex_aes_ecbb_en_a[] = {{".2", u3wea_ecbb_en}, {}};
static c3_c* _140_hex_aes_ecbb_en_ha[] = {
  "2c0e3c8f4d741b37324563ecd0ab8fbf87721d1e017f1eeeaf8b6a60515c483b",
  0
};
static u3j_harm _140_hex_aes_ecbb_de_a[] = {{".2", u3wea_ecbb_de}, {}};
static c3_c* _140_hex_aes_ecbb_de_ha[] = {
  "cf78f314a1dbbc53b28d6405b98c66a4451350757872d8f7cf0477411e731acc",
  0
};
static u3j_harm _140_hex_aes_ecbc_en_a[] = {{".2", u3wea_ecbc_en}, {}};
static c3_c* _140_hex_aes_ecbc_en_ha[] = {
  "f56450f2662082e27ba1aecd2fe04c66aa8641d6eb155f8d3707e242a1e1cf1c",
  0
};
static u3j_harm _140_hex_aes_ecbc_de_a[] = {{".2", u3wea_ecbc_de}, {}};
static c3_c* _140_hex_aes_ecbc_de_ha[] = {
  "28c8c44002799fbe94d4aa07922f2b74dbbf234c84684cd8c3187622b4be6a5f",
  0
};

static u3j_harm _140_hex_aes_cbca_en_a[] = {{".2", u3wea_cbca_en}, {}};
static c3_c* _140_hex_aes_cbca_en_ha[] = {
  "f85366d520b3179c5dabfb58ee1fa0554f5044f676340439db875841cd4058de",
  0
};
static u3j_harm _140_hex_aes_cbca_de_a[] = {{".2", u3wea_cbca_de}, {}};
static c3_c* _140_hex_aes_cbca_de_ha[] = {
  "8b876fbdb1849d8fbabba5e143aea0532a0e5dfff1c784d7ad15fd497ea376b1",
  0
};
static u3j_harm _140_hex_aes_cbcb_en_a[] = {{".2", u3wea_cbcb_en}, {}};
static c3_c* _140_hex_aes_cbcb_en_ha[] = {
  "6f961e0629c5efce47793e6a352220d355bb8fba6656c6941a68efb3ba10999d",
  0
};
static u3j_harm _140_hex_aes_cbcb_de_a[] = {{".2", u3wea_cbcb_de}, {}};
static c3_c* _140_hex_aes_cbcb_de_ha[] = {
  "7ee2f33f80612e91fda1fd84201266dea6cab596a4e23a535d1a14fb0763f1a3",
  0
};
static u3j_harm _140_hex_aes_cbcc_en_a[] = {{".2", u3wea_cbcc_en}, {}};
static c3_c* _140_hex_aes_cbcc_en_ha[] = {
  "b2578cf17a3095f48cc96cf7690dd7ab4f4e0b76b2578eadc1dce31a075f5b12",
  0
};
static u3j_harm _140_hex_aes_cbcc_de_a[] = {{".2", u3wea_cbcc_de}, {}};
static c3_c* _140_hex_aes_cbcc_de_ha[] = {
  "36586f89d702bedb8c2a01ea3614f61627e762488e373106cbb1b27c46e3493c",
  0
};
static u3j_harm _140_hex_aes_siva_en_a[] = {{".2", u3wea_siva_en}, {}};
static c3_c* _140_hex_aes_siva_en_ha[] = {
  "2a137039301788b8540ed81cbfafe450c9306348d02a6576f5c14f6d6f20ba81",
  0
};
static u3j_harm _140_hex_aes_siva_de_a[] = {{".2", u3wea_siva_de}, {}};
static c3_c* _140_hex_aes_siva_de_ha[] = {
  "d0130e9229e71c589429dd87843dc104bc4ee5b426400a547081d8c91a548eaa",
  0
};
static u3j_harm _140_hex_aes_sivb_en_a[] = {{".2", u3wea_sivb_en}, {}};
static c3_c* _140_hex_aes_sivb_en_ha[] = {
  "1638f56e8728f285e4175c7b514c5a4e1a24205acf33e105f0513cad7ae843cf",
  0
};
static u3j_harm _140_hex_aes_sivb_de_a[] = {{".2", u3wea_sivb_de}, {}};
static c3_c* _140_hex_aes_sivb_de_ha[] = {
  "64c9b199fffd6d31baf7457bf27d5a510121be45201b2ae5cc1d9565c0bdd0ff",
  0
};
static u3j_harm _140_hex_aes_sivc_en_a[] = {{".2", u3wea_sivc_en}, {}};
static c3_c* _140_hex_aes_sivc_en_ha[] = {
  "d721486dea943efd52d9e6450f4f48dd191c89637a2f842d3ff6edfd3beecbb9",
  0
};
static u3j_harm _140_hex_aes_sivc_de_a[] = {{".2", u3wea_sivc_de}, {}};
static c3_c* _140_hex_aes_sivc_de_ha[] = {
  "3ded831b992ea100582229a4d1d9b5c80380128ae6b59b5bb36403ed13dc5d55",
  0
};

static u3j_core _140_hex_aes_ecba_d[] =
  { { "en", 7, _140_hex_aes_ecba_en_a, 0, _140_hex_aes_ecba_en_ha },
    { "de", 7, _140_hex_aes_ecba_de_a, 0, _140_hex_aes_ecba_de_ha },
    {}
  };
static c3_c* _140_hex_aes_ecba_ha[] = {
  "693409d27b777f73ce92cebd38e9ebceebe1e9c27ad8c9de9afc091e31bd7d9f",
  0
};

static u3j_core _140_hex_aes_ecbb_d[] =
  { { "en", 7, _140_hex_aes_ecbb_en_a, 0, _140_hex_aes_ecbb_en_ha },
    { "de", 7, _140_hex_aes_ecbb_de_a, 0, _140_hex_aes_ecbb_de_ha },
    {}
  };
static c3_c* _140_hex_aes_ecbb_ha[] = {
  "7255c39f10e068007d0d64dd40e4f6e83492ed2e3222919440be0d28fd42a5e3",
  0
};

static u3j_core _140_hex_aes_ecbc_d[] =
  { { "en", 7, _140_hex_aes_ecbc_en_a, 0, _140_hex_aes_ecbc_en_ha },
    { "de", 7, _140_hex_aes_ecbc_de_a, 0, _140_hex_aes_ecbc_de_ha },
    {}
  };
static c3_c* _140_hex_aes_ecbc_ha[] = {
  "65da73b8de06a6660ca2d36be881b708362e1f8bc12c01290aed90fdb2977667",
  0
};

static u3j_core _140_hex_aes_cbca_d[] =
  { { "en", 7, _140_hex_aes_cbca_en_a, 0, _140_hex_aes_cbca_en_ha },
    { "de", 7, _140_hex_aes_cbca_de_a, 0, _140_hex_aes_cbca_de_ha },
    {}
  };
static c3_c* _140_hex_aes_cbca_ha[] = {
  "420d04c03b7816b656fe4e8d9fce04f7e6d51d3d274c6511e89cfdb43ebf107e",
  0
};

static u3j_core _140_hex_aes_cbcb_d[] =
  { { "en", 7, _140_hex_aes_cbcb_en_a, 0, _140_hex_aes_cbcb_en_ha },
    { "de", 7, _140_hex_aes_cbcb_de_a, 0, _140_hex_aes_cbcb_de_ha },
    {}
  };
static c3_c* _140_hex_aes_cbcb_ha[] = {
  "1b84daab497795f2afd7238a6a6090be68b78eb6051b0ffa076b2ed9fedeebe5",
  0
};

static u3j_core _140_hex_aes_cbcc_d[] =
  { { "en", 7, _140_hex_aes_cbcc_en_a, 0, _140_hex_aes_cbcc_en_ha },
    { "de", 7, _140_hex_aes_cbcc_de_a, 0, _140_hex_aes_cbcc_de_ha },
    {}
  };
static c3_c* _140_hex_aes_cbcc_ha[] = {
  "3bab1a59c7673afeb659821d54754e8e5e281243e79624fdbe4d7f85cae192c5",
  0
};

static u3j_core _140_hex_aes_siva_d[] =
  { { "en", 7, _140_hex_aes_siva_en_a, 0, _140_hex_aes_siva_en_ha },
    { "de", 7, _140_hex_aes_siva_de_a, 0, _140_hex_aes_siva_de_ha },
    {}
  };
static c3_c* _140_hex_aes_siva_ha[] = {
  "435c5c769d2522d71ab60332bb57440d69c6803d5ca9a5faae88c825cb55d72e",
  0
};
static u3j_core _140_hex_aes_sivb_d[] =
  { { "en", 7, _140_hex_aes_sivb_en_a, 0, _140_hex_aes_sivb_en_ha },
    { "de", 7, _140_hex_aes_sivb_de_a, 0, _140_hex_aes_sivb_de_ha },
    {}
  };
static c3_c* _140_hex_aes_sivb_ha[] = {
  "0683f3f9067c2a16f68805c778c404179dc5df9019bbbe0f8d680a99a69e61fc",
  0
};
static u3j_core _140_hex_aes_sivc_d[] =
  { { "en", 7, _140_hex_aes_sivc_en_a, 0, _140_hex_aes_sivc_en_ha },
    { "de", 7, _140_hex_aes_sivc_de_a, 0, _140_hex_aes_sivc_de_ha },
    {}
  };
static c3_c* _140_hex_aes_sivc_ha[] = {
  "23ef582f110d28aff82b0795305b02e5a718d667bcae97091c08bc26790e7176",
  0
};

static u3j_core _140_hex_aes_d[] =
  { { "ecba", 7, 0, _140_hex_aes_ecba_d, _140_hex_aes_ecba_ha },
    { "ecbb", 7, 0, _140_hex_aes_ecbb_d, _140_hex_aes_ecbb_ha },
    { "ecbc", 7, 0, _140_hex_aes_ecbc_d, _140_hex_aes_ecbc_ha },
    { "cbca", 7, 0, _140_hex_aes_cbca_d, _140_hex_aes_cbca_ha },
    { "cbcb", 7, 0, _140_hex_aes_cbcb_d, _140_hex_aes_cbcb_ha },
    { "cbcc", 7, 0, _140_hex_aes_cbcc_d, _140_hex_aes_cbcc_ha },
    { "siva", 7, 0, _140_hex_aes_siva_d, _140_hex_aes_siva_ha },
    { "sivb", 7, 0, _140_hex_aes_sivb_d, _140_hex_aes_sivb_ha },
    { "sivc", 7, 0, _140_hex_aes_sivc_d, _140_hex_aes_sivc_ha },
    {}
  };
static c3_c* _140_hex_aes_ha[] = {
  "ca4c1b0cff03db74ceca1b844f0223d669aa42934152a784d431469f0eb71527",
  0
};

static u3j_harm _140_hex_leer_a[] = {{".2", u3we_leer}, {}};
static c3_c* _140_hex_leer_ha[] = {
  "8a4486bb09639f6b8cf7631f9bf883256529c6d7d9aa320ab6dfa5517611f0d7",
  0
};
static u3j_harm _140_hex_lore_a[] = {{".2", u3we_lore}, {}};
static c3_c* _140_hex_lore_ha[] = {
  "19b13cfea49fd14aafbb20b8b888ba454f809c3f50a7cfeebd43f87336fe052d",
  0
};
static u3j_harm _140_hex_loss_a[] = {{".2", u3we_loss}, {}};
static c3_c* _140_hex_loss_ha[] = {
  "67aacd21484078828ad4342297d44c38d9213b8809f83f695c2996378a92dc2a",
  0
};
static u3j_harm _140_hex_lune_a[] = {{".2", u3we_lune}, {}};
static c3_c* _140_hex_lune_ha[] = {
  "417472f35b885fe6dd0715e78fd0920cb59f68b738aadc9768e73bc5efa0e570",
  0
};

static u3j_harm _140_hex_coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
static c3_c* _140_hex_coed__ed_puck_ha[] = {
  "1bc694675842345c50b0e20a2193bb5bcbb42f163fc832431a3d1822a81e4c98",
  0
};
static u3j_harm _140_hex_coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
static c3_c* _140_hex_coed__ed_sign_ha[] = {
  "34ad749bf8443611cbf1f7de90a066318bd12be36f2f7f6f55281f6f7ed79754",
  0
};
static u3j_harm _140_hex_coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
static c3_c* _140_hex_coed__ed_veri_ha[] = {
  "047a7eeccb2e68aeeee631b6db86e11a5a3aa9e179660553eca6304327612dcf",
  0
};
static u3j_harm _140_hex_coed__ed_shar_a[] = {{".2", u3wee_shar}, {}};
static c3_c* _140_hex_coed__ed_shar_ha[] = {
  "52d3b0a2f51f2b0a9dd72bb33db38c73dc873029c365d871d0559a1472a80e72",
  0
};

static u3j_harm _140_hex_coed__ed_point_add_a[] =
    {{".2", u3wee_point_add}, {}};

static u3j_harm _140_hex_coed__ed_scalarmult_a[] =
    {{".2", u3wee_scalarmult}, {}};
static c3_c* _140_hex_coed__ed_scalarmult_ha[] = {
  "72e71cd3aa3af429cd65baa78632500c60edd1d4c82a39d3ba7f231ef97e6316",
  0
};

static u3j_harm _140_hex_coed__ed_scalarmult_base_a[] =
    {{".2", u3wee_scalarmult_base}, {}};
static c3_c* _140_hex_coed__ed_scalarmult_base_ha[] = {
  "976fdb8251f9b767af689a0d2c41b88941921bf53777c1ceeb5297511021f9d8",
  0
};

static u3j_harm _140_hex_coed__ed_add_scalarmult_scalarmult_base_a[] =
    {{".2", u3wee_add_scalarmult_scalarmult_base}, {}};
static c3_c* _140_hex_coed__ed_add_scalarmult_scalarmult_base_ha[] = {
  "11819071c24a2d7b36daea6e16c78b2e05f9ca3e857cf4815ffe652ce677a61a",
  0
};

static u3j_harm _140_hex_coed__ed_add_double_scalarmult_a[] =
    {{".2", u3wee_add_double_scalarmult}, {}};
static c3_c* _140_hex_coed__ed_add_double_scalarmult_ha[] = {
  "0fab78a1e890e53cecade1c22b95813db77e066044e33417a0919695b6cde9ba",
  0
};

static u3j_core _140_hex_coed__ed_d[] =
  { { "sign", 7, _140_hex_coed__ed_sign_a, 0, _140_hex_coed__ed_sign_ha },
    { "puck", 7, _140_hex_coed__ed_puck_a, 0, _140_hex_coed__ed_puck_ha },
    { "veri", 7, _140_hex_coed__ed_veri_a, 0, _140_hex_coed__ed_veri_ha },
    { "shar", 7, _140_hex_coed__ed_shar_a, 0, _140_hex_coed__ed_shar_ha },
    { "point-add", 7, _140_hex_coed__ed_point_add_a, 0, 0 },
    { "scalarmult", 7, _140_hex_coed__ed_scalarmult_a, 0,
      _140_hex_coed__ed_scalarmult_ha },
    { "scalarmult-base", 7, _140_hex_coed__ed_scalarmult_base_a, 0,
      _140_hex_coed__ed_scalarmult_base_ha },
    { "add-scalarmult-scalarmult-base", 7,
      _140_hex_coed__ed_add_scalarmult_scalarmult_base_a, 0,
      _140_hex_coed__ed_add_scalarmult_scalarmult_base_ha },
    { "add-double-scalarmult", 7,
      _140_hex_coed__ed_add_double_scalarmult_a, 0,
      _140_hex_coed__ed_add_double_scalarmult_ha },
    {}
  };
static c3_c* _140_hex_coed__ed_ha[] = {
  "7a44a962aa72933588b5c99a8b68ebac21ce3c4710c081cb66b3599b45af9ced",
  0
};

static u3j_core _140_hex_coed_d[] =
{ { "ed", 3, 0, _140_hex_coed__ed_d, _140_hex_coed__ed_ha },
  {}
};
static c3_c* _140_hex_coed_ha[] = {
  "4be0254f06d953b69509eb15550595ffad8767d3c3dc2dafcd7c22f92f7704c4",
  0
};

  static u3j_harm _140_hex_hmac_hmac_a[] = {{".2", u3we_hmac}, {}};
  static c3_c* _140_hex_hmac_hmac_ha[] = {
    "d0dbd778156aef21d18f44a8cffd87296826120af5a4af020dd7aff0f95f03b1",
    0
  };
static u3j_core _140_hex_hmac_d[] =
  { { "hmac", 7, _140_hex_hmac_hmac_a, 0, _140_hex_hmac_hmac_ha },
    {}
  };
static c3_c* _140_hex_hmac_ha[] = {
  "976bb4508dbe659eb12aa32d4a481dbd885e40f8a15c505762f1acf43b744234",
  0
};

  static u3j_harm _140_hex_argon2_a[] = {{".2", u3we_argon2}, {}};
  static c3_c* _140_hex_argon2_ha[] = {
    "4df7cec141ffa2cc76b058846474ca42cc9840666ee3e7e80e565803e83ea98b",
    0
  };
static u3j_core _140_hex_argon_d[] =
  { { "argon2", 511, _140_hex_argon2_a, 0, _140_hex_argon2_ha },
    {}
  };
static c3_c* _140_hex_argon_ha[] = {
  "dc704c786192ecd09d4c206a8f28db3202b6e0eb03e3ce63a95987510ac312d6",
  0
};

static c3_c* _140_hex_secp_secp256k1_make_ha[] = { 0 };
static u3j_harm _140_hex_secp_secp256k1_make_a[] = {{".2", u3we_make, c3y}, {}};
static c3_c* _140_hex_secp_secp256k1_sign_ha[] = {
  "3e75b3452b74776488d5eec75a91211700d9f360a4e06dd779600d5128d9c600",
  0
};
static u3j_harm _140_hex_secp_secp256k1_sign_a[] = {{".2", u3we_sign, c3y}, {}};
static c3_c* _140_hex_secp_secp256k1_reco_ha[] = {
  "449f3aa878b61962c3048e167c23ba54a0736d3aa1ab7762bd54016fbba136ee",
  0
};
static u3j_harm _140_hex_secp_secp256k1_reco_a[] = {{".2", u3we_reco, c3y}, {}};

static c3_c* _140_hex_secp_secp256k1_ha[] = {
  "e7fc0971a970aba7ded43bd89e9c82623eb2f346c9c720c63b22f2a646927861",
  0
};
static u3j_core _140_hex_secp_secp256k1_d[] =
  { { "make", 7, _140_hex_secp_secp256k1_make_a, 0, _140_hex_secp_secp256k1_make_ha },
    { "sign", 7, _140_hex_secp_secp256k1_sign_a, 0, _140_hex_secp_secp256k1_sign_ha },
    { "reco", 7, _140_hex_secp_secp256k1_reco_a, 0, _140_hex_secp_secp256k1_reco_ha },
    {}
  };

static c3_c* _140_hex_secp_ha[] = {
  "9f5c23f0e7923b6cf1603388ba52401b6e43881be3560b3acfaab20b25071792",
  0
};
static u3j_core _140_hex_secp_d[] =
  { { "secp256k1", 3, 0, _140_hex_secp_secp256k1_d, _140_hex_secp_secp256k1_ha },
    {}
  };

  static u3j_harm _140_hex_blake2b_a[] = {{".2", u3we_blake, c3y}, {}};
  static c3_c* _140_hex_blake2b_ha[] = {
    "c432216ca53b5ad2284259167952761bb1046e280268c4d3b9ca70a2024e1934",
    0
  };
static u3j_core _140_hex_blake_d[] =
  { { "blake2b", 7, _140_hex_blake2b_a, 0, _140_hex_blake2b_ha },
    {}
  };
static c3_c* _140_hex_blake_ha[] = {
  "ff30d99ffb3e13d8aa50b2b8461c8edfabf0e76de22312d16d1d6daaf3636b5f",
  0
};

  static u3j_harm _140_hex_ripemd_160_a[] = {{".2", u3we_ripe, c3y}, {}};
  static c3_c* _140_hex_ripemd_160_ha[] = {
    "176684b29926a01f5c60fa584e4691b0cbdc9b93608dcbe7d0cf3585683fa42f",
    0
  };
static u3j_core _140_hex_ripe_d[] =
  { { "ripemd160", 7, _140_hex_ripemd_160_a, 0, _140_hex_ripemd_160_ha },
    {}
  };
static c3_c* _140_hex_ripe_ha[] = {
  "b0cb16bf206c0496bb480e5759ea1afa7dee1748b64e5243c23fddb09720ebd0",
  0
};


static u3j_core _140_hex_d[] =
{ { "lore",   63, _140_hex_lore_a, 0, _140_hex_lore_ha },
  { "leer",   63, _140_hex_leer_a, 0, _140_hex_leer_ha },
  { "loss",   63, _140_hex_loss_a, 0, _140_hex_loss_ha },
  { "lune",  127, _140_hex_lune_a, 0, _140_hex_lune_ha },

  { "coed",   63, 0, _140_hex_coed_d, _140_hex_coed_ha },
  { "aes",    31, 0, _140_hex_aes_d, _140_hex_aes_ha },

  { "hmac",   63, 0, _140_hex_hmac_d,  _140_hex_hmac_ha  },
  { "argon",  31, 0, _140_hex_argon_d, _140_hex_argon_ha },
  { "blake",  31, 0, _140_hex_blake_d, _140_hex_blake_ha },
  { "ripemd", 31, 0, _140_hex_ripe_d,  _140_hex_ripe_ha  },
  { "secp",    6, 0, _140_hex_secp_d, _140_hex_secp_ha },
  { "mimes",  31, 0, _140_hex_mimes_d, _140_hex_mimes_ha  },
  {}
};
static c3_c* _140_hex_ha[] = {
  "7e393356dd7ac64eed5cd9f5cf0e320d401ca36a0a0ce0f954e7538824114844",
  0
};

/* layer five
*/
static u3j_harm _140_pen_cell_a[] = {{".2", u3wf_cell}, {}};
static c3_c* _140_pen_cell_ha[] = {
  "411649e69ff5c5d4a2976b300d213b99af3de724cec0e95f48404b808fc4f428",
  0
};
static u3j_harm _140_pen_comb_a[] = {{".2", u3wf_comb}, {}};
static c3_c* _140_pen_comb_ha[] = {
  "f9e37c3b3d5036c31af60f7047391594068638b54db7cf94bfea9dabbdffa547",
  0
};
static u3j_harm _140_pen_cons_a[] = {{".2", u3wf_cons}, {}};
static c3_c* _140_pen_cons_ha[] = {
  "b698cc6bc49ea0473e344c784075e99a433b4c5738f90fc58ab17c3eaa44b2e9",
  0
};
static u3j_harm _140_pen_core_a[] = {{".2", u3wf_core}, {}};
static c3_c* _140_pen_core_ha[] = {
  "1180e9371cf3465783ef192f9a7f580cd90533f5b52b77b456287f1d580a3535",
  0
};
static u3j_harm _140_pen_face_a[] = {{".2", u3wf_face}, {}};
static c3_c* _140_pen_face_ha[] = {
  "a184c44d57f5c94b84a3258b05bf891f3c96a4a4bbff3a8934d11cad0efa81d8",
  0
};
static u3j_harm _140_pen_fitz_a[] = {{".2", u3wf_fitz}, {}};
static c3_c* _140_pen_fitz_ha[] = {
  "469abe976ec15eeff9a87bce385f2c87c9bd89814ce2858aa9fee094beea1e5d",
  0
};
static u3j_harm _140_pen_flan_a[] = {{".2", u3wf_flan}, {}};
static c3_c* _140_pen_flan_ha[] = {
  "cc00cb9373b0274af4e17d7acd77f65d8a2fa886e422c949c12d9d9e7cb3525b",
  0
};
static u3j_harm _140_pen_flip_a[] = {{".2", u3wf_flip}, {}};
static c3_c* _140_pen_flip_ha[] = {
  "6e97fab9d039e715a30af5da93ef97389babfdcae7ef87655d278e77a1af0f0c",
  0
};
static u3j_harm _140_pen_flor_a[] = {{".2", u3wf_flor}, {}};
static c3_c* _140_pen_flor_ha[] = {
  "ab5360aacf0c9a325727e90e1caea9c42f5d94ccc248c9e1f253b0922b4c4e63",
  0
};
static u3j_harm _140_pen_fork_a[] = {{".2", u3wf_fork}, {}};
static c3_c* _140_pen_fork_ha[] = {
  "36f0ea0e2eb30328b8b83ed43a81c8c9a1f5b4c5a03fd68fd25701991a40b9dd",
  0
};

// hike disabled while implementing edit
// static u3j_harm _140_pen_hike_a[] = {{".2", u3wf_hike}, {}};
// static c3_c* _140_pen_hike_ha[] = {0};

static u3j_harm _140_pen_look_a[] = {{".2", u3wf_look}, {}};
static c3_c* _140_pen_look_ha[] = {
  "fdda2166a2b9e1a9bda6ab375dd6fb6c610e18f54636a5e89896b45fd0a7169b",
  0
};
static u3j_harm _140_pen_loot_a[] = {{".2", u3wf_loot}, {}};
static c3_c* _140_pen_loot_ha[] = {
  "e275da4562ae6da9bd333aeae6b9829e886874c8b891898c0ef5306268eb45c1",
  0
};

  static u3j_harm _140_pen__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
  static c3_c* _140_pen__ut_crop_ha[] = {
    "e2c6fc3e714a3a98ccd28423dcb9f2c6480935e26b54dd0581eb2ad7e5b16d6f",
    0
  };
  static u3j_harm _140_pen__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
  static c3_c* _140_pen__ut_fish_ha[] = {
    "080caee60b5ee4616bf9568bdbceabbf044379c47466e0ae3968cb0146049a84",
    0
  };
  static u3j_harm _140_pen__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
  static c3_c* _140_pen__ut_fuse_ha[] = {
    "519aac7b40b7018d5df00ddf3977c2ebe0c2e05bcee34796d56a1d54c15e0c84",
    0
  };
  static u3j_harm _140_pen__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
  static c3_c* _140_pen__ut_mint_ha[] = {
    "7d980f7425b51bb10fbbd8b465b5d83f5dd4cb6e66d88758a9f7490b812a765e",
    0
  };
  static u3j_harm _140_pen__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
  static c3_c* _140_pen__ut_mull_ha[] = {
    "c806329aefd920501ea0faa0cfb0ce3280a74408782efe6d82878ec43ec44fb7",
    0
  };

    static u3j_harm _140_pen__ut_nest_dext_a[] = {{".2", u3wfu_nest_dext}, {}};
    static c3_c* _140_pen__ut_nest_dext_ha[] = {
      "72f33df96800034fc63531293f9b110e6505027195bf8a10ff94b9a1f1ef719b",
      0
    };
    static u3j_core _140_pen__ut_nest_in_d[] =
      {
        { "nest-dext", 3, _140_pen__ut_nest_dext_a, 0, _140_pen__ut_nest_dext_ha },
        {}
      };
    static c3_c* _140_pen__ut_nest_in_ha[] = {
      "68378dfa1d1fee0b1cd9593fb561234cec2ae9371a5ffa287c3d2ab9620e198c",
      0
    };

  static u3j_core _140_pen__ut_nest_d[] =
    {
      { "nest-in", 7, 0, _140_pen__ut_nest_in_d, _140_pen__ut_nest_in_ha },
      {}
    };
  static c3_c* _140_pen__ut_nest_ha[] = {
    "1e8de5d1225facc1158c92c2ea5e0dc84129cbb317fde3691e224b8c2550d950",
    0
  };

  static u3j_harm _140_pen__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
  static c3_c* _140_pen__ut_rest_ha[] = {
    "b4a83073f4cb03898ef099fab5722a046122dc96a5332ffc82f988df6c186e74",
    0
  };

static u3j_core _140_pen__ut_d[] =
  {
    { "crop", 7, _140_pen__ut_crop_a, 0, _140_pen__ut_crop_ha },
    { "fish", 7, _140_pen__ut_fish_a, 0, _140_pen__ut_fish_ha },
    { "fuse", 7, _140_pen__ut_fuse_a, 0, _140_pen__ut_fuse_ha },
    { "mint", 7, _140_pen__ut_mint_a, 0, _140_pen__ut_mint_ha },
    { "mull", 7, _140_pen__ut_mull_a, 0, _140_pen__ut_mull_ha },
    { "nest", 7, 0, _140_pen__ut_nest_d, _140_pen__ut_nest_ha },
    { "rest", 7, _140_pen__ut_rest_a, 0, _140_pen__ut_rest_ha },
    {}
  };

static c3_c* _140_pen__ut_ha[] = {
  "50c79204c82a3ba8f01e085a2e27e7716e5c7ab1929f94423ef1da92cf5ac631",
  0
};

static u3j_hood _140_pen__ut_ho[] = {
  { "ar",     12282 },
  { "fan",  28, c3n },
  { "rib",  58, c3n },
  { "vet",  59, c3n },

  { "blow",    6015 },
  { "burp",     342 },
  { "busk",    1373 },
  { "buss",     374 },
  { "crop",    1494 },
  { "duck",    1524 },
  { "dune",    2991 },
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

//  XX unused, hook removed, delete source
//
#if 0
static u3j_harm _140_pen__ap_a[] =
  { {"open", u3wfp_open},
    {"rake", u3wfp_rake},
    {}
  };
static c3_c* _140_pen__ap_ha[] = {0};
#endif

static u3j_core _140_pen_d[] =
{ { "hex", 7, 0, _140_hex_d, _140_hex_ha },

  { "cell", 7, _140_pen_cell_a, 0, _140_pen_cell_ha },
  { "comb", 7, _140_pen_comb_a, 0, _140_pen_comb_ha },
  { "cons", 7, _140_pen_cons_a, 0, _140_pen_cons_ha },
  { "core", 7, _140_pen_core_a, 0, _140_pen_core_ha },
  { "face", 7, _140_pen_face_a, 0, _140_pen_face_ha },
  { "fitz", 7, _140_pen_fitz_a, 0, _140_pen_fitz_ha },
  { "flan", 7, _140_pen_flan_a, 0, _140_pen_flan_ha },
  { "flip", 7, _140_pen_flip_a, 0, _140_pen_flip_ha },
  { "flor", 7, _140_pen_flor_a, 0, _140_pen_flor_ha },
  { "fork", 7, _140_pen_fork_a, 0, _140_pen_fork_ha },
  //  XX implementation obsolete, update or remove
  //
  // { "hike", 7, _140_pen_hike_a, 0, _140_pen_hike_ha },
  { "look", 7, _140_pen_look_a, 0, _140_pen_look_ha },
  { "loot", 7, _140_pen_loot_a, 0, _140_pen_loot_ha },

  //  XX unused, hook removed, delete source
  //
  // { "ap", 7, _140_pen__ap_a, 0, _140_pen__ap_ha },
  { "ut", 15, 0, _140_pen__ut_d, _140_pen__ut_ha, _140_pen__ut_ho },
  {}
};
static c3_c* _140_pen_ha[] = {
  "e6c9e2362bdf2d1f9a2837a0efa154c0b8b9d51aea03a86b5aece573ff423cb1",
  0
};

static u3j_hood _140_pen_ho[] = {
  { "ap", 22 },
  { "ut", 86 },
  {},
};

/* layer four
*/
static u3j_harm _140_qua_trip_a[] = {{".2", u3we_trip}, {}};
static c3_c* _140_qua_trip_ha[] = {
  "05423b940d10d03891cc23f36eea14b233e5884ef539de3d985d6818dd427b05",
  0
};

static u3j_harm _140_qua_slaw_a[] = {{".2", u3we_slaw}, {}};
static c3_c* _140_qua_slaw_ha[] = {
  "306c9692f48e2700675ed6581e9df4feaee951e1bed3cad7f89aab392e80000f",
  0
};
static u3j_harm _140_qua_scot_a[] = {{".2", u3we_scot}, {}};
static c3_c* _140_qua_scot_ha[] = {
  0
};
static u3j_harm _140_qua_scow_a[] = {{".2", u3we_scow}, {}};
static c3_c* _140_qua_scow_ha[] = {
  0
};

static u3j_harm _140_qua__po_ind_a[] = {{".2", u3wcp_ind}, {}};
static c3_c* _140_qua__po_ind_ha[] = {
  "95bbe9867dbbd1b9ce12671d64cf7b1dee8d987c6770955a83c73291c4537a61",
  0
};
static u3j_harm _140_qua__po_ins_a[] = {{".2", u3wcp_ins}, {}};
static c3_c* _140_qua__po_ins_ha[] = {
  "aae783fb258dff7f8ade49756e01f96a2d2100411a88a886732270dcf9f174f0",
  0
};
static u3j_harm _140_qua__po_tod_a[] = {{".2", u3wcp_tod}, {}};
static c3_c* _140_qua__po_tod_ha[] = {
  "153aeba45ca2a87aa918e9cea1b26e8104a6e4395979257b075546c1e2654a17",
  0
};
static u3j_harm _140_qua__po_tos_a[] = {{".2", u3wcp_tos}, {}};
static c3_c* _140_qua__po_tos_ha[] = {
  "7c5ffad03bcf8b4ea9bdf0c7f7500351923bc0431f3d62d6ce0472790f668fb4",
  0
};
  static u3j_core _140_qua__po_d[] =
    { { "ind", 7, _140_qua__po_ind_a, 0, _140_qua__po_ind_ha },
      { "ins", 7, _140_qua__po_ins_a, 0, _140_qua__po_ins_ha },
      { "tod", 7, _140_qua__po_tod_a, 0, _140_qua__po_tod_ha },
      { "tos", 7, _140_qua__po_tos_a, 0, _140_qua__po_tos_ha },
      {}
    };
  static c3_c* _140_qua__po_ha[] = {
    "efc5fa7c0efedd490e9a270bb5cf9f90809e6b224f8a381a6b8a481253b237a1",
    0
  };

static u3j_harm _140_qua__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
static c3_c* _140_qua__bend_fun_ha[] = {
  "e6ea05e3d765a005fccde9eb88fb93e06f0b6ea198afa8ed599b056ba179396a",
  0
};
  static u3j_core _140_qua__bend_d[] =
    { { "fun", 7, _140_qua__bend_fun_a, 0, _140_qua__bend_fun_ha },
      {}
    };
  static c3_c* _140_qua__bend_ha[] = {
    "adc59c6db6d5b26122bc6c04e25c3efe830c9eef68ecf81c492a59ee5e9e20a2",
    0
  };

static u3j_harm _140_qua__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
static c3_c* _140_qua__cold_fun_ha[] = {
  "ad5a0ab7405be9ffda0a9dd34580c6039f6bbb0301920fb2df0c31be3c72c58e",
  0
};
  static u3j_core _140_qua__cold_d[] =
    { { "fun", 7, _140_qua__cold_fun_a, 0, _140_qua__cold_fun_ha },
      {}
    };
  static c3_c* _140_qua__cold_ha[] = {
    "06c49d8dab6cb057e3ae1d1164e96950944ea48e54ad72347239ea434f1d4b9c",
    0
  };

static u3j_harm _140_qua__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
static c3_c* _140_qua__cook_fun_ha[] = {
  "f2a45612ad9c279b723334ab0915d3ed3ece7727309968c2555f45e0668eeb27",
  0
};
  static u3j_core _140_qua__cook_d[] =
    { { "fun", 7, _140_qua__cook_fun_a, 0, _140_qua__cook_fun_ha },
      {}
    };
  static c3_c* _140_qua__cook_ha[] = {
    "3ccd46dd21828d7be11f8c093536e305b1df982393a69c40ea73f63a574b3bb1",
    0
  };

static u3j_harm _140_qua__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
static c3_c* _140_qua__comp_fun_ha[] = {
  "bd7fdba84b05b00a63c24d19a03b882578ee9a3b922a3a688f7827c6e64daf96",
  0
};
  static u3j_core _140_qua__comp_d[] =
    { { "fun", 7, _140_qua__comp_fun_a, 0, _140_qua__comp_fun_ha },
      {}
    };
  static c3_c* _140_qua__comp_ha[] = {
    "7baad25ba87bcbba8ce4fe328280a799765dcf62a8bb761ffd87b939dd8734f2",
    0
  };

static u3j_harm _140_qua__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
static c3_c* _140_qua__easy_fun_ha[] = {
  "4bbbc43ece463d961e572301d0824d3e3cab3ba09ec2756cbefae63ee106044b",
  0
};
  static u3j_core _140_qua__easy_d[] =
    { { "fun", 7, _140_qua__easy_fun_a, 0, _140_qua__easy_fun_ha },
      {}
    };
  static c3_c* _140_qua__easy_ha[] = {
    "fdcf833943e24323deb6b071497498933ce6c4ba7d4742f2752b6ddb7fb9634e",
    0
  };

static u3j_harm _140_qua__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
static c3_c* _140_qua__glue_fun_ha[] = {
  "ffe0fe8815a2298c51a58e963efbbb7af90830abf11ce50bf9a47f479ce452fb",
  0
};
  static u3j_core _140_qua__glue_d[] =
    { { "fun", 7, _140_qua__glue_fun_a, 0, _140_qua__glue_fun_ha },
      {}
    };
  static c3_c* _140_qua__glue_ha[] = {
    "079c8a395428c2921b266a84bcf271fbe62f3d873b26680661e13a78df1a3989",
    0
  };

static u3j_harm _140_qua__here_fun_a[] = {{".2", u3we_here_fun}, {}};
static c3_c* _140_qua__here_fun_ha[] = {
  "47ec445fcfa89d266dae3c3590ed041d1b05f92d1bce360f232da5d496e4f2eb",
  0
};
  static u3j_core _140_qua__here_d[] =
    { { "fun", 7, _140_qua__here_fun_a, 0, _140_qua__here_fun_ha },
      {}
    };
  static c3_c* _140_qua__here_ha[] = {
    "4600093be5becba9a65a14b67624e3d9e4c66e0c97ba57b2bc271907eea32ffe",
    0
  };

static u3j_harm _140_qua__just_fun_a[] = {{".2", u3we_just_fun}, {}};
static c3_c* _140_qua__just_fun_ha[] = {
  "38bf1fb843bc29837868f2828f32d7e2bbb419b0cb9a1236adea28dfc6ce1040",
  0
};
  static u3j_core _140_qua__just_d[] =
    { { "fun", 7, _140_qua__just_fun_a, 0, _140_qua__just_fun_ha },
      {}
    };
  static c3_c* _140_qua__just_ha[] = {
    "7d6b2165e52dec478d96cf72478a35b7a92b014e6a15f046f026c0c8cb07679b",
    0
  };

static u3j_harm _140_qua__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
static c3_c* _140_qua__mask_fun_ha[] = {
  "892dfcd5f3d90981fa6e7608e93f0517000d316e7d9c07b3bd390c4966c97f5f",
  0
};
  static u3j_core _140_qua__mask_d[] =
    { { "fun", 7, _140_qua__mask_fun_a, 0, _140_qua__mask_fun_ha },
      {}
    };
  static c3_c* _140_qua__mask_ha[] = {
    "48e11fc12d7c453cda6ca42577d68e968446aa4d0ad3b99cc674affc7f4507b4",
    0
  };

static u3j_harm _140_qua__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
static c3_c* _140_qua__shim_fun_ha[] = {
  "4e26a0e98adb13ee6718fd68d90910c630df9bb7023b3e3ef40cda6710075fc9",
  0
};
  static u3j_core _140_qua__shim_d[] =
    { { "fun", 7, _140_qua__shim_fun_a, 0, _140_qua__shim_fun_ha },
      {}
    };
  static c3_c* _140_qua__shim_ha[] = {
    "226b96d1a59daada23a1ea80227c2dbf32ddd748d4c6363f316147ab7f292ced",
    0
  };

static u3j_harm _140_qua__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
static c3_c* _140_qua__stag_fun_ha[] = {
  "f76c7205c23e77809af793bc506f4727071fd029d234317fe78a7f65e2b7d6ea",
  0
};
  static u3j_core _140_qua__stag_d[] =
    { { "fun", 7, _140_qua__stag_fun_a, 0, _140_qua__stag_fun_ha },
      {}
    };
  static c3_c* _140_qua__stag_ha[] = {
    "fc978af18fb13dc0d77501b6f3eeb538b079b4345eb9195c3dd44c516e69424a",
    0
  };

static u3j_harm _140_qua__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
static c3_c* _140_qua__stew_fun_ha[] = {
  "a700f6bdfdb83ba33b2a3fe92fda3cb1bbfe95e595401538c8371b55fcc61447",
  0
};
  static u3j_core _140_qua__stew_d[] =
    { { "fun", 31, _140_qua__stew_fun_a, 0, _140_qua__stew_fun_ha },
      {}
    };
  static c3_c* _140_qua__stew_ha[] = {
    "29303fd6ab78cbbccdfc5bcf23d0bff126a0ef2bf4fa11ce70fcf4e6aa5fe60b",
    0
  };

static u3j_harm _140_qua__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
static c3_c* _140_qua__stir_fun_ha[] = {
  "6251308ea3c741e76ef9cb2dc5a71c9d8706d6cce6fdb420fef12915e0c032d6",
  0
};
  static u3j_core _140_qua__stir_d[] =
    { { "fun", 7, _140_qua__stir_fun_a, 0, _140_qua__stir_fun_ha },
      {}
    };
  static c3_c* _140_qua__stir_ha[] = {
    "4e466aef4d91f0ced008c00e8a4330afb3a43ef81dc1a6d93f1853685f69b9ca",
    0
  };

static u3j_harm _140_qua_pfix_a[] = {{".2", u3we_pfix}, {}};
static c3_c* _140_qua_pfix_ha[] = {
  "f7019bccc8b3b04a969878ffed84c9eba4dfa60ee32f984119cacb0c2381656b",
  0
};

static u3j_harm _140_qua_plug_a[] = {{".2", u3we_plug}, {}};
static c3_c* _140_qua_plug_ha[] = {
  "5f5a9824e0952fd565748cc0a20f96cf883a41e2f5707c8a7797e6edd617b79c",
  0
};
static u3j_harm _140_qua_pose_a[] = {{".2", u3we_pose}, {}};
static c3_c* _140_qua_pose_ha[] = {
  "5c77203f288ef0f7bcd87871c69673db7fc804b647ecc42992707dc32f0f4611",
  0
};

static u3j_harm _140_qua_sfix_a[] = {{".2", u3we_sfix}, {}};
static c3_c* _140_qua_sfix_ha[] = {
  "00987ed37104b902c5264d4d013826d762bfa80a6b29cfe4b7fa61b1ddd9cfac",
  0
};

static u3j_harm _140_qua_mink_a[] = {{".2", u3we_mink}, {}};
static c3_c* _140_qua_mink_ha[] = {
  "99b653da6a21fa3375424811af288f59164592ece4a072abc460df03e81abcaf",
  0
};
static u3j_harm _140_qua_mole_a[] = {{".2", u3we_mole}, {}};
static c3_c* _140_qua_mole_ha[] = {
  "029c1acaff1911c54ce31a3693397394604ea970bf076078c1a1cfa23d2fa74e",
  0
};
static u3j_harm _140_qua_mule_a[] = {{".2", u3we_mule}, {}};
static c3_c* _140_qua_mule_ha[] = {
  "d54688d726565ddade7f2636741cad7209ea40fab28d3335555d8a02ff6001c4",
  0
};

static u3j_core _140_qua_d[] =
{ { "pen", 3, 0, _140_pen_d, _140_pen_ha, _140_pen_ho },

  { "po", 7, 0, _140_qua__po_d, _140_qua__po_ha },

  { "trip", 7, _140_qua_trip_a, 0, _140_qua_trip_ha },

  { "bend", 7, 0, _140_qua__bend_d, _140_qua__bend_ha },
  { "cold", 7, 0, _140_qua__cold_d, _140_qua__cold_ha },
  { "comp", 7, 0, _140_qua__comp_d, _140_qua__comp_ha },
  { "cook", 7, 0, _140_qua__cook_d, _140_qua__cook_ha },
  { "easy", 7, 0, _140_qua__easy_d, _140_qua__easy_ha },
  { "glue", 7, 0, _140_qua__glue_d, _140_qua__glue_ha },
  { "here", 7, 0, _140_qua__here_d, _140_qua__here_ha },
  { "just", 7, 0, _140_qua__just_d, _140_qua__just_ha },
  { "mask", 7, 0, _140_qua__mask_d, _140_qua__mask_ha },
  { "shim", 7, 0, _140_qua__shim_d, _140_qua__shim_ha },
  { "stag", 7, 0, _140_qua__stag_d, _140_qua__stag_ha },
  { "stew", 7, 0, _140_qua__stew_d, _140_qua__stew_ha },
  { "stir", 7, 0, _140_qua__stir_d, _140_qua__stir_ha },

  { "pfix", 7, _140_qua_pfix_a, 0, _140_qua_pfix_ha },
  { "plug", 7, _140_qua_plug_a, 0, _140_qua_plug_ha },
  { "pose", 7, _140_qua_pose_a, 0, _140_qua_pose_ha },
  { "sfix", 7, _140_qua_sfix_a, 0, _140_qua_sfix_ha },

  { "mink", 7, _140_qua_mink_a, 0, _140_qua_mink_ha },
  { "mole", 7, _140_qua_mole_a, 0, _140_qua_mole_ha },
  { "mule", 7, _140_qua_mule_a, 0, _140_qua_mule_ha },

  //  XX disabled, implicated in memory corruption
  //  write tests and re-enable
  //
  // { "scot", 7, _140_qua_scot_a, 0, _140_qua_scot_ha },
  // { "scow", 7, _140_qua_scow_a, 0, _140_qua_scow_ha },
  { "slaw", 7, _140_qua_slaw_a, 0, _140_qua_slaw_ha },
  {}
};
static c3_c* _140_qua_ha[] = {
  "db9b4b21c0a8a8324105cbccc1421ef2a715ef0562c280b943fe1d96651cd9cc",
  0
};

static u3j_hood _140_qua_ho[] = {
  { "mute", 0x2fbabe },
  { "show",    24406 },
  { "mure",     1404 },
  {},
};

/* layer three
*/
  static u3j_harm _140_tri__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
  static c3_c* _140_tri__cofl__drg_ha[] = {
    "6063adb8cac639f7b20d5e7700c8108266be04f99cce4434f906240b424bf36d",
    0
  };
  static u3j_harm _140_tri__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
  static c3_c* _140_tri__cofl__lug_ha[] = {
    "f146a84731447e5c4b1e7b6e9331b33d1babed09bb0618e134c9535062154a87",
    0
  };
static u3j_core _140_tri__cofl_d[] =
  { { "drg", 7, _140_tri__cofl__drg_a, 0, _140_tri__cofl__drg_ha },
    { "lug", 7, _140_tri__cofl__lug_a, 0, _140_tri__cofl__lug_ha },
    {}
  };
static c3_c* _140_tri__cofl_ha[] = {
  "f320c5bf51db85f55b900f4160f7e0ab9ef267f43ddb698900de034c6e2600d5",
  0
};

  static u3j_harm _140_tri__rd_add_a[] = {{".2", u3wer_add}, {}};
  static c3_c* _140_tri__rd_add_ha[] = {
    "90dfaaadb2878d6d89a808ce4199e5bb239fa981e1c2edf24dc54aa3fcab55a5",
    0
  };
  static u3j_harm _140_tri__rd_sub_a[] = {{".2", u3wer_sub}, {}};
  static c3_c* _140_tri__rd_sub_ha[] = {
    "5898a2424ba815d66d83917953f01860e63207f4200a447f632d9a5cc77a8a9c",
    0
  };
  static u3j_harm _140_tri__rd_mul_a[] = {{".2", u3wer_mul}, {}};
  static c3_c* _140_tri__rd_mul_ha[] = {
    "a3af44ef4cd89afe78f1088bddb7d56dfa7fc209153256557c98ff34b67976bc",
    0
  };
  static u3j_harm _140_tri__rd_div_a[] = {{".2", u3wer_div}, {}};
  static c3_c* _140_tri__rd_div_ha[] = {
    "9be3b38b9b4b0b0cd0bb060529c8f439cb0589aa9c3528efc519fbcc6845e98d",
    0
  };
  static u3j_harm _140_tri__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
  static c3_c* _140_tri__rd_sqt_ha[] = {
    "0413678ac8ec89c3425ba762cd24b2a8a455543ec9b5dd719524e6b834e0c99c",
    0
  };
  static u3j_harm _140_tri__rd_fma_a[] = {{".2", u3wer_fma}, {}};
  static c3_c* _140_tri__rd_fma_ha[] = {
    "7c920238dd42fb645c057cc950ed7ece775d5f502a0faf6ef5d17d348e0fc3e3",
    0
  };
  static u3j_harm _140_tri__rd_lth_a[] = {{".2", u3wer_lth}, {}};
  static c3_c* _140_tri__rd_lth_ha[] = {
    "a108f1ac15c1d4e2c86457c9afc97a97a5954003c709c3c19c722b37255bcba9",
    0
  };
  static u3j_harm _140_tri__rd_lte_a[] = {{".2", u3wer_lte}, {}};
  static c3_c* _140_tri__rd_lte_ha[] = {
    "ef9a6b2c5cdd0d4de550a32679d242693131cdc3cf40ac914eadfa7d6d9f1bac",
    0
  };
  static u3j_harm _140_tri__rd_equ_a[] = {{".2", u3wer_equ}, {}};
  static c3_c* _140_tri__rd_equ_ha[] = {
    "c93cdb951dca3b0ac61070780f95e1baa3718fe519d0268305c032cf14a21e39",
    0
  };
  static u3j_harm _140_tri__rd_gte_a[] = {{".2", u3wer_gte}, {}};
  static c3_c* _140_tri__rd_gte_ha[] = {
    "6857077d97e2fc203b555dc20748c33d34b694d33c48f628543bea6491e722a6",
    0
  };
  static u3j_harm _140_tri__rd_gth_a[] = {{".2", u3wer_gth}, {}};
  static c3_c* _140_tri__rd_gth_ha[] = {
    "87fd815913fa590c715d3a96ada5bae6298f3e7823af90ffcdf235f493c56330",
    0
  };
static u3j_core _140_tri__rd_d[] =
  { { "add", 7, _140_tri__rd_add_a, 0, _140_tri__rd_add_ha },
    { "sub", 7, _140_tri__rd_sub_a, 0, _140_tri__rd_sub_ha },
    { "mul", 7, _140_tri__rd_mul_a, 0, _140_tri__rd_mul_ha },
    { "div", 7, _140_tri__rd_div_a, 0, _140_tri__rd_div_ha },
    { "sqt", 7, _140_tri__rd_sqt_a, 0, _140_tri__rd_sqt_ha },
    { "fma", 7, _140_tri__rd_fma_a, 0, _140_tri__rd_fma_ha },
    { "lth", 7, _140_tri__rd_lth_a, 0, _140_tri__rd_lth_ha },
    { "lte", 7, _140_tri__rd_lte_a, 0, _140_tri__rd_lte_ha },
    { "equ", 7, _140_tri__rd_equ_a, 0, _140_tri__rd_equ_ha },
    { "gte", 7, _140_tri__rd_gte_a, 0, _140_tri__rd_gte_ha },
    { "gth", 7, _140_tri__rd_gth_a, 0, _140_tri__rd_gth_ha },
    {}
  };
static c3_c* _140_tri__rd_ha[] = {
  "0afab285837f9c88faff3ac8f3f49b5e259da253e0505cd823a53d4e826261f7",
  0
};

  static u3j_harm _140_tri__rs_add_a[] = {{".2", u3wet_add}, {}};
  static c3_c* _140_tri__rs_add_ha[] = {
    "b89a1e348628fd9b4fd520aabbf6c53e12be5bbf661b9094f7a9841be3f51de9",
    0
  };
  static u3j_harm _140_tri__rs_sub_a[] = {{".2", u3wet_sub}, {}};
  static c3_c* _140_tri__rs_sub_ha[] = {
    "9d13b86d17908830f93b920e80e1a985105583597f6fb640f175c98052e357f1",
    0
  };
  static u3j_harm _140_tri__rs_mul_a[] = {{".2", u3wet_mul}, {}};
  static c3_c* _140_tri__rs_mul_ha[] = {
    "5c61a9e335f6c139332523d871ca5773ab476365b623444370474fde08639061",
    0
  };
  static u3j_harm _140_tri__rs_div_a[] = {{".2", u3wet_div}, {}};
  static c3_c* _140_tri__rs_div_ha[] = {
    "a7147a81be3ef5a1c0de6587edb2990e30057d7871d73632eabc8dc567e751c8",
    0
  };
  static u3j_harm _140_tri__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
  static c3_c* _140_tri__rs_sqt_ha[] = {
    "c403bb48d78bcfa9cd4128b7f9eb362e71e5269ab6578fb0bb5e70b4244c5781",
    0
  };
  static u3j_harm _140_tri__rs_fma_a[] = {{".2", u3wet_fma}, {}};
  static c3_c* _140_tri__rs_fma_ha[] = {
    "afc14914eb8f579a0c6620db69bc951329325747a8ef01c2d0e5feb29e9a9ddd",
    0
  };
  static u3j_harm _140_tri__rs_lth_a[] = {{".2", u3wet_lth}, {}};
  static c3_c* _140_tri__rs_lth_ha[] = {
    "e7efa422def6b6dbd25b5f664462adb3895b8b45f9531877c9dbfe40a96612bf",
    0
  };
  static u3j_harm _140_tri__rs_lte_a[] = {{".2", u3wet_lte}, {}};
  static c3_c* _140_tri__rs_lte_ha[] = {
    "b45e1ef16c47dc9b99865c7e75d4c2094c0e206ed538818a38da0adb7fbe2ce3",
    0
  };
  static u3j_harm _140_tri__rs_equ_a[] = {{".2", u3wet_equ}, {}};
  static c3_c* _140_tri__rs_equ_ha[] = {
    "8c1178311ded837292c297380e48cf7e0bc4d83962dadcafda0c9ef9f20e39f2",
    0
  };
  static u3j_harm _140_tri__rs_gte_a[] = {{".2", u3wet_gte}, {}};
  static c3_c* _140_tri__rs_gte_ha[] = {
    "50f5eb237b74e772eb6a3257441078461450cd4a25cf9bd97cb1a5e00f4ff4d2",
    0
  };
  static u3j_harm _140_tri__rs_gth_a[] = {{".2", u3wet_gth}, {}};
  static c3_c* _140_tri__rs_gth_ha[] = {
    "505842ff486f0bb8fa63f04d2fd6f806dc760f9b4a12a3bf2d57da92f560785b",
    0
  };
static u3j_core _140_tri__rs_d[] =
  { { "add", 7, _140_tri__rs_add_a, 0, _140_tri__rs_add_ha },
    { "sub", 7, _140_tri__rs_sub_a, 0, _140_tri__rs_sub_ha },
    { "mul", 7, _140_tri__rs_mul_a, 0, _140_tri__rs_mul_ha },
    { "div", 7, _140_tri__rs_div_a, 0, _140_tri__rs_div_ha },
    { "sqt", 7, _140_tri__rs_sqt_a, 0, _140_tri__rs_sqt_ha },
    { "fma", 7, _140_tri__rs_fma_a, 0, _140_tri__rs_fma_ha },
    { "lth", 7, _140_tri__rs_lth_a, 0, _140_tri__rs_lth_ha },
    { "lte", 7, _140_tri__rs_lte_a, 0, _140_tri__rs_lte_ha },
    { "equ", 7, _140_tri__rs_equ_a, 0, _140_tri__rs_equ_ha },
    { "gte", 7, _140_tri__rs_gte_a, 0, _140_tri__rs_gte_ha },
    { "gth", 7, _140_tri__rs_gth_a, 0, _140_tri__rs_gth_ha },
    {}
  };
  static c3_c* _140_tri__rs_ha[] = {
    "6c7027c5de34540a4b1548039e5423fb44727079af054b7135a3e961ec8dede8",
    0
  };

  static u3j_harm _140_tri__rq_add_a[] = {{".2", u3weq_add}, {}};
  static c3_c* _140_tri__rq_add_ha[] = {
    "9c4c2a37550930605495401886d41fb9fbc2eba487e0ba845130fe88e4c52a01",
    0
  };
  static u3j_harm _140_tri__rq_sub_a[] = {{".2", u3weq_sub}, {}};
  static c3_c* _140_tri__rq_sub_ha[] = {
    "f3b027090a1bb5af74234301facfbf64503b3e0599501ade12cb05aa158a79a3",
    0
  };
  static u3j_harm _140_tri__rq_mul_a[] = {{".2", u3weq_mul}, {}};
  static c3_c* _140_tri__rq_mul_ha[] = {
    "15c03b1f3081514c4767d86297aaebf1d62af7f3437f30821f011b17ed769f3a",
    0
  };
  static u3j_harm _140_tri__rq_div_a[] = {{".2", u3weq_div}, {}};
  static c3_c* _140_tri__rq_div_ha[] = {
    "094105c77e37e548ea1b8d49a132ab97d90a0ab5f329340400c381bcd44de347",
    0
  };
  static u3j_harm _140_tri__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
  static c3_c* _140_tri__rq_sqt_ha[] = {
    "a031cd5b8e05f997323b0ca1ca9d2419401d7d2acc2da6fc6387fe57701b84b0",
    0
  };
  static u3j_harm _140_tri__rq_fma_a[] = {{".2", u3weq_fma}, {}};
  static c3_c* _140_tri__rq_fma_ha[] = {
    "871664a9305808a671aacf1de0e83f8e951611033170d86370352afb363b79bc",
    0
  };
  static u3j_harm _140_tri__rq_lth_a[] = {{".2", u3weq_lth}, {}};
  static c3_c* _140_tri__rq_lth_ha[] = {
    "b10822caa442c8e9f6b9eb52aac5796a42bed5ae0eef7fe49d4075b38bf78ddd",
    0
  };
  static u3j_harm _140_tri__rq_lte_a[] = {{".2", u3weq_lte}, {}};
  static c3_c* _140_tri__rq_lte_ha[] = {
    "3316346be3e9464fbb5b0feff16cd2252086004229d08f8eaa320f9509d6a029",
    0
  };
  static u3j_harm _140_tri__rq_equ_a[] = {{".2", u3weq_equ}, {}};
  static c3_c* _140_tri__rq_equ_ha[] = {
    "154be82a7b8ecf4571015d9ef6c0e90ffd8c3e8803441d9c2df5a4ea484ccf3b",
    0
  };
  static u3j_harm _140_tri__rq_gte_a[] = {{".2", u3weq_gte}, {}};
  static c3_c* _140_tri__rq_gte_ha[] = {
    "edbe94f9cfefa89deef7f6c11f4ce8240fd93970dbd6130e632d0447452a612a",
    0
  };
  static u3j_harm _140_tri__rq_gth_a[] = {{".2", u3weq_gth}, {}};
  static c3_c* _140_tri__rq_gth_ha[] = {
    "50e85936cfad61659ed1bfdab26fda2b2696571b9aa4b4c55dc5c0d919edc296",
    0
  };
static u3j_core _140_tri__rq_d[] =
  { { "add", 7, _140_tri__rq_add_a, 0, _140_tri__rq_add_ha },
    { "sub", 7, _140_tri__rq_sub_a, 0, _140_tri__rq_sub_ha },
    { "mul", 7, _140_tri__rq_mul_a, 0, _140_tri__rq_mul_ha },
    { "div", 7, _140_tri__rq_div_a, 0, _140_tri__rq_div_ha },
    { "sqt", 7, _140_tri__rq_sqt_a, 0, _140_tri__rq_sqt_ha },
    { "fma", 7, _140_tri__rq_fma_a, 0, _140_tri__rq_fma_ha },
    { "lth", 7, _140_tri__rq_lth_a, 0, _140_tri__rq_lth_ha },
    { "lte", 7, _140_tri__rq_lte_a, 0, _140_tri__rq_lte_ha },
    { "equ", 7, _140_tri__rq_equ_a, 0, _140_tri__rq_equ_ha },
    { "gte", 7, _140_tri__rq_gte_a, 0, _140_tri__rq_gte_ha },
    { "gth", 7, _140_tri__rq_gth_a, 0, _140_tri__rq_gth_ha },
    {}
  };
static c3_c* _140_tri__rq_ha[] = {
  "b772cd5901a18dc91852fada74c2a1e4b1bbf4e7465451b0d8bdcc86d2288c22",
  0
};

  static u3j_harm _140_tri__rh_add_a[] = {{".2", u3wes_add}, {}};
  static c3_c* _140_tri__rh_add_ha[] = {
    "7e2b600eced08d774800a6a3d82e18189db85010b870c26905ee38008d3d301e",
    0
  };
  static u3j_harm _140_tri__rh_sub_a[] = {{".2", u3wes_sub}, {}};
  static c3_c* _140_tri__rh_sub_ha[] = {
    "827dfb41660cb4743a88d921b4185bd2000ee6f0708ec36ac8aba2e4e19c0875",
    0
  };
  static u3j_harm _140_tri__rh_mul_a[] = {{".2", u3wes_mul}, {}};
  static c3_c* _140_tri__rh_mul_ha[] = {
    "ec2a010128aca0a6f74196f3de4fe7b6617fd810d3b19f7bf5878afb787e8b86",
    0
  };
  static u3j_harm _140_tri__rh_div_a[] = {{".2", u3wes_div}, {}};
  static c3_c* _140_tri__rh_div_ha[] = {
    "067a3dafb0158bc2441729beb2c0934c6c6ccf0a9b5193db9c16f22b490b27c6",
    0
  };
  static u3j_harm _140_tri__rh_sqt_a[] = {{".2", u3wes_sqt}, {}};
  static c3_c* _140_tri__rh_sqt_ha[] = {
    "3fdbba47626d91d41fcdf460ad38018c78b0233d7ec4d0fac406a8c5357a2384",
    0
  };
  static u3j_harm _140_tri__rh_fma_a[] = {{".2", u3wes_fma}, {}};
  static c3_c* _140_tri__rh_fma_ha[] = {
    "ada2adf5a88ba61759219926aef950e72ae6926c6e10c30ecd0f9c99e79beca0",
    0
  };
  static u3j_harm _140_tri__rh_lth_a[] = {{".2", u3wes_lth}, {}};
  static c3_c* _140_tri__rh_lth_ha[] = {
    "70c9bc0073d23371d8155c28795f5acbff1a9504a5a3881c8693aa7c1f3d35d4",
    0
  };
  static u3j_harm _140_tri__rh_lte_a[] = {{".2", u3wes_lte}, {}};
  static c3_c* _140_tri__rh_lte_ha[] = {
    "6157e766050f9697c05b111ad2a582459a63a98b3b1ec70881f9ad943f951a0d",
    0
  };
  static u3j_harm _140_tri__rh_equ_a[] = {{".2", u3wes_equ}, {}};
  static c3_c* _140_tri__rh_equ_ha[] = {
    "72664392e2a00137383aa5a55e947c5e95c7b3172f71a7238dc0d55050196884",
    0
  };
  static u3j_harm _140_tri__rh_gte_a[] = {{".2", u3wes_gte}, {}};
  static c3_c* _140_tri__rh_gte_ha[] = {
    "145355b13712b9471031e755d0ab271907efdb9a287b1bacb2f1d0338d28dbf6",
    0
  };
  static u3j_harm _140_tri__rh_gth_a[] = {{".2", u3wes_gth}, {}};
  static c3_c* _140_tri__rh_gth_ha[] = {
    "d53219ee10acdd291a1e2b34fa5f543c780a0301357e93cfebd466e556f9824e",
    0
  };
static u3j_core _140_tri__rh_d[] =
  { { "add", 7, _140_tri__rh_add_a, 0, _140_tri__rh_add_ha },
    { "sub", 7, _140_tri__rh_sub_a, 0, _140_tri__rh_sub_ha },
    { "mul", 7, _140_tri__rh_mul_a, 0, _140_tri__rh_mul_ha },
    { "div", 7, _140_tri__rh_div_a, 0, _140_tri__rh_div_ha },
    { "sqt", 7, _140_tri__rh_sqt_a, 0, _140_tri__rh_sqt_ha },
    { "fma", 7, _140_tri__rh_fma_a, 0, _140_tri__rh_fma_ha },
    { "lth", 7, _140_tri__rh_lth_a, 0, _140_tri__rh_lth_ha },
    { "lte", 7, _140_tri__rh_lte_a, 0, _140_tri__rh_lte_ha },
    { "equ", 7, _140_tri__rh_equ_a, 0, _140_tri__rh_equ_ha },
    { "gte", 7, _140_tri__rh_gte_a, 0, _140_tri__rh_gte_ha },
    { "gth", 7, _140_tri__rh_gth_a, 0, _140_tri__rh_gth_ha },
    {}
  };
static c3_c* _140_tri__rh_ha[] = {
  "c38f8c0a7e2f1fccb52c459f60a30ec5d21635cafaf1aa120b70c1fa91cf7da5",
  0
};

  static u3j_harm _140_tri__og_raw_a[] = {{".2", u3weo_raw}, {}};
  static c3_c* _140_tri__og_raw_ha[] = {
    "bbcbefc237dbebf6c141ba14fd9e0464a836127fd123d10da5f121e82d49ebdb",
    0
  };
static u3j_core _140_tri__og_d[] =
  { { "raw", 7, _140_tri__og_raw_a, 0, _140_tri__og_raw_ha },
    {}
  };
static c3_c* _140_tri__og_ha[] = {
  "74b9ae67eeabbffcff969ac7fdc7f4f0f4f67af64931e969bcac50d084e15fc0",
  0
};

  static u3j_harm _140_tri__sha_sha1_a[] = {{".2", u3we_sha1}, {}};
  static c3_c* _140_tri__sha_sha1_ha[] = {
    "75aababa0688619d9df36238269119302a64ad2e3c69c53bd0057fe6b1abaf0c",
    0
  };
static u3j_core _140_tri__sha_d[] =
  { { "sha1", 7, _140_tri__sha_sha1_a, 0, _140_tri__sha_sha1_ha },
    {}
  };
static c3_c* _140_tri__sha_ha[] = {
  "3c22d2f8719cb626e8dfe1a4206bcbc14b678c1422c48322054b40f84416d557",
  0
};

static u3j_harm _140_tri_shax_a[] = {{".2", u3we_shax}, {}};
static c3_c* _140_tri_shax_ha[] = {
  "0fc53de3ddc8b8f84a46136f1728fa3ed66a5113888d14907589d16bf5927ad8",
  0
};
static u3j_harm _140_tri_shay_a[] = {{".2", u3we_shay}, {}};
static c3_c* _140_tri_shay_ha[] = {
  "b6dbc72e15c2204f83f902619b7a60328f29c9d302ddb35c435111dea28c5470",
  0
};
static u3j_harm _140_tri_shas_a[] = {{".2", u3we_shas}, {}};
static c3_c* _140_tri_shas_ha[] = {
  "5230583767b7625b3496248ed03b6b94c1d4ee9b26342f9390bf999ec9b6cfdb",
  0
};
static u3j_harm _140_tri_shal_a[] = {{".2", u3we_shal}, {}};
static c3_c* _140_tri_shal_ha[] = {
  "3242912e29e3e1ed8d1a395cc860a82d78961b4278ed79bbdeb37cb5615bbf20",
  0
};

static u3j_harm _140_ob_fein_a[] = {{".2", u3we_fein_ob}, {}};
static c3_c* _140_ob_fein_ha[] = {
  0
};

static u3j_harm _140_ob_fynd_a[] = {{".2", u3we_fynd_ob}, {}};
static c3_c* _140_ob_fynd_ha[] = {
  0
};


static u3j_core _140_ob_d[] = {
  { "fein", 7, _140_ob_fein_a, 0, _140_ob_fein_ha },
  { "fynd", 7, _140_ob_fynd_a, 0, _140_ob_fynd_ha },
  {}
};
static c3_c* _140_ob_ha[] = {
  "13ebfbdee69396bc1d980fc4dcbcdaa9cc3fb9c011e6cf188e71311a8bffc8e6",
  0
};
static u3j_hood _140_ob_ho[] = {
  { "fein",  42 },
  { "fynd",  20 },
  {},
};

static u3j_core _140_tri_d[] =
{ { "qua",  3, 0, _140_qua_d, _140_qua_ha, _140_qua_ho },
  { "cofl", 7, 0, _140_tri__cofl_d, _140_tri__cofl_ha },
  { "rd",   7, 0, _140_tri__rd_d, _140_tri__rd_ha },
  { "rs",   7, 0, _140_tri__rs_d, _140_tri__rs_ha },
  { "rq",   7, 0, _140_tri__rq_d, _140_tri__rq_ha },
  { "rh",   7, 0, _140_tri__rh_d, _140_tri__rh_ha },
  { "og",   7, 0, _140_tri__og_d, _140_tri__og_ha },

  { "sha",  7, 0, _140_tri__sha_d, _140_tri__sha_ha },
  { "shax", 7, _140_tri_shax_a, 0, _140_tri_shax_ha },
  { "shay", 7, _140_tri_shay_a, 0, _140_tri_shay_ha },
  { "shas", 7, _140_tri_shas_a, 0, _140_tri_shas_ha },
  { "shal", 7, _140_tri_shal_a, 0, _140_tri_shal_ha },
  { "ob",   3, 0, _140_ob_d, _140_ob_ha, _140_ob_ho },
  {}
};
static c3_c* _140_tri_ha[] = {
  "e7339eb317038f64555717c5624e4571fe9654d471c1a78454129afdbcad9b53",
  0
};

static u3j_hood _140_tri_ho[] = {
  { "ob",      20 },
  { "yore",  5462 },
  { "year", 44975 },
  {},
};

/* layer two
*/
static u3j_harm _140_two_find_a[] = {{".2", u3wb_find, c3y}, {}};
static c3_c* _140_two_find_ha[] = {
  "cab18d537962b48d38fa061844f44c4635ee11c74fdf403aa80d3a6d1b15c177",
  0
};
static u3j_harm _140_two_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
static c3_c* _140_two_flop_ha[] = {
  "73d496aac2ce6fd9475645c76f949ae0228f8f5ae6738529b08ed9aeb58255fe",
  0
};
static u3j_harm _140_two_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
static c3_c* _140_two_lent_ha[] = {
  "1b98ab19350f6a6753ea4bd6daf4509a7c5681b7ac20c83204fe62846d46c2c3",
  0
};
static u3j_harm _140_two_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
static c3_c* _140_two_levy_ha[] = {
  "634f1f506b17b4b50e6902f6e21b290ffc5305d1546075cba745c9e195fcc56b",
  0
};
static u3j_harm _140_two_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
static c3_c* _140_two_lien_ha[] = {
  "2ffb70864f2be120b48869b27c614aadeed1390bde497d8940fe85b7861093ea",
  0
};
static u3j_harm _140_two_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
static c3_c* _140_two_murn_ha[] = {
  "53257aaee131c2a892529c2ee75271160811814086456e8fdf249eebdf31b990",
  0
};
static u3j_harm _140_two_need_a[] = {{".2", u3wb_need, c3y}, {}};
static c3_c* _140_two_need_ha[] = {
  "bfdd39af478811efe816e69e8c9202d10c41f646c0d27f39c23e4fe1aec807dd",
  0
};
static u3j_harm _140_two_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
static c3_c* _140_two_reap_ha[] = {
  "cf6bd10b97b418b67c645374712c768d9e7e9809c14ecf36a5c507e5fc4b4039",
  0
};
static u3j_harm _140_two_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
static c3_c* _140_two_reel_ha[] = {
  "36108d1ba09617cf62e739e0ff2dcf9286f322ca0e8faa3521ef127e9840eebf",
  0
};
static u3j_harm _140_two_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
static c3_c* _140_two_roll_ha[] = {
  "42abc6b3defd7c5eb8f6d14d57a14ba2a02d559907c03141c70a65e0803c01e5",
  0
};
static u3j_harm _140_two_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
static c3_c* _140_two_skid_ha[] = {
  "832366432a85005f9a9849d6de9a9045c8f9a591050519b06aec6a9a1a54c360",
  0
};
static u3j_harm _140_two_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
static c3_c* _140_two_skim_ha[] = {
  "ccbecb459b90d05ed6c1073859a58987bf9a479820b5550fa75f37b95f98a279",
  0
};
static u3j_harm _140_two_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
static c3_c* _140_two_skip_ha[] = {
  "873e3e4d6b8f16212911aa982065dd0d36b1f6e8834828d5eb5d59afa9da2384",
  0
};
static u3j_harm _140_two_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
static c3_c* _140_two_scag_ha[] = {
  "ddba868a28eb9655c9f6e06cfecb4ec9e9ff78277290579b9bb9b25339bb4ab9",
  0
};
static u3j_harm _140_two_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
static c3_c* _140_two_slag_ha[] = {
  "811f7f67de7ab3f33b85198a69b1bf344498cd4922b0273d918a48b803b7877d",
  0
};
static u3j_harm _140_two_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
static c3_c* _140_two_snag_ha[] = {
  "12a1d53541d4df9be60bda45f2dae6c6e6381f85edd4de062af23c6654860591",
  0
};
static u3j_harm _140_two_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
static c3_c* _140_two_sort_ha[] = {
  "dc14f91fdedacd3b77bdf241d22555fe2bf0a231e9cab58b4ae779791e54c4e7",
  0
};
static u3j_harm _140_two_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
static c3_c* _140_two_turn_ha[] = {
  "e13d9f52434ba810e182017f50a73d4d44eaa298a833231e90353f2a32ea6a78",
  0
};
static u3j_harm _140_two_weld_a[] = {{".2", u3wb_weld, c3y}, {}};
static c3_c* _140_two_weld_ha[] = {
  "d855628821d57392f575c5da000c7326eaaa19e08cda967a4772859269669df2",
  0
};
static u3j_harm _140_two_welp_a[] = {{".2", u3wb_welp, c3y}, {}};
static c3_c* _140_two_welp_ha[] = {
  "0bccae6625e62ce622c62f9e828a2a6469e2fbf42342d95e23c3b926f340140d",
  0
};
static u3j_harm _140_two_zing_a[] = {{".2", u3wb_zing, c3y}, {}};
static c3_c* _140_two_zing_ha[] = {
  "113bdea043e9e05cf4a63dac793caf34634bc58414d00250af87139405521b9d",
  0
};

static u3j_harm _140_two_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
static c3_c* _140_two_bex_ha[] = {
  "ee7a095ea21b6438ec19ab235e73877b96108f0a14cae02cecbd8a48c44e70e3",
  0
};
static u3j_harm _140_two_can_a[] = {{".2", u3wc_can, c3y}, {}};
static c3_c* _140_two_can_ha[] = {
  "c49ee52487369ba17a0105a61aa658df60e7a537e3e8737ab582644fe00b3938",
  0
};
static u3j_harm _140_two_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
static c3_c* _140_two_cat_ha[] = {
  "467f007931110ac0755dcd44c5aaee65785a63b9042b8eea6a7838fa86cc5d8f",
  0
};
static u3j_harm _140_two_con_a[] = {{".2", u3wc_con, c3y}, {}};
static c3_c* _140_two_con_ha[] = {
  "d20f091bd4f28d37c1a78373df939f3d3a41e025129e9a2bb5e2b9a710358965",
  0
};
static u3j_harm _140_two_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
static c3_c* _140_two_cut_ha[] = {
  "96bb4e9a259d6a1ede5461956b6a6fb73f05cb8e745c4803c2bae4ec0b7f0800",
  0
};
static u3j_harm _140_two_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
static c3_c* _140_two_dis_ha[] = {
  "4b3987314451e20a45d2c7baff51d5d39be57e5970f23f86df4dd6569826ddff",
  0
};
static u3j_harm _140_two_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
static c3_c* _140_two_dor_ha[] = {
  "277927a2e49e4d942e81ffc7740a71e68a7b732df886a9f84dc7d914be911879",
  0
};
static u3j_harm _140_two_end_a[] = {{".2", u3wc_end, c3y}, {}};
static c3_c* _140_two_end_ha[] = {
  "403c9f12f2481966ffb07842006713149960c67c6bcad8edd78cdf837bc0d854",
  0
};
static u3j_harm _140_two_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
static c3_c* _140_two_gor_ha[] = {
  "8a1e3ed1de749ff2ff61d489466df618e4e0773498cb9693ec2e612e9733385c",
  0
};
static u3j_harm _140_two_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
static c3_c* _140_two_lsh_ha[] = {
  "3db89b02bc596a57c7fb72a991c9fbf3197de501c56b3d1df26911b664c45f3d",
  0
};
static u3j_harm _140_two_met_a[] = {{".2", u3wc_met, c3y}, {}};
static c3_c* _140_two_met_ha[] = {
  "39dc9b1d10d9e93414b43f315f9a375596c99b4e8172d71d26759996bb7bab08",
  0
};
static u3j_harm _140_two_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
static c3_c* _140_two_mix_ha[] = {
  "c84b3e487850d73dd5e4af18fb54b623028be3c45ae9b712718754233057fbc3",
  0
};
static u3j_harm _140_two_mor_a[] = {{".2", u3wc_mor, c3y}, {}};
static c3_c* _140_two_mor_ha[] = {
  "7c2d86e952606e571e5bcd988e70ded072c0eaa45d1fd958849d76360a763ddf",
  0
};
static u3j_harm _140_two_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
static c3_c* _140_two_mug_ha[] = {
  "6da3f3aa1e951ef2d00e5131945d140fb52728558867237891e029160b7f5010",
  0
};
static u3j_harm _140_two_muk_a[] = {{".2", u3wc_muk, c3y}, {}};
static c3_c* _140_two_muk_ha[] = {
  "5a04a09bf7d22c8ef048ba2cc86be8f3a02066eab84cf4a45bbdf2bf534ff9f6",
  0
};
static u3j_harm _140_two_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
static c3_c* _140_two_pow_ha[] = {
  "6cfcb9da6ad812eb72788e22e1370b4ab1b6ab64ab0628dfdff78ccead325406",
  0
};
static u3j_harm _140_two_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
static c3_c* _140_two_rap_ha[] = {
  "f694f96bcbf97b339285d6c73ed5d33d112b911f7a991acefdef223ff01d8834",
  0
};
static u3j_harm _140_two_rep_a[] = {{".2", u3wc_rep, c3y}, {}};
static c3_c* _140_two_rep_ha[] = {
  "25aa2f1746e1cf2235117f22a3db152fa86e003d9bf9f9cfcda79e76e51f382f",
  0
};
static u3j_harm _140_two_rev_a[] = {{".2", u3wc_rev, c3y}, {}};
static c3_c* _140_two_rev_ha[] = {
  "15e20592ac1d9c0c80d99589e67cadb4ed7566be1d21844bbe7ef936e0db4524",
  0
};
static u3j_harm _140_two_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
static c3_c* _140_two_rip_ha[] = {
  "16026c27499953978f69dbf81c1530b2dec8d5a2403c5561f7a5afcc180e129e",
  0
};
static u3j_harm _140_two_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
static c3_c* _140_two_rsh_ha[] = {
  "55bd777f239a2a7c849e0c7a35bb967b79279c79bbd985f31ba272761f97928f",
  0
};
static u3j_harm _140_two_swp_a[] = {{".2", u3wc_swp, c3y}, {}};
static c3_c* _140_two_swp_ha[] = {
  "2c4583c36d73c9c2857052b893b87e1170a794e0edbbdba9d767ba7639e7c1ec",
  0
};
static u3j_harm _140_two_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
static c3_c* _140_two_sqt_ha[] = {
  "fc28ff327ae69f55ccf257b69a1477b845552ef9e615e85718902c249bdeca6f",
  0
};
static u3j_harm _140_two_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
static c3_c* _140_two_xeb_ha[] = {
  "41403aafe1e2ccb1a02edde96fe742085feffe028d02529eb2b13f925884a499",
  0
};

  static u3j_harm _140_two__in_apt_a[] = {{".2", u3wdi_apt}, {}};
  static c3_c* _140_two__in_apt_ha[] = {
    "a40812fa255f13afdaf196bff38d2d9bfcb38f09c48ace9139a2701a555a0c9a",
    0
  };
  static u3j_harm _140_two__in_bif_a[] = {{".2", u3wdi_bif}, {}};
  static c3_c* _140_two__in_bif_ha[] = {
    "edd0d727b9099e75c3e5b73b3025ad9737136eacedc2f8088b6edb02dbe06cb3",
    0
  };
  static u3j_harm _140_two__in_del_a[] = {{".2", u3wdi_del}, {}};
  static c3_c* _140_two__in_del_ha[] = {
    "33a21e7aaf71105e2d48e1af61ff463fb8a0b7e04f8a8c30a6f6a2d1f967795f",
    0
  };
  static u3j_harm _140_two__in_dif_a[] = {{".2", u3wdi_dif}, {}};
  static c3_c* _140_two__in_dif_ha[] = {
    "a488f0be5adbb1c04e2038a2315ac065591e7daadcafc1d47aea272979680468",
    0
  };
  static u3j_harm _140_two__in_gas_a[] = {{".2", u3wdi_gas}, {}};
  static c3_c* _140_two__in_gas_ha[] = {
    "223a60a43a10f1f90a3b205ecfce8e17af1adfcf9dbf3cff9b8b1362656b1af1",
    0
  };
  static u3j_harm _140_two__in_has_a[] = {{".2", u3wdi_has}, {}};
  static c3_c* _140_two__in_has_ha[] = {
    "a65e666e92176401040a883801e4f05bd650fe6c094a6c8d7f4afcaee9cf55ad",
    0
  };

  static u3j_harm _140_two__in_int_a[] = {{".2", u3wdi_int}, {}};
  static c3_c* _140_two__in_int_ha[] = {
    "a71b0e355fa02d18447c02922f69096f42043da451e8c79e7a9270460c3a44e6",
    0
  };

  static u3j_harm _140_two__in_put_a[] = {{".2", u3wdi_put}, {}};
  static c3_c* _140_two__in_put_ha[] = {
    "19b27267e18ef156d85d84d37e02692a17fec0b7a2a0fe4120a3ae02b841c8f4",
    0
  };
  static u3j_harm _140_two__in_rep_a[] = {{".2", u3wdi_rep}, {}};
  static c3_c* _140_two__in_rep_ha[] = {
    "05bfb84a52ed8ccc330a96faca29a49afd28300960ac089d00dba32212b971a7",
    0
  };
  static u3j_harm _140_two__in_run_a[] = {{".2", u3wdi_run}, {}};
  static c3_c* _140_two__in_run_ha[] = {
    "7f2061dbee19fa20925bd5a80cc41ed71e462e0f49ee6e845fd750c219734864",
    0
  };
  static u3j_harm _140_two__in_tap_a[] = {{".2", u3wdi_tap}, {}};
  static c3_c* _140_two__in_tap_ha[] = {
    "7dde59e2bd7684e785ce9787bc394571bd1216d7a62398c703447fc951c6b352",
    0
  };
  static u3j_harm _140_two__in_wyt_a[] = {{".2", u3wdi_wyt}, {}};
  static c3_c* _140_two__in_wyt_ha[] = {
    "fac9248ebd1defade9df695cd81f94355bebb271f85b164ff34658a5f45c71a0",
    0
  };
  static u3j_harm _140_two__in_uni_a[] = {{".2", u3wdi_uni}, {}};
  static c3_c* _140_two__in_uni_ha[] = {
    "6bd72ef1fb12482a839f4435a2b163ace1b56036297a3cec6968be33d6863096",
    0
  };

static u3j_core _140_two__in_d[] =
  { { "apt", 7, _140_two__in_apt_a, 0, _140_two__in_apt_ha },
    { "bif", 7, _140_two__in_bif_a, 0, _140_two__in_bif_ha },
    { "del", 7, _140_two__in_del_a, 0, _140_two__in_del_ha },
    { "dif", 7, _140_two__in_dif_a, 0, _140_two__in_dif_ha },
    { "gas", 7, _140_two__in_gas_a, 0, _140_two__in_gas_ha },
    { "has", 7, _140_two__in_has_a, 0, _140_two__in_has_ha },
    { "int", 7, _140_two__in_int_a, 0, _140_two__in_int_ha },
    { "put", 7, _140_two__in_put_a, 0, _140_two__in_put_ha },
    { "rep", 7, _140_two__in_rep_a, 0, _140_two__in_rep_ha },
    { "run", 7, _140_two__in_run_a, 0, _140_two__in_run_ha },
    { "tap", 7, _140_two__in_tap_a, 0, _140_two__in_tap_ha },
    { "uni", 7, _140_two__in_uni_a, 0, _140_two__in_uni_ha },
    { "wyt", 3, _140_two__in_wyt_a, 0, _140_two__in_wyt_ha },
    {}
  };
static c3_c* _140_two__in_ha[] = {
  "8bbb90ce0a49d627194aa267f6cf1fd78df677111b553ce03119fea19f9d763c",
  0
};
  static u3j_harm _140_two__by_all_a[] = {{".2", u3wdb_all, c3y}, {}};
  static c3_c* _140_two__by_all_ha[] = {
    "c2e87d0047c14b4488d03aad98fa43080c736d86d2ff723a037aaf1843aa9285",
    0
  };
  static u3j_harm _140_two__by_any_a[] = {{".2", u3wdb_any, c3y}, {}};
  static c3_c* _140_two__by_any_ha[] = {
    "96b95c942dcbc97f5291fa6f7342c3e19a87d69cc254965b0f75d95133a19301",
    0
  };
  static u3j_harm _140_two__by_apt_a[] = {{".2", u3wdb_apt, c3y}, {}};
  static c3_c* _140_two__by_apt_ha[] = {
    "1f0a6f8b945b243520b77069060589938d9e651e34b24924db9528d02a98014f",
    0
  };
  static u3j_harm _140_two__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
  static c3_c* _140_two__by_bif_ha[] = {
    "d377a032a3866e76f6f5217c7c0ed0519b768d8b1c5107e35f7dbf18d8f60880",
    0
  };
  static u3j_harm _140_two__by_del_a[] = {{".2", u3wdb_del, c3y}, {}};
  static c3_c* _140_two__by_del_ha[] = {
    "09f78d6235d3fce8303c7bc663988349b7d4592abdacfb09b833d2f43629b6b6",
    0
  };
  static u3j_harm _140_two__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
  static c3_c* _140_two__by_dif_ha[] = {
    "0334e6df6fd0bd5013b94a1b22c29e4c436da0a2d5573f1992faad1c8a059cc7",
    0
  };
  static u3j_harm _140_two__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
  static c3_c* _140_two__by_gas_ha[] = {
    "43046602e0b9e568b09448cfe18527e2331f3393a2f32e485d9707a14c346698",
    0
  };
  static u3j_harm _140_two__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
  static c3_c* _140_two__by_get_ha[] = {
    "4de4cea8fa98ef48e9faae10c90ba5bd77971670030ffb00483d0608af4c466f",
    0
  };
  static u3j_harm _140_two__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
  static c3_c* _140_two__by_has_ha[] = {
    "04ecc67ab25961bee1b7c9dbcf42965d16f32474b9bbdd2b286983f998e3957a",
    0
  };

  static u3j_harm _140_two__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
  static c3_c* _140_two__by_int_ha[] = {
    "a2345429482c271a1668f3c0675a559452bb7b13cb7393c3acb7de44c603aef9",
    0
  };

  static u3j_harm _140_two__by_jab_a[] = {{".2", u3wdb_jab, c3y}, {}};
  static c3_c* _140_two__by_jab_ha[] = {
    "48930133d9b26e912dce54d1bc486cfe9dcb32bb3c2b1ad76143382799aec156",
    0
  };
  static u3j_harm _140_two__by_key_a[] = {{".2", u3wdb_key, c3y}, {}};
  static c3_c* _140_two__by_key_ha[] = {
    "0096c77b93e9fe36b98d9f433eb73300f024283b93b3d73a4001afb9f9804d1b",
    0
  };
  static u3j_harm _140_two__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
  static c3_c* _140_two__by_put_ha[] = {
    "b7307589fed604bfb92e8ad5ffad611c82d835baf02a86c6911b279930f4e8d7",
    0
  };
  static u3j_harm _140_two__by_rep_a[] = {{".2", u3wdb_rep, c3y}, {}};
  static c3_c* _140_two__by_rep_ha[] = {
    "05bfb84a52ed8ccc330a96faca29a49afd28300960ac089d00dba32212b971a7",
    0
  };
  static u3j_harm _140_two__by_run_a[] = {{".2", u3wdb_run, c3y}, {}};
  static c3_c* _140_two__by_run_ha[] = {
    "adea01e9036e0b40e4969814d4eed935d7d69a52e4a55de5520df2fa5204d8e7",
    0
  };
  static u3j_harm _140_two__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
  static c3_c* _140_two__by_tap_ha[] = {
    "7dde59e2bd7684e785ce9787bc394571bd1216d7a62398c703447fc951c6b352",
    0
  };
  static u3j_harm _140_two__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
  static c3_c* _140_two__by_uni_ha[] = {
    "f18bc4dac19abe14a6f56afc15d838b7394d48969156f4b37c3c84edd5d46752",
    0
  };
  static u3j_harm _140_two__by_urn_a[] = {{".2", u3wdb_urn, c3y}, {}};
  static c3_c* _140_two__by_urn_ha[] = {
    "a409cf78e7f1c2ce8440115730f74367839b658cde2d6a1daa8af067b790eb83",
    0
  };
  static u3j_harm _140_two__by_wyt_a[] = {{".2", u3wdb_wyt, c3y}, {}};
  static c3_c* _140_two__by_wyt_ha[] = {
    "fac9248ebd1defade9df695cd81f94355bebb271f85b164ff34658a5f45c71a0",
    0
  };

static u3j_core _140_two__by_d[] =
  { { "all", 7, _140_two__by_all_a, 0, _140_two__by_all_ha },
    { "any", 7, _140_two__by_any_a, 0, _140_two__by_any_ha },
    { "apt", 7, _140_two__by_apt_a, 0, _140_two__by_apt_ha },
    { "bif", 7, _140_two__by_bif_a, 0, _140_two__by_bif_ha },
    { "del", 7, _140_two__by_del_a, 0, _140_two__by_del_ha },
    { "dif", 7, _140_two__by_dif_a, 0, _140_two__by_dif_ha },
    { "gas", 7, _140_two__by_gas_a, 0, _140_two__by_gas_ha },
    { "get", 7, _140_two__by_get_a, 0, _140_two__by_get_ha },
    { "has", 7, _140_two__by_has_a, 0, _140_two__by_has_ha },
    { "int", 7, _140_two__by_int_a, 0, _140_two__by_int_ha },
    { "jab", 7, _140_two__by_jab_a, 0, _140_two__by_jab_ha },
    { "key", 7, _140_two__by_key_a, 0, _140_two__by_key_ha },
    { "put", 7, _140_two__by_put_a, 0, _140_two__by_put_ha },
    { "rep", 7, _140_two__by_rep_a, 0, _140_two__by_rep_ha },
    { "run", 7, _140_two__by_run_a, 0, _140_two__by_run_ha },
    { "tap", 7, _140_two__by_tap_a, 0, _140_two__by_tap_ha },
    { "uni", 7, _140_two__by_uni_a, 0, _140_two__by_uni_ha },
    { "urn", 7, _140_two__by_urn_a, 0, _140_two__by_urn_ha },
    { "wyt", 3, _140_two__by_wyt_a, 0, _140_two__by_wyt_ha },
    {}
  };
static c3_c* _140_two__by_ha[] = {
  "9c70e973de46335405a7ff932d4742743f54db579f2584758ef2b02afd4fbfe8",
  0
};

static u3j_harm _140_two_cue_a[] = {{".2", u3we_cue}, {}};
static c3_c* _140_two_cue_ha[] = {
  "a52b584c5a92fc653e47f50c3389caf3427e13d20ddb8bd701a2d7bca12cb742",
  0
};
static u3j_harm _140_two_jam_a[] = {{".2", u3we_jam}, {}};
static c3_c* _140_two_jam_ha[] = {
  "61f86be74cb1fd5a1d7f531cc9588f8f34a972be8de487c93d25c8e026592ed2",
  0
};
static u3j_harm _140_two_mat_a[] = {{".2", u3we_mat}, {}};
static c3_c* _140_two_mat_ha[] = {
  "b5cd9fd1eded54fcb9bfd06af3c34460c1aa4cfc46f1ee9bd3f6476aa8fbb8c8",
  0
};
static u3j_harm _140_two_rub_a[] = {{".2", u3we_rub}, {}};
static c3_c* _140_two_rub_ha[] = {
  "87fcf40fb6fce8c3cb778373670d0682785ae650f785531db8ff69d431bc14c6",
  0
};

static u3j_core _140_two_d[] =
{ { "tri",  3, 0, _140_tri_d, _140_tri_ha, _140_tri_ho },

  { "find", 7, _140_two_find_a, 0, _140_two_find_ha },
  { "flop", 7, _140_two_flop_a, 0, _140_two_flop_ha },
  { "lent", 7, _140_two_lent_a, 0, _140_two_lent_ha },
  { "levy", 7, _140_two_levy_a, 0, _140_two_levy_ha },
  { "lien", 7, _140_two_lien_a, 0, _140_two_lien_ha },
  { "murn", 7, _140_two_murn_a, 0, _140_two_murn_ha },
  { "need", 7, _140_two_need_a, 0, _140_two_need_ha },
  { "reap", 7, _140_two_reap_a, 0, _140_two_reap_ha },
  { "reel", 7, _140_two_reel_a, 0, _140_two_reel_ha },
  { "roll", 7, _140_two_roll_a, 0, _140_two_roll_ha },
  { "skid", 7, _140_two_skid_a, 0, _140_two_skid_ha },
  { "skim", 7, _140_two_skim_a, 0, _140_two_skim_ha },
  { "skip", 7, _140_two_skip_a, 0, _140_two_skip_ha },
  { "scag", 7, _140_two_scag_a, 0, _140_two_scag_ha },
  { "slag", 7, _140_two_slag_a, 0, _140_two_slag_ha },
  { "snag", 7, _140_two_snag_a, 0, _140_two_snag_ha },
  { "sort", 7, _140_two_sort_a, 0, _140_two_sort_ha },
  { "turn", 7, _140_two_turn_a, 0, _140_two_turn_ha },
  { "weld", 7, _140_two_weld_a, 0, _140_two_weld_ha },
  { "welp", 7, _140_two_welp_a, 0, _140_two_welp_ha },
  { "zing", 7, _140_two_zing_a, 0, _140_two_zing_ha },

  { "bex", 7, _140_two_bex_a, 0, _140_two_bex_ha },
  { "cat", 7, _140_two_cat_a, 0, _140_two_cat_ha },
  { "can", 7, _140_two_can_a, 0, _140_two_can_ha },
  { "con", 7, _140_two_con_a, 0, _140_two_con_ha },
  { "cue", 7, _140_two_cue_a, 0, _140_two_cue_ha },
  { "cut", 7, _140_two_cut_a, 0, _140_two_cut_ha },
  { "dis", 7, _140_two_dis_a, 0, _140_two_dis_ha },
  { "dor", 7, _140_two_dor_a, 0, _140_two_dor_ha },
  { "end", 7, _140_two_end_a, 0, _140_two_end_ha },
  { "gor", 7, _140_two_gor_a, 0, _140_two_gor_ha },
  { "jam", 7, _140_two_jam_a, 0, _140_two_jam_ha },
  { "lsh", 7, _140_two_lsh_a, 0, _140_two_lsh_ha },
  { "mat", 7, _140_two_mat_a, 0, _140_two_mat_ha },
  { "met", 7, _140_two_met_a, 0, _140_two_met_ha },
  { "mix", 7, _140_two_mix_a, 0, _140_two_mix_ha },
  { "mor", 7, _140_two_mor_a, 0, _140_two_mor_ha },
  { "mug", 7, _140_two_mug_a, 0, _140_two_mug_ha },
  { "muk", 59, _140_two_muk_a, 0, _140_two_muk_ha },
  { "rap", 7, _140_two_rap_a, 0, _140_two_rap_ha },
  { "rep", 7, _140_two_rep_a, 0, _140_two_rep_ha },
  { "rev", 7, _140_two_rev_a, 0, _140_two_rev_ha },
  { "rip", 7, _140_two_rip_a, 0, _140_two_rip_ha },
  { "rsh", 7, _140_two_rsh_a, 0, _140_two_rsh_ha },
  { "swp", 7, _140_two_swp_a, 0, _140_two_swp_ha },
  { "rub", 7, _140_two_rub_a, 0, _140_two_rub_ha },
  { "pow", 7, _140_two_pow_a, 0, _140_two_pow_ha },
  { "sqt", 7, _140_two_sqt_a, 0, _140_two_sqt_ha },
  { "xeb", 7, _140_two_xeb_a, 0, _140_two_xeb_ha },

  { "by",  7, 0, _140_two__by_d, _140_two__by_ha },
  { "in",  7, 0, _140_two__in_d, _140_two__in_ha },
  {}
};
static c3_c* _140_two_ha[] = {
  "f693e1f5ff57ec741fe28a48a18252b3e12dead2bfe3bcd4ea8e904a36905c0b",
  0
};

/* layer one
*/
static u3j_harm _140_one_add_a[] = {{".2", u3wa_add, c3y}, {}};
static c3_c* _140_one_add_ha[] = {
  "46407e27fe5d7c20b3ba25c02657c227b37217ddab8501b2d3b70b818aca7a44",
  0
};
static u3j_harm _140_one_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
static c3_c* _140_one_dec_ha[] = {
  "6345d28d34c62c4b4f9da98828574bc9060ff0869789968d9045d90faeb3580c",
  0
};
static u3j_harm _140_one_div_a[] = {{".2", u3wa_div, c3y}, {}};
static c3_c* _140_one_div_ha[] = {
  "e3292e76feb274b9314e7693827de11e96677629c556b3a6c72cc15ebad45113",
  0
};
static u3j_harm _140_one_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static c3_c* _140_one_dvr_ha[] = {
  "fc259f46d770f82767163544f3662dfd45b1484a7bcffad396c7420651f092a4",
  0
};
static u3j_harm _140_one_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
static c3_c* _140_one_gte_ha[] = {
  "f3ff2c0fc1f386226183e8834cff87420a1206583f8710e1e75f0e34ed8df5fe",
  0
};
static u3j_harm _140_one_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
static c3_c* _140_one_gth_ha[] = {
  "62692d64c8166c7d48bb2a00713064846da9629a1dd2d924c3b15cfd18a5912a",
  0
};
static u3j_harm _140_one_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
static c3_c* _140_one_lte_ha[] = {
  "6ca61752aa27b453f28f20e12f652610d45695c3bd965190d5b4fa8b9daa518c",
  0
};
static u3j_harm _140_one_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
static c3_c* _140_one_lth_ha[] = {
  "39260325faffbbf5bd88c4abb3efb09c5a7e1deb81a2126498d6c0f49474955e",
  0
};
static u3j_harm _140_one_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
static c3_c* _140_one_mod_ha[] = {
  "374d2f3cd0ece33f680bd7103b99891d7dae03590f9eb9faac03a4a501f17038",
  0
};
static u3j_harm _140_one_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
static c3_c* _140_one_mul_ha[] = {
  "51e45dbea29cf65a5c26ead095a20eb12ba078840652c88b9c1997820e670bc6",
  0
};
static u3j_harm _140_one_sub_a[] = {{".2", u3wa_sub, c3y}, {}};
static c3_c* _140_one_sub_ha[] = {
  "016695719ffe93c177e8a03afa5d29fc428ff596bb8962ace50f7706cd6e53a6",
  0
};

static u3j_harm _140_one_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static c3_c* _140_one_cap_ha[] = {
  "407e764ee978c712b81c9c3452932e0f7d33faeda36dfe99aaf81d543db16254",
  0
};
static u3j_harm _140_one_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static c3_c* _140_one_peg_ha[] = {
  "8b608d2d2e2eccec3e2fc8cd2d92fd69504c72b26581bb9cbfa4ff51f997251f",
  0
};
static u3j_harm _140_one_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static c3_c* _140_one_mas_ha[] = {
  "1439dcd809f0819b09fb5fe7e83bc1292ca6fd33b5819d78e706d402c053b02a",
  0
};

static u3j_core _140_one_d[] =
{ { "two", 3, 0, _140_two_d, _140_two_ha },

  { "add", 7, _140_one_add_a, 0, _140_one_add_ha },
  { "dec", 7, _140_one_dec_a, 0, _140_one_dec_ha },
  { "div", 7, _140_one_div_a, 0, _140_one_div_ha },
  { "dvr", 7, _140_one_dvr_a, 0, _140_one_dvr_ha },
  { "gte", 7, _140_one_gte_a, 0, _140_one_gte_ha },
  { "gth", 7, _140_one_gth_a, 0, _140_one_gth_ha },
  { "lte", 7, _140_one_lte_a, 0, _140_one_lte_ha },
  { "lth", 7, _140_one_lth_a, 0, _140_one_lth_ha },
  { "mod", 7, _140_one_mod_a, 0, _140_one_mod_ha },
  { "mul", 7, _140_one_mul_a, 0, _140_one_mul_ha },
  { "sub", 7, _140_one_sub_a, 0, _140_one_sub_ha },

  { "cap", 7, _140_one_cap_a, 0, _140_one_cap_ha },
  { "mas", 7, _140_one_mas_a, 0, _140_one_mas_ha },
  { "peg", 7, _140_one_peg_a, 0, _140_one_peg_ha },
  {}
};
static c3_c* _140_one_ha[] = {
  "2501f8dbe62384d144ab0f805501ed66325bd77a733eca0c80d1da673e4b16fb",
  0
};

u3j_core _k140_d[] =
{ { "one", 3, 0, _140_one_d, _140_one_ha },
  {}
};
static c3_c* _k140_ha[] = {
  "9b82a903093c077afb3f0b9d4e95e1a9c9789d1ca605b57bbacf79857e3d5c52",
  0
};

static u3j_core _d[] = {
  { "k140", 0, 0, _k140_d, _k140_ha, 0, (u3j_core*) 140, 0 },
  {}
};

u3j_dash
u3j_Dash = {
  _d,
  0,
  0
};
