/* j/dash.c
**
*/
#include "all.h"

static u3j_harm _mood__hoon_add_a[] = {{".2", u3wa_add, c3y}, {}};
static u3j_harm _mood__hoon_dec_a[] = {{".2", u3wa_dec, c3y}, {}};
static u3j_harm _mood__hoon_div_a[] = {{".2", u3wa_div, c3y}, {}};
static u3j_harm _mood__hoon_gte_a[] = {{".2", u3wa_gte, c3y}, {}};
static u3j_harm _mood__hoon_gth_a[] = {{".2", u3wa_gth, c3y}, {}};
static u3j_harm _mood__hoon_lte_a[] = {{".2", u3wa_lte, c3y}, {}};
static u3j_harm _mood__hoon_lth_a[] = {{".2", u3wa_lth, c3y}, {}};
static u3j_harm _mood__hoon_mod_a[] = {{".2", u3wa_mod, c3y}, {}};
static u3j_harm _mood__hoon_mul_a[] = {{".2", u3wa_mul, c3y}, {}};
static u3j_harm _mood__hoon_sub_a[] = {{".2", u3wa_sub, c3y}, {}};

static u3j_harm _mood__hoon_bind_a[] = {{".2", u3wb_bind, c3y}, {}};
static u3j_harm _mood__hoon_clap_a[] = {{".2", u3wb_clap, c3y}, {}};
static u3j_harm _mood__hoon_drop_a[] = {{".2", u3wb_drop, c3y}, {}};
static u3j_harm _mood__hoon_flop_a[] = {{".2", u3wb_flop, c3y}, {}};
static u3j_harm _mood__hoon_lent_a[] = {{".2", u3wb_lent, c3y}, {}};
static u3j_harm _mood__hoon_levy_a[] = {{".2", u3wb_levy, c3y}, {}};
static u3j_harm _mood__hoon_lien_a[] = {{".2", u3wb_lien, c3y}, {}};
static u3j_harm _mood__hoon_murn_a[] = {{".2", u3wb_murn, c3y}, {}};
static u3j_harm _mood__hoon_need_a[] = {{".2", u3wb_need, c3y}, {}};
static u3j_harm _mood__hoon_reap_a[] = {{".2", u3wb_reap, c3y}, {}};
static u3j_harm _mood__hoon_reel_a[] = {{".2", u3wb_reel, c3y}, {}};
static u3j_harm _mood__hoon_roll_a[] = {{".2", u3wb_roll, c3y}, {}};
static u3j_harm _mood__hoon_skid_a[] = {{".2", u3wb_skid, c3y}, {}};
static u3j_harm _mood__hoon_skim_a[] = {{".2", u3wb_skim, c3y}, {}};
static u3j_harm _mood__hoon_skip_a[] = {{".2", u3wb_skip, c3y}, {}};
static u3j_harm _mood__hoon_scag_a[] = {{".2", u3wb_scag, c3y}, {}};
static u3j_harm _mood__hoon_slag_a[] = {{".2", u3wb_slag, c3y}, {}};
static u3j_harm _mood__hoon_snag_a[] = {{".2", u3wb_snag, c3y}, {}};
// static u3j_harm _mood__hoon_sort_a[] = {{".2", u3wb_sort, c3y}, {}};
static u3j_harm _mood__hoon_turn_a[] = {{".2", u3wb_turn, c3y}, {}};
static u3j_harm _mood__hoon_weld_a[] = {{".2", u3wb_weld, c3y}, {}};

static u3j_harm _mood__hoon_bex_a[] = {{".2", u3wc_bex, c3y}, {}};
static u3j_harm _mood__hoon_xeb_a[] = {{".2", u3wc_xeb, c3y}, {}};
static u3j_harm _mood__hoon_can_a[] = {{".2", u3wc_can, c3y}, {}};
static u3j_harm _mood__hoon_cap_a[] = {{".2", u3wc_cap, c3y}, {}};
static u3j_harm _mood__hoon_cat_a[] = {{".2", u3wc_cat, c3y}, {}};
static u3j_harm _mood__hoon_con_a[] = {{".2", u3wc_con, c3y}, {}};
static u3j_harm _mood__hoon_cut_a[] = {{".2", u3wc_cut, c3y}, {}};
static u3j_harm _mood__hoon_dis_a[] = {{".2", u3wc_dis, c3y}, {}};
static u3j_harm _mood__hoon_dor_a[] = {{".2", u3wc_dor, c3y}, {}};
static u3j_harm _mood__hoon_dvr_a[] = {{".2", u3wc_dvr, c3y}, {}};
static u3j_harm _mood__hoon_end_a[] = {{".2", u3wc_end, c3y}, {}};
static u3j_harm _mood__hoon_gor_a[] = {{".2", u3wc_gor, c3y}, {}};
static u3j_harm _mood__hoon_hor_a[] = {{".2", u3wc_hor, c3y}, {}};
static u3j_harm _mood__hoon_lsh_a[] = {{".2", u3wc_lsh, c3y}, {}};
static u3j_harm _mood__hoon_mas_a[] = {{".2", u3wc_mas, c3y}, {}};
static u3j_harm _mood__hoon_met_a[] = {{".2", u3wc_met, c3y}, {}};
static u3j_harm _mood__hoon_mix_a[] = {{".2", u3wc_mix, c3y}, {}};
static u3j_harm _mood__hoon_mug_a[] = {{".2", u3wc_mug, c3y}, {}};
static u3j_harm _mood__hoon_peg_a[] = {{".2", u3wc_peg, c3y}, {}};
static u3j_harm _mood__hoon_pow_a[] = {{".2", u3wc_pow, c3y}, {}};
static u3j_harm _mood__hoon_rap_a[] = {{".2", u3wc_rap, c3y}, {}};
static u3j_harm _mood__hoon_rip_a[] = {{".2", u3wc_rip, c3y}, {}};
static u3j_harm _mood__hoon_rsh_a[] = {{".2", u3wc_rsh, c3y}, {}};
static u3j_harm _mood__hoon_sqt_a[] = {{".2", u3wc_sqt, c3y}, {}};
static u3j_harm _mood__hoon_vor_a[] = {{".2", u3wc_vor, c3y}, {}};

static u3j_harm _mood__hoon__po_ind_a[] = {{".2", u3wcp_ind}, {}};
static u3j_harm _mood__hoon__po_ins_a[] = {{".2", u3wcp_ins}, {}};
static u3j_harm _mood__hoon__po_tod_a[] = {{".2", u3wcp_tod}, {}};
static u3j_harm _mood__hoon__po_tos_a[] = {{".2", u3wcp_tos}, {}};
static u3j_core _mood__hoon__po_d[] =
  { { "ind", _mood__hoon__po_ind_a },
    { "ins", _mood__hoon__po_ins_a },
    { "tod", _mood__hoon__po_tod_a },
    { "tos", _mood__hoon__po_tos_a },
    {}
  };

static u3j_harm _mood__hoon__by_bif_a[] = {{".2", u3wdb_bif, c3y}, {}};
static u3j_harm _mood__hoon__by_dif_a[] = {{".2", u3wdb_dif, c3y}, {}};
static u3j_harm _mood__hoon__by_gas_a[] = {{".2", u3wdb_gas, c3y}, {}};
static u3j_harm _mood__hoon__by_get_a[] = {{".2", u3wdb_get, c3y}, {}};
static u3j_harm _mood__hoon__by_has_a[] = {{".2", u3wdb_has, c3y}, {}};
// static u3j_harm _mood__hoon__by_int_a[] = {{".2", u3wdb_int, c3y}, {}};
static u3j_harm _mood__hoon__by_put_a[] = {{".2", u3wdb_put, c3y}, {}};
static u3j_harm _mood__hoon__by_tap_a[] = {{".2", u3wdb_tap, c3y}, {}};
// static u3j_harm _mood__hoon__by_uni_a[] = {{".2", u3wdb_uni, c3y}, {}};
static u3j_core _mood__hoon__by_d[] =
  { { "bif", _mood__hoon__by_bif_a },
    { "dif", _mood__hoon__by_dif_a },
    { "gas", _mood__hoon__by_gas_a },
    { "get", _mood__hoon__by_get_a },
    { "has", _mood__hoon__by_has_a },
    // { "int", _mood__hoon__by_int_a },
    { "put", _mood__hoon__by_put_a },
    { "tap", _mood__hoon__by_tap_a },
    // { "uni", _mood__hoon__by_uni_a },
    {}
  };

static u3j_harm _mood__hoon__in_bif_a[] = {{".2", u3wdi_bif}, {}};
static u3j_harm _mood__hoon__in_dif_a[] = {{".2", u3wdi_dif}, {}};
static u3j_harm _mood__hoon__in_gas_a[] = {{".2", u3wdi_gas}, {}};
static u3j_harm _mood__hoon__in_has_a[] = {{".2", u3wdi_has}, {}};
static u3j_harm _mood__hoon__in_mer_a[] = {{".2", u3wdi_mer}, {}};
// static u3j_harm _mood__hoon__in_int_a[] = {{".2", u3wdi_int}, {}};
static u3j_harm _mood__hoon__in_put_a[] = {{".2", u3wdi_put}, {}};
static u3j_harm _mood__hoon__in_tap_a[] = {{".2", u3wdi_tap}, {}};
// static u3j_harm _mood__hoon__in_uni_a[] = {{".2", u3wdi_uni}, {}};
static u3j_core _mood__hoon__in_d[] =
  { { "bif", _mood__hoon__in_bif_a },
    { "dif", _mood__hoon__in_dif_a },
    { "gas", _mood__hoon__in_gas_a },
    { "has", _mood__hoon__in_has_a },
    { "mer", _mood__hoon__in_mer_a },
    // { "int", _mood__hoon__in_int_a },
    { "put", _mood__hoon__in_put_a },
    { "tap", _mood__hoon__in_tap_a },
    // { "uni", _mood__hoon__in_uni_a },
    {}
  };

static u3j_harm _mood__hoon_cue_a[] = {{".2", u3we_cue}, {}};
static u3j_harm _mood__hoon_jam_a[] = {{".2", u3we_jam}, {}};
static u3j_harm _mood__hoon_mat_a[] = {{".2", u3we_mat}, {}};
static u3j_harm _mood__hoon_rub_a[] = {{".2", u3we_rub}, {}};
static u3j_harm _mood__hoon_lore_a[] = {{".2", u3we_lore}, {}};
static u3j_harm _mood__hoon_loss_a[] = {{".2", u3we_loss}, {}};
static u3j_harm _mood__hoon_mink_a[] = {{".2", u3we_mink}, {}};
static u3j_harm _mood__hoon_mule_a[] = {{".2", u3we_mule}, {}};
static u3j_harm _mood__hoon_repg_a[] = {{".2", u3we_repg}, {}};
static u3j_harm _mood__hoon_rexp_a[] = {{".2", u3we_rexp}, {}};
static u3j_harm _mood__hoon_trip_a[] = {{".2", u3we_trip}, {}};

static u3j_harm _mood__hoon__aesc_en_a[] = {{".2", u3wea_en}, {}};
static u3j_harm _mood__hoon__aesc_de_a[] = {{".2", u3wea_de}, {}};
static u3j_core _mood__hoon__aesc_d[] =
  { { "en", _mood__hoon__aesc_en_a },
    { "de", _mood__hoon__aesc_de_a },
    {}
  };

static u3j_harm _mood__hoon__bend_fun_a[] = {{".2", u3we_bend_fun}, {}};
static u3j_core _mood__hoon__bend_d[] =
  { { "fun", _mood__hoon__bend_fun_a },
    {}
  };
static u3j_harm _mood__hoon__cold_fun_a[] = {{".2", u3we_cold_fun}, {}};
static u3j_core _mood__hoon__cold_d[] =
  { { "fun", _mood__hoon__cold_fun_a },
    {}
  };
static u3j_harm _mood__hoon__cook_fun_a[] = {{".2", u3we_cook_fun}, {}};
static u3j_core _mood__hoon__cook_d[] =
  { { "fun", _mood__hoon__cook_fun_a },
    {}
  };
static u3j_harm _mood__hoon__comp_fun_a[] = {{".2", u3we_comp_fun}, {}};
static u3j_core _mood__hoon__comp_d[] =
  { { "fun", _mood__hoon__comp_fun_a },
    {}
  };
static u3j_harm _mood__hoon__easy_fun_a[] = {{".2", u3we_easy_fun}, {}};
static u3j_core _mood__hoon__easy_d[] =
  { { "fun", _mood__hoon__easy_fun_a },
    {}
  };
static u3j_harm _mood__hoon__glue_fun_a[] = {{".2", u3we_glue_fun}, {}};
static u3j_core _mood__hoon__glue_d[] =
  { { "fun", _mood__hoon__glue_fun_a },
    {}
  };
static u3j_harm _mood__hoon__here_fun_a[] = {{".2", u3we_here_fun}, {}};
static u3j_core _mood__hoon__here_d[] =
  { { "fun", _mood__hoon__here_fun_a },
    {}
  };
static u3j_harm _mood__hoon__just_fun_a[] = {{".2", u3we_just_fun}, {}};
static u3j_core _mood__hoon__just_d[] =
  { { "fun", _mood__hoon__just_fun_a },
    {}
  };
static u3j_harm _mood__hoon__mask_fun_a[] = {{".2", u3we_mask_fun}, {}};
static u3j_core _mood__hoon__mask_d[] =
  { { "fun", _mood__hoon__mask_fun_a },
    {}
  };
static u3j_harm _mood__hoon__shim_fun_a[] = {{".2", u3we_shim_fun}, {}};
static u3j_core _mood__hoon__shim_d[] =
  { { "fun", _mood__hoon__shim_fun_a },
    {}
  };
static u3j_harm _mood__hoon__stag_fun_a[] = {{".2", u3we_stag_fun}, {}};
static u3j_core _mood__hoon__stag_d[] =
  { { "fun", _mood__hoon__stag_fun_a },
    {}
  };
static u3j_harm _mood__hoon__stew_fun_a[] = {{".2", u3we_stew_fun}, {}};
static u3j_core _mood__hoon__stew_d[] =
  { { "fun", _mood__hoon__stew_fun_a },
    {}
  };
static u3j_harm _mood__hoon__stir_fun_a[] = {{".2", u3we_stir_fun}, {}};
static u3j_core _mood__hoon__stir_d[] =
  { { "fun", _mood__hoon__stir_fun_a },
    {}
  };

static u3j_harm _mood__hoon__og_raw_a[] = {{".2", u3weo_raw}, {}};
static u3j_core _mood__hoon__og_d[] =
  { { "raw", _mood__hoon__og_raw_a },
    {}
  };

static u3j_harm _mood__hoon__cofl__drg_a[] = {{".2", u3wef_drg}, {}};
static u3j_harm _mood__hoon__cofl__lug_a[] = {{".2", u3wef_lug}, {}};
static u3j_core _mood__hoon__cofl_d[] =
  { { "drg", _mood__hoon__cofl__drg_a },
    { "lug", _mood__hoon__cofl__lug_a },
    {}
  };

static u3j_harm _mood__hoon__rd_add_a[] = {{".2", u3wer_add}, {}};
static u3j_harm _mood__hoon__rd_sub_a[] = {{".2", u3wer_sub}, {}};
static u3j_harm _mood__hoon__rd_mul_a[] = {{".2", u3wer_mul}, {}};
static u3j_harm _mood__hoon__rd_div_a[] = {{".2", u3wer_div}, {}};
static u3j_harm _mood__hoon__rd_sqt_a[] = {{".2", u3wer_sqt}, {}};
static u3j_harm _mood__hoon__rd_fma_a[] = {{".2", u3wer_fma}, {}};
static u3j_harm _mood__hoon__rd_lth_a[] = {{".2", u3wer_lth}, {}};
static u3j_harm _mood__hoon__rd_lte_a[] = {{".2", u3wer_lte}, {}};
static u3j_harm _mood__hoon__rd_equ_a[] = {{".2", u3wer_equ}, {}};
static u3j_harm _mood__hoon__rd_gte_a[] = {{".2", u3wer_gte}, {}};
static u3j_harm _mood__hoon__rd_gth_a[] = {{".2", u3wer_gth}, {}};
static u3j_core _mood__hoon__rd_d[] =
  { { "add", _mood__hoon__rd_add_a },
    { "sub", _mood__hoon__rd_sub_a },
    { "mul", _mood__hoon__rd_mul_a },
    { "div", _mood__hoon__rd_div_a },
    { "sqt", _mood__hoon__rd_sqt_a },
    { "fma", _mood__hoon__rd_fma_a },
    { "lth", _mood__hoon__rd_lth_a },
    { "lte", _mood__hoon__rd_lte_a },
    { "equ", _mood__hoon__rd_equ_a },
    { "gte", _mood__hoon__rd_gte_a },
    { "gth", _mood__hoon__rd_gth_a },
    {}
  };

static u3j_harm _mood__hoon__rs_add_a[] = {{".2", u3wet_add}, {}};
static u3j_harm _mood__hoon__rs_sub_a[] = {{".2", u3wet_sub}, {}};
static u3j_harm _mood__hoon__rs_mul_a[] = {{".2", u3wet_mul}, {}};
static u3j_harm _mood__hoon__rs_div_a[] = {{".2", u3wet_div}, {}};
static u3j_harm _mood__hoon__rs_sqt_a[] = {{".2", u3wet_sqt}, {}};
static u3j_harm _mood__hoon__rs_fma_a[] = {{".2", u3wet_fma}, {}};
static u3j_harm _mood__hoon__rs_lth_a[] = {{".2", u3wet_lth}, {}};
static u3j_harm _mood__hoon__rs_lte_a[] = {{".2", u3wet_lte}, {}};
static u3j_harm _mood__hoon__rs_equ_a[] = {{".2", u3wet_equ}, {}};
static u3j_harm _mood__hoon__rs_gte_a[] = {{".2", u3wet_gte}, {}};
static u3j_harm _mood__hoon__rs_gth_a[] = {{".2", u3wet_gth}, {}};
static u3j_core _mood__hoon__rs_d[] =
  { { "add", _mood__hoon__rs_add_a },
    { "sub", _mood__hoon__rs_sub_a },
    { "mul", _mood__hoon__rs_mul_a },
    { "div", _mood__hoon__rs_div_a },
    { "sqt", _mood__hoon__rs_sqt_a },
    { "fma", _mood__hoon__rs_fma_a },
    { "lth", _mood__hoon__rs_lth_a },
    { "lte", _mood__hoon__rs_lte_a },
    { "equ", _mood__hoon__rs_equ_a },
    { "gte", _mood__hoon__rs_gte_a },
    { "gth", _mood__hoon__rs_gth_a },
    {}
  };

static u3j_harm _mood__hoon__rq_add_a[] = {{".2", u3weq_add}, {}};
static u3j_harm _mood__hoon__rq_sub_a[] = {{".2", u3weq_sub}, {}};
static u3j_harm _mood__hoon__rq_mul_a[] = {{".2", u3weq_mul}, {}};
static u3j_harm _mood__hoon__rq_div_a[] = {{".2", u3weq_div}, {}};
static u3j_harm _mood__hoon__rq_sqt_a[] = {{".2", u3weq_sqt}, {}};
static u3j_harm _mood__hoon__rq_fma_a[] = {{".2", u3weq_fma}, {}};
static u3j_harm _mood__hoon__rq_lth_a[] = {{".2", u3weq_lth}, {}};
static u3j_harm _mood__hoon__rq_lte_a[] = {{".2", u3weq_lte}, {}};
static u3j_harm _mood__hoon__rq_equ_a[] = {{".2", u3weq_equ}, {}};
static u3j_harm _mood__hoon__rq_gte_a[] = {{".2", u3weq_gte}, {}};
static u3j_harm _mood__hoon__rq_gth_a[] = {{".2", u3weq_gth}, {}};
static u3j_core _mood__hoon__rq_d[] =
  { { "add", _mood__hoon__rq_add_a },
    { "sub", _mood__hoon__rq_sub_a },
    { "mul", _mood__hoon__rq_mul_a },
    { "div", _mood__hoon__rq_div_a },
    { "sqt", _mood__hoon__rq_sqt_a },
    { "fma", _mood__hoon__rq_fma_a },
    { "lth", _mood__hoon__rq_lth_a },
    { "lte", _mood__hoon__rq_lte_a },
    { "equ", _mood__hoon__rq_equ_a },
    { "gte", _mood__hoon__rq_gte_a },
    { "gth", _mood__hoon__rq_gth_a },
    {}
  };

static u3j_harm _mood__hoon__coed__ed_puck_a[] = {{".2", u3wee_puck}, {}};
static u3j_harm _mood__hoon__coed__ed_sign_a[] = {{".2", u3wee_sign}, {}};
static u3j_harm _mood__hoon__coed__ed_veri_a[] = {{".2", u3wee_veri}, {}};
static u3j_core _mood__hoon__coed__ed_d[] =
  { { "sign", _mood__hoon__coed__ed_sign_a },
    { "puck", _mood__hoon__coed__ed_puck_a },
    { "veri", _mood__hoon__coed__ed_veri_a },
    {}
  };
static u3j_core _mood__hoon__coed_d[] =
  { { "ed", 0, _mood__hoon__coed__ed_d },
    {}
  };

static u3j_harm _mood__hoon__scr_hsh_a[] = {{".2", u3wes_hsh}, {}};
static u3j_harm _mood__hoon__scr_hsl_a[] = {{".2", u3wes_hsl}, {}};
static u3j_harm _mood__hoon__scr_pbk_a[] = {{".2", u3wes_pbk}, {}};
static u3j_harm _mood__hoon__scr_pbl_a[] = {{".2", u3wes_pbl}, {}};
static u3j_core _mood__hoon__scr_d[] =
  {  { "hsh", _mood__hoon__scr_hsh_a },
     { "hsl", _mood__hoon__scr_hsl_a },
     { "pbk", _mood__hoon__scr_pbk_a },
     { "pbl", _mood__hoon__scr_pbl_a },
     {}
  };

static u3j_harm _mood__hoon_pfix_a[] = {{".2", u3we_pfix}, {}};
static u3j_harm _mood__hoon_plug_a[] = {{".2", u3we_plug}, {}};
static u3j_harm _mood__hoon_pose_a[] = {{".2", u3we_pose}, {}};
static u3j_harm _mood__hoon_sfix_a[] = {{".2", u3we_sfix}, {}};
static u3j_harm _mood__hoon_shax_a[] = {{".2", u3we_shax}, {}};
static u3j_harm _mood__hoon_shay_a[] = {{".2", u3we_shay}, {}};
static u3j_harm _mood__hoon_shas_a[] = {{".2", u3we_shas}, {}};
static u3j_harm _mood__hoon_shal_a[] = {{".2", u3we_shal}, {}};

static u3j_harm _mood__hoon_cell_a[] = {{".2", u3wf_cell}, {}};
static u3j_harm _mood__hoon_comb_a[] = {{".2", u3wf_comb}, {}};
static u3j_harm _mood__hoon_cons_a[] = {{".2", u3wf_cons}, {}};
static u3j_harm _mood__hoon_core_a[] = {{".2", u3wf_core}, {}};
static u3j_harm _mood__hoon_face_a[] = {{".2", u3wf_face}, {}};
static u3j_harm _mood__hoon_fitz_a[] = {{".2", u3wf_fitz}, {}};
static u3j_harm _mood__hoon_flan_a[] = {{".2", u3wf_flan}, {}};
static u3j_harm _mood__hoon_flay_a[] = {{".2", u3wf_flay}, {}};
static u3j_harm _mood__hoon_flip_a[] = {{".2", u3wf_flip}, {}};
static u3j_harm _mood__hoon_flor_a[] = {{".2", u3wf_flor}, {}};
static u3j_harm _mood__hoon_fork_a[] = {{".2", u3wf_fork}, {}};
static u3j_harm _mood__hoon_hike_a[] = {{".2", u3wf_hike}, {}};
static u3j_harm _mood__hoon_look_a[] = {{".2", u3wf_look}, {}};

static u3j_harm _mood__hoon__ut_burn_a[] = {{".2", u3wfu_burn}, {}};
static u3j_harm _mood__hoon__ut_conk_a[] = {{".2", u3wfu_conk}, {}};
static u3j_harm _mood__hoon__ut_crop_a[] = {{".2", u3wfu_crop}, {}};
static u3j_harm _mood__hoon__ut_fire_a[] = {{".2", u3wfu_fire}, {}};
static u3j_harm _mood__hoon__ut_fish_a[] = {{".2", u3wfu_fish}, {}};
static u3j_harm _mood__hoon__ut_fuse_a[] = {{".2", u3wfu_fuse}, {}};
static u3j_harm _mood__hoon__ut_mint_a[] = {{".2", u3wfu_mint}, {}};
static u3j_harm _mood__hoon__ut_mull_a[] = {{".2", u3wfu_mull}, {}};
static u3j_harm _mood__hoon__ut_nest_a[] = {{".2", u3wfu_nest}, {}};
static u3j_harm _mood__hoon__ut_park_a[] = {{".2", u3wfu_park}, {}};
static u3j_harm _mood__hoon__ut_peek_a[] = {{".2", u3wfu_peek}, {}};
static u3j_harm _mood__hoon__ut_play_a[] = {{".2", u3wfu_play}, {}};
static u3j_harm _mood__hoon__ut_rest_a[] = {{".2", u3wfu_rest}, {}};
static u3j_harm _mood__hoon__ut_toss_a[] = {{".2", u3wfu_toss}, {}};
static u3j_harm _mood__hoon__ut_wrap_a[] = {{".2", u3wfu_wrap}, {}};

static u3j_core _mood__hoon__ut_d[] =
  { 
    { "burn", _mood__hoon__ut_burn_a },
    { "conk", _mood__hoon__ut_conk_a },
    { "crop", _mood__hoon__ut_crop_a },
    { "fire", _mood__hoon__ut_fire_a },
    { "fish", _mood__hoon__ut_fish_a },
    { "fuse", _mood__hoon__ut_fuse_a },
    { "mint", _mood__hoon__ut_mint_a },
    { "mull", _mood__hoon__ut_mull_a },
    { "nest", _mood__hoon__ut_nest_a },
    { "park", _mood__hoon__ut_park_a },
    { "peek", _mood__hoon__ut_peek_a },
    { "play", _mood__hoon__ut_play_a },
    { "rest", _mood__hoon__ut_rest_a },
    { "toss", _mood__hoon__ut_toss_a },
    { "wrap", _mood__hoon__ut_wrap_a },
    {}
  };
static u3j_harm _mood__hoon__ut_a[] = 
  { {"burn", u3wfu_burn},
    {"repo", u3wfu_repo},
    {}
  };

#if 0
static u3j_harm _mood__hoon__ap_a[] = 
  { // {"hack", u3wfp_open},
    // {"late", u3wfp_open},
    {"open", u3wfp_open},
    {"rake", u3wfp_rake},
    {}
  };
static u3j_harm _mood__hoon__al_a[] = 
  { {"bunt", u3wfl_bunt},
    {"whip", u3wfl_whip},
    {}
  };
#endif

static u3j_harm _down_mark_a[] = {{".2", u3wg_down, c3y}, {}};
static u3j_core _down_d[] =
  { { "mark", _down_mark_a },
    {}
  };

static u3j_core _arvo_d[] =
  { { "down", 0, _down_d },
    {}
  };

static u3j_core _mood__hoon_d[] = 
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
    { "murn", _mood__hoon_murn_a },
    { "need", _mood__hoon_need_a },
    { "reap", _mood__hoon_reap_a },
    { "reel", _mood__hoon_reel_a },
    { "roll", _mood__hoon_roll_a },
    { "skid", _mood__hoon_skid_a },
    { "skim", _mood__hoon_skim_a },
    { "skip", _mood__hoon_skip_a },
    { "scag", _mood__hoon_scag_a },
    { "slag", _mood__hoon_slag_a },
    { "snag", _mood__hoon_snag_a },
//  { "sort", _mood__hoon_sort_a },
    { "turn", _mood__hoon_turn_a },
    { "weld", _mood__hoon_weld_a },

    { "bex", _mood__hoon_bex_a },
    { "xeb", _mood__hoon_xeb_a },
    { "can", _mood__hoon_can_a },
    { "cap", _mood__hoon_cap_a },
    { "cat", _mood__hoon_cat_a },
    { "con", _mood__hoon_con_a },
    { "cut", _mood__hoon_cut_a },
    { "dis", _mood__hoon_dis_a },
    { "dor", _mood__hoon_dor_a },
    { "dvr", _mood__hoon_dvr_a },
    { "end", _mood__hoon_end_a },
    { "gor", _mood__hoon_gor_a },
    { "hor", _mood__hoon_hor_a },
    { "lsh", _mood__hoon_lsh_a },
    { "mas", _mood__hoon_mas_a },
    { "met", _mood__hoon_met_a },
    { "mix", _mood__hoon_mix_a },
    { "mug", _mood__hoon_mug_a },
    { "peg", _mood__hoon_peg_a },
    { "pow", _mood__hoon_pow_a },
    { "rap", _mood__hoon_rap_a },
    { "rip", _mood__hoon_rip_a },
    { "rsh", _mood__hoon_rsh_a },
    { "sqt", _mood__hoon_sqt_a },
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

    { "cofl", 0, _mood__hoon__cofl_d },
    { "rd", 0, _mood__hoon__rd_d },
    { "rs", 0, _mood__hoon__rs_d },
    { "rq", 0, _mood__hoon__rq_d },
    { "og", 0, _mood__hoon__og_d },
    { "coed", 0, _mood__hoon__coed_d },
    { "scr", 0, _mood__hoon__scr_d },

    { "pfix", _mood__hoon_pfix_a },
    { "plug", _mood__hoon_plug_a },
    { "pose", _mood__hoon_pose_a },
    { "sfix", _mood__hoon_sfix_a },

    { "shax", _mood__hoon_shax_a },
    { "shay", _mood__hoon_shay_a },
    { "shas", _mood__hoon_shas_a },
    { "shal", _mood__hoon_shal_a },

    { "cell", _mood__hoon_cell_a },
    { "comb", _mood__hoon_comb_a },
    { "cons", _mood__hoon_cons_a },
    { "core", _mood__hoon_core_a },
    { "face", _mood__hoon_face_a },
    { "fitz", _mood__hoon_fitz_a },
    { "flan", _mood__hoon_flan_a },
    { "flay", _mood__hoon_flay_a },
    { "flip", _mood__hoon_flip_a },
    { "flor", _mood__hoon_flor_a },
    { "fork", _mood__hoon_fork_a },
    { "hike", _mood__hoon_hike_a },
    { "look", _mood__hoon_look_a },

    // { "ap", _mood__hoon__ap_a },
    // { "al", _mood__hoon__al_a },
    { "ut", _mood__hoon__ut_a, _mood__hoon__ut_d },
    { "arvo", 0, _arvo_d },
#endif
    {}
  };

static u3j_core _mood_d[] =
  { { "hoon", 0, _mood__hoon_d },
    {}
  };

static u3j_core _k161_d[] =
  { { "mood", 0, _mood_d },
    {}
  };

static u3j_core _d[] = {
  { "k161", 0, _k161_d},
  {}
};

u3j_dash 
u3j_Dash = {
  _d,
  0,
  0
};
