/* j/dash.c
**
** This file is in the public domain.
*/
#include "all.h"

static u3_cs_harm _k164__add_a[] = {{".2", u3_cwa_add}, {}};
static u3_cs_harm _k164__dec_a[] = {{".2", u3_cwa_dec}, {}};
static u3_cs_harm _k164__div_a[] = {{".2", u3_cwa_div}, {}};
static u3_cs_harm _k164__gte_a[] = {{".2", u3_cwa_gte}, {}};
static u3_cs_harm _k164__gth_a[] = {{".2", u3_cwa_gth}, {}};
static u3_cs_harm _k164__lte_a[] = {{".2", u3_cwa_lte}, {}};
static u3_cs_harm _k164__lth_a[] = {{".2", u3_cwa_lth}, {}};
static u3_cs_harm _k164__mod_a[] = {{".2", u3_cwa_mod}, {}};
static u3_cs_harm _k164__mul_a[] = {{".2", u3_cwa_mul}, {}};
static u3_cs_harm _k164__sub_a[] = {{".2", u3_cwa_sub}, {}};

static u3_cs_harm _k164__bind_a[] = {{".2", u3_cwb_bind}, {}};
static u3_cs_harm _k164__clap_a[] = {{".2", u3_cwb_clap}, {}};
static u3_cs_harm _k164__drop_a[] = {{".2", u3_cwb_drop}, {}};
static u3_cs_harm _k164__flop_a[] = {{".2", u3_cwb_flop}, {}};
static u3_cs_harm _k164__lent_a[] = {{".2", u3_cwb_lent}, {}};
static u3_cs_harm _k164__levy_a[] = {{".2", u3_cwb_levy}, {}};
static u3_cs_harm _k164__lien_a[] = {{".2", u3_cwb_lien}, {}};
static u3_cs_harm _k164__need_a[] = {{".2", u3_cwb_need}, {}};
static u3_cs_harm _k164__reel_a[] = {{".2", u3_cwb_reel}, {}};
static u3_cs_harm _k164__roll_a[] = {{".2", u3_cwb_roll}, {}};
static u3_cs_harm _k164__skim_a[] = {{".2", u3_cwb_skim}, {}};
static u3_cs_harm _k164__skip_a[] = {{".2", u3_cwb_skip}, {}};
static u3_cs_harm _k164__scag_a[] = {{".2", u3_cwb_scag}, {}};
static u3_cs_harm _k164__slag_a[] = {{".2", u3_cwb_slag}, {}};
static u3_cs_harm _k164__snag_a[] = {{".2", u3_cwb_snag}, {}};
static u3_cs_harm _k164__sort_a[] = {{".2", u3_cwb_sort}, {}};
static u3_cs_harm _k164__turn_a[] = {{".2", u3_cwb_turn}, {}};
static u3_cs_harm _k164__weld_a[] = {{".2", u3_cwb_weld}, {}};

static u3_cs_harm _k164__bex_a[] = {{".2", u3_cwc_bex}, {}};
static u3_cs_harm _k164__can_a[] = {{".2", u3_cwc_can}, {}};
static u3_cs_harm _k164__cap_a[] = {{".2", u3_cwc_cap}, {}};
static u3_cs_harm _k164__cat_a[] = {{".2", u3_cwc_cat}, {}};
static u3_cs_harm _k164__con_a[] = {{".2", u3_cwc_con}, {}};
static u3_cs_harm _k164__cut_a[] = {{".2", u3_cwc_cut}, {}};
static u3_cs_harm _k164__dis_a[] = {{".2", u3_cwc_dis}, {}};
static u3_cs_harm _k164__dor_a[] = {{".2", u3_cwc_dor}, {}};
static u3_cs_harm _k164__end_a[] = {{".2", u3_cwc_end}, {}};
static u3_cs_harm _k164__gor_a[] = {{".2", u3_cwc_gor}, {}};
static u3_cs_harm _k164__hor_a[] = {{".2", u3_cwc_hor}, {}};
static u3_cs_harm _k164__lsh_a[] = {{".2", u3_cwc_lsh}, {}};
static u3_cs_harm _k164__mas_a[] = {{".2", u3_cwc_mas}, {}};
static u3_cs_harm _k164__met_a[] = {{".2", u3_cwc_met}, {}};
static u3_cs_harm _k164__mix_a[] = {{".2", u3_cwc_mix}, {}};
static u3_cs_harm _k164__peg_a[] = {{".2", u3_cwc_peg}, {}};
static u3_cs_harm _k164__rap_a[] = {{".2", u3_cwc_rap}, {}};
static u3_cs_harm _k164__rip_a[] = {{".2", u3_cwc_rip}, {}};
static u3_cs_harm _k164__rsh_a[] = {{".2", u3_cwc_rsh}, {}};
static u3_cs_harm _k164__vor_a[] = {{".2", u3_cwc_vor}, {}};

static u3_cs_harm _k164__cue_a[] = {{".2", u3_cwe_cue}, {}};
static u3_cs_harm _k164__jam_a[] = {{".2", u3_cwe_jam}, {}};
static u3_cs_harm _k164__mat_a[] = {{".2", u3_cwe_mat}, {}};
static u3_cs_harm _k164__rub_a[] = {{".2", u3_cwe_rub}, {}};
static u3_cs_harm _k164__lore_a[] = {{".2", u3_cwe_lore}, {}};
static u3_cs_harm _k164__loss_a[] = {{".2", u3_cwe_loss}, {}};
static u3_cs_harm _k164__mink_a[] = {{".2", u3_cwe_mink}, {}};
static u3_cs_harm _k164__mule_a[] = {{".2", u3_cwe_mule}, {}};
static u3_cs_harm _k164__repg_a[] = {{".2", u3_cwe_repg}, {}};
static u3_cs_harm _k164__rexp_a[] = {{".2", u3_cwe_rexp}, {}};
static u3_cs_harm _k164__trip_a[] = {{".2", u3_cwe_trip}, {}};

static u3_cs_harm _k164__pfix_a[] = {{".2", u3_cwe_pfix}, {}};
static u3_cs_harm _k164__plug_a[] = {{".2", u3_cwe_plug}, {}};
static u3_cs_harm _k164__pose_a[] = {{".2", u3_cwe_pose}, {}};
static u3_cs_harm _k164__sfix_a[] = {{".2", u3_cwe_sfix}, {}};
static u3_cs_harm _k164__shax_a[] = {{".2", u3_cwe_shax}, {}};
static u3_cs_harm _k164__shas_a[] = {{".2", u3_cwe_shas}, {}};
static u3_cs_harm _k164__shal_a[] = {{".2", u3_cwe_shal}, {}};

static u3_cs_harm _k164__bull_a[] = {{".2", u3_cwf_bull}, {}};
static u3_cs_harm _k164__cell_a[] = {{".2", u3_cwf_cell}, {}};
static u3_cs_harm _k164__comb_a[] = {{".2", u3_cwf_comb}, {}};
static u3_cs_harm _k164__cons_a[] = {{".2", u3_cwf_cons}, {}};
static u3_cs_harm _k164__core_a[] = {{".2", u3_cwf_core}, {}};
static u3_cs_harm _k164__cube_a[] = {{".2", u3_cwf_cube}, {}};
static u3_cs_harm _k164__face_a[] = {{".2", u3_cwf_face}, {}};
static u3_cs_harm _k164__fitz_a[] = {{".2", u3_cwf_fitz}, {}};
static u3_cs_harm _k164__flan_a[] = {{".2", u3_cwf_flan}, {}};
static u3_cs_harm _k164__flay_a[] = {{".2", u3_cwf_flay}, {}};
static u3_cs_harm _k164__flip_a[] = {{".2", u3_cwf_flip}, {}};
static u3_cs_harm _k164__flor_a[] = {{".2", u3_cwf_flor}, {}};
static u3_cs_harm _k164__fork_a[] = {{".2", u3_cwf_fork}, {}};
static u3_cs_harm _k164__hike_a[] = {{".2", u3_cwf_hike}, {}};
static u3_cs_harm _k164__look_a[] = {{".2", u3_cwf_look}, {}};

static u3_cs_core _k164__mood__hoon_d[] = 
  { { "add", _k164__add_a },
    { "dec", _k164__dec_a },
    { "div", _k164__div_a },
    { "gte", _k164__gte_a },
    { "gth", _k164__gth_a },
    { "lte", _k164__lte_a },
    { "lth", _k164__lth_a },
    { "mod", _k164__mod_a },
    { "mul", _k164__mul_a },
    { "sub", _k164__sub_a },

    { "bind", _k164__bind_a },
    { "clap", _k164__clap_a },
    { "drop", _k164__drop_a },
    { "flop", _k164__flop_a },
    { "lent", _k164__lent_a },
    { "levy", _k164__levy_a },
    { "lien", _k164__lien_a },
    { "need", _k164__need_a },
    { "reel", _k164__reel_a },
    { "roll", _k164__roll_a },
    { "skim", _k164__skim_a },
    { "skip", _k164__skip_a },
    { "scag", _k164__scag_a },
    { "slag", _k164__slag_a },
    { "snag", _k164__snag_a },
    { "sort", _k164__sort_a },
    { "turn", _k164__turn_a },
    { "weld", _k164__weld_a },

    { "bex", _k164__bex_a },
    { "can", _k164__can_a },
    { "cap", _k164__cap_a },
    { "cat", _k164__cat_a },
    { "con", _k164__con_a },
    { "cut", _k164__cut_a },
    { "dis", _k164__dis_a },
    { "dor", _k164__dor_a },
    { "end", _k164__end_a },
    { "gor", _k164__gor_a },
    { "hor", _k164__hor_a },
    { "lsh", _k164__lsh_a },
    { "mas", _k164__mas_a },
    { "met", _k164__met_a },
    { "mix", _k164__mix_a },
    { "peg", _k164__peg_a },
    { "rap", _k164__rap_a },
    { "rip", _k164__rip_a },
    { "rsh", _k164__rsh_a },
    { "vor", _k164__vor_a },

    { "cue", _k164__cue_a },
    { "jam", _k164__jam_a },
    { "mat", _k164__mat_a },
    { "rub", _k164__rub_a },
    { "lore", _k164__lore_a },
    { "loss", _k164__loss_a },
    { "mink", _k164__mink_a },
    { "mule", _k164__mule_a },
    { "repg", _k164__repg_a },
    { "rexp", _k164__rexp_a },
    { "trip", _k164__trip_a },

    { "pfix", _k164__pfix_a },
    { "plug", _k164__plug_a },
    { "pose", _k164__pose_a },
    { "sfix", _k164__sfix_a },

    { "shax", _k164__shax_a },
    { "shas", _k164__shas_a },
    { "shal", _k164__shal_a },

    { "bull", _k164__bull_a },
    { "cell", _k164__cell_a },
    { "comb", _k164__comb_a },
    { "cons", _k164__cons_a },
    { "core", _k164__core_a },
    { "cube", _k164__cube_a },
    { "face", _k164__face_a },
    { "fitz", _k164__fitz_a },
    { "flan", _k164__flan_a },
    { "flay", _k164__flay_a },
    { "flip", _k164__flip_a },
    { "flor", _k164__flor_a },
    { "fork", _k164__fork_a },
    { "hike", _k164__hike_a },
    { "look", _k164__look_a },
    {}
  };

static u3_cs_core _k164__mood_d[] =
  { { "hoon", 0, _k164__mood__hoon_d },
    {}
  };

static u3_cs_core _k164_d[] =
  { { "mood", 0, _k164__mood_d },
    {}
  };

static u3_cs_core _d[] = {
  { "k164", 0, _k164_d},
  {}
};

u3_cs_dash 
u3_Dash = {
  _d,
  0,
  0
};
