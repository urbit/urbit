/* j/dash.c
**
** This file is in the public domain.
*/
#include "all.h"

static u3_cs_harm _add_a[] = {{".2", u3_cwa_add}, {}};
static u3_cs_harm _dec_a[] = {{".2", u3_cwa_dec}, {}};
static u3_cs_harm _div_a[] = {{".2", u3_cwa_div}, {}};
static u3_cs_harm _gte_a[] = {{".2", u3_cwa_gte}, {}};
static u3_cs_harm _gth_a[] = {{".2", u3_cwa_gth}, {}};
static u3_cs_harm _lte_a[] = {{".2", u3_cwa_lte}, {}};
static u3_cs_harm _lth_a[] = {{".2", u3_cwa_lth}, {}};
static u3_cs_harm _mod_a[] = {{".2", u3_cwa_mod}, {}};
static u3_cs_harm _mul_a[] = {{".2", u3_cwa_mul}, {}};
static u3_cs_harm _sub_a[] = {{".2", u3_cwa_sub}, {}};

static u3_cs_harm _bind_a[] = {{".2", u3_cwb_bind}, {}};
static u3_cs_harm _clap_a[] = {{".2", u3_cwb_clap}, {}};
static u3_cs_harm _drop_a[] = {{".2", u3_cwb_drop}, {}};
static u3_cs_harm _flop_a[] = {{".2", u3_cwb_flop}, {}};
static u3_cs_harm _lent_a[] = {{".2", u3_cwb_lent}, {}};
static u3_cs_harm _levy_a[] = {{".2", u3_cwb_levy}, {}};
static u3_cs_harm _lien_a[] = {{".2", u3_cwb_lien}, {}};
static u3_cs_harm _need_a[] = {{".2", u3_cwb_need}, {}};
static u3_cs_harm _reel_a[] = {{".2", u3_cwb_reel}, {}};
static u3_cs_harm _roll_a[] = {{".2", u3_cwb_roll}, {}};
static u3_cs_harm _skim_a[] = {{".2", u3_cwb_skim}, {}};
static u3_cs_harm _skip_a[] = {{".2", u3_cwb_skip}, {}};
static u3_cs_harm _scag_a[] = {{".2", u3_cwb_scag}, {}};
static u3_cs_harm _slag_a[] = {{".2", u3_cwb_slag}, {}};
static u3_cs_harm _snag_a[] = {{".2", u3_cwb_snag}, {}};
static u3_cs_harm _sort_a[] = {{".2", u3_cwb_sort}, {}};
static u3_cs_harm _turn_a[] = {{".2", u3_cwb_turn}, {}};
static u3_cs_harm _weld_a[] = {{".2", u3_cwb_weld}, {}};

static u3_cs_harm _bex_a[] = {{".2", u3_cwc_bex}, {}};
static u3_cs_harm _can_a[] = {{".2", u3_cwc_can}, {}};
static u3_cs_harm _cap_a[] = {{".2", u3_cwc_cap}, {}};
static u3_cs_harm _cat_a[] = {{".2", u3_cwc_cat}, {}};
static u3_cs_harm _con_a[] = {{".2", u3_cwc_con}, {}};
static u3_cs_harm _cut_a[] = {{".2", u3_cwc_cut}, {}};
static u3_cs_harm _dis_a[] = {{".2", u3_cwc_dis}, {}};
static u3_cs_harm _dor_a[] = {{".2", u3_cwc_dor}, {}};
static u3_cs_harm _end_a[] = {{".2", u3_cwc_end}, {}};
static u3_cs_harm _gor_a[] = {{".2", u3_cwc_gor}, {}};
static u3_cs_harm _hor_a[] = {{".2", u3_cwc_hor}, {}};
static u3_cs_harm _lsh_a[] = {{".2", u3_cwc_lsh}, {}};
static u3_cs_harm _mas_a[] = {{".2", u3_cwc_mas}, {}};
static u3_cs_harm _met_a[] = {{".2", u3_cwc_met}, {}};
static u3_cs_harm _mix_a[] = {{".2", u3_cwc_mix}, {}};
static u3_cs_harm _peg_a[] = {{".2", u3_cwc_peg}, {}};
static u3_cs_harm _rap_a[] = {{".2", u3_cwc_rap}, {}};
static u3_cs_harm _rip_a[] = {{".2", u3_cwc_rip}, {}};
static u3_cs_harm _rsh_a[] = {{".2", u3_cwc_rsh}, {}};
static u3_cs_harm _vor_a[] = {{".2", u3_cwc_vor}, {}};

static u3_cs_harm _cue_a[] = {{".2", u3_cwe_cue}, {}};
static u3_cs_harm _jam_a[] = {{".2", u3_cwe_jam}, {}};
static u3_cs_harm _mat_a[] = {{".2", u3_cwe_mat}, {}};
static u3_cs_harm _rub_a[] = {{".2", u3_cwe_rub}, {}};
static u3_cs_harm _lore_a[] = {{".2", u3_cwe_lore}, {}};
static u3_cs_harm _loss_a[] = {{".2", u3_cwe_loss}, {}};
static u3_cs_harm _mink_a[] = {{".2", u3_cwe_mink}, {}};
static u3_cs_harm _mule_a[] = {{".2", u3_cwe_mule}, {}};
static u3_cs_harm _repg_a[] = {{".2", u3_cwe_repg}, {}};
static u3_cs_harm _rexp_a[] = {{".2", u3_cwe_rexp}, {}};
static u3_cs_harm _trip_a[] = {{".2", u3_cwe_trip}, {}};

static u3_cs_harm _pfix_a[] = {{".2", u3_cwe_pfix}, {}};
static u3_cs_harm _plug_a[] = {{".2", u3_cwe_plug}, {}};
static u3_cs_harm _pose_a[] = {{".2", u3_cwe_pose}, {}};
static u3_cs_harm _sfix_a[] = {{".2", u3_cwe_sfix}, {}};
static u3_cs_harm _shax_a[] = {{".2", u3_cwe_shax}, {}};
static u3_cs_harm _shas_a[] = {{".2", u3_cwe_shas}, {}};
static u3_cs_harm _shal_a[] = {{".2", u3_cwe_shal}, {}};

static u3_cs_harm _bull_a[] = {{".2", u3_cwf_bull}, {}};
static u3_cs_harm _cell_a[] = {{".2", u3_cwf_cell}, {}};
static u3_cs_harm _comb_a[] = {{".2", u3_cwf_comb}, {}};
static u3_cs_harm _cons_a[] = {{".2", u3_cwf_cons}, {}};
static u3_cs_harm _core_a[] = {{".2", u3_cwf_core}, {}};
static u3_cs_harm _cube_a[] = {{".2", u3_cwf_cube}, {}};
static u3_cs_harm _face_a[] = {{".2", u3_cwf_face}, {}};
static u3_cs_harm _fitz_a[] = {{".2", u3_cwf_fitz}, {}};
static u3_cs_harm _flan_a[] = {{".2", u3_cwf_flan}, {}};
static u3_cs_harm _flay_a[] = {{".2", u3_cwf_flay}, {}};
static u3_cs_harm _flip_a[] = {{".2", u3_cwf_flip}, {}};
static u3_cs_harm _flor_a[] = {{".2", u3_cwf_flor}, {}};
static u3_cs_harm _fork_a[] = {{".2", u3_cwf_fork}, {}};
static u3_cs_harm _hike_a[] = {{".2", u3_cwf_hike}, {}};
static u3_cs_harm _look_a[] = {{".2", u3_cwf_look}, {}};

static u3_cs_core _mood__hoon_d[] = 
  { { "add", _add_a },
    { "dec", _dec_a },
    { "div", _div_a },
    { "gte", _gte_a },
    { "gth", _gth_a },
    { "lte", _lte_a },
    { "lth", _lth_a },
    { "mod", _mod_a },
    { "mul", _mul_a },
    { "sub", _sub_a },

    { "bind", _bind_a },
    { "clap", _clap_a },
    { "drop", _drop_a },
    { "flop", _flop_a },
    { "lent", _lent_a },
    { "levy", _levy_a },
    { "lien", _lien_a },
    { "need", _need_a },
    { "reel", _reel_a },
    { "roll", _roll_a },
    { "skim", _skim_a },
    { "skip", _skip_a },
    { "scag", _scag_a },
    { "slag", _slag_a },
    { "snag", _snag_a },
    { "sort", _sort_a },
    { "turn", _turn_a },
    { "weld", _weld_a },

    { "bex", _bex_a },
    { "can", _can_a },
    { "cap", _cap_a },
    { "cat", _cat_a },
    { "con", _con_a },
    { "cut", _cut_a },
    { "dis", _dis_a },
    { "dor", _dor_a },
    { "end", _end_a },
    { "gor", _gor_a },
    { "hor", _hor_a },
    { "lsh", _lsh_a },
    { "mas", _mas_a },
    { "met", _met_a },
    { "mix", _mix_a },
    { "peg", _peg_a },
    { "rap", _rap_a },
    { "rip", _rip_a },
    { "rsh", _rsh_a },
    { "vor", _vor_a },

    { "cue", _cue_a },
    { "jam", _jam_a },
    { "mat", _mat_a },
    { "rub", _rub_a },
    { "lore", _lore_a },
    { "loss", _loss_a },
    { "mink", _mink_a },
    { "mule", _mule_a },
    { "repg", _repg_a },
    { "rexp", _rexp_a },
    { "trip", _trip_a },

    { "pfix", _pfix_a },
    { "plug", _plug_a },
    { "pose", _pose_a },
    { "sfix", _sfix_a },

    { "shax", _shax_a },
    { "shas", _shas_a },
    { "shal", _shal_a },

    { "bull", _bull_a },
    { "cell", _cell_a },
    { "comb", _comb_a },
    { "cons", _cons_a },
    { "core", _core_a },
    { "cube", _cube_a },
    { "face", _face_a },
    { "fitz", _fitz_a },
    { "flan", _flan_a },
    { "flay", _flay_a },
    { "flip", _flip_a },
    { "flor", _flor_a },
    { "fork", _fork_a },
    { "hike", _hike_a },
    { "look", _look_a },
    {}
  };

static u3_cs_core _mood_d[] =
  { { "hoon", 0, _mood__hoon_d },
    {}
  };

static u3_cs_core _k164_d[] =
  { { "mood", 0, _mood_d },
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
