/* include/watt/plow.h
**
** This file is in the public domain.
*/
  /** Container types by current nomenclature.
  **/
    typedef u4_log  u4_list;
    typedef u4_bag  u4_pool;
    typedef u4_tab  u4_book;

  /** Noun types.  See kernel for definition.
  **/
    typedef u4_atom u4_axis;
    typedef u4_noun u4_bank;
    typedef u4_noun u4_cord;
    typedef u4_noun u4_door;
    typedef u4_noun u4_gene;
    typedef u4_noun u4_home;
    typedef u4_noun u4_menu;
    typedef u4_noun u4_plan;
    typedef u4_noun u4_plot;
    typedef u4_noun u4_prop; 
    typedef u4_noun u4_rack;
    typedef u4_noun u4_rail;
    typedef u4_noun u4_rope;
    typedef u4_noun u4_seed;
    typedef u4_noun u4_spec;
    typedef u4_noun u4_tack;
    typedef u4_noun u4_term;
    typedef u4_noun u4_type;
    typedef u4_noun u4_tool;
    typedef u4_noun u4_loaf;

  /** Yes and no.  Other stuff.
  **/
    #define u4_yes        u4_noun_0
    #define u4_no         u4_noun_1
    #define u4_nul        u4_noun_0
    #define u4_blip       u4_noun_0

    #define u4_nock_frag  u4_noun_0
    #define u4_nock_bone  u4_noun_1
    #define u4_nock_sail  u4_noun_2
    #define u4_nock_dust  u4_noun_3
    #define u4_nock_vint  u4_noun_4
    #define u4_nock_sing  u4_noun_5
    #define u4_nock_trol  u4_noun_6
    #define u4_nock_flac  u4_noun_7
    #define u4_nock_gant  u4_noun_8
    #define u4_nock_mung  u4_noun_9
    #define u4_nock_germ  u4_noun_10
    #define u4_nock_hint  u4_cod_in(11)
    #define u4_nock_coat  u4_cod_in(12)

    #define u4_axis_0     u4_noun_0
    #define u4_axis_1     u4_noun_1
    #define u4_axis_2     u4_noun_2
    #define u4_axis_3     u4_noun_3
    #define u4_axis_4     u4_noun_4
    #define u4_axis_5     u4_noun_5
    #define u4_axis_6     u4_noun_6
    #define u4_axis_7     u4_noun_7
    #define u4_axis_8     u4_noun_8
    #define u4_axis_9     u4_noun_9
    #define u4_axis_10    u4_noun_10

    #define u4_math_0     u4_noun_0
    #define u4_math_1     u4_noun_1
    #define u4_math_2     u4_noun_2

    #define u4_brut(p, cat, typ) \
      u4_burp(p->lan, cat, _dump_type(p, typ))

    #define u4_so(x)      u4_n_zero(x)
    #define u4_say(x)     ( (x) ? u4_noun_0 : u4_noun_1 )
    #define u4_and(x, y)  ( (u4_so(x) && u4_so(y)) ? u4_yes : u4_no )
    #define u4_or(x, y)   ( (u4_so(x) || u4_so(y)) ? u4_yes : u4_no )

    #define u4_kc(l, p, q)        u4_k_cell(l, p, q)
    #define u4_kt(l, p, q, r)     u4_k_trel(l, p, q, r)
    #define u4_kq(l, p, q, r, s)  u4_k_qual(l, p, q, r, s)
    #define u4_ku(l, n)           u4_k_cell(l, u4_noun_0, n)
    #define u4_kl(l, n)           u4_k_cell(l, n, u4_noun_0)

    /* The main plow structure.
    */
      typedef struct _u4_plow {
        /* For all internal allocation.
        */
        u4_lane lan;

        /* Internal counters for perf testing.
        */
        uint32_t prf;
        uint32_t prg;
        uint32_t prh;
        uint32_t pri;

        /* Set [*type *gene] in repo.
        */
        u4_pool fan;

        /* Set [*type] in verify.
        */
        u4_pool ver;

        /* Debug depth.
        */
        u4_atom bug;

        /* Trap - *(list &[p=*text q=*spot])
        */
        u4_noun meb;

        /* Book to memoize nest.
        */
        u4_book vus;

        /* Book to memoize null.
        */
        u4_book tyc;

        /* Book to memoize orth.
        */
        u4_book gam;

        /* Book to memoize show.
        */
        u4_book hos;

        /* Book to memoize play.
        */
        u4_book zor;

        /* Book to memoize make.
        */
        u4_book niq;

        /* Book to memoize safe.
        */
        u4_book fac;

        /* Book to memoize fine.
        */
        u4_book vom;

        /* Book to memoize open.
        */
        u4_book pon;

        /* Book to memoize find.
        */
        u4_book fin;

        /* Book to memoize half.
        */
        u4_book huf;
      } 
        *u4_plow;

  /** *:plow
  **/
    void
    _plow_trap(u4_plow p,
               const char *msg);

    u4_noun
    _plow_fail(u4_plow p,
               const char *msg);
    void
    u4_plow_init(u4_plow p,
                 u4_lane lan);

    u4_noun u4_plow_make(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_play(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_show(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_pass(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_shop(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_wish(u4_lane lan, u4_type sub, u4_gene gen);
    u4_noun u4_plow_mill(u4_lane lan, u4_type sub, u4_gene gen);

    u4_rope
    _plow_rake(u4_plow p,
               u4_gene gen);

  /** *:dump
  **/
    u4_prep
    _dump_durb(u4_plow p,
               u4_rail bar);
    u4_prep
    _dump_type(u4_plow p,
               u4_type typ);
    u4_atom
    _dump_size(u4_lane lan, 
               u4_noun box);

  /** *:gull
  **/
    u4_atom
    _gull_size(u4_plow p,
               u4_book dab);
    u4_unit
    _gull_look(u4_plow p,
               u4_spec dab,
               u4_term cog);
    u4_spec
    _gull_fill(u4_plow p,
               u4_spec dab,
               u4_menu sem);
  /** *:iris
  **/
    u4_type
    _iris_burn(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_tack tac);
    u4_flag
    _iris_cull(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe);
    u4_plan
    _iris_find(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_term cog);
    u4_tool
    _iris_fish(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe);
    u4_type
    _iris_half(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_axis had);
    u4_flag
    _iris_nest(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_type bon);
    u4_type
    _iris_peek(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_axis bow);
    void
    _iris_slip(u4_plow p,
               u4_type *sub,
               u4_rail *bar,
               u4_axis *axe,
               u4_axis had);
    u4_type
    _iris_snap(u4_plow p,
               u4_type sub,
               u4_rail bar,
               u4_axis axe,
               u4_type bon);
  /** *:lark
  **/
    u4_tack
    _lark_cram(u4_plow p,
               u4_type sub,
               u4_tack tac,
               u4_tack les);
    u4_tack
    _lark_feed(u4_plow p,
               u4_type sub,
               u4_tack tac,
               u4_list mut);
    u4_tack
    _lark_nuke(u4_plow p,
               u4_type sub,
               u4_tack tac,
               u4_rope rop,
               u4_type feg);
  /** *:lily
  **/
    u4_tool
    _lily_cons(u4_plow p,
               u4_tool vor,
               u4_tool sed);
    u4_tool
    _lily_comb(u4_plow p,
               u4_tool mal,
               u4_tool buz);
    u4_tool
    _lily_flan(u4_plow p,
               u4_tool zeg,
               u4_tool dac);
    u4_tool
    _lily_flor(u4_plow p,
               u4_tool bos,
               u4_tool nif);
    u4_tool
    _lily_flop(u4_plow p,
              u4_tool zet);
    u4_tool
    _lily_hike(u4_plow p,
               u4_axis axe,
               u4_list vix);
  /** *:open
  **/
    /* open:plow
    */
    u4_gene
    _plow_open(u4_plow p,
               u4_gene gen);
  /** *:rose
  **/
    u4_flag
    _rose_fine(u4_plow p,
               u4_type sub);
    u4_type
    _rose_edit(u4_plow p,
               u4_type sub,
               u4_list mut);
    u4_type
    _rose_gain(u4_plow p,
               u4_type sub,
               u4_gene gen);
    u4_type
    _rose_both(u4_plow p,
               u4_type sub,
               u4_type hoc);
    u4_type
    _rose_eith(u4_plow p,
               u4_type sub,
               u4_type hoc);
    u4_tool
    _rose_make(u4_plow p,
               u4_type sub,
               u4_gene gen);
    u4_flag
    _rose_null(u4_plow p,
               u4_type sub);
    u4_flag
    _rose_orth(u4_plow p,
               u4_type sub,
               u4_type ref);
    u4_type
    _rose_play(u4_plow p,
               u4_type sub,
               u4_gene gen);
    u4_type
    _rose_repo(u4_plow p,
               u4_type sub,
               u4_gene gen);
    u4_flag
    _rose_safe(u4_plow p,
               u4_type sub,
               u4_plot mut);
    u4_plan
    _rose_seek(u4_plow p,
               u4_type sub,
               u4_gene gen);
    u4_flag
    _rose_show(u4_plow p,
               u4_type sub,
               u4_gene gen);
