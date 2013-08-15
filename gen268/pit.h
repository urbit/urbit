/* include/pit.h
**
** This file is in the public domain.
*/
  /** Revision number for built-in jets.
  **/
#   define  Pit   watt_268

  /** Test codes.
  **/
#   define  Tier1         u2_jet_live
#   define  Tier2         u2_jet_live
#   define  Tier3         u2_jet_live 
#   define  Tier4         u2_jet_live
#   define  Tier5         u2_jet_live
#   define  Tier6         u2_jet_live
#   define  Tier6_a       Tier6
#   define  Tier6_b       Tier6
// #   define  Tier6_b       (u2_jet_live | u2_jet_test)
// #   define  Tier6_b       u2_jet_dead
#   define  Tier6_c       Tier6
#   define  Tier6_t       (u2_jet_live | u2_jet_test)
#   define  Tier6_l       (u2_jet_live | u2_jet_test | u2_jet_leak)
#   define  Tier6_l_memo  Tier6_l

#   define  Tier5_test    (Tier5 | u2_jet_test)
#   define  Tier6_a_memo  (Tier6_a | u2_jet_memo)
#   define  Tier6_b_memo  (Tier6_b | u2_jet_memo)
#   define  Tier6_c_memo  (Tier6_c | u2_jet_memo)
#   define  Tier6_x_memo  (Tier6_x)
#   define  Tier6_t_memo  (Tier6_t | u2_jet_memo)
#   define  Tier6_b_test  (Tier6_b | u2_jet_test)

  /** Cosmetic noun types.
  **/
#if 0
  // get them from funj.h for now
  //
    typedef u2_noun u2_bank;
    typedef u2_noun u2_cord;
    typedef u2_noun u2_door;
    typedef u2_noun u2_gene;
    typedef u2_noun u2_home;
    typedef u2_noun u2_init;
    typedef u2_noun u2_menu;
    typedef u2_noun u2_plan;
    typedef u2_noun u2_plot;
    typedef u2_noun u2_rack;
    typedef u2_noun u2_rung;
    typedef u2_noun u2_rope;
    typedef u2_noun u2_spec;
    typedef u2_noun u2_tack;
    typedef u2_noun u2_type;
    typedef u2_noun u2_prep;
    typedef u2_noun u2_dump;
#endif

  /** Direct C interfaces.
  **/
    /** Tier 1.
    **/
      u2_weak                                                     //  transfer
      j2_mbc(Pit, add)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, bex)(u2_wire wir_r, 
                       u2_atom a);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, dec)(u2_wire wir_r, 
                       u2_atom a);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, div)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, gte)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, gth)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, lte)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, lth)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, mul)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, sub)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
    /** Tier 2.
    **/
      u2_weak                                                     //  transfer
      j2_mbc(Pit, bind)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, clap)(u2_wire wir_r,  
                        u2_noun a,                                //  retain
                        u2_noun b,                                //  retain
                        u2_noun c);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, drop)(u2_wire wir_r, 
                        u2_noun a);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, flop)(u2_wire wir_r, 
                        u2_noun a);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pit, lent)(u2_wire wir_r, 
                        u2_noun a);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, levy)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, lien)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, need)(u2_wire wir_r, 
                        u2_noun a);                               //  retain


      u2_weak                                                     //  transfer
      j2_mbc(Pit, reel)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, rest)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun leg);                         //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, roll)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, skim)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, skip)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, snag)(u2_wire wir_r, 
                        u2_atom a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, sort)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, turn)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, weld)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

    /** Tier 3.
    **/
      u2_weak                                                     //  transfer
      j2_mbc(Pit, cap)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, cat)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, con)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, cut)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c,                                 //  retain
                       u2_atom d);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, dor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, dis)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain


      u2_weak                                                     //  transfer
      j2_mbc(Pit, end)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, gor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, hor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, lsh)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, mas)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, met)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, mix)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, peg)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, rap)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_noun b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, rip)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, rsh)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pit, vor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
  
    /** Tier 4.
    **/
      u2_weak                                                     //  transfer
      j2_mcc(Pit, in, put)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pit, in, gas)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_flag 
      j2_mcc(Pit, in, has)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer 
      j2_mcc(Pit, in, tap)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pit, by, put)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b,                             //  retain
                           u2_noun c);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pit, by, gas)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain
    /** Tier 5.
    **/
      u2_noun                                                     //  transfer
      j2_mcy(Pit, to, fly)(u2_wire wir_r,
                           u2_noun pup,                           //  retain
                           u2_noun bor);                          //  retain

      u2_atom                                                     //  transfer
      j2_mcy(Pit, to, ram)(u2_wire wir_r,
                           u2_noun pup);                          //  retain

      u2_atom                                                     //  transfer
      j2_mcy(Pit, to, wit)(u2_wire wir_r,
                           u2_noun pup);                          //  retain

    /** Tier 6.
    **/
      u2_noun                                                     //  transfer
      j2_mby(Pit, cell)(u2_wire wir_r, 
                        u2_noun hed,                              //  retain
                        u2_noun tal);                             //  retain
      u2_noun                                                     //  transfer
      j2_mbc(Pit, comb)(u2_wire wir_r, 
                        u2_noun mal,                              //  retain
                        u2_noun buz);                             //  retain
      u2_noun                                                     //  transfer
      j2_mby(Pit, cons)(u2_wire wir_r, 
                        u2_noun vur,                              //  retain
                        u2_noun sed);                             //  retain
      u2_noun                                                     //  transfer
      j2_mby(Pit, core)(u2_wire wir_r, 
                        u2_noun pac,                              //  retain
                        u2_noun con);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, fork)(u2_wire wir_r, 
                        u2_noun hoz,                              //  retain
                        u2_noun bur);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, face)(u2_wire wir_r, 
                        u2_noun cog,                              //  retain
                        u2_noun tip);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, flip)(u2_wire wir_r, 
                        u2_noun hel);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, flor)(u2_wire wir_r, 
                        u2_noun bos,                              //  retain
                        u2_noun nif);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, flan)(u2_wire wir_r,
                        u2_noun bos,                              //  retain
                        u2_noun nif);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, hack)(u2_wire wir_r, 
                        u2_noun gen);                             //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, heal)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun qog,                          //  retain
                            u2_noun axe,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, hike)(u2_wire wir_r, 
                        u2_noun axe,                              //  retain
                        u2_noun pac);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pit, look)(u2_wire wir_r, 
                        u2_noun cog,                              //  retain
                        u2_noun dab);                             //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ap, open)(u2_wire wir_r,
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ap, rake)(u2_wire wir_r,
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, bake)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun dab);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, burn)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, cull)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_atom axe,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, edit)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun mun,                          //  retain
                            u2_noun har);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, emit)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_atom axe,                          //  retain
                            u2_noun mun,                          //  retain
                            u2_noun har);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, fern)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun dox,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, find)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun cog);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, fire)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun hag);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, fish)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_atom axe);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, fret)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun dox,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, fuse)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, gain)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, mint)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gol,                          //  retain
                            u2_noun gen);                         //  retain
      
      u2_flag                                                     //  transfer
      j2_mcy(Pit, ut, nest)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_flag                                                     //  transfer
      j2_mcy(Pit, ut, orth)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, park)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun axe);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, peek)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun axe);                         //  retain
      
      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, play)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, repo)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, seek)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,
                            u2_noun hep);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, snap)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, tuck)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun rup,                          //  retain
                            u2_noun boz,                          //  retain
                            u2_noun rix);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pit, ut, tusk)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun rup,                          //  retain
                            u2_noun boz,                          //  retain
                            u2_noun rix);                         //  retain

    /** Direct ut axes.
    **/
#     define j2_ut_van_fan  20
#     define j2_ut_van_rib  42
#     define j2_ut_van_vrf  43
#     define j2_ut_van_vet  86
#     define j2_ut_van_fab  87

    /** Debugging hacks.
    **/
      void
      j2_mby(Pit, type)(u2_wire  wir_r,
                        u2_noun  typ);                            //  retain
      void
      j2_mcy(Pit, ut, dupt)(u2_wire     wir_r,
                            u2_noun     van,                      //  retain
                            const c3_c* cap_c,
                            u2_noun     typ);                     //  retain
