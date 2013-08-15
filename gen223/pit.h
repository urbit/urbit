/* include/pit.h
**
** This file is in the public domain.
*/
  /** Driver control strings for each tier.
  **/
#   define Pt0           k_223
#   define Pt1           k_223__a
#   define Pt2           k_223__a__b
#   define Pt3           k_223__a__b__c
#   define Pt4           k_223__a__b__c__d
#   define Pt5           k_223__a__b__c__d__e
#   define Pt6           k_223__a__b__c__d__e__f

  /** Test codes.
  **/
#   define  Tier1         u2_jet_live
#   define  Tier2         u2_jet_live
#   define  Tier3         u2_jet_live 
#   define  Tier4         u2_jet_live
#   define  Tier5         u2_jet_live
//#   define  Tier5_b       (u2_jet_live | u2_jet_test | u2_jet_memo)
#   define  Tier5_b       u2_jet_live
#   define  Tier6         u2_jet_live
#   define  Tier6_a       Tier6
#   define  Tier6_b       Tier6
// #   define  Tier6_b       (u2_jet_live | u2_jet_test)
// #   define  Tier6_b       u2_jet_dead
// #   define  Tier6_c       (u2_jet_live | u2_jet_test)
//
#   define  Tier6_c       Tier6
#   define  Tier6_t       (u2_jet_live | u2_jet_test)
#   define  Tier6_l       (u2_jet_live | u2_jet_test | u2_jet_leak)
#   define  Tier6_l_memo  Tier6_l

#   define  Tier3_test    (Tier3 | u2_jet_test)
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
      j2_mbc(Pt1, add)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, dec)(u2_wire wir_r, 
                       u2_atom a);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, div)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, gte)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, gth)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, inc)(u2_wire wir_r, 
                       u2_atom a);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, lte)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, lth)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, mul)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt1, sub)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
    /** Tier 2.
    **/
      u2_weak                                                     //  transfer
      j2_mbc(Pt2, bind)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, clap)(u2_wire wir_r,  
                        u2_noun a,                                //  retain
                        u2_noun b,                                //  retain
                        u2_noun c);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt2, drop)(u2_wire wir_r, 
                        u2_noun a);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt2, flop)(u2_wire wir_r, 
                        u2_noun a);                               //  retain
      u2_weak                                                     //  transfer
      j2_mbc(Pt2, lent)(u2_wire wir_r, 
                        u2_noun a);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, levy)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, lien)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, need)(u2_wire wir_r, 
                        u2_noun a);                               //  retain


      u2_weak                                                     //  transfer
      j2_mbc(Pt2, reel)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, roll)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, skim)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, skip)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, snag)(u2_wire wir_r, 
                        u2_atom a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, sort)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, turn)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt2, weld)(u2_wire wir_r, 
                        u2_noun a,                                //  retain
                        u2_noun b);                               //  retain

    /** Tier 3.
    **/
      u2_weak                                                     //  transfer
      j2_mbc(Pt3, bex)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_noun                                                     //  transfer
      j2_mbc(Pt3, can)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_noun b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, cap)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, cat)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, con)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, cut)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c,                                 //  retain
                       u2_atom d);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, dor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, dis)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain


      u2_weak                                                     //  transfer
      j2_mbc(Pt3, end)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, gor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, hor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, lsh)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, mas)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, met)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, mix)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, peg)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, rap)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_noun b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, rip)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, rsh)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b,                                 //  retain
                       u2_atom c);                                //  retain

      u2_weak                                                     //  transfer
      j2_mbc(Pt3, vor)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain
  
    /** Tier 4.
    **/
      u2_weak                                                     //  transfer
      j2_mcc(Pt4, in, put)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pt4, in, gas)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_flag 
      j2_mcc(Pt4, in, has)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer 
      j2_mcc(Pt4, in, tap)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_noun                                                     //  transfer
      j2_mcc(Pt4, by, get)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pt4, by, put)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b,                             //  retain
                           u2_noun c);                            //  retain

      u2_weak                                                     //  transfer
      j2_mcc(Pt4, by, gas)(u2_wire wir_r, 
                           u2_noun a,                             //  retain
                           u2_noun b);                            //  retain
    /** Tier 5.
    **/
      u2_noun                                                     //  transfer
      j2_mby(Pt5, cue)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt5, jam)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_noun                                                     //  produce
      j2_mby(Pt5, mat)(u2_wire wir_r, 
                       u2_atom a);                                //  retain

      u2_noun                                                     //  produce
      j2_mby(Pt5, rub)(u2_wire wir_r, 
                       u2_atom a,                                 //  retain
                       u2_atom b);                                //  retain

    /** Tier 6.
    **/
      u2_noun                                                     //  transfer
      j2_mby(Pt6, cell)(u2_wire wir_r, 
                        u2_noun hed,                              //  retain
                        u2_noun tal);                             //  retain
      u2_noun                                                     //  transfer
      j2_mbc(Pt6, comb)(u2_wire wir_r, 
                        u2_noun mal,                              //  retain
                        u2_noun buz);                             //  retain
      u2_noun                                                     //  transfer
      j2_mby(Pt6, cons)(u2_wire wir_r, 
                        u2_noun vur,                              //  retain
                        u2_noun sed);                             //  retain
      u2_noun                                                     //  transfer
      j2_mby(Pt6, core)(u2_wire wir_r, 
                        u2_noun pac,                              //  retain
                        u2_noun con);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, cube)(u2_wire wir_r, 
                        u2_noun dil,                              //  retain
                        u2_noun goq);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, face)(u2_wire wir_r, 
                        u2_noun cog,                              //  retain
                        u2_noun tip);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, fine)(u2_wire wir_r, 
                        u2_noun fuv,                              //  retain
                        u2_noun lup,                              //  retain
                        u2_noun mar);                             //  retain

      u2_noun                                                     //  produce 
      j2_mby(Pt6, foam)(u2_wire wir_r, 
                        u2_noun pok);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, fork)(u2_wire wir_r, 
                        u2_noun hoz,                              //  retain
                        u2_noun bur);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, flip)(u2_wire wir_r, 
                        u2_noun hel);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, flor)(u2_wire wir_r, 
                        u2_noun bos,                              //  retain
                        u2_noun nif);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, flan)(u2_wire wir_r,
                        u2_noun bos,                              //  retain
                        u2_noun nif);                             //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, heal)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun qog,                          //  retain
                            u2_noun axe,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, hike)(u2_wire wir_r, 
                        u2_noun axe,                              //  retain
                        u2_noun pac);                             //  retain

      u2_noun                                                     //  transfer
      j2_mby(Pt6, look)(u2_wire wir_r, 
                        u2_noun cog,                              //  retain
                        u2_noun dab);                             //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ap, hack)(u2_wire wir_r, 
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ap, open)(u2_wire wir_r,
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ap, rake)(u2_wire wir_r,
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ap, late)(u2_wire wir_r, 
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, burn)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, bust)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun dib);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, crop)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, cull)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_flag pol,                          //  retain
                            u2_atom axe,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, find)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun cog);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, fink)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun cog);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, fire)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun hag);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, firm)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gib);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, fish)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_atom axe);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, fuse)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, gain)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, lose)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, mint)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gol,                          //  retain
                            u2_noun gen);                         //  retain
      
      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, mull)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gol,                          //  retain
                            u2_noun dox,                          //  retain
                            u2_noun gen);                         //  retain

      u2_flag                                                     //  transfer
      j2_mcy(Pt6, ut, nest)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_flag tel,                          //  retain
                            u2_noun ref);                         //  retain

      u2_flag                                                     //  transfer
      j2_mcy(Pt6, ut, orth)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun ref);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, park)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun axe);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, peek)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,                          //  retain
                            u2_noun axe);                         //  retain
      
      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, play)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, repo)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, rest)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun leg);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, seek)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun way,
                            u2_noun hep);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, snap)(u2_wire wir_r, 
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun gen);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, tack)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun peh,                          //  retain
                            u2_noun mur);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, tock)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun peh,                          //  retain
                            u2_noun mur,                          //  retain
                            u2_noun men);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, swab)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun men,                          //  retain
                            u2_noun har);                         //  retain

      u2_noun                                                     //  transfer
      j2_mcy(Pt6, ut, wrap)(u2_wire wir_r,
                            u2_noun van,                          //  retain
                            u2_noun sut,                          //  retain
                            u2_noun yoz);                         //  retain
    /** Direct ut axes.
    **/
#     define j2_ut_van_fan  18
#     define j2_ut_van_rib  38
#     define j2_ut_van_vrf  39
#     define j2_ut_van_vet  78
#     define j2_ut_van_fab  79

    /** Debugging hacks.
    **/
      void
      j2_mby(Pt6, type)(u2_wire  wir_r,
                        u2_noun  typ);                            //  retain

      u2_noun                                                     //  produce
      j2_mcy(Pt6, ut, dunq)(u2_wire     wir_r,
                            u2_noun     van,                      //  retain
                            const c3_c* cap_c,                    //  retain
                            u2_noun     typ);                     //  retain

      u2_noun                                                     //  produce
      j2_mcy(Pt6, ut, shew)(u2_wire     wir_r,
                            u2_noun     van,                      //  retain
                            u2_noun     mol);                     //  submit

      u2_noun                                                     //  produce
      j2_mcy(Pt6, ut, shep)(u2_wire     wir_r,
                            u2_noun     van,                      //  retain
                            const c3_c* paz_c,                    //  retain
                            u2_noun     sty,                      //  retain
                            u2_noun     mol);                     //  submit
