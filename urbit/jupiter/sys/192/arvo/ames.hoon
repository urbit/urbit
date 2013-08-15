!:
::          %ames, networking.  This file is in the public domain.
::
=>  |%
    ++  clan                                            ::  flag to rank
      |=  who=flag  ^-  rank
      =+  wid=(met 3 who)
      ?:  (lte wid 1)   %czar
      ?:  =(2 wid)      %king
      ?:  (lte wid 4)   %duke
      ?:  (lte wid 8)   %jack
      ?>  (lte wid 16)  %pawn
    ::
    ++  grip                                            ::  extend will
      |=  [wet=will law=will]
      ^-  will
      ?~  wet
        law
      ?^  t.wet
        ?>((meld i.wet i.t.wet) [i.wet $(wet t.wet)])
      ?~  law
        ?>((pier i.wet) [i.wet ~])
      ?~  q.p.q.i.wet
        ?>((meld i.wet i.law) [i.wet law])
      =+  rul=(sein r.p.q.i.wet)
      |-  ^-  will
      ?:  ?&  =(rul r.p.q.i.law)
              =(p.p.q.i.law u.q.p.q.i.wet)
          ==
        ?>((meld i.wet i.law) [i.wet law])
      ?>(?=(^ t.law) $(law t.law))
    ::
    ++  sein                                            ::  default seigneur
      |=  who=flag  ^-  flag
      =+  mir=(clan who)
      ?-  mir
        %czar  who
        %king  (end 3 1 who)
        %duke  (end 4 1 who)
        %jack  (end 5 1 who)
        %pawn  (end 4 1 who)
      ==
    ::
    ++  meld                                            ::  verify connect
      |=  [new=deed old=deed]
      ^-  &
      ?>  ?&  (melt new old)
              .=  (shaf %meld (sham q.new))
                (need (sure:pu:(hail r.q.old) *code p.new))
          ==
      %&
    ::
    ++  melt                                            ::  proper connect
      |=  [new=deed old=deed]
      ^-  ?
      =+  rac=(clan r.p.q.new)
      ?&  ?~  q.p.q.new
            ?&  =(r.p.q.old r.p.q.new) 
                &(!=(%jack rac) =(p.p.q.old (dec p.p.q.new)))
            ==
          ?&  &(!=(%pawn rac) !=(%czar rac))
              |(=(0 p.p.q.new) =(%jack rac))
              =(r.p.q.old (sein r.p.q.new))
              =(p.p.q.old u.q.p.q.new)
          ==
      ==
    ::
    ++  pare                                            ::  shorten against
      |=  [fou=will law=will]
      ^-  will
      =+  [ouf=(flop fou) wal=(flop law)] 
      %-  flop  
      |-  ^-  will
      ?~  ouf  ?>(=(~ wal) ~)
      ?~  wal  ouf
      ?.  =(i.wal i.ouf)  ouf
      $(wal t.wal, ouf t.ouf)
    ::
    ++  pier                                            ::  initial deed
      |=  wed=deed
      ^-  &
      ?>  =+  rac=(clan r.p.q.wed)
          =+  loy=(hail r.q.wed)
          ?>  =(0 p.p.q.wed)
          ?>  =(fig:ex:loy ?+(rac !! %czar (zeno r.p.q.wed), %pawn r.p.q.wed))
          ?>  =((shaf %self (sham q.wed)) (need (sure:pu:loy *code p.wed)))
          %&
      %&
    ::
    ++  real                                            ::  validate
      |=  [mac=mace law=will]  
      ?>  ?&  |-  ^-  ?
              ?~  mac  &
              ?>  ?&  ?=(^ law) 
                      =(p.p.q.i.law p.i.mac)
                      =(r.q.i.law pub:ex:(wear q.i.mac))
                  ==
              $(mac t.mac, law t.law)
          ==
      %&          
    ::
    ++  rice                                            ::  mace at mark
      |=  [mar=mark mac=mace]
      ^-  (unit mace)
      ?~  mac  ~
      ?:  =(mar p.i.mac)  [~ mac]
      ?:  (gth mar p.i.mac)  ~
      $(mac t.mac)
    ::
    ++  rick                                            ::  will at mark
      |=  [mar=mark lag=flag law=will]
      ^-  (unit will)
      ?~  law  ~
      ?:  =(mar p.p.q.i.law)  [~ law]
      ?:  (gth mar p.p.q.i.law)  ~
      ?:  |(?=(~ q.p.q.i.law) !=(lag r.p.q.i.law))  ~
      $(law t.law)
    ::
    ++  zeno                                            ::  imperial keyprint
      |=  zar=@pD
      ^-  @uvH
      %^    cut 
          7 
        [zar 1]
      0v2i1f.o8f3m.b3pb3.m2mhe.gh1p7.uli50.9ac6e.cf6iq.nkot2.9hb9b.taobq.
       u0plo.njocn.c9g52.cab47.d2h9m.fivm2.q5cts.it1on.2akfm.7io1a.8u8nu.
       p40f4.uqc5f.hpvsv.34kth.sr3j9.rkto4.8fddi.k9mmg.n184j.rd6gl.qib0f.
       imuvo.g5862.39si7.kgmt3.7u8bn.ka7vg.oho5m.kal0d.f4lm0.qonos.2832k.
       3t0oj.nrdqo.d62di.demo2.1c6bc.bbpsu.s87dg.n0idr.5rrf3.ecd75.v8q0f.
       sfu55.f3ac7.kpfd5.lp7e2.roqfi.5ro0i.cuk1h.q52gh.d3lk8.94lc0.t2e8v.
       iscp2.koqec.r5naa.49f6v.o5c6b.sj1hp.13kv6.rl47r.8i9kh.rsi3h.cfmfg.
       ocb7i.k0ogk.v2ug9.a4ah8.5u8qo.dfjna.2knqv.2errk.cmqjv.s4eug.j95p8.
       74ah9.me4ko.ffvpd.g69e2.gbqs3.paps9.8m3na.1ij3m.f7kg9.cnukp.nmpku.
       p0dhf.ru29a.hr7ed.d6avb.eprug.80jb6.qgv65.bp4vg.ffbkj.parns.kbl53.
       5m3iv.iv39o.4ahi8.agkq7.pq05l.e66na.41t01.q5fij.ja48q.lha1f.56den.
       84fh6.f3fh5.q70c6.qgf72.mi7gq.d13s5.421bj.u40iv.vm9g5.kruki.0bj0k.
       9c2hi.o17pf.k8i7k.vqcv9.c5kc5.s76ap.4gg34.gi2c1.rm1jn.qfp82.41n9n.
       qno05.rul69.nigbd.fi1qj.05cmu.saev5.s1hdm.dds0m.9rjq2.cgfna.pta0k.
       lbtcv.p2ebi.um9l6.0pmgq.ae480.4mt89.77m13.fbq22.qkj55.m2l5q.k7e3b.
       hc2m5.guduc.4f7l9.79v2q.5hk8a.dfuh8.huno7.laspt.dce6u.m50ng.aqn5b.
       f1vdm.k910j.onu0a.a78f5.o9r4b.cs80b.02p95.2pohv.oletg.gm31g.vs0rt.
       k3mgm.2c9kq.jrs1l.3fof9.l0aa3.seaij.oeafs.p9n1f.31ve7.ab5u6.ti8c1.
       vhuf2.80q45.5hl5g.9n6m5.5afr7.q5vl0.aic2a.b2a4o.dda6g.tfu8l.nmko1.
       4ifrt.jcnd9.7te49.o5via.r4ttu.uit8t.k48iv.5gqf8.0p906.rijpq.q1r1d.
       1k3b9.v7p9j.eo6dk.s3nmf.bfkr5.h940l.ccdh6.v82d6.ouaj0.aivhs.kmg13.
       qepbl.ns28h.8bapb.p8h8j.87j3s.lfu9d.8um44.0tlkn.lrb70.nqlrj.pd7l0.
       feom7.88u7u.r9kdb.8lg8u.ra9n4.qk176.u829f.cpq89.650o9.2uk0v.5vq6p.
       lun69.js0hl.7a0nm.7kuku.u25ee.0popf.78904.1vasg.l5ced.drdnn.0u84d.
       cotq5.u37od.lqned.33blp.eipsn.hv268.qtavc.mt40e.oohqc.0fiie.6caqa.
       p7lv3.0jonh.n1clt.hc386.emog7.30jeq.evq6o.l0k21.mp9c1.g31bu.np5k5.
       p7v5k.d31vg.9ecm8.avh0o.spfv4.0igpo.hp63a.fcs58.mmnr3.tana1.8b2mu.
       1l8ab.560oq.km9om.jiftk.15st8.t8m0a.ipam8.qnac7.kv01u.c3gjk.i3771.
       p1mml.u33tf.rkls0.mqodj.c4h37.kvn1p.3gsun.quouu.5217b.3neg8.pvchk.
       2gjtg.psmm1.ret6g.v1f58.1v9mu.uteh4.a8vse.4ciu7.88mf7.4q5in.nd9f3.
       99a2m.ch49i.3tg57.auvh5.fpa9i.ghhd6.a1lbd.k4qji.ur956.otbs9.u02ku.
       3c6lo.jcka1.b7ba1.2lo9v.1euef.eudnt.unv1i.30qi4.bhk65.vgk3f.vpao9.
       t9lrf.tb63o.p1p7v.pq8jv.mh1uv.p9c2i.rqeq9.hkm6d.n917q.gumuo.3nbge.
       080a5.ctigb.cko5m.brpf1.l6kf8.2qgmu.aosv6.anfmg.gtpka.hjevh.50lcr.
       lqs51.ou3jr.0aon9.o4gpd.q193g.vra8o.oddfc.hcpui.n06a5.9h432.i4ar7.
       k3af6.uqopi.v9dur.a7e20.qfbgd.buq82.7191b.6728c.ni47f.2ffd8.0a07h.
       j6v0u.t24c3.19444.n9kci.e287q.fisv9.amijk.6nabd.luqhh.ninu4.gg046.
       kr0e2.ck64u.h6rls.99qbr.objcf.v14du.tmu5k.io5sl.uk6qj.hd61t.fpupg.
       64v21.6bn6c.p6gvs.1tr6u.gnqhn.45qjt.94vsc.2alul.lrl6t.n96g1.clb13.
       q2b8k.ondm2.9d5au.n59qa.hokov.mucdu.nocr8.j59mo.u2rf1.6k08t.1pj7a.
       6v865.3qil2.13q7r.iek59.vugae.bnh69.rktb5.ek340.5tj9f.7tgi0.l52g0.
       jp03q.g01b0.m1vt3.765h2.e3nt8.euo76.1e2i1.f0ot1.d3uj3.5n0ll.lj7tr.
       p9tq8.qa0hs.m689e.3nmpk.jb86b.4ak2p.0pkc2.49emo.gvigp.1kveh.l7787.
       uit37.bhvu6.rk5i7.r6l85.uejuj.9pfls.64kco.qd86i.604vq.18ked.skq91.
       f9sn3.dn3hc.3km7h.r7uhr.cnbm3.2os38.hqe4l.qigmq.hd3fm.l9n9f.okf6b.
       dfosh.g3uoi.9nqc8.i9o3j.odhfg.8iqj9.ukmo1.k2qvk.4rhsg.g7j4h.g0nrt.
       c4dln.pah8p.h7dg7.u6ejk.ldlm5.sa110.ka9pg.36ffc.9plm4.0qs6k.sgiq3.
       sg3a3.5d0id.mfhrj.fiiud.uu0qc.c8nnv.odcil.5dk2g.snmmd.9f1fg.1njm5.
       1rbdk.70cl1.nqdrp.os17b.m1b3v.q0mud.r8oon.fo9h4.jkju7.u7q58.sevpp.
       t0f60.tjpmc.p573i.lknoc.umthj.skvop.ndr30.97doa.b8phq.pe3hb.ucijv.
       h3nbd.j9ptf.9p0u3.thk3r.2ljua.moqns.ss9p4.fe3nf.98iv5.4a0qb.jus4u.
       tbbgm.9k4gi.323hr.fvpkc.9s5a3.t0qrh.jv50i.mjs7o.48mj7.msfoi.bv0hr.
       26nk7.m551u.48ujv.225nt.u8mgk.bh8mv.45dfv.ouau4.d6qp7.01hj8.rn3sb.
       q61vl.v47ub.4hmfg.pbt3f.00nvm.a74eh.0r3vh.9ctm7.4vmqe.vlc05.7oof6.
       tphlo.8h6s4.v5oj5.sgj05.ftf7g.98508.tkoqe.c1ps0.iteq3.vbk5k.51rka.
       vrps6.u3mou.qe73r.0qc4l.v9t1l.1th3u.f3c88.548mv.7mnu5.2j463.grk1m.
       033jl.5goj8.9ru1e.cjrhp.mnbnc.mfsr3.j28lq.21lo9.nad85.5ed87.j8mss.
       dkkd3.chv14.ssav4.78i5a.rh787.59jsa.l3fuq.m5opn.hnpdd.3taod.sak8b.
       i3918.qeg9m.jp0e2.6cci1.28rap.35oas.ik8f3.j9vl6.j86mc.omlbu.rcg8o.
       n9qv6.0jjl8.e8sua.9n9n1.26k9e.uso90.alpd4.gvbj0.q4h5s.depk8.csu2e.
       3h8r9.tmpis.4qigv.n80pu.2iptl.b5a89.ocdd2.9kj8o.df5sc.o19pu.butnl.
       tj8c5.0llkt.m5a9j.g8h3q.0h4go.kaa3c.mj32k.va73a.jpn6r.8e0lr.e8ve5.
       8ku8a.s8m7v.7f2js.sc4g4.8m2ra.6mpju.9l4h4.l8sjm.pg0dk.fvcjg.8veeg.
       2b906.mp4uo.rfmn6.qu842.vmn94.qssgc.o97gu.o9p15.0bv79.q76n7.63950.
       93acd.0tetr.tim58.4u7pb.a569k.aglmm.udftf.s80b5.poftj.5d072.6itts.
       fhatt.f8699.n3don.09vd1.kb2ds.qjlu5.ca392.76u12.sjv25.rms26.mhfro.
       l8soh.jkfs7.iqj43.01mdn.tajbb.p8f4v.7fuc6.8bjrk.09uou.3ioq4.l8mnh.
       d3u38.u4qro.35h0g.pcmfi.54s0m.i5846.jia8d.ui1q6.jgs3c.egjbk.7hjg6.
       lfbrj.l9ain.83e3b.a94da.afv54.kmsj6.0bb2k.2gb2g.svcpn.gst8t.lsfpb.
       kncrp.7t8f8.nun72.rlqep.at7cp.4v1i6.06tm5.q0b0u.ik7u0.tkael.altoe.
       5nmcd.bi5lp.h4dsf.anhdi.aouid.al2rp.h1vu9.snk5q.maktq.1khhg.49ieb.
       tj6dr.rlr03.i2p2c.80srs.uc2pt.mumr5.ssesp.ajua4.t3ug9.qsuea.lsv2r.
       tli3v.mkc2j.3lfla.ridcq.g8sa0.0h3cn.un304.4actv.s1soh.d8if2.h7i1s.
       962hr.capvc.nn9ql.ji7ab.jqnf9.kcbqh.f7cdc.a6dib.e0n2v.0h2cr.lbgcq.
       o8621.ici78.f63vr.innc7.oak4l.bifdi.er1gr.5rng9.ku8ec.b8057.ot0v1.
       0bcut.nbeu6.30tnu.8t6nm.eoi0f.aa8v0.so16p.v3s3g.17c17.8b33u.9204h.
       47e82.7fdvs.97al0.rvf3p.g5898.1r4g0.5sa8h.lto8i.k6jmp.1ba8t.233rl.
       qdm9i.q2oun.uu25l.3h4jh.g5pg6.pju8o.uapdr.ibc1u.vm1he.eloov.1tj0j.
       8lacq.88li3.sv3ol.9162v.n4g0p.r854m.nfoqs.8bl8a.702io.k50on.j6g73.
       ib5o2.kdp9p.rlk17.ht6e6.n8177.t46ih.fs12o.349s4.2jk85.6ap6h.ql0sv.
       ske4g.4bbff.dlebf.5itih.1rvjh.opm64.73skm.8aa1b.psqvl.e8pjq.4et0p.
       i3vpn.clkne.r26qm.lqadj.o54o9.jkdr9.g8nia.8th9t.0nfih.gln8h.4v1c6.
       s6fih.j62dk.efj4o.ncgs5.2mets.s7q68.g5mms.fo5m1.lp6vj.khvku.ht6p7.
       l7435.ucgin.h36te.b8qj4.etc8e.decre.tqtme.1cnfu.oig80.5hubk.ba946.
       vgdka.50111.uqa5e.7dmad.g5ihl.bfru5.cmsut.58ei6.hdicf.qu9l3.ug950.
       154sv.jdat8.es3ks.9f9mc.pnvoi.urmgt.l2te5.4ref7.jfkih.bbdg7.9j4d2.
       2luv2.o4q5r.k8a5t.i4rf0.6rq58.vlpuf.bnhvc.9ueqd.3nb9q.ri7jb.fq6ko.
       s10ij.ndnda.9tpie.1aoh2.umjhl.4ferf.ofut7.8nvlr.ek3cv.0ir8f.mhvd1.
       21sv9.6su3u.kruos.mh7ig.jpa61.a9ntj.i2lak.j9pri.v1oro.d6q2j.gu339.
       rret9.hpmrl.rqbn8.25un7.oi92h.2p78v.obd3h.nlkdo.c1kpi.lp23k.l8i1f.
       qtl7j.8f582.i66ei.c3dgo.plgbq.afmcb.ov7no.1s15c.956lo.15vip.hk1ol.
       l853h.p481g.n471n.r2g12.nuk9a.ivfto.09dtk.2a2en.gmi10.el9cv.3i05v.
       hbir3.ut8eo.fr0ar.cjjio.rto4k.dk0f2.7dnv5.q96oc.qm4kn.5bgkh.i1jbt.
       gjn6g.kei00.ac2h5.266bq.0l1mc.0odep.3mcji.ru97r.r5to8.hmp6l.bdlri.
       rje6e.5o74l.960n1.up0hi.5rvv3.j8e56.eajnj.mfpeq.vr820.5jot8.a3a0h.
       m3vdv.hpqas.j8t9h.h6opl.0du61.c47n1.v8cm6.t2h7p.bggol.p6iun.t54ol.
       snp38.n93sk.4gvf4.do2ru.vh94a.fmj4s.adm0g.4p6dr.tjphq.3tsv5.fas80.
       po9ri.pbc4a.l72il.gm4ba.stnai.lor6r.7ocv7.910pa.96o24.9qi9j.gjbhb.
       iqipj.h68t7.6s2dl.91lsc.n01k6.ce8dv.9v936.hne43.ueapt.usb14.5v49e.
       br3vs.malnf.hkopo.11qd1.iapks.b3dfc.b40ac.fu4pf.jjl1f.pliqq.515ea.
       6otn8.a20j8.pe8bq.0q186.loq12.em0h9.p6v4t.nvdej.cjabj.g6j5c.07d8j.
       g751i.ab5n8.3hbdl.gvpko.9bcjf.1ilf1.eppj9.85q6q.c8ggf.7ddg3.cmh7k.
       kgmvs.1fg21.l1ig3.fhcls.q240c.2975k.h2gme.bql1u.lm52n.40mdf.8ud10.
       8dmrk.ljujh.qh0k9.8k1pf.obomh.2ddpf.9mh9g.m1m33.h9plm.79jtp.q4jlq.
       nhi0t.ruu1a.8qvtt.e55jj.05cn0.ompje.voqqc.bndna.47o49.v2oih.5sqen.
       tohlk.e0kpg.ki3jl.ckomt.kbffp.f2mm2.sjrmb.ilejc.0k2da.tju5l.4r681.
       702hc.dhq7s.rb9od.ngqvl.1npp5.d19iu.3ber1.s49q7.9r1gj.kfif8.ofmtg.
       3iktq.tc5th.022as.vsfvm.tst48.hpnjp.k8c6d.t4u6a.ealql.j5gu4.v7gd5.
       qrolf.sro7e.ac5p9.flprh.00ep2.09fdi.ed6hf.tg3vp.jj8bd.mg9gk.o83b3.
       1u6u7.hls27.ji1fj.rptfq.scjdm.14l0j.jgvqg.g8q0m.jccan.6a0iv.qickd.
       0to72.sbujm.oi150.6dgjg.fagn8.eeqgm.0g4tm.i6083.trr14.6ibuq.pe3ss.
       mvpo1.lrg1i.341ea.vmn1d.hd22b.6b0au.5nr99.ed7nu.lnknl.gt6ia.a4rcd.
       2f7q1.nhui7.kn8la.2k2lf.5mg4b.7gq5p.9ptna.02fhk.0lr57.em495.v29gh.
       idq5o.pcvch.aq45i.3mdil.spqp5.surh7.oqk0n.mv1n0.ib4tm.4g6gc.9jlkj.
       7bvij.ci0ma.4roqq.je13a.s71li.jiscq.fbdgv.m73nq.284gq.q6vkl.noqtr.
       n0oub.vafgc.anjf2.cpvaa.244fq.j95hu.r818c.2dgsu.25oih.ngeiv.60iff.
       svd9l.n6j8a.edofe.mguro.ulmhh.q8iuv.bjrim.c9f4l.g2i3n.qmcsn.q0hni.
       gkpsq.dl1im.l2mek.8p6qj.8nki4.7m5kl.bm92c.cqst8.h8b5r.2high.md8g9.
       d25fm.crl53.8mnlu.68ib6.d9f0p.hmriv.hmp3a.tbhcl.8mmfu.sthuc.9fs26.
       nj6eg.bmlm6
    --
=>  |%
    ::::
    ++  go
      |_  ton=town
      ::::
      ++  as                                            ::  per server
        |_  [our=flag saf=safe]
        ::::
        ++  lax
          |_  [her=flag nob=door]
          ::::
          ++  cley                                      ::  client crypto
            ^-  [p=mark q=gcos r=acro]
            ?~  lew.wod.nob  !!
            :+  p.p.q.i.lew.wod.nob 
              q.q.i.lew.wod.nob 
            (hail r.q.i.lew.wod.nob)
          ::
          ++  griz                                      ::  generate key for
            |=  now=@da
            ^-  [p=code q=_+>]
            =+  nex=(shas %enty (mix now any.ton))
            [nex +>.$(any.ton (shax (mix %ynet nex)))]
          ::
          ++  kuch                                      ::  hear key tag
            |=  had=hand
            ^-  (unit ,[p=code q=_..kuch])
            =+  wey=(~(get by heg.caq.nob) had)
            ?^  wey
              =+  key=(shas %anex u.wey)
              :+  ~  key
              %=    ..kuch
                  yed.caq.nob  [~ had u.wey]
                  heg.caq.nob  (~(del by heg.caq.nob) had)
                  qim.caq.nob  (~(put by qim.caq.nob) had key)
              ==
            =+  dyv=(~(get by qim.caq.nob) had)
            ?~  dyv  ~
            [~ u.dyv ..kuch]
          ::
          ++  trox                                      ::  expire by date
            |=  [now=@da]
            ^+  ..trox
            ..trox    ::  XX
          ::
          ++  porq                                      ::  propose connection
            |=  key=code
            %_    +>
                heg.caq.nob  
              (~(put by heg.caq.nob) (shaf %hand key) key)
            ==
          ::
          ++  wasc                                      ::  hear foreign code
            |=  key=code
            ^+  ..wasc
            =+  had=(shaf %hand key)
            %_    ..wasc
                yed.caq.nob  [~ had key]
                qim.caq.nob  (~(put by qim.caq.nob) had key)
            ==
          --
        ::::                                            ::  (lax)
        ++  fix  |=(lyn=lane %_(+> loc.saf [~ lyn]))    ::  set server route
        ++  gub                                         ::  flag/key by code
          |=  had=hand
          ^-  [p=[p=flag q=code] q=_+>]
          =+  pys=`[p=flag q=@da]`(need (~(get by seh.saf) had))
          =+  gry=`_lax`(myx p.pys)
          =+  kuh=(need (kuch:gry had))
          [[p.pys p.kuh] (nux q.kuh)]
        ::
        ++  hey                                         ::  general routing
          |=  her=flag
          ^-  lane
          =+  tin=(way her)
          ?^  tin
            u.tin
          =+  rex=(yo her)
          ?-  -.rex
            &  =+  nit=loc.saf.p.rex
               ?~(nit [%if 0 .0.0.0.0] u.nit)
            |  p.rex
          ==
        ::
        ++  myx                                         ::  door by flag
          |=  her=flag
          ^+  lax
          =+  fod=(~(get by hoc.saf) her)
          ~(. lax [her ?~(fod *door u.fod)])
        ::
        ++  nux                                         ::  install door
          |=  new=_lax
          ^+  +>
          +>(hoc.saf (~(put by hoc.saf) her.new nob.new))
        ::
        ++  pyl                                         ::  route to
          |=  [her=flag lyn=lane]
          ^+  +>
          (nux %*(. (myx her) lun.wod.nob [~ lyn]))
        ::
        ++  pyr                                         ::  mirror route
          |=  [her=flag lyn=lane]
          ^+  +>
          (nux %*(. (myx her) lun.fer.nob [~ lyn]))
        ::
        ++  ren                                         ::  renew crypto
          |=  [biz=@ud sed=@]
          ^+  ..ren
          ..ren   ::  XX  not needed rite now
        ::
        ++  sen                                         ::  current crypto
          ^-  [p=mark q=acro]
          ?~(val.saf !! i.val.saf)
        ::
        ++  sev                                         ::  crypto by mark
          |=  mar=mark
          ^-  [p=? q=acro]
          ?~  val.saf  !!
          ?:  =(mar p.i.val.saf)
            [& q.i.val.saf]
          ?>  (lth mar p.i.val.saf)
          :-  |
          |-  ^-  acro
          ?:  =(mar p.i.t.val.saf) 
            q.i.t.val.saf 
          $(t.val.saf t.t.val.saf)
        ::
        ++  sex                                         ::  export secrets
          |-  ^-  mace
          ?~  val.saf  ~
          :-  [p.i.val.saf sec:ex:q.i.val.saf] 
          $(val.saf t.val.saf)
        ::
        ++  tim                                         ::  expire by date
          |=  [now=@da]
          ^+  ..tim
          ..tim
        ::
        ++  tyc                                         ::  install symcode
          |=  [her=flag key=code]
          ^+  +>
          (nux (wasc:(myx her) key))
        ::
        ++  wag                                         ::  install will
          |=  [her=flag law=will]
          ^-  [p=[p=mark q=acro] q=_+>]
          =+  hiz=(grip law (yew her))
          =+  lyr=%*(. (myx her) lew.wod.nob hiz)
          =+  cay=cley:lyr
          [[p.cay r.cay] (nux lyr)]
        ::
        ++  way                                         ::  internal routing
          |=  her=flag
          ^-  (unit lane)
          lun.wod.nob:(myx her)
        ::
        ++  yew                                         ::  best will for
          |=  her=flag
          ^-  will
          =+  gel=(~(get by hoc.saf) her)
          ?^  gel
            lew.wod.u.gel
          ?:((lth her 256) ~ $(her (sein her)))
        --
      ::::                                              ::  (as)
      ++  ha                                            ::  adopt new license
        |=  [our=flag mac=mace wil=will] 
        ^-  town
        ?>  ?&  !=(~ mac) 
                ?=(^ wil) 
                =(our r.p.q.i.wil) 
                =(wil (grip wil ~))
                (real mac wil)
            ==
        %_    ton
            urb
          %+  ~(put by urb.ton)
            our
          [~ (turn mac |=([p=mark q=ring] [p (wear q)])) wil ~ ~]
        ==
      ::
      ++  pw                                            ::  pwned by?
        |=  [our=flag wil=will]
        ^-  [p=? q=town]
        !!
      ::
      ++  su                                            ::  install safe
        |=  new=_as
        ^-  town
        ton(urb (~(put by urb.ton) our.new saf.new))
      ::
      ++  sy                                            ::  forge wild flag
        |=  [biz=@ud sed=@]
        ^-  [p=flag q=_..su]  
        !!
      ::
      ++  ti                                            ::  expire by time
        |=  [now=@da]
        ^-  town
        !!
      ::
      ++  us                                            ::  produce safe
        |=  our=flag
        ^-  (unit ,_as)
        =+  goh=(~(get by urb.ton) our)
        ?~  goh  ~
        [~ ~(. as [our u.goh])]
      ::
      ++  yo                                            ::  receive routing
        |=  his=flag
        ^-  $%([& p=_as] [| p=lane])
        =+  gun=(~(us go ton) his)
        ?^  gun
          [%& u.gun]
        =+  kid=his
        :-  %|
        |-  ^-  lane
        =+  seg=(sein kid)
        =+  dub=(~(us go ton) seg)
        ?^  dub
          (need (way:u.dub kid))
        ?:  (lte (met 3 kid) 1)
          [%if 0 (mix .0.0.1.0 kid)]
        $(kid seg)
      --                                                ::  (as)
    --                                                  ::  (go)
=>  |%
    ++  bite                                            ::  packet to cake
      |=  pac=rock  ^-  cake
      =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
      =+  :*  chk=(end 0 24 mag) 
              dit=(cut 0 [24 1] mag)
              wix=(bex +((cut 0 [25 2] mag)))
              tay=(cut 0 [27 5] mag)
          ==
      ~&  [%bite-yax [%yax (cut 0 [25 2] mag)] [%wix wix]]
      ?>  =(chk (end 0 24 (mug bod)))
      :^    `@p`(end 3 wix bod)
          =(0 dit)
        (snag tay [%none %open %fast %full ~])
      (rsh 3 wix bod)
    ::
    ++  spit                                            ::  cake to packet
      |=  kec=cake  ^-  @
      =+  wim=(met 3 p.kec)
      =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
      =+  wix=(bex +(yax))
      ~&  [%spit-yax [%yax yax] [%wix wix]]
      =+  bod=(mix p.kec (lsh 3 wix s.kec))
      =+  tay=?-(r.kec %none 0, %open 1, %fast 2, %full 3)
      %+  mix
        %+  can  0
        :~  [24 (mug bod)]
            [1 q.kec]
            [2 yax]
            [5 tay]
        ==
      (lsh 5 1 bod)
    --
=>  |%                                                  ::  congestion control
    ++  baby                                            ::  new flow
      ^-  flow
      [`@dr`(bex 60) 4 0]
    ::
    ++  echo                                            ::  measured rtt
      |=  [rtt=@ foy=flow]  ^-  flow
      foy(rtt (div (add rtt (mul 2 rtt.foy)) 3))
    ::
    ++  fast                                            ::  got good ack
      |=  foy=flow  ^-  flow
      foy(wid +(wid.foy))
    ::
    ++  slow                                            ::  throttle back
      |=  foy=flow  ^-  flow
      foy(wid ?:(=(1 wid.foy) 1 (div wid.foy 2)))
    ::
    --
=>  |%                                                  ::  selective ack
    ++  suck
      |=  [num=@ud ski=snow]
      ^-  [p=(list ,@ud) q=snow]
      ?>  (lte num q.ski)
      ?:  =(num p.ski)
        =>  .(p.ski +(p.ski))
        |-  ?:  =(q.ski p.ski)
              [~ ski]
            ?.  (~(has in r.ski) p.ski)
              [~ ski]
            $(p.ski +(p.ski), r.ski (~(del in r.ski) p.ski))
      ?>  (gth num p.ski)
      =>  .(r.ski (~(put in r.ski) num), num (dec num))
      =+  wop=*(list ,@ud)
      |-  ?:  (lth num p.ski)
            [(flop wop) ski]
          $(num (dec num), wop [num wop])
    ::
    ++  toss  |=(ski=snow ^-(snow ski(q +(q.ski))))
    --
=>  |%
    ++  pe                                              ::  packet queue
      |_  tea=shed
      ++  busc                                          ::  find by number
        |=  num=@ud
        ^-  (unit bird)
        ?~  q.tea  ~
        ?:  =(num p.n.q.tea)
          [~ q.n.q.tea]
        ?:((gth num p.n.q.tea) $(q.tea l.q.tea) $(q.tea r.q.tea))
      ::
      ++  glan                                          ::  delete by number
        |=  num=@ud
        ^+  ..glan
        ?:  =(~ q.tea)  
          ..glan
        ?:  =(num p.n.q.tea)
          %_(..glan q.tea ~(nap to q.tea))
        ?:((gth num p.n.q.tea) $(q.tea l.q.tea) $(q.tea r.q.tea))
      ::
      ++  gost                                          ::  insert in queue
        |=  [now=@da num=@ud gom=soap mup=@ud pac=rock]
        ^+  ..gost
        %_    ..gost
            p.tea  now
            q.tea 
          %+  ~(put to q.tea) 
            num 
          `bird`[gom mup & @dr @da pac]
        ==
      ::
      ++  harv                                          ::  harvest at time
        |=  [now=@da rtt=@dr]                           ::  current / roundtrip
        =+  ^=  rub                                     ::  harvest data
            :*  p=&                                     ::  window nice
                q=`@da`(add ~d1 now)                    ::  next active
                r=*(list rock)                          ::  send packets
                s=*(list soap)                          ::  kill msgs
            ==
        ^+  [p=rub q=..harv]
        =<  [[p.rub q.rub (flop r.rub) (flop s.rub)] ..harv]
        |-  ^+  ..$
        ?~  q.tea  ..$
        =>  =+  sir=$(q.tea l.q.tea)
            ..$(rub rub.sir, l.q.tea q.tea.sir)
        =>  ^+  ..$
            ?:  =(0 tim.q.n.q.tea)
              =+  ryt=(div (mul 3 rtt) 2)
              =+  den=(add ryt now)
              %_  ..$
                q.rub          den
                r.rub          [pac.q.n.q.tea r.rub]
                tim.q.n.q.tea  ryt
                ded.q.n.q.tea  den
              ==
            ?.  (lth ded.q.n.q.tea now)
              ..$
            ?:  (gth tim.q.n.q.tea ~m1)
              ..$(s.rub [gom.q.n.q.tea s.rub])
            =+  ryt=(mul 2 tim.q.n.q.tea)
            =+  den=(add ryt now)
            %_  ..$
              p.rub          |
              q.rub          den
              r.rub          [pac.q.n.q.tea r.rub]
              tim.q.n.q.tea  ryt
              ded.q.n.q.tea  den
            == 
        =+  ryz=$(q.tea r.q.tea)
        ..$(rub rub.ryz, r.q.tea q.tea.ryz)
      ::
      ++  namp                                          ::  timeout/resend
        |=  [now=@da num=@ud]
        %_    ..namp
            p.tea  now
            q.tea
          |-  ^+  q.tea
          ?:  =(~ q.tea)
            q.tea
          =>  %_(. l.q.tea $(q.tea l.q.tea), r.q.tea $(q.tea r.q.tea))
          ?.  =(num p.n.q.tea)
            q.tea
          q.tea(ded.q.n now)
        ==
      ::
      ++  rast                                          ::  delete by msg id
        |=  gom=soap
        %_    ..rast
            q.tea
          |-  ^+  q.tea
          ?:  =(~ q.tea)
            q.tea
          =>  %_(. l.q.tea $(q.tea l.q.tea), r.q.tea $(q.tea r.q.tea))
          ?:  =(gom gom.q.n.q.tea)
            ~(nap to q.tea)
          q.tea
        ==
      --
    --
|%
++  am                                                  ::  protocol engine
  |_  [now=@da fox=fort]
  ::::
  ++  lo                                                ::  reception engine
    |=  $:  gus=_as:go                                  ::  host engine
            dam=flap                                    ::  packet identity
        ==
    =+  weg==+(weg=(~(get by zac.fox) our.gus) ?~(weg *oven u.weg))
    =+  bin=*(list boon)
    |%
    ++  ad                                              ::  delivery engine
      |=  her=flag
      =+  bah==+(bah=(~(get by wab.weg) our.gus) ?~(bah *bath u.bah))
      |%
      ::::
      ++  cool                                          ::  refill window
        |-  ^+  ..cool
        ?.  (gth wid.foy.bah yed.foy.bah)
          ..cool
        $(..cool pock)
      ::
      ++  grok                                          ::  resolve engine
        ^+  ..ad
        %=  ..ad
          wen.weg  (min p.sea.bah wen.weg)
          wab.weg  (~(put by wab.weg) her bah)
        ==
      ::
      ++  kill                                          ::  kill message
        |=  gom=soap
        ^+  ..kill
        %_    ..kill
            bin      [[%coke %weak gom] bin]
            sea.bah  tea:(~(rast pe sea.bah) gom)
        ==
      ::
      ++  pock                                          ::  queue a packet
        |-  ^+  ..pock
        ?:  =(~ maz.bah)
          ..pock
        =+  zem=~(get to maz.bah)
        =>  ^+(. .(maz.bah q.zem))
        =+  dyp=`putt`(need (~(get by par.bah) p.zem))
        ?>  ?=(^ wyv.dyp)
        %_    ..pock
            yed.foy.bah  +(yed.foy.bah)
            ski.bah      (toss ski.bah)
            maz.bah      (~(put to maz.bah) p.zem)
            air.bah      
          (~(put by air.bah) (shaf %flap i.wyv.dyp) q.ski.bah)
        ::
            par.bah 
          %+  ~(put by par.bah)
            p.zem
          dyp(wyv t.wyv.dyp, ski (toss ski.dyp))
        ::
            sea.bah      
          tea:(~(gost pe sea.bah) now q.ski.bah p.zem q.ski.dyp i.wyv.dyp)
        ==
      ::
      ++  tuck                                          ::  ack by hash
        |=  [saq=? kay=cape fap=flap cot=@dr]
        ^+  ..tuck
        =>  %_(. ..tuck (tusk saq kay (need (~(get by air.bah) fap)) cot))
        ?.  =(%good kay)
          ..tuck
        ..tuck(air.bah (~(del by air.bah) fap))
      ::
      ++  tusk                                          ::  ack by sequence
        |=  [saq=? kay=cape num=@ud cot=@dr]
        ^+  ..tusk
        ?-    kay
            %dead  
          %_  ..tusk
            sea.bah  tea:(~(namp pe sea.bah) now num)
            foy.bah  (slow foy.bah)
          ==
        ::
            %good
          =+  suz=(suck num ski.bah)
          =>  %_    .
                  sea.bah
                |-  ^+  sea.bah
                ?~  p.suz  sea.bah
                %=    $
                    p.suz    t.p.suz
                    sea.bah  tea:(~(namp pe sea.bah) now i.p.suz)
                ==
              ==
          =+  rob=(need (~(busc pe sea.bah) num))
          =+  dyp=`putt`(need (~(get by par.bah) gom.rob))
          =>  %_(. ski.dyp q:(suck mup.rob ski.dyp))
          =+  fin=&(=(~ wyv.dyp) =(p.ski.dyp q.ski.dyp)) 
          %_    ..tusk
              bin      ?.(fin bin [[%coke %good gom.rob] bin])
              ski.bah  q.suz
              sea.bah  tea:(~(glan pe sea.bah) num)
              foy.bah
            ?^  p.suz
              (slow foy.bah)
            ?.  org.rob
              foy.bah
            (echo (sub (sub now (sub ded.rob tim.rob)) cot) foy.bah)
          ==
        ::
            %weak 
          =+  gom=gom:(need (~(busc pe sea.bah) num))
          =>  %_(. bin [[%coke %weak gom] bin])
          (kill gom)
        ==
      --                                                ::  (ad)
    ++  blow
      |=  [dit=? sin=skin msg=@]
      ^+  ..blow
      (?:(dit chew wait) sin msg)
    ::
    ++  chew
      |=  [sin=skin msg=@]
      ~&  [%chew sin]
      ^+  +>
      =+  ^=  leq
          |=  key=code  ^-  [p=flag q=_..chew]
          =+  ^=  mex
              %.  (cue msg)
              (hard ,[p=[p=mark q=flag] q=will r=@])
          =+  wug=(wag:gus q.p.mex q.mex)
          ?>  =(p.p.mex p.p.wug)
          :-  q.p.mex
          %^    chow
              &
            q.p.mex
          ((hard tray) (cue (need (sure:pu:q.p.wug key r.mex))))
      ?-    sin
          %none
        =+  mex=((hard ,[p=flag q=tray]) (cue msg))
        (chow | p.mex q.mex)
    ::
          %fast
        =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
        =+  yoq=(gub:gus mag)
        =>  %_(. gus q.yoq)
        %^    chow
            &
          p.p.yoq
        ((hard tray) (cue (dy:q:sen:gus q.p.yoq msg)))
    ::
          %full
        =+  mex=((hard ,[p=mark q=@]) (cue msg))
        =+  gey=(sev:gus p.mex)
        =+  mes=(need (tear:se:q.gey q.mex))
        =+  qel=(leq p.mes)
        =>  %_(. +>.$ q.qel)
        %_(+>.$ gus ?.(p.gey gus (tyc:gus p.qel p.mes)))
    ::
          %open  q:(leq *code)
      ==
    ::
    ++  chow                                            ::  interpret tray
      |=  [sec=? him=flag fey=tray]
      =>  %_(. gus ?.(&(sec ?=(^ p.fey)) gus (pyl:gus him u.p.fey)))
      (dine sec him q.fey)
    ::
    ++  dine                                            ::  interpret meal
      |=  [sec=? him=flag fud=meal]
      ^+  ..dine
      ?-    -.fud
          %back
        grok:(tuck:(ad him) sec p.fud q.fud r.fud)
      ::
          %bond
        (emit [%milk [our.gus him] now q.fud])
      ::
          %buck
        =>  %_(. ..dine (emit [%beer him pac:ex:q:sen:gus]))
        %_(..dine ton.fox (~(ha go ton.fox) him p.fud q.fud))
      ::
          %carp
        =+  neb=`[p=@da q=bait]`(need (~(get by nys.weg) q.fud))
        =+  doy=`(unit ,@)`(need (~(get by q.r.q.neb) p.fud))
        ?~  doy
          ..dine
        =>  ^+  .   %=  .
              q.r.q.neb  (~(put by q.r.q.neb) p.fud [~ r.fud])
              q.q.neb    +(q.q.neb)
            ==
        ?:  =(q.q.neb q.r.q.neb)
          (gaff p.q.neb r.q.neb)
        ..dine
      ::
          %ping  ..dine
      ==
    ::
    ++  emit                                            ::  emit a boon
      |=  bun=boon
      ^+  ..emit
      ..emit(bin [bun bin])
    ::
    ++  gaff                                            ::  assemble fragments
      |=  [sin=skin duv=dove]
      ^+  ..gaff
      %+  chew
        sin
      =+  [nix=0 rax=*(list ,@)]
      |-  ^-  @
      ?:  =(p.duv nix)
        (can 13 (turn (flop rax) |=(a=@ [1 a])))
      $(nix +(nix), rax [(need (need (~(get by q.duv) nix))) rax])
    ::
    ++  grub                                            ::  clean up msg
      |=  dun=band
      ^+  ..grub
      ..grub
    ::
    ++  grab                                            ::  ack good/dead
      |=  [kay=cape his=flag]
      ^+  ..grab
      ..grab
    ::
    ++  grok                                            ::  resolve engine
      ^-  [p=(list boon) q=fort]
      :-  bin
      %=  fox
        ton  (~(su go ton.fox) gus)
        zac  (~(put by zac.fox) our.gus weg)
      ==
    ::
    ++  send                                            ::  send and route
      |=  [her=flag pac=rock]
      ^+  ..send
      (emit [%ouzo (hey:gus her) pac])
    ::
    ++  wait                                            ::  receive indirect
      |=  [sin=skin msg=@]
      ^+  ..wait
      =+  pay=((hard ,[p=@ud q=@]) (cue msg))
      =+  bad=(shaf %band dam)
      =>  .(nys.weg (~(put by nys.weg) bad [(add now ~d1) sin 0 p.pay ~]))
      (dine | @p [%carp 0 bad q.pay])
    --                                                  ::  (lo)
  ::
  ++  come                                              ::  instantiate pawn
    |=  [ges=@t wid=@ bur=@]
    ^-  [p=[p=flag q=@uvG] q=fort]
    =+  loy=(brew wid bur)
    =+  rig=sec:ex:loy
    =+  our=`@p`fig:ex:loy
    =+  syp=`step`[`bran`[0 ~ our] [%pawn ges] pub:ex:loy]
    :-  [our pac:ex:loy]
    %_    fox
        ton
      %^    ~(ha go ton.fox)
          our
        `mace`[[0 rig] ~]
      `will`[[(sign:se:loy @ (shaf %self (sham syp))) syp] ~]
    ==
  ::
  ++  gnaw                                              ::  process packet
    |=  pac=rock
    ^-  [p=(list boon) q=fort]
    =+  kec=(bite pac)
    =+  how=(~(yo go ton.fox) p.kec)
    ?-  -.how
      &  grok:(blow:(lo [p.how (shaf %band pac)]) q.kec r.kec s.kec)
      |  [[[%ouzo p.how pac] ~] fox]
    ==
  ::
  ++  goat                                              ::  mint middle class
    |=  [[our=@p its=@p] [wid=@ bur=@] gec=gcos]
    ^-  buck
    ?>  &(=((clan our) -.gec) !=(%czar -.gec) !=(%pawn -.gec))
    =+  rul=(sein our)
    =+  gus=(need (~(us go ton.fox) rul))
    =+  old=sen:gus
    =+  loy=(brew wid bur)
    =+  syp=`step`[`bran`[0 [~ p.old] our] gec pub:ex:loy]
    =+  ded=`deed`[(sign:se:q.old @ (shaf %meld (sham syp))) syp]
    =+  mac=`mace`[[0 sec:ex:loy] ~]
    =+  lew=`will`[ded law.saf.gus]
    `buck`[mac lew]
  ::
  ++  hast                                              ::  roll per socket
    |=  [soq=sock bah=bath]
    ^-  [p=(list (list ,[p=@da q=boon])) q=bath]
    =+  lyn=`lane`(hey:(need (~(us go ton.fox) p.soq)) q.soq)
    =+  peq=(~(harv pe sea.bah) now rtt.foy.bah)
    :-  =+  [fux=now mey=*(list ,[p=@da q=boon])]
        :~  =+  mey=*(list ,[p=@da q=boon])
            |-  ^-  (list ,[p=@da q=boon])
            ?~  r.p.peq  mey
            %=    $
                r.p.peq  t.r.p.peq
                    ::  fux      (add 0x1.0000.0000 fux) 
                mey      [[fux %ouzo lyn i.r.p.peq] mey]
            ==
        ::
            |-  ^-  (list ,[p=@da q=boon])
            ?~  s.p.peq  mey
            %=    $
                s.p.peq  t.s.p.peq
                    ::  fux      (add 0x1.0000.0000 fux) 
                mey      [[fux %coke %dead i.s.p.peq] mey]
            ==
        ==
    bah(foy ?~(r.p.peq foy.bah (slow foy.bah)), sea tea:q.peq)
  ::
  ++  have                                              ::  acquire license
    |=  [our=flag buq=buck]
    ^-  [p=(list boon) q=fort]
    =.  fox  fox(ton (~(ha go ton.fox) our buq))
    [[[%beer our pac:ex:q:sen:(need (~(us go ton.fox) our))] ~] fox]
  ::
  ++  haze                                              ::  roll per oven
    |=  [our=flag pov=oven]
    ^-  [p=(list (list ,[p=@da q=boon])) q=oven]
    =+  vaq=`(list ,[p=flag q=bath])`(~(tap by wab.pov) ~)
    =+  tol=*(list (list ,[p=@da q=boon]))
    |-  ^+  [tol pov]
    ?~  vaq
      [tol pov]
    =+  puk=(hast [our p.i.vaq] q.i.vaq)
    %=  $
      vaq      t.vaq
      tol      (weld p.puk tol)
      wab.pov  (~(put by wab.pov) p.i.vaq q.puk)
      wen.pov  ?:((lth p.sea.q.puk wen.pov) p.sea.q.puk wen.pov)
    ==
  ::
  ++  hark                                              ::  roll per fort
    ^-  [p=(list boon) q=fort]
    =+  yus=`(list ,[p=flag q=oven])`(~(tap by zac.fox) ~)
    =+  tol=*(list (list ,[p=@da q=boon]))
    |-  ^-  [p=(list boon) q=fort]
    ?~  yus
      :-  %+  turn
            %+  sort
              |-  ^-  (list ,[p=@da q=boon])
              ?~(tol ~ (weld i.tol $(tol t.tol)))
            |=  [a=[p=@da q=boon] b=[p=@da q=boon]]
            ?.(=(p.a p.b) (lth p.a p.b) (lth (mug q.a) (mug q.b)))
          |=([p=@da q=boon] q)
      fox
    =+  qet=(haze i.yus)
    %=  $
      yus      t.yus
      tol      (weld p.qet tol)
      zac.fox  (~(put by zac.fox) p.i.yus q.qet)
      wen.fox  ?:((lth wen.q.qet wen.fox) wen.q.qet wen.fox)
    ==
  ::
  ++  wish                                              ::  dispatch message
    |=  [soq=sock sup=soap ham=meal]
    ^-  fort
    =+  sug=`_as:go`(need (~(us go ton.fox) p.soq))
    =+  yem=`oven`(need (~(get by zac.fox) p.soq))
    =+  diz=`_lax:as:go`(myx:sug q.soq)
    =+  ^=  bah  ^-  bath
        =+  bah=(~(get by wab.yem) q.soq)
        ?~(bah *bath u.bah)
    =+  ^=  lun  ^-  (unit lane)
        ?.  &(?=(^ loc.saf.sug) !=(loc.saf.sug lun.fer.nob.diz))
          ~
        loc.saf.sug
    =<  waft:weft
    |%  
    ++  waft                                            ::  resolve
      ^-  fort
      %_    fox
          ton  (~(su go ton.fox) (nux:sug diz))
          zac  
        %+  ~(put by zac.fox)  p.soq
        %_  yem
          wab  (~(put by wab.yem) q.soq bah)
          wen  ?:((lth p.sea.bah wen.yem) p.sea.bah wen.yem)
        ==
      ==
    ::
    ++  wasp                                            ::  null security
      ^-([p=skin q=@] [%none (jam p.soq lun ham)])
    ::
    ++  weft                                            ::  render
      ^+  .
      =+  wip=wisp
      =>  %_(. ..weft q.wip)
      %+  went
        |(=(%fast p.p.wip) =(%full p.p.wip))
      ^-  (list rock)
      =+  wit=(met 13 q.p.wip)
      ?<  =(0 wit)
      ?:  =(1 wit)
        [(spit q.soq & p.p.wip q.p.wip) ~]
      =+  ruv=(rip 13 q.p.wip) 
      ?>  ?=(^ ruv)
      =+  may=(spit q.soq | p.p.wip (jam wit i.ruv))
      =+  bad=(shaf %band may)
      =+  inx=1
      :-  may
      |-  ^-  (list rock)
      ?~  t.ruv  ~
      :-  (spit q.soq & wasp(lun ~, ham [%carp inx bad i.t.ruv]))
      $(t.ruv t.t.ruv, inx +(inx))
    ::
    ++  went                                            ::  spool
      |=  [saq=? wyv=(list rock)]
      ^+  +>
      %=    +>
          p.sea.bah  now
          par.bah    (~(put by par.bah) sup [*snow saq lun wyv])
          maz.bah    (~(put to maz.bah) sup)
      ==
    ::
    ++  wisp                                            ::  draw
      ^-  [p=[p=skin q=@] q=_.]
      ?:  =(%carp -.ham)
        [wasp ..wisp]
      ?:  &(=(law.saf.sug lew.fer.nob.diz) ?=(^ yed.caq.nob.diz))
        :_  ..wisp
        :-  %fast
        %^  cat  7
          p.u.yed.caq.nob.diz 
        (en:r:cley:diz q.u.yed.caq.nob.diz (jam lun ham))
      ?:  =(%back -.ham)
        [wasp ..wisp]
      =+  yig=sen:sug
      =+  ^=  guf
          |=  key=code
          %^    jam
              [p.yig p.soq] 
            (pare lew.fer.nob.diz law.saf.sug) 
          (sign:se:q.yig key (jam lun ham))
      ?:  =(~ lew.wod.nob.diz)
        [[%open (guf *code)] ..wisp]
      =+  fuy=(griz:diz now)
      =>  %_(. diz q.fuy)
      :_  ..wisp
      :-  %full
      =+  cay=cley:diz
      (jam p.cay (seal:pu:r.cay p.fuy (guf p.fuy)))
    --                                                  ::  (wish)
  --                                                    ::  (am)
::
++  ames                                                ::  terminal handling
  ^-  vane                                              ::  kernel instrument
  =|                                                    ::  instrument state
      $:  fox=fort                                      ::  network state
      ==                                                ::
  |=  [now=@da eny=@ sky=||(* (unit))]                  ::  current invocation
  ^?                                                    ::  opaque core
  =<
    |%                                                  ::  poke/peek pattern
    ++  beat                                            ::  process move
      |=  [whu=(unit flag) tea=tire hen=hose fav=card]
      ^-  [p=(list move) q=vane]
      =^  duy  ..knap
        (knap hen fav)
      [duy ..^$]
    ::
    ++  scry
      |=  [our=flag ren=@tas his=flag lot=coin tyl=path]
      ^-  (unit)
      ?.  =(0 ren)  ~
      ?+    lot  ~
          [%% %ud @]
        (perm our his q.p.lot tyl)
      ::
          [%% %da @]
        ?.  =(now q.p.lot)  ~
        (temp our his tyl)
      ==
    --
  |%
  ++  clop
    |=  [now=@da hen=hose bon=boon]
    ^-  [(list move) _+>]
    ?-    -.bon
        %beer
      :-  :~  [[~ p.bon] [/c/ hen] [%keep p.bon]]
              [[~ p.bon] hen [%init p.bon]]
          ==
      +>
    ::
        %coke  !!
        %mead  !!
        %milk  !!
        %ouzo  !!
        %sack  !!
        %wine  !!
    ==
  ::
  ++  knap
    |=  [hen=hose fav=card]
    ^-  [(list move) _+>]
    =+  ^=  fuy  ^-  [p=(list boon) q=fort]
        ?+    -.fav  !!
            %cash
          (~(have am [now fox]) p.fav q.fav)
        ::
            %hear
          (~(gnaw am [now fox]) p.fav)
        ::
            %junk
          [~ fox(any.ton (shax (mix any.ton.fox p.fav)))]
        ::
            %make
          =+  vun=(~(come am [now fox]) p.fav q.fav r.fav)
          [[[%beer p.vun] ~] q.vun]
        ==
    =>  %_(. fox q.fuy)
    =|  out=(list move)
    |-  ^-  [p=(list move) q=_+>.^$]
    ?~  p.fuy
      [(flop out) +>.^$]
    =^  toe  +>.^$
      (clop now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld (flop toe) out))
  ::
  ++  perm
    |=  [our=flag his=flag mar=@ud tyl=path]
    ^-  (unit)
    ?~  tyl  ~
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      ?:  ?=([%will ~] tyl)
        =+  fod=(~(get by hoc.saf.u.gys) his)
        ?~  fod  ~
        (rick mar his lew.wod.u.fod)
      ~
    ?:  ?=([%buck ~] tyl)
      =+  muc=(rice mar sex:u.gys)
      =+  luw=(rick mar our law.saf.u.gys)
      ?.  &(?=(^ muc) ?=(^ luw))  ~
      [~ `buck`[u.muc u.luw]]
    ?:  ?=([%will ~] tyl)
      (rick mar our law.saf.u.gys)
    ~
  ::
  ++  rand
    |=  [lem=@ ent=*]
    ^-  [p=@uvI q=_+>]
    =+  yan=(mix (sham ent) any.ton.fox)
    [(~(raw og yan) lem) +>.$(any.ton.fox (shax yan))]
  ::
  ++  temp
    |=  [our=flag his=flag tyl=path]
    ::  ~&  [%temp our his tyl]
    ^-  (unit)
    ?.  ?=([%mark ~] tyl)
      =+  muc=$(tyl [%mark ~])
      ?~  muc  ~
      (perm our his (,@ud u.muc) tyl)
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      =+  fod=(~(get by hoc.saf.u.gys) his)
      ?~  lew.wod.u.fod  ~
      [~ `@ud`p.p.q.i.lew.wod.u.fod]
    ?~  val.saf.u.gys  ~
    [~ `@ud`p.i.val.saf.u.gys]
  --
--
