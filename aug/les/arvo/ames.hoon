!:
::  ames (4a), networking
::
  |=  pit=vase
  ^-  vane
  =>  =~
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aA, identity logic           ::
  ::
  |%
  ::
  ++  grip                                              ::  extend will
    |=  [wet=will law=will]
    ^-  will
    ?~  wet  law
    ?:  =(wet law)  law
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
  ++  meld                                              ::  verify connect
    |=  [new=deed old=deed]
    ^-  &
    ?>  (melt new old)
    ?>  =((shaf %meld (sham q.new)) (need (sure:pu:(hail r.q.old) *code p.new)))
    %&
  ::
  ++  melt                                              ::  proper connect
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
  ++  pare                                              ::  shorten against
    |=  [fou=will law=will]
    ::  ~&  [%pare-fou fou]
    ::  ~&  [%pare-law law]
    ^-  will
    =+  [ouf=(flop fou) wal=(flop law)] 
    %-  flop  
    |-  ^-  will
    ?~  ouf  wal
    ?~  wal  ?>(=(~ ouf) ~)
    ?.  =(i.wal i.ouf)  ouf
    $(wal t.wal, ouf t.ouf)
  ::
  ++  pier                                              ::  initial deed
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
  ++  real                                              ::  validate
    |=  [mac=mace law=will]  
    ?>  ?&  |-  ^-  ?
            ?~  mac  &
            ?>  ?&  ?=(^ law)
                    (lth p.p.q.i.law 9)                 ::  9-lives rule
                    =(p.p.q.i.law p.i.mac)
                    =(r.q.i.law pub:ex:(wear q.i.mac))
                ==
            $(mac t.mac, law t.law)
        ==
    %&          
  ::
  ++  rice                                              ::  mace at life
    |=  [mar=life mac=mace]
    ^-  (unit mace)
    ?~  mac  ~
    ?:  =(mar p.i.mac)  [~ mac]
    ?:  (gth mar p.i.mac)  ~
    $(mac t.mac)
  ::
  ++  rick                                              ::  will at life
    |=  [mar=life lag=seat law=will]
    ^-  (unit will)
    ?~  law  ~
    ?:  =(mar p.p.q.i.law)  [~ law]
    ?:  (gth mar p.p.q.i.law)  ~
    ?:  |(?=(~ q.p.q.i.law) !=(lag r.p.q.i.law))  ~
    $(law t.law)
  ::
  ++  zeno                                              ::  imperial keyprint
    |=  zar=@pD
    ^-  @uvH  ^-  @
    %+  snag  zar
    :~  0w3S.~jSFT.Tvv6a.QoDQ8.1mNQS  0w30.ND~fW.anIzT.qYdW1.S6ln~  ::   0
        0w38.Se5UL.G8DX-.FTRGy.i5Yff  0w3M.0pNL6.fSzmw.HMWVx.APFVF  ::   2
        0w2-.RYAgv.YL-Pa.irOZy.tk5Vy  0wt.EVqLx.FlTgt.2toOs.rrG8L   ::   4
        0w1Y.MZuVL.6mijb.VS4Kz.cqe6g  0w1n.~qRP8.kGXJd.Ru~aJ.lHgZH  ::   6
        0wg.Wr-Xu.AskUk.4Q8qw.tWxes   0w22.RvZr7.~LmGf.ILCzr.b5UIW  ::   8
        0w3b.K--FX.k3in2.3jjmx.d1gyw  0w3l.fgx0I.L4ZEj.KkQmK.NYYqE  ::   10
        0wh.g7gbM.vZXiO.pIFKr.Og2I2   0w36.wWYRk.pBRW1.MsrEv.jZav5  ::   12
        0w2o.g0CEB.zmtmB.9F-C0.itjGE  0wk.6BuqK.t~Dwn.NvL3s.Ofo5-   ::   14
        0wY.N8qMo.PqaM~.qyjCn.Nr8ij   0w2h.FGOp6.eNq2N.DpPNh.dR5Bi  ::   16
        0w2a.Iy2Yh.IkjLD.U52Vd.7LDc0  0w18.mrVGR.ITtzj.NoRER.XPitH  ::   18
        0w1x.vL5bh.dGDxJ.xdP8Y.fBLh2  0w2H.ZAAZk.Dp5Kg.J8fim.lZk~~  ::   20
        0w2z.Ul9bk.ANzG4.TnmLi.k5fM6  0w32.XkwN3.pYHKR.0cdHB.38sYB  ::   22
        0w3l.q0Eob.P1Yhg.pYxMf.Fsalh  0w1x.5oS3C.uOPyK.80jPk.9M6i6  ::   24
        0w1e.u4jEM.CY7XO.ZLy51.~dYCZ  0w1A.rb5s1.cV5L3.vVURh.icf6d  ::   26
        0wD.Y2VGx.ut-00.A0P8Z.MWWh1   0w1u.koQrY.t5PNs.duv-O.esqpA  ::   28
        0w12.dxP-u.9uPND.J0VWa.u9Ko5  0w32.EDCeC.B7hS8.6TXyj.3yRS2  ::   30
        0w1O.X0TRZ.AuriA.Uh0Nm.9GdYD  0w14.88B~P.6unHk.LgOQe.r6cta  ::   32
        0w1R.0Q0WY.pQCkk.jvgwl.8gjha  0w3i.MIko2.kVgPj.Tr-cx.jqDJe  ::   34
        0w1x.ukLOn.WZ9vO.rlRZM.vgMW6  0w1R.Bo5P0.~oCCD.B1TRz.zl9xb  ::   36
        0w36.~NNif.NuEVa.6ucpM.qcC95  0w8.KBjKH.-uoDx.J72hB.vU4ax   ::   38
        0w36.FzrMs.lMrX-.qPs6W.hTWqr  0wxe-VB.vhARe.DWuQA.ZSiyd     ::   40
        0w3b.Iq0fg.zFDHS.AP0z7.ml7~y  0wU.B9l~R.qkNnQ.O~ID6.J78K-   ::   42
        0w1q.liHGK.IXit2.GZf7z.DWzEx  0wc.h5XmS.HAGbG.oR0Mw.jrpR4   ::   44
        0w1a.as~x6.vZdvb.zPWFd.xaPkX  0w2b.eeIGA.6Jeyg.cJw1c.1Qst6  ::   46
        0w1J.cUzsd.x0G2u.Xal20.qF0rX  0w2a.I5cQ7.AAv~0.xWn-Q.8jMuc  ::   48
        0w3X.n6zna.Sigk3.piuU8.ygp4b  0w2f.i4gtc.IA9iP.86Lpi.I~pM8  ::   50
        0w2a.ExCEn.HUzHA.-SHQj.afFrj  0w8.wnYei.iTU10.GXNRO.bH-1k   ::   52
        0w17.m7rio.h00a3.WNF4U.lp0cl  0w3b.FIWWS.RAqFC.EtEPC.UWQN8  ::   54
        0wI.fQO1d.xx1yR.t3df-.J3j-l   0w3f.UK9y6.igC6M.fFXIU.OTM0Z  ::   56
        0w3T.fA2Gc.Mby28.2x6hB.wyKyl  0w2F.AjZqU.9v1CU.-x1G8.6-sNQ  ::   58
        0wH.fyopd.Cw6xh.i9Nje.m6jTm   0wn.Nzlwv.cmdeq.Uc8Gj.Jg2in   ::   60
        0w4.LWGCU.DdAp7.UyGte.00cve   0w2K.6U5Hq.mBNoY.fBJZP.PvBfm  ::   62
        0w1J.eGEA9.Eu0Yb.FJHxg.dthi~  0w2g.NKa13.9rSVu.aVbD4.7qorD  ::   64
        0wN.R0W1o.Cfh2P.O53jO.R0U3v   0w3w.CmuOI.wcVen.7Res8.BfQdF  ::   66
        0w1x.oj4Ls.0-D0m.hTWnI.zNCQl  0w29.PLdYn.6OO8f.8RumG.xsjPX  ::   68
        0wa.wUvMq.X-o0r.VNJbR.mgrPZ   0w~.jUiaH.RGdon.-PajT.9wP7-   ::   70
        0w2~.aUH4q.S5N1W.8xPxY.M2~zO  0w1C.os7Nj.Vx35d.7Q-fA.Eg1DG  ::   72
        0w3h.bHzLo.j8j76.RLLaD.dIA4k  0w3y.VSAY8.WYsQE.bO-BH.G9mL9  ::   74
        0w24.~6NCR.yfssv.855xz.W6WRY  0wv.gnZVt.PbBBu.sGceB.YN4Ei   ::   76
        0w1m.5C4DD.xc6PQ.xpf2P.sok64  0w3~.diSDP.Ii7lo.ODX69.7yHlT  ::   78
        0w2K.2nme~.A7cAq.kAUlv.dLyH7  0w32.pzrrj.TLc0W.2cpnT.2mg73  ::   80
        0w2z.aZk0W.y~D8o.CP9yz.Q7dLF  0w1N.OliXg.TSA09.FAEbC.RMCwB  ::   82
        0ws.3mp4d.dj8KM.g8~y5.6IBeB   0w2h.SizdC.qp4Ms.uX~7o.R6ZUj  ::   84
        0w3d.1AOBs.BFrDI.BFsMh.lFPQ8  0wA.UnHHL.rK4~E.t0b6f.yTYF4   ::   86
        0wr.A~Nqs.1fj1i.PC-lk.F9iE~   0wI.PEphk.Wyj5-.dLEFT.kCdVT   ::   88
        0w1o.bM7sz.~tMd4.dLjdi.xW3Ls  0w3Y.-aRL2.KQHKh.yoXTG.mmDHD  ::   90
        0w2J.KBBNK.U74hE.lix12.BavH2  0w13.qdig~.M4Uuq.MRib4.Q3Ky6  ::   92
        0w23.bZI5L.6mTMr.My6X6.865m7  0w1h.nz0KB.aUUIF.n-D5a.4xtPB  ::   94
        0w2L.fhTRg.~rW1e.VwGax.8IviJ  0w31.FL3Tz.wvH02.7XNCq.NR2nv  ::   96
        0w20.P-IPC.lgAWB.LGkW6.rAE1Q  0w4.r4lBB.8OYkg.Kvghs.07VlL   ::   98
        0wz.-oU3r.NwhLX.wBZxF.qQ6AZ   0w27.ctO2S.2bW2V.v7JlZ.x3fLn  ::   100
        0wa.pFu1U.d3bbE.WnUY1.Yag3K   0w1L.W2vj8.hGiRy.iJXoz.qjPRp  ::   102
        0w1.Om393.lhj7B.DTzoX.0wAOc   0w1e.0T3YN.h0J7G.zw5Ab.mO2QB  ::   104
        0w4.s7XS1.f2Tj2.YlVxM.ILkFH   0w3f.k9yrH.SxmBr.YSPhv.aqLkY  ::   106
        0w2K.jsuN4.ZfMtj.o-ocH.6cNpQ  0we.vLn5o.DllGu.kEdNm.kgLpJ   ::   108
        0w26.NW4Uc.aEMAk.vo1HD.ddE68  0w3e.x5kBS.5SsJH.9dWVb.S70xb  ::   110
        0w21.XAbQj.paeqm.tXZKO.5fMGk  0wH.P3jm8.2RIRA.XQCH4.Q71jo   ::   112
        0w2C.-f8PI.j0XIS.5fjHZ.s84UU  0w3d.alVIB.bTuW~.ByD21.68JxA  ::   114
        0w2f.NTomJ.tZ4dQ.ZHDDV.Z0ipN  0w12.7khnd.VDpwi.fShgm.JXG2b  ::   116
        0wN.u8bGU.DbGvy.Za8Aq.CeNqE   0w3e.f42fy.sU3Z6.fqYTe.d5edO  ::   118
        0w2r.pbf2S.PHGY3.Ze39R.A0NDE  0w3.DdLZb.OgNTx.EGwBQ.WuFbq   ::   120
        0w3X.ahHsz.zsSUg.DPlEl.zoWDg  0w1b.t9vRF.S7QZA.bngDD.Zlh~0  ::   122
        0wd.wVHob.9tzEJ.NRufV.Z6hjF   0w1R.a9Idk.~GFW7.SGd55.lM6MY  ::   124
        0w1f.AfxPt.V49b2.FyFB9.x9oW0  0w3z.PJSCS.tXrGp.4Abs6.PGz9p  ::   126
        0w3K.p9kbC.nBcft.xlw7Q.NYbMi  0w1O.Ms9k-.CrtRV.eG50S.Spdbp  ::   128
        0wF.tLOu9.vjUbL.IHrH1.~vLPJ   0w3W.a9ef2.LAUjL.GgEsN.GiugV  ::   130
        0w1j.K6mVX.5ACQ7.rXcTK.lu21O  0w2z.OPxvQ.kLIeh.jXQVG.rFYfc  ::   132
        0wI.Cw6Zq.71WIO.Ty-CK.zKjcc   0w2Q.S0cSl.QZUxA.bJ2d5.N2UgG  ::   134
        0w1w.NuykY.eXAag.-k2t8.hgjPr  0w2U.Af8yO.SL7uc.vhxTH.XnH0Z  ::   136
        0w3e.SEnhl.0iyEb.SSF-J.zQtZu  0w1B.Uro1o.4tne5.gHcXg.pT7MA  ::   138
        0wf.zM-YB.fmCaO.jtLFS.~evFs   0w3M.dUiZE.~~llD.way6e.BJA6g  ::   140
        0w3Z.BEMuW.pSkP9.L6Ex0.zfjI4  0w3f.iAieq.9A2~g.2yP6c.HSIvn  ::   142
        0w2G.pk07~.dom4v.leM-y.RYiDm  0wM.oCXXi.rjCy0.AYczB.MGz61   ::   144
        0w3M.1p9yX.2C7vD.PTk9E.maHEI  0w21.lsmsz.rJ2kh.rEwsN.PV5qZ  ::   146
        0w-.~i5Wy.sQhQp.LZedM.7C2u6   0w3B.vDzKx.kkBpv.gHhR0.AclVS  ::   148
        0w2v.eSYMB.DPJRf.daDE5.jwcDL  0wy.CpDQP.pjFLF.h2kTw.qDmdG   ::   150
        0w1b.snC0c.QYJTr.LgIfa.mVTnK  0wt.IOiHu.sWx85.658dr.CG6X7   ::   152
        0w23.1ogCR.B8fuF.-saEx.HNxKx  0w34.HOzse.VexO4.o7yMQ.noLM5  ::   154
        0w1x.zY3Kl.Wxf5l.hzbSG.GIKtp  0w2F.eUFB7.bkP4w.vwYp3.D44AX  ::   156
        0wL.7l2X5.2PVtz.WQBZ8.i60Qs   0w1f.U2mM9.XLver.PCh6E.oGhc3  ::   158
        0w2l.qE~AA.Jw5-r.vmi40.Kj5qm  0wU.wiL0I.pzMtI.MICej.WHlfA   ::   160
        0w3w.ZDeAB.0eJfq.PZflU.Sdab8  0w10.k5psC.Ndt2f.WCRjm.Lvfyh  ::   162
        0wc.d1AQF.oTu4F.f~WNk.f48uh   0w2S.WwOiS.LjfFM.eBDkr.t~B2Q  ::   164
        0w2P.JTZOH.1zgRw.Ds-tK.aTmxU  0ww.ElLh7.abdpW.NLq9Y.Wr-Bx   ::   166
        0w19.t5jic.8ngs7.nZO1W.~GQa8  0w31.qBEzw.ryf0S.WryVm.DBNhY  ::   168
        0w2q.emSKl.dFIZG.78de7.~~SL9  0w2Y.hm5zi.Rqu~m.uym-Q.WLOkB  ::   170
        0w2M.aUUYp.xsOmR.CN14v.UexEp  0ww.Za857.qUERi.5QcJ5.W9a-q   ::   172
        0wW.ExlFh.y2aKD.ETZIw.VeDNW   0w6.PrCJQ.YlHWR.-xtmu.b51bd   ::   174
        0w1z.9Gyea.18KDm.RcBD8.CS2BD  0w2u.IKecS.x35k7.phrz0.qOTec  ::   176
        0w1s.zBB0N.Q~AvX.FMwfW.Uer4C  0w1k.o0x38.Jtxke.p1gvE.11qzS  ::   178
        0wg.cifn9.XHPZA.cfXlu.AvRbL   0wv.w~fGn.jo2Ez.a2tuI.99T2V   ::   180
        0w2b.QA0JN.cP1TS.poivA.5d3fP  0w2K.OIeSP.xRiDn.nvHx7.xfFjf  ::   182
        0wC.h9IaN.aG649.xbXxg.DJpP8   0w1R.LkE0N.Vqp9W.CAjej.HT1Yg  ::   184
        0w3p.gFUjP.kjIHY.OEggl.F5vfb  0wy.eivdj.5YSZx.hLXJl.COBez   ::   186
        0wC.IQjBn.Ik90-.mjL60.AD0WO   0w2d.oGU4e.7tOM2.efCgF.uABvW  ::   188
        0w2V.r9jPm.U8zn1.9hfMt.fiTDX  0w3P.x1KaW.mKR9k.f39kG.Ppofh  ::   190
        0wtfvvK.7YcA-.PXK39.MUVll     0w1t.2w61e.hWl4E.IMgSN.f4SWt  ::   192
        0w8.2B3qo.TIbCg.SxPF8.lFbMi   0w36.rDl-H.MhEap.EMEXW.vUOWH  ::   194
        0w1n.INtMj.x1WMR.0VFNk.AgYbM  0wH.ZkgU3.~lvfq.hVM7i.9591C   ::   196
        0w2E.Gm4nI.hCQKy.2glWa.M0WjD  0w1j.AEjKH.gc~DF.2uXVG.aLrby  ::   198
        0w2j.LF6Qh.3o3a5.XRJPQ.J5FpM  0ww.6LB2C.UoF09.9GZfi.gch2f   ::   200
        0w2x.7wdmP.Y8DS4.XnAM1.30Wkv  0w1G.qYMLN.Vxhgy.wdUN9.5WaI~  ::   202
        0w2p.~jWkf.JLvZi.HmWJE.au0Po  0w2V.RpCf7.PbS4g.os6hn.lu1-a  ::   204
        0w1z.GHYrP.jzLDr.W5nRz.LJmyZ  0w1x.JRmWD.APKn6.FsA4y.3RCE5  ::   206
        0w1T.D8e0D.3ypXE.rc9Lt.c2s2I  0w2G.6VyYW.SqNbp.0yfeJ.aTnav  ::   208
        0w3V.ZRpHJ.Ox-CH.B0qts.p4z1o  0w39.7N3Iz.1KDde.i7rNR.EFN~y  ::   210
        0w21.0rU1q.b~YTZ.ZFRP3.zMf9I  0wh.Rc7FB.nmz7g.3ExfQ.mMe3p   ::   212
        0w1R.IuTL1.-EEB0.0odzt.5AhVA  0wn.2fNnS.TxhlZ.EBDPU.~f9l1   ::   214
        0wp.H5FLb.xC2Gm.sLlL8.nyR7X   0w2f.KzUDv.mZm~F.es74S.l8fiy  ::   216
        0w3P.Xfw1w.zZd2x.tHjaC.ao3P3  0w31.Og7rL.bhHvy.qTJis.T7e-z  ::   218
        0w2g.8YwXo.xRTKA.ZYhWB.Rq28z  0wY.RhnaN.b0ko5.GTHTT.Ddvva   ::   220
        0w28.4S34l.q61J8.VPTDl.NRnEn  0w2g.bm1-u.6dpht.Cezlg.PWM0I  ::   222
        0wp.~mtzy.vnfCp.46j87.Bxx5I   0w3l.9~XqY.X7ddr.sXfw1.sm3SZ  ::   224
        0w1v.q04XA.H8pdl.05j~O.VJJRk  0w3j.diQXO.cRdyk.6eMBn.O4BWa  ::   226
        0w2J.zHD5i.BWFZu.sjy3r.VLUAK  0w1P.mp4uM.NStQ2.m43VG.jpqD9  ::   228
        0w3e.a76P-.bYOkJ.XN8Ln.UMIZe  0w1t.IFkPX.~vYgQ.mher3.-gr1d  ::   230
        0w2U.2Gj5j.fbcq~.r7EWM.Lwguk  0w18.wq3oy.T9mWp.rec0M.iB5-i  ::   232
        0w2b.jQbjN.gql8T.b0HMF.vExaA  0w3c.xsS4n.zHncI.WAsQm.dLLEN  ::   234
        0w3d.v53m-.hDHXl.n9cjB.twhJL  0w2g.ymdjB.hjojf.d9vYT.MrFxX  ::   236
        0w3p.btBOc.y9FZI.SxD8c.M8XYF  0w2P.JTw6N.GJAIT.TzUM5.-mmBW  ::   238
        0wN.pF9v5.tMMw7.3o046.QvRTP   0w3s.XI-zP.4vbbh.QhSSB.C2zcz  ::   240
        0wM.MR-TA.niALg.Nc7vP.h~rBF   0wr.ah6Hr.dicUS.4Zihk.Q5BaU   ::   242
        0wc.R5xdR.IkvuE.LflTY.Fh9VD   0w3t.vPYH-.AD0Eu.Zx3zW.yc~Wi  ::   244
        0wr.C1cXX.q3mYj.HTSmz.3xbnu   0wF.6WcHt.KWcYY.o-aYv.-crZo   ::   246
        0w2~.T2HZ5.5JpXI.tm6Yl.-i2IH  0w1A.uhltk.we6mf.79anL.8BCLh  ::   248
        0w2X.QHRaP.SWX87.40tak.Tc5JY  0w1I.3mZvr.R2O2y.Pf3vn.0xHe3  ::   250
        0w2k.EqHtx.~PpLs.flWnF.kedAk  0w2S.ZcRF6.7qHEk.0NcHS.wT9gD  ::   252
        0wW.pTnj-.7YASZ.L~tww.Sm3~W   0w1s.~FgPE.spDGJ.0XtT6.KoAQb  ::   254
    ==
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aB, packet format            ::
  ::
  |%
  ++  bite                                              ::  packet to cake
    |=  pac=rock  ^-  cake
    =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
    =+  :*  vez=(end 0 3 mag)                           ::  protocol version
            chk=(cut 0 [3 19] mag)                      ::  checksum
            dit=(cut 0 [22 1] mag)                      ::  fragment bit
            wix=(bex +((cut 0 [23 2] mag)))             ::  width of receiver
            vix=(bex +((cut 0 [25 2] mag)))             ::  width of sender
            tay=(cut 0 [27 5] mag)                      ::  message type
        ==
    ?>  =(0 vez)
    ?>  =(chk (end 0 19 (mug bod)))
    :^    [(end 3 wix bod) (cut 3 [wix vix] bod)]
        =(0 dit)
      (snag tay [%none %open %fast %full ~])
    (rsh 3 (add wix vix) bod)
  ::
  ++  spit                                              ::  cake to packet
    |=  kec=cake  ^-  @
    =+  wim=(met 3 p.p.kec)
    =+  dum=(met 3 q.p.kec)
    =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
    =+  qax=?:((lte dum 2) 0 ?:((lte dum 4) 1 ?:((lte dum 8) 2 3)))
    =+  wix=(bex +(yax))
    =+  vix=(bex +(qax))
    =+  bod=:(mix p.p.kec (lsh 3 wix q.p.kec) (lsh 3 (add wix vix) s.kec))
    =+  tay=?-(r.kec %none 0, %open 1, %fast 2, %full 3)
    %+  mix
      %+  can  0
      :~  [3 0]
          [19 (mug bod)]
          [1 q.kec]
          [2 yax]
          [2 qax]
          [5 tay]
      ==
    (lsh 5 1 bod)
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aC, PKI engine               ::
  ::
  |%
  ++  go                                                ::    go
    |_  ton=town                                        ::  ames state
    ++  as                                              ::    as:go
      |_  [our=seat saf=safe]                           ::  per server
      ++  born                                          ::    born:as:go
        |=  [now=@da her=@p tic=@pG gec=gcos pub=pass]  ::  register user
        ^-  [(unit will) _+>]
        ?.  =(our (sein her))  [~ +>.$]
        =+  nes=sen
        ?.  =(tic (end 6 1 (shaf %tick (mix her (shax sec:ex:q.nes)))))
          [~ +>.$]
        =+  rad=(~(get by hoc.saf) her)
        ?^  rad
          ?>  ?=(^ lew.wod.u.rad)
          ?.  =(pub r.q.i.lew.wod.u.rad)  [~ +>.$]
          [[~ lew.wod.u.rad] +>.$] 
        =+  syp=[[0 [~ p.nes] her now] gec pub]
        =+  ded=[(sign:se:q.nes *code (shaf %meld (sham syp))) syp]
        =+  wil=[ded law.saf]
        ?>  =(wil (grip wil ~))
        :-  [~ wil]
        +>.$(hoc.saf (~(put by hoc.saf) her [[~31337.1.1 ~ wil] ~ *cask]))
      :: 
      ++  lax                                           ::    lax:as:go
        |_  [her=seat dur=door]                         ::  per client
        ++  cluy                                        ::    cluy:lax:as:go
          ^-  [p=life q=gcos r=acro]                    ::  client crypto
          ?~  lew.wod.dur  !!
          :+  p.p.q.i.lew.wod.dur 
            q.q.i.lew.wod.dur 
          (hail r.q.i.lew.wod.dur)
        ::
        ++  clon
          ^-  life
          ?~(lew.wod.dur 0 p.p.q.i.lew.wod.dur)
        ::
        ++  deng
          |=  law=will
          %_(+> lew.wod.dur (grip law lew.wod.dur))
        ::
        ++  griz                                        ::    griz:lax:as:go
          |=  now=@da                                   ::  generate key for
          ^-  [p=code q=_+>]
          =+  key=(shas %enty (mix now any.ton))
          :-  key
          %=  +>.$
            any.ton      (shax (mix now any.ton))
            heg.caq.dur  (~(put by heg.caq.dur) (shaf %hand key) key)
          ==
        ::
        ++  kuch                                        ::    kuch:lax:as:go
          |=  had=hand                                  ::  hear key tag
          ^-  (unit ,[code _+>])
          =+  wey=(~(get by heg.caq.dur) had)
          ?^  wey
            =+  key=u.wey
            :+  ~  key
            %=    ..kuch
                yed.caq.dur  [~ had u.wey]
                heg.caq.dur  (~(del by heg.caq.dur) had)
                qim.caq.dur  (~(put by qim.caq.dur) had key)
            ==
          =+  dyv=(~(get by qim.caq.dur) had)
          ?~  dyv  ~
          [~ u.dyv ..kuch]
        ::
        ++  trox                                        ::    trox:lax:as:go
          |=  [now=@da]                                 ::  expire by date
          ^+  +>
          +>    ::  XX
        ::
        ++  wasc                                        ::    wasc:lax:as:go
          |=  key=code                                  ::  hear foreign code
          ^+  +>
          =+  had=(shaf %hand key)
          %_    ..wasc
              yed.caq.dur  [~ had key]
              qim.caq.dur  (~(put by qim.caq.dur) had key)
          ==
        ::
        ++  wisk                                        ::    wisk:lax:as:go
          |=  [way=seat ryn=(unit lane) pac=rock]       ::  route via
          ^-  [p=lane q=rock]
          ?:  &(=(way her) =(~ ryn) ?=(^ lun.wod.dur))
            ?>  ?=(^ lun.wod.dur)
            [u.lun.wod.dur pac]
          =+  rad=(~(get by hoc.saf) way)
          ?>  &(?=(^ rad) ?=(^ lun.wod.u.rad))
          =+  mal=(jam `meal`[%fore her ryn pac])
          :-  u.lun.wod.u.rad
          %-  spit
          ^-  cake
          :*  [our way]
              &
              ?~  yed.caq.u.rad
                [%none mal]
              :-  %fast
              %^  cat  7
                p.u.yed.caq.u.rad 
              %+  en:crya                               ::  XX assumes crya
                q.u.yed.caq.u.rad
              (jam `meal`[%fore her ryn pac])
          ==
        ::
        ++  xang                                        ::    xang:lax:as:go
          ^-  (unit seat)                               ::  general routing
          ?^  lun.wod.dur  [~ her]
          =+  [pig=& seg=(sein her)]
          |-  ^-  (unit seat)
          ?:  =(seg our)  ~
          ?:  =(seg her)  ?.(pig ~ $(pig |, seg (sein our)))
          =+  zep=(~(get by hoc.saf) seg)
          ?:  &(?=(^ zep) ?=(^ lun.wod.u.zep) ?=(^ yed.caq.u.zep))
            [~ seg]
          ?:  (lth seg 256)  ~
          $(seg (sein seg))
        ::
        ++  zuul                                        ::    zuul:lax:as:go
          |=  [now=@da ham=meal]                        ::  encode message
          ^-  [p=(list rock) q=_+>]
          =<  weft
          |%
          ++  wasp                                      ::  null security
            ^-([p=skin q=@] [%none (jam ham)])
          ::
          ++  weft                                      ::  fragment message
            ^-  [p=(list rock) q=_+>.$]
            =^  gim  ..weft  wisp
            :_  +>.$
            ^-  (list rock)
            =+  wit=(met 13 q.gim)
            ?<  =(0 wit)
            ?:  =(1 wit)
              =+  yup=(spit [our her] & p.gim q.gim)
              [yup ~]
            =+  ruv=(rip 13 q.gim)
            ?>  ?=(^ ruv)
            =+  may=(spit [our her] | p.gim (jam wit (shaf %weft q.gim) i.ruv))
            =+  dam=(shaf %flap may)
            =+  inx=1
            :-  may
            |-  ^-  (list rock)
            ?~  t.ruv  ~
            =+  ^=  vie
                %^    spit
                    [our her]
                  &
                wasp(ham [%carp inx dam i.t.ruv])
            :-  vie
            $(t.ruv t.t.ruv, inx +(inx))
          ::
          ++  wisp                                      ::  generate message
            ^-  [[p=skin q=@] q=_..wisp]
            ?:  =(%carp -.ham)
              [wasp ..wisp]
            ?:  !=(~ yed.caq.dur)
              :_  ..wisp
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dur 
              (en:r:cluy q.u.yed.caq.dur (jam ham))
            ?:  &(=(~ lew.wod.dur) =(%back -.ham))
              [wasp ..wisp]
            =^  tuy  +>.$
              ?:(=(~ lew.wod.dur) [*code +>.$] (griz now))
            :_  ..wisp
            =+  yig=sen
            =+  ^=  gom
                %^    jam
                    `life`p.yig
                  ::  `will`(pare wyl.dur law.saf)      ::  XX not set
                  law.saf                               ::  XX send whole will
                `@`(sign:se:q.yig tuy (jam ham))
            ?:  =(~ lew.wod.dur)
              [%open gom]
            :-  %full
            =+  cay=cluy
            (jam p.cay (seal:pu:r.cay tuy gom))
          --                                            ::  --zuul:lax:as:go
        --                                              ::  --lax:as:go
      ::
      ++  gur                                           ::  default door
        |=  her=seat
        ^-  door
        =+  def=?.((lth her 256) ~ [~ %if 13.337 (mix her .0.0.1.0)])
        [[~2100.1.1 def ~] ~ *cask]
      ::
      ++  myx                                           ::  door by seat
        |=  her=seat
        ^+  lax
        =+  fod=(~(get by hoc.saf) her)
        ~(. lax [her ?~(fod (gur her) u.fod)])
      ::
      ++  nux                                           ::  install door
        |=  new=_lax
        ^+  +>
        +>(hoc.saf (~(put by hoc.saf) her.new dur.new))
      ::
      ++  sen                                           ::  current crypto
        ^-  [p=life q=acro]
        ?~(val.saf !! [p.i.val.saf r.i.val.saf])
      ::
      ++  sev                                           ::  crypto by life
        |=  mar=life
        ^-  [p=? q=acro]
        ?~  val.saf  !!
        ?:  =(mar p.i.val.saf)
          [& r.i.val.saf]
        ?>  (lth mar p.i.val.saf)
        :-  |
        |-  ^-  acro
        ?:  =(mar p.i.t.val.saf) 
          r.i.t.val.saf 
        $(t.val.saf t.t.val.saf)
      ::
      ++  sex                                           ::  export secrets
        |-  ^-  mace
        ?~  val.saf  ~
        :-  [p.i.val.saf sec:ex:r.i.val.saf] 
        $(val.saf t.val.saf)
      ::
      ++  yew                                           ::  best will for
        |=  her=seat
        ^-  will
        =+  gel=(~(get by hoc.saf) her)
        ?^  gel
          lew.wod.u.gel
        ?:((lth her 256) ~ $(her (sein her)))
      --                                                ::  --as:go
    ::
    ++  ha                                              ::  adopt new license
      |=  [our=seat mac=mace wil=will] 
      ^-  town
      ?>  !=(~ mac) 
      ?>  ?=(^ wil) 
      ::  ?>  =(our r.p.q.i.wil) 
      ?>  =(wil (grip wil ~))
      ?>  (real mac wil)
      %_    ton
          urb
        %+  ~(put by urb.ton)
          our
        :*  %-  flop
            |-  ^-  (list seat) 
            ?:((lth our 256) ~ =+(seg=(sein our) [seg $(our seg)]))
        ::
            (turn mac |=([p=life q=ring] [p q (wear q)])) 
            wil 
            ~
            ~
        ==
      ==
    ::
    ++  su                                              ::  install safe
      |=  new=_as
      ^-  town
      ton(urb (~(put by urb.ton) our.new saf.new))
    ::
    ++  ti                                              ::  expire by time
      |=  [now=@da]
      ^-  town
      !!
    ::
    ++  us                                              ::  produce safe
      |=  our=seat
      ^-  (unit ,_as)
      =+  goh=(~(get by urb.ton) our)
      ?~  goh  ~
      [~ ~(. as [our u.goh])]
    --                                                ::  --go
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::                section 4aD, congestion control       ::
  ::
  |%
  ++  baby                                            ::  new flow
    ^-  flow
    [~s1 4]
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
    |=  [rot=@ foy=flow]  ^-  flow
    ?:  =(0 rot)  foy
    $(rot (dec rot), wid.foy ?:(=(1 wid.foy) 1 (div wid.foy 2)))
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aE, acknowledgments          ::
  ::
  |%
  ::
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
    |-  ?:  =(num p.ski)
          [(flop wop) ski]
        $(num (dec num), wop [num wop])
  ::
  ++  toss  |=(ski=snow ^-(snow ski(q +(q.ski))))
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aF, packet queue             ::
  ::
  |%
  ++  pe                                                ::  packet queue
    |_  shed
    ++  busc                                            ::    busc:pe
      |=  num=@ud                                       ::  find by number
      ^-  (unit bird)
      ?~  puq  ~
      ?:  =(num p.n.puq)
        [~ q.n.puq]
      ?:((gth num p.n.puq) $(puq l.puq) $(puq r.puq))
    ::
    ++  doze                                            ::    doze:pe
      |=  [now=@da wid=@ud]                             ::  next activation
      =|  nex=(unit ,@da)                             
      =<  q
      |-  ^+  [p=@ud q=*(unit ,@da)]
      ?~  puq  [wid nex]
      =+  rit=$(puq r.puq)
      =>  %_(. wid p.rit, nex q.rit)
      ?:  =(0 wid)  [wid nex]
      =:  wid  (dec wid)
          nex  %+  hunt  nex 
               ?:(=(0 pex.q.n.puq) [~ now] [~ pex.q.n.puq])
        ==
      $(puq l.puq)
    ::
    ++  durk                                            ::    durk:pe
      ^+  .                                             ::  XX stateless stats
      =:  niq  0
          nif  0
          cop  0
        ==
      |-  ^+  +
      ?~  puq  +
      =+  lef=$(puq l.puq)
      =+  rih=$(puq r.puq)
      %_    +.$
        niq  +((add niq.lef niq.rih))
        nif  %+  add  (add nif.lef nif.rih)
             =(0 pex.q.n.puq)
        cop  %+  add  (add cop.lef cop.rih)
             (lte nux.q.n.puq 4)
      ==
    ::
    ++  glan                                            ::    glan:pe
      |=  num=@ud                                       ::  delete by number
      %_    +>
          puq
        |-  ^+  puq
        ?:  =(num p.n.puq)
          ~(nap to puq)
        ?:  (gth num p.n.puq) 
          [n.puq $(puq l.puq) r.puq]
        [n.puq l.puq $(puq r.puq)]
      ==
    ::
    ++  gost                                            ::    gost:pe
      |=  [num=@ud rob=bird]                            ::  insert in queue
      +>(puq (~(put to puq) num rob))
    ::
    ++  harv                                            ::    harv:pe
      |=  [now=@da wid=@ud rtt=@dr]                     ::  harvest queue
      ^-  [p=(list rock) q=_+>]
      =|  rub=(list rock)
      =-  [(flop q.vah) +>.$(puq r.vah)]
      ^=  vah
      |-  ^+  [[p=wid q=rub] r=puq]
      ?~  puq  [[wid rub] puq]
      =^  bwr  r.puq  $(puq r.puq)
      =>  %_(. wid p.bwr, rub q.bwr)
      ?:  =(0 wid)  [[wid rub] puq]
      =.  wid  (dec wid)
      =^  gyt  n.puq 
          ^+  [rub n.puq]
          ?.  =(0 pex.q.n.puq)
            [rub n.puq]
          :-  [pac.q.n.puq rub]
          %=    n.puq
              nux.q  +(nux.q.n.puq)
              pex.q  %+  add  now
                     %+  min  ~s16
                     (mul rtt (bex (min 12 +(nux.q.n.puq))))
          ==
      =.  rub  gyt
      =^  fyg  l.puq  $(puq l.puq)
      [fyg puq]
    ::                                                  ::    nams:pe
    ++  nams                                            ::  implicit nacks
      |=  nus=(list ,@ud)
      =+  ^=  loy
          |-  ^+  [p=@ud q=puq]
          ?:  =(~ puq)
            [0 ~]
          =^  lef  l.puq  $(puq l.puq)
          =^  rih  r.puq  $(puq r.puq)
          =+  lal=(add lef rih)
          ?.  (lien nus |=(a=@ud =(a p.n.puq)))
            [lal puq]
          [+(lal) puq(pex.q.n `@`0)]
      +>.$(cux (add cux p.loy), puq q.loy)
    ::
    ++  nomb                                            ::    nomb:pe
      |=  now=@da                                       ::  explicit timeouts
      ^-  [p=@ud q=_+>]
      =-  [p.vin +>.$(cux (add p.vin cux), puq q.vin)]
      ^=  vin
      |-  ^+  [p=@ud q=puq]
      ?~  puq  [0 puq]
      =+  lam=&(!=(0 pex.q.n.puq) (gth now pex.q.n.puq))
      =^  nod  n.puq  
               ?.  &(!=(0 pex.q.n.puq) (gth now pex.q.n.puq))
                 [0 n.puq] 
               [1 n.puq(pex.q `@`0)]
      =^  lef  l.puq  $(puq l.puq)
      =^  rit  r.puq  $(puq r.puq)
      [:(add nod lef rit) puq]
    ::
    ++  rast                                            ::    rast:pe
      |=  gom=soap                                      ::  delete by msg id
      %_    +>
          puq
        |-  ^+  puq
        ?:  =(~ puq)
          puq
        =>  %_(. l.puq $(puq l.puq), r.puq $(puq r.puq))
        ?:  =(gom gom.q.n.puq)
          ~(nap to puq)
        puq
      ==
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aG, protocol engine          ::
  ::
  |%
  ++  am                                                ::    am
    |_  [now=@da fox=fort]                              ::  protocol engine
    ++  boot                                            ::    boot:am
      ^-  fort                                          ::  restore from noun
      %=    fox
          urb.ton
        %-  ~(gas by *(map seat safe))
        %+  turn
          (~(tap by urb.ton.fox) ~)
        |=  [p=seat q=safe]  ^-  [p=seat q=safe]
        :-  p
        %=    q
            val
          (turn val.q |=([p=life q=ring r=acro] [p q (wear q)]))
        ==
      ==
    ++  come                                            ::    come:am
      |=  [ges=@t wid=@ bur=@]                          ::  instantiate pawn
      ^-  [p=[p=seat q=@uvG] q=fort]
      =+  loy=(brew wid bur)
      =+  rig=sec:ex:loy
      =+  our=`@p`fig:ex:loy
      =+  syp=[[0 ~ our now] [%pawn ges] pub:ex:loy]
      :-  [our pac:ex:loy]
      %_    fox
          ton
        %^    ~(ha go ton.fox)
            our
          `mace`[[0 rig] ~]
        `will`[[(sign:se:loy @ (shaf %self (sham syp))) syp] ~]
      ==
    ::
    ++  czar                                            ::    czar:am
      |=  [our=seat nam=@t gen=@uw]                     ::  instantiate emperor
      ^-  [p=(list boon) q=fort]
      =+  loy=(brew 2.048 gen)
      ?>  =(fig:ex:loy (zeno our))
      =+  mac=`mace`[[0 sec:ex:loy] ~]
      =+  syp=`step`[`bray`[0 ~ our now] [%czar nam] pub:ex:loy]
      =+  ded=`deed`[(sign:se:loy @ (shaf %self (sham syp))) syp]
      =+  buq=`buck`[mac [ded ~]]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *oven)
        ==
      [[[%beer our pac:ex:loy] ~] fox]
    ::
    ++  gnaw                                            ::    gnaw:am
      |=  [ryn=lane pac=rock]                           ::  process packet
      ^-  [p=(list boon) q=fort]
      =+  kec=(bite pac)
      ?.  (~(has by urb.ton.fox) q.p.kec)
        [~ fox]
      =<  zork
      =<  zank
      ::  ~&  [%hear p.p.kec ryn `@p`(mug (shaf %flap pac))]
      %-  ~(blow la:(ho:(um q.p.kec) p.p.kec) ryn %none (shaf %flap pac))
      [q.kec r.kec s.kec]
    ::
    ++  hall                                            ::    hall:am
      ^-  (list sock)                                   ::  all sockets 
      =|  sox=(list sock)                               ::  XX hideous
      |-  ^+  sox 
      ?~  zac.fox  sox
      =.  sox  $(zac.fox l.zac.fox)
      =.  sox  $(zac.fox r.zac.fox)
      |-  ^+  sox
      ?~  wab.q.n.zac.fox  sox
      =.  sox  $(wab.q.n.zac.fox l.wab.q.n.zac.fox)  
      =.  sox  $(wab.q.n.zac.fox r.wab.q.n.zac.fox)  
      [[p.n.zac.fox p.n.wab.q.n.zac.fox] sox]
    ::
    ++  have                                            ::    have:am 
      |=  [our=seat buq=buck]                           ::  acquire license
      ^-  [p=(list boon) q=fort]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *oven)
        ==
      [[[%beer our pac:ex:q:sen:(need (~(us go ton.fox) our))] ~] fox]
    ::
    ++  hole                                            ::    hole:am
      |=  [ryn=lane pac=rock]                           ::  bad packet
      ^-  [p=(list boon) q=fort]
      =+  kec=(bite pac)
      ?.  (~(has by urb.ton.fox) q.p.kec)
        ~&  [%wooh p.kec]
        [~ fox]
      ~&  [%hole p.kec ryn `@p`(mug (shaf %flap pac))]
      =<  zork
      =<  zank
      (~(cock la:(ho:(um q.p.kec) p.p.kec) ryn %none (shaf %flap pac)) %dead)
    ::
    ++  kick                                            ::    kick:am
      |=  hen=duct                                      ::  refresh net
      =+  aks=(turn (~(tap by urb.ton.fox) ~) |=([p=seat q=safe] p))
      |-  ^-  [p=(list boon) q=fort]
      ?~  aks  [~ fox]
      =^  buz  fox  zork:(kick:(um i.aks) hen)
      =^  biz  fox  $(aks t.aks)
      [(weld p.buz p.biz) fox]
    ::
    ++  wake                                            ::    wake:am
      ^-  [p=(list boon) q=fort]                        ::  harvest packets
      =+  sox=hall
      =|  bin=(list boon)
      |-  ^-  [p=(list boon) q=fort]
      ?~  sox  [bin fox]
      =^  bun  fox  zork:zank:cool:tung:turk:(ho:(um p.i.sox) q.i.sox)
      $(sox t.sox, bin (weld p.bun bin))
    ::
    ++  wash                                            ::    wash:am
      |=  [soq=sock sup=soap ham=meal]                  ::  dispatch and send
      ^-  [p=(list boon) q=fort]
      zork:zank:cool:tung:(wind:(ho:(um p.soq) q.soq) sup ham)
    ::
    ++  went                                            ::    went:am
      |=  [soq=sock hen=duct cap=cape sup=soap]         ::  internal react
      ^-  [p=(list boon) q=fort]
      zork:(kick:(um p.soq) hen)
    ::
    ++  wert                                            ::    wert:am
      |=  [soq=sock hen=duct inx=@ud rot=riot]          ::  serve a file
      ^-  [p=(list boon) q=fort]
      ::  ~&  [%wert soq inx]
      =+  ruv=(need (~(get by rop.fox) [inx soq]))
      (wise soq [/a hen] %ru [inx rot])
    ::
    ++  wise                                            ::    wise:am
      |=  [soq=sock hen=duct cha=@ta val=*]             ::  send a statement
      ^-  [p=(list boon) q=fort]
      =<  zork:zank:tung
      (wool:(ho:(um p.soq) q.soq) hen cha val)
    ::
    ++  um                                              ::  per server
      |=  our=seat
      =+  gus=(need (~(us go ton.fox) our))
      =+  ^=  weg  ^-  oven
          =+  weg=(~(get by zac.fox) our)
          ?^(weg u.weg *oven)
      =|  bin=(list boon)
      |%
      ++  ho                                            ::    ho:um:am
        |=  her=seat                                    ::  per friend
        =+  diz=(myx:gus her)
        =+  ^=  bah  ^- bath
            =+  bah=(~(get by wab.weg) her)               
            ?^(bah u.bah %*(. *bath foy baby))
        |%
        ++  busk                                        ::    busk:ho:um:am
          |=  [way=seat pax=(list rock)]                ::  send packets
          %_    +>
              bin
            (weld (turn pax |=(pac=rock [%ouzo (wisk:diz way ~ pac)])) bin)
          ==
        ::
        ++  cool                                        ::    cool:ho:um:am
          |-  ^+  +                                     ::  fill pump
          ?.  ?&  ?=(^ maz.bah) 
        ::          (gth wid.foy.bah niq.sea.bah)
              ==
            +
          $(+ pock)
        ::
        ++  done                                        ::    done:ho:um:am
          |=  [cha=@ta num=@ud]                         ::  complete outgoing
          ^-  [(unit duct) _+>]
          =+  rol=(need (~(get by ryl.bah) cha))
          =+  rix=(~(get by san.rol) num)
          ?~  rix  [~ +>.$]
          :-  rix
          %_    +>.$
              ryl.bah  
            (~(put by ryl.bah) cha rol(san (~(del by san.rol) num)))
          ==
        ::
        ++  la                                          ::    la:ho:um:am
          |_  [ryn=lane aut=skin dam=flap]              ::  per packet
          ::
          ++  blow                                      ::    blow:la:ho:um:am
            |=  [dit=? sin=skin msg=@]                  ::  analyze
            ^+  ..blow
            (?:(dit chew wait) sin msg)
          ::
          ++  chew                                      ::    chew:la:ho:um:am
            |=  [sin=skin msg=@]                        ::  receive
            ^+  +>
            =<  east
            |%  
            ++  east
              ^+  +>.$
              ?-    sin
                  %none  
                ::  ~&  %chew-none
                (chow ((hard meal) (cue msg)))
              ::
                  %fast
                ::  ~&  %chew-fast
                =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
                =+  dey=(kuch:diz mag)
                ?~  dey  +>.$                           ::  ignore unknown key
                =.  +>.$  enuf
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:q:sen:gus key bod))))
              ::
                  %full
                ::  ~&  %chew-full
                =+  mex=((hard ,[p=life q=@]) (cue msg))
                =+  gey=(sev:gus p.mex)
                =+  mes=(need (tear:se:q.gey q.mex))
                =.  diz  (wasc:diz p.mes)
                =.  +>.$  enuf
                (west(msg q.mes) p.mes)
              ::
                  %open  
                ::  ~&  %chew-open
                (west *code)
              ==
            ++  west
              |=  key=code
              =+  ^=  mex
                  %.  (cue msg)
                  (hard ,[p=life q=will r=@])
              =.  diz  (deng:diz q.mex)
              =+  wug=cluy:diz
              ?>  =(p.mex p.wug)
              %-  chow(aut sin)
              ((hard meal) (cue (need (sure:pu:r.wug key r.mex))))
            --
          ::
          ++  chow                                      ::    chow:la:ho:um:am 
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            =.  lun.wod.dur.diz  ?:(=(%none aut) lun.wod.dur.diz [~ ryn])
            (dine fud)
          ::
          ++  cock                                      ::    cock:la:ho:um:am
            |=  cap=cape  ^+  +>                        ::  acknowledgment
            =^  pax  diz  (zuul:diz now [%back cap dam ~s0])
            %_    +>.$
                bin
              (weld (turn p.pax |=(pac=rock [%ouzo ryn pac])) bin)
            ==
          ::
          ++  coot                                      ::    coot:la:ho:um:am
            |=  [cha=@ta rum=race]                      ::  update input race
            ^+  +>
            =+  cun=(~(get by mis.rum) did.rum)
            ?~  cun  
              +>.$(raz.bah (~(put by raz.bah) cha rum))
            =.  +>.$  (cock(dam p.u.cun) %good)
            =.  +>.$  (emit [%milk [our her] cha did.rum q.u.cun])
            %=  $
              mis.rum  (~(del by mis.rum) did.rum)
              did.rum  +(did.rum)
            ==
          ::
          ++  dear                                      ::    dear:la:ho:um:am
            |=  [cha=@ta num=@ud dut=(unit)]            ::  interpret message
            ^+  +>
            =+  ^=  rum  ^-  race
                =+  rum=(~(get by raz.bah) cha)
                ?~(rum *race u.rum)
            ?.  (gte num did.rum)
              +>.$                                      ::  duplicate
            (coot cha rum(mis (~(put by mis.rum) num [dam dut])))
          ::
          ++  dine                                      ::    dine:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            ?-    -.fud
                %back
              ::  ~&  [%back aut her ryn]
              =.  +>  ?.(=(%full aut) +> (cock %good))  ::  finish key exch
              +>(..la (tuck p.fud q.fud r.fud))
            ::
                %bond
              ::  ~&  [%bond aut her ryn]
              ?>  =(p:sen:gus p.fud)
              (dear q.fud r.fud [~ s.fud])
            ::
                %bonk
              ::  ~&  [%bonk aut her ryn]
              ?.  =(p:sen:gus p.fud)  +>
              (dear q.fud r.fud ~)
            ::
                %carp
              =+  neb=(~(get by nys.weg) q.fud)
              ?~  neb
                (cock ?:((~(has in old.weg) q.fud) %good %dead))
              =>  .(neb u.neb)
              ?>  (lth p.fud p.r.neb)
              =+  doy=`(unit ,@)`(~(get by r.r.neb) p.fud)
              ?^  doy
                +>.$
              =>  ^+  .   %=  .
                    r.r.neb  (~(put by r.r.neb) p.fud r.fud)
                    q.neb    +(q.neb)
                  ==
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) q.fud)
                    old.weg  (~(put in old.weg) q.fud)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  (cock %good)
              +>.$(nys.weg (~(put by nys.weg) q.fud neb))
            ::
                %fore
              =+  lyn=?~(q.fud ryn u.q.fud)
              ?:  =(our p.fud)
                (emit %mead lyn r.fud) 
              =+  zid=(myx:gus p.fud)
              =+  rut=xang:zid 
              ?~  rut
                ::  ~&  [%fore-dead p.fud]
                +>.$
              ::  ~&  [%fore u.rut p.fud lyn]
              (emit %ouzo (wisk:zid u.rut [~ lyn] r.fud))
            ==
          ::
          ++  emit                                      ::    emit:la:ho:um:am
            |=  bun=boon                                ::  emit a boon
            +>(bin [bun bin]) 
          ::
          ++  enuf                                      ::    enuf:la:ho:um:am
            %_    .                                     ::  heard fast on
                gay.bah  & 
                laz.bah  [~ now]
                bin
              ?.  |(!gay.bah =(~ laz.bah))  bin
              :_  bin
              [%wine [our her] ?:(gay.bah " is your neighbor" " is ok")]
            ==
          ::
          ++  golf                                      ::    golf:la:ho:um:am 
            |=  [sin=skin duv=dove]                     ::  assemble fragments
            ^+  +>
            %+  chew  sin
            =+  [nix=0 rax=*(list ,@)]
            |-  ^-  @
            ?:  =(p.duv nix)
              (can 13 (turn (flop rax) |=(a=@ [1 a])))
            $(nix +(nix), rax [(need (~(get by r.duv) nix)) rax])
          ::
          ++  wait                                      ::    wait:la:ho:um:am
            |=  [sin=skin msg=@]                        ::  receive indirect
            ^+  +>
            =+  pay=((hard ,[p=@ud q=@uvH r=@]) (cue msg))
            =.  nys.weg  (~(put by nys.weg) dam [sin 0 p.pay q.pay ~])
            (dine [%carp 0 dam r.pay])
          --                                            ::  --la:ho:um:am
        ::
        ++  pock                                        ::    pock:ho:um:am
          |-  ^+  +                                     ::  queue a packet
          ?:  =(~ maz.bah)
            ..pock
          =+  zem=~(get to maz.bah)
          =>  ^+(. .(maz.bah q.zem))
          =+  dyp=`putt`(need (~(get by par.bah) p.zem))
          ?>  ?=(^ wyv.dyp)
          %_    ..pock
              ski.bah      (toss ski.bah)
              maz.bah      
            ?~(t.wyv.dyp maz.bah (~(put to maz.bah) p.zem))
          ::
              air.bah      
            (~(put by air.bah) (shaf %flap i.wyv.dyp) q.ski.bah)
          ::
              par.bah 
            %+  ~(put by par.bah)
              p.zem
            dyp(wyv t.wyv.dyp, ski (toss ski.dyp))
          ::
              sea.bah
            =<  +<
            =<  durk
            (~(gost pe sea.bah) q.ski.bah [p.zem q.ski.dyp 0 | now i.wyv.dyp])
          ==
        ::
        ++  pong                                        ::    pong:ho:um:am
          |=  hen=duct                                  ::  test connection
          ^+  [? +>]
          ?.  |(?=(~ laz.bah) (lth u.laz.bah hop.fox) !gay.bah)
            ::  ~&  [%pong-no her]
            [| +>.$]
          ::  ~&  [%pong-yes now her]
          [& (wool [/a hen] %pi ~)]
        ::
        ++  tuck                                        ::    tuck:ho:um:am
          |=  [kay=cape fap=flap cot=@dr]               ::  ack by hash
          ^+  +> 
          =+  fov=(~(get by air.bah) fap)
          ?~  fov
            ::  ~&  [%limp `@p`(mug fap)]
            +>.$
          =.  +>.$  (tusk kay u.fov cot)
          ?.  =(%good kay)
            +>.$
          +>.$(air.bah (~(del by air.bah) fap))
        ::
        ++  tung                                        ::    tung:ho:um:am
          ^+  .                                         ::  harvest packets
          =+  rut=xang:diz
          =+  pez=%*(. pe +< sea.bah)
          =^  wyv  pez  (harv:pez now wid.foy.bah rtt.foy.bah)
          =.  sea.bah  +<:durk:pez
          =+  yag==(0 cop.sea.bah)
          =.  ..tung
            ?:  =(yag gay.bah)  ..tung
            %_    ..tung
                gay.bah          yag
                lun.wod.dur.diz  ?:(|(yag (lth her 256)) lun.wod.dur.diz ~)
                bin  
              :_  bin
              :+  %wine  [our her]
              ?:(yag " is ok" " not responding still trying")
            == 
          ?~  rut  ..tung
          (busk u.rut p.wyv)
        ::
        ++  turk                                        ::    turk:ho:um:am
          ^+  .                                         ::  update by date
          =+  pez=%*(. pe +< sea.bah)
          =^  rem  pez  (nomb:pez now)
          %=  ..turk 
            foy.bah  (slow rem foy.bah)
            sea.bah  +<:durk:pez
          ==
        ::
        ++  tusk                                        ::    tusk:ho:um:am
          |=  [kay=cape num=@ud cot=@dr]                ::  ack by sequence
          ^+  +>
          =^  suz  ski.bah  (suck num ski.bah)
          =+  rob=(need (~(busc pe sea.bah) num))
          =.  sea.bah  +<:durk:(nams:(~(glan pe sea.bah) num) suz)
          =.  foy.bah  ?~(suz (fast foy.bah) (slow (lent suz) foy.bah))
          ?-    kay
              %good
            =+  dyp=`putt`(need (~(get by par.bah) gom.rob))
            =>  %_(. ski.dyp q:(suck mup.rob ski.dyp))
            ?.  &(=(~ wyv.dyp) =(p.ski.dyp q.ski.dyp))
              +>.$(par.bah (~(put by par.bah) gom.rob dyp))
            =.  par.bah (~(del by par.bah) gom.rob)
            =^  hud  +>.$  (done q.gom.rob r.gom.rob)
            ?~  hud  +>.$
            +>.$(bin [[%coke [our her] %good gom.rob u.hud] bin])
          ::
              %dead 
            =.  par.bah  (~(del by par.bah) gom.rob)
            =.  sea.bah  +<:durk:(~(rast pe sea.bah) gom.rob)
            =^  hud  +>.$  (done q.gom.rob r.gom.rob)
            ?~  hud  +>.$
            =.  +>.$  (wind gom.rob [%bonk q.p.gom.rob q.gom.rob r.gom.rob])
            +>.$(bin [[%coke [our her] %dead gom.rob u.hud] bin])
          ==
        ::
        ++  wind                                        ::    wind:ho:um:am
          |=  [sup=soap ham=meal]
          ^+  +>
          =^  wyv  diz  (zuul:diz now ham)
          =:  par.bah    (~(put by par.bah) sup [*snow wyv])
              maz.bah    (~(put to maz.bah) sup)
            ==
          cool 
        ::
        ++  wool                                        ::    wool:ho:um:am
          |=  [hen=duct cha=@ta val=*]                  ::  send a statement
          ^+  +>
          =+  ^=  rol  ^-  rill
              =+  rol=(~(get by ryl.bah) cha)
              ?~(rol *rill u.rol)
          =+  sex=sed.rol
          =.  ryl.bah  
              %+  ~(put by ryl.bah)  cha
              rol(sed +(sed.rol), san (~(put by san.rol) sex hen))
          =+  cov=[p=p:sen:gus q=clon:diz]
          (wind [cov cha sex] [%bond q.cov cha sex val])
        ::
        ++  zank                                        ::    zank:ho:um:am
          %=  +>.$                                      ::  resolve
            gus (nux:gus diz)
            wab.weg (~(put by wab.weg) her bah)
          ==
        --                                              ::  --ho:um:am
      ::
      ++  kick                                          ::    kick:um:am
        |=  hen=duct                                    ::  test connection
        =+  hoy=hoy.saf.gus
        |-  ^+  +>.^$
        ?~  hoy
          +>.^$
        =^  fyx  +>.^$  (pong i.hoy hen)
        ?:  fyx  +>.^$ 
        $(hoy t.hoy)
      ::
      ++  pals                                          ::    pals:um:am
        ^-  (list ,@p)                                  ::  active neighbors
        %+  turn  
          ::  (skim (~(tap by wab.weg) ~) |=([a=seat b=bath] gay.b))
          (~(tap by wab.weg) ~)                         ::  everyone for now
        |=([a=seat b=bath] a)
      ::
      ++  pong                                          ::    pong:um:am
        |=  [her=seat hen=duct]                         ::  test neighbor
        ^+  [? +>]
        =+  xup=(pong:(ho her) hen)
        ?.  -.xup  [| +>.$]
        [& zank:+.xup] 
      ::
      ++  zork                                          ::    zork:um:am
        ^-  [p=(list boon) q=fort]                      ::  resolve
        :-  (flop bin)
        %_  fox
          ton  (~(su go ton.fox) gus)
          zac  (~(put by zac.fox) our.gus weg)
        ==
      --                                                ::  --um:am
    --                                                  ::  --am
  --
  . ==
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aH, protocol vane            ::
  :: 
  =|  $:  fox=fort                                      ::  kernel state
      ==                                                ::
  |=  [now=@da eny=@ sky=||(* (unit))]                  ::  current invocation
  ^?                                                    ::  opaque core
  =< 
    |%                                                  ::  vane interface
    ++  beat
      |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
      =>  .(fav ((hard card) fav))
      ^-  [p=(list move) q=vane]
      =^  duy  ..knap
        (knap wru tea hen fav)
      [duy ..^$]
    ::
    ++  come  
      |=  old=vase
      ^-  vane
      ~|(%load-nest-ames !!)
    ::
    ++  doze
      |=  [now=@da hen=duct]
      =|  doz=(unit ,@da)
      |-  ^+  doz
      ?~  zac.fox  doz
      =.  doz  $(zac.fox l.zac.fox)
      =.  doz  $(zac.fox r.zac.fox)
      =+  yem=q.n.zac.fox
      |-  ^+  doz
      ?~  wab.yem  doz
      =.  doz  $(wab.yem l.wab.yem)
      =.  doz  $(wab.yem r.wab.yem)
      =+  bah=q.n.wab.yem
      (hunt doz (~(doze pe sea.bah) now wid.foy.bah))
    ::
    ++  flee  stay
    ++  load
      |=  new=vase
      ^-  vane
      ?.  (~(nest ut -:!>(fox)) & p.new)
        (come new)
      ..^$(fox ~(boot am [now (fort q.new)]))
    ::
    ++  raze  
      ^-  vane
      ..$(fox *fort)
    ::
    ++  scry
      |=  [our=seat ren=@tas his=seat syd=disc lot=coin tyl=path]
      ^-  (unit)
      ?.  =(0 ren)  ~
      ?+    lot  ~
          [%% %ud @]
        (perm our his q.p.lot [syd tyl])
      ::
          [%% %da @]
        ?.  =(now q.p.lot)  ~
        (temp our his [syd tyl])
      ==
    ::
    ++  stay  `vase`!>(fox)
    --
  |%
  ++  claw  |=(our=seat ^-(duct hen:(need (~(get by zac.fox) our))))
  ++  clop
    |=  [wru=(unit writ) now=@da hen=duct bon=boon]
    ^-  [(list move) fort]
    ?-    -.bon
        %beer
      :_  fox(zac (~(put by zac.fox) p.bon `oven`[hen ~ ~ ~]))
      :*  [[~ %gold p.bon] [/c hen] [%init p.bon]]
          [[~ %gold p.bon] hen [%init p.bon]]
          [[~ %gold p.bon] [/a hen] [%kick now]]
          =+  bos=(sein p.bon)
          ?:  =(bos p.bon)  ~
          :~  [[~ %iron p.bon] [/c hen] [%pull bos %main ~[%main]]]
              [[~ %iron p.bon] [/c hen] [%pull bos %doc ~[%doc]]]
              [[~ %iron p.bon] [/c hen] [%pull bos %try ~[%try]]]
              ::  [[~ %iron p.bon] [/c hen] [%pull bos %arvo ~[%arvo]]]
          ==
      ==
    ::
        %coke  
      :_  fox
      :~  [[~ %iron p.p.bon] s.bon [%went q.p.bon q.bon r.bon]]
      ==
    ::
        %mead  :_(fox [[wru hen [%hear p.bon q.bon]] ~])
        %milk 
      ?+    q.bon
        :_  fox
        :~  :+  [~ %iron p.p.bon] 
              (claw p.p.bon)
            [%wart q.p.bon q.bon r.bon s.bon]
        ==
      ::                                                
          %hi                                           ::    %hi
        %=    $
            bon
          :+  %wine  p.bon
          ^-  tape
          ?~  s.bon 
            " is not feeling well"
          ?:  =(0 u.s.bon)
            =+  hum=(end 0 3 (mug r.bon))
            ?+   hum  !!
              0  " isn't sure what to say" 
              1  " prefers not to comment"
              2  " has no words for what just happened"
              3  " is in the building"
              4  " remains quietly present"
              5  " isn't into drama"
              6  " appreciates the silence"
              7  " pauses to consider"
            ==
          =+  str=(need ((sand %t) ((hard ,@) u.s.bon)))
          [':' ' ' (trip str)]
        ==
      ::                                                
          %ye                                           ::    %ye
        ?~  s.bon  [~ fox]
        ?>  =(p.p.bon (sein q.p.bon))
        =+  ^=  paz  ^-  (list ,@p)
            %+  skim  pals:(~(um am [now fox]) p.p.bon)
            |=(a=@p =(p.p.bon (sein a)))
        :_  fox
        %+  turn  paz
        |=  him=seat
        :+  [~ %iron p.p.bon]
          [/a /a hen]
        [%want him %yu [q.p.bon u.s.bon]]
      ::
          %yu                                           ::    %yu
        ?.  =(q.p.bon (sein p.p.bon))  [~ fox]
        ?~  s.bon  [~ fox]
        =+  dof=((hard ,[p=@p q=@t]) u.s.bon)
        $(bon [%milk [p.p.bon p.dof] %hi r.bon [~ q.dof]])
      ::
          %pi                                           ::    %pi
        $(bon [%wine p.bon " sent a ping"])             ::  ping
      ::
          %ta                                           ::    %ta
        ?~  s.bon  [~ fox]                              ::  register
        =+  gox=((hard ,[p=@p q=@pG r=gcos s=pass]) u.s.bon)
        =+  gus=(need (~(us go ton.fox) p.p.bon))
        =^  wyl  gus  (born:gus now gox)
        =.  ton.fox  (~(su go ton.fox) gus)
        :_  fox
        :~  :+  [~ %iron p.p.bon]
              [/a /a hen]
            [%want q.p.bon %to wyl]
        ==
      ::
          %re                                           ::    %re
        ?~  s.bon  [~ fox]                              ::  file request
        =+  gox=((hard ,[p=@ud q=riff]) u.s.bon)
        ::  ~&  [%ames-ask gox]
        =+  gut=(~(get by rop.fox) [p.gox p.bon])
        =.  rop.fox
          ?^  gut
            ?>(?=(~ q.q.gox) (~(del by rop.fox) [p.gox p.bon]))
          ?>(?=(^ q.q.gox) (~(put by rop.fox) [p.gox p.bon] q.gox))
        :_  fox
        :~  :+  [~ %iron p.p.bon] 
              [/c [%a (scot %p q.p.bon) (scot %ud p.gox) ~] hen]
            [%warp p.p.bon q.gox]
        ==
      ::
          %ru                                           ::    %ru
        :_  fox
        :~  :+  [~ %iron p.p.bon]                       ::  file response
              [/c (claw p.p.bon)] 
            [%wart q.p.bon q.bon r.bon s.bon]
        ==
      ==
    ::
        %ouzo  
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))]
      :_  fox
      [[wru hen [%send p.bon q.bon]] ~]
        
        %wine
      :_  fox
      =+  nym=(temp p.p.bon q.p.bon /name)
      =+  fom=~(rend co %% %p q.p.bon)
      :~  :+  wru  [/d hen]
          :+  %flog  %text
          ;:  weld
            "; "
            ?~(nym fom :(weld fom " (" (trip ((hard ,@) u.nym)) ")"))
            q.bon
          ==
      ==
    ==
  ::
  ++  knap
    |=  [wru=(unit writ) tea=wire hen=duct fav=card]
    ^-  [(list move) _+>]
    ?:  ?=([%crud *] fav)
      [[[wru [/d hen] [%flog fav]] ~] +>]
    =+  ^=  fuy  ^-  [p=(list boon) q=fort]
        ?+    -.fav  
          [~ fox]
        ::
            %cash
          (~(have am [now fox]) p.fav q.fav)
        ::
            %hear
          (~(gnaw am [now fox]) p.fav q.fav)
        ::
            %hole
          (~(hole am [now fox]) p.fav q.fav)
        ::
            %junk
          [~ fox(any.ton (shax (mix any.ton.fox p.fav)))]
        ::
            %kick
          (~(kick am [now fox(hop p.fav)]) hen)
        ::
            %make
          =+  vun=(~(come am [now fox]) p.fav (bex q.fav) r.fav)
          [[[%beer p.vun] ~] q.vun]
        ::
            %sith
          (~(czar am [now fox]) p.fav q.fav r.fav)
        ::
            %want
          ?>  ?=(^ wru)
          (~(wise am [now fox]) [q.u.wru p.fav] hen q.fav r.fav)
        ::
            %went 
          (~(went am [now fox]) [q.u.wru p.fav] hen q.fav r.fav)
        ::
            %wake
          ~(wake am [now fox])
        ::
            %writ
          ?>  ?=(^ wru)
          ?>  ?=([@ @ ~] tea)
          =+  fyg=(slay i.tea)
          =+  haw=(slay i.t.tea)
          ?>  &(?=([~ %% %p @] fyg) ?=([~ %% %ud @] haw))
          (~(wert am [now fox]) [q.u.wru q.p.u.fyg] hen q.p.u.haw p.fav)
        ==
    =>  %_(. fox q.fuy)
    =|  out=(list move)
    |-  ^-  [p=(list move) q=_+>.^$]
    ?~  p.fuy
      [out +>.^$]
    =^  toe  fox  (clop wru now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld toe out))
  ::
  ++  perm
    |=  [our=seat his=seat mar=@ud tyl=path]
    ^-  (unit)
    ?~  tyl  ~
    ?:  ?=([%name ~] tyl)
      =+  wul=$(tyl [%will ~])
      ?~(wul ~ [~ (gnow q.q:((hard deed) -.u.wul))])
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
    ?:  ?=([%tick @ ~] tyl)
      =+  hur=(slaw %p i.t.tyl)
      ?~  hur  ~
      [~ (end 6 1 (shaf %tick (mix u.hur (shax sec:ex:q:sen:u.gys))))]
    ?:  ?=([%will ~] tyl)
      (rick mar our law.saf.u.gys)
    ~
  ::
  ++  temp
    |=  [our=seat his=seat tyl=path]
    ::  ~&  [%temp our his tyl]
    ^-  (unit)
    ?.  ?=([%life ~] tyl)
      =+  muc=$(tyl [%life ~])
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
