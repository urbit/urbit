::  ::  ames (4a), networking
::
  |=  pit=vase
  =>  =~
::  structures
=,  ames
=+  protocol-version=0
|%
+=  move  [p=duct q=(wind note:able gift:able)]         ::  local move
--
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aA, identity logic           ::
  ::
  |%
  ::
  ++  grip                                              ::  extend wyll
    |=  [wet=wyll law=wyll]
    ^-  wyll
    ?~  wet  law
    ?:  =(wet law)  law
    ?^  t.wet
      ?>((meld i.wet i.t.wet) [i.wet $(wet t.wet)])
    ?~  law
      ?>((pier i.wet) [i.wet ~])
    ?~  q.p.q.i.wet
      ?>((meld i.wet i.law) [i.wet law])
    =+  rul=(sein:title r.p.q.i.wet)
    |-  ^-  wyll
    ?:  ?&  =(rul r.p.q.i.law)
            =(p.p.q.i.law u.q.p.q.i.wet)
        ==
      ?>((meld i.wet i.law) [i.wet law])
    ?>(?=(^ t.law) $(law t.law))
  ::
  ++  meld                                              ::  verify connect
    |=  [new=deyd old=deyd]
    ^-  $&
    ?>  (melt new old)
    ?>  =((shaf %meld (sham q.new)) (need (sure:as:(com:nu:crub:crypto r.q.old) *code p.new)))
    %&
  ::
  ++  melt                                              ::  proper connect
    |=  [new=deyd old=deyd]
    ^-  ?
    =+  rac=(clan:title r.p.q.new)
    ?&  =(r.new r.old)                                  ::  match fake
        ?~  q.p.q.new
          ?&  =(r.p.q.old r.p.q.new)
              &(!=(%earl rac) =(p.p.q.old (dec p.p.q.new)))
          ==
        ?&  &(!=(%pawn rac) !=(%czar rac))
            |(=(0 p.p.q.new) =(%earl rac))
            =(r.p.q.old (sein:title r.p.q.new))
            =(p.p.q.old u.q.p.q.new)
        ==
    ==
  ::
  ++  pare                                              ::  shorten against
    |=  [fou=wyll law=wyll]
    ::  ~&  [%pare-fou fou]
    ::  ~&  [%pare-law law]
    ^-  wyll
    =+  [ouf=(flop fou) wal=(flop law)]
    %-  flop
    |-  ^-  wyll
    ?~  ouf  wal
    ?~  wal  !!
    ?.  =(i.wal i.ouf)  ouf
    $(wal t.wal, ouf t.ouf)
  ::
  ++  pier  !:                                          ::  initial deyd
    |=  wed=deyd
    ^-  $&
    ?>  =+  rac=(clan:title r.p.q.wed)
        =+  loy=(com:nu:crub:crypto r.q.wed)
        ?:  &(r.wed =(rac %czar))  %&
        ?>  =(0 p.p.q.wed)
        ?>  =(fig:ex:loy ?+(rac !! %czar (zeno r.p.q.wed), %pawn r.p.q.wed))
        ?>  =((shaf %self (sham q.wed)) (need (sure:as:loy *code p.wed)))
        %&
    %&
  ::
  ++  real                                              ::  validate
    |=  [mac=mace law=wyll]
    ?>  ?&  |-  ^-  ?
            ?~  mac  &
            ?>  ?&  ?=(^ law)
                    (lth p.p.q.i.law 9)                 ::  9-lives rule
                    =(p.p.q.i.law p.i.mac)
                    =(r.q.i.law pub:ex:(nol:nu:crub:crypto q.i.mac))
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
  ++  rick                                              ::  wyll at life
    |=  [mar=life lag=ship law=wyll]
    ^-  (unit wyll)
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
    ^-  (list @uw)
    :~  0wN.ymbEn.wyNdE.RFNRb.SnEQH   ::    0, ~zod, urbit.org
        0w0                           ::    1, ~nec, Curtis Yarvin
        0w0                           ::    2, ~bud, Tlon Investor 1
        0w0                           ::    3, ~wes, Tlon Investor 2
        0w0                           ::    4, ~sev, Tlon Investor 2
        0wt.cKYxs.Yb5VZ.boSwm.l0yYc   ::    5, ~per, Tlon Investor 3  (oldkey)
        0w0                           ::    6, ~sut, Tlon Investor 4
        0w0                           ::    7, ~let, Tlon Investor 4
        0w0                           ::    8, ~ful, Tlon Investor 4
        0w0                           ::    9, ~pen, Tlon Investor 4
        0w0                           ::   10, ~syt, Tlon Investor 4
        0w0                           ::   11, ~dur, Tlon Investor 4
        0w0                           ::   12, ~wep, Sam Putman
        0w0                           ::   13, ~ser, Tlon Investor 5
        0w3j.H0sty.jHa3F.JlD26.4LPwV  ::   14, ~wyl, Zimran Ahmed (oldkey)
        0w3F.QdvV-.toAsR.hvUNk.fHjW6  ::   15, ~sun, Colin Smith  (oldkey)
        0w0                           ::   16, ~ryp, Tlon Investor 6
        0w0                           ::   17, ~syx, Tlon Investor 6
        0w0                           ::   18, ~dyr, Tlon Investor 6
        0w0                           ::   19, ~nup, Tlon Investor 6
        0w0                           ::   20, ~heb, Tlon Investor 6
        0w0                           ::   21, ~peg, Tlon Investor 6
        0w0                           ::   22, ~lup, Tlon Investor 6
        0w0                           ::   23, ~dep, Tlon Investor 6
        0w0                           ::   24, ~dys, Mike Gogulski
        0w0                           ::   25, ~put, Tlon Investor 7
        0w0                           ::   26, ~lug, Tlon Investor 8
        0w0                           ::   27, ~hec, Tlon Investor 8
        0w0                           ::   28, ~ryt, Tlon Investor 8
        0w0                           ::   29, ~tyv, Tlon Investor 8
        0w0                           ::   30, ~syd, Curtis Yarvin
        0wp.BgRGJ.kslnv.PLAqb.TRKbr   ::   31, ~nex, Prakhar Goel (oldkey)
        0w0                           ::   32, ~lun, Tlon Investor 9
        0w0                           ::   33, ~mep, Tlon Investor 9
        0w0                           ::   34, ~lut, Tlon Investor 9
        0w0                           ::   35, ~sep, Tlon Investor 9
        0w0                           ::   36, ~pes, Curtis Yarvin
        0w10.5s1K0.dv9NT.02fPe.PyDYa  ::   37, ~del, Kingdon Barrett
        0w1w.KF-J1.5I63F.khFyv.h0n4J  ::   38, ~sul, John Burnham (oldkey)
        0w1A.OcPXS.oQi8K.g-E0d.zTRph  ::   39, ~ped, Jeremy Wall
        0w2.Mr2Id.SX8xI.MAs-j.5Y-1W   ::   40, ~tem, Tlon Investor 10 (oldkey)
        0w0                           ::   41, ~led, Nick Caruso
        0w0                           ::   42, ~tul, Curtis Yarvin
        0w0                           ::   43, ~met, Curtis Yarvin
        0w0                           ::   44, ~wen, Curtis Yarvin
        0w0                           ::   45, ~byn, Curtis Yarvin
        0w0                           ::   46, ~hex, James Torre
        0w0                           ::   47, ~feb, urbit.org
        0wK.GoKEY.rMjfn.ZcvFQ.n4BmX   ::   48, ~pyl, Michael Hartl (oldkey)
        0w0                           ::   49, ~dul, Galen Wolfe-Pauly
        0w0                           ::   50, ~het, Galen Wolfe-Pauly
        0w0                           ::   51, ~mev, Curtis Yarvin
        0w0                           ::   52, ~rut, Curtis Yarvin
        0w2L.M6-o5.DDTFL.R4sFL.7Zuay  ::   53, ~tyl, Tlon Investor 11 (oldkey)
        0w0                           ::   54, ~wyd, Curtis Yarvin
        0w0                           ::   55, ~tep, Curtis Yarvin
        0w0                           ::   56, ~bes, Curtis Yarvin
        0w0                           ::   57, ~dex, Jared Hance
        0w0                           ::   58, ~sef, Owen Rescher
        0w0                           ::   59, ~wyc, Galen Wolfe-Pauly
        0w0                           ::   60, ~bur, Galen Wolfe-Pauly
        0w0                           ::   61, ~der, Galen Wolfe-Pauly
        0w0                           ::   62, ~nep, Galen Wolfe-Pauly
        0w0                           ::   63, ~pur, Paul Driver
        0w30.VtXvV.S~xIV.iMCL~.j9zTC  ::   64, ~rys, Charlie Cummings (oldkey)
        0w0                           ::   65, ~reb, Curtis Yarvin
        0wp.LslIa.IFSM9.mIp-z.KBIBh   ::   66, ~den, Michael Hartl (oldkey)
        0w0                           ::   67, ~nut, Curtis Yarvin
        0w0                           ::   68, ~sub, Curtis Yarvin
        0w0                           ::   69, ~pet, Curtis Yarvin
        0w0                           ::   70, ~rul, Curtis Yarvin
        0w0                           ::   71, ~syn, Pantera
        0w0                           ::   72, ~reg, Henry Ault
        0w0                           ::   73, ~tyd, Henry Ault
        0w0                           ::   74, ~sup, Henry Ault
        0w0                           ::   75, ~sem, Michael Livshin
        0w0                           ::   76, ~wyn, Anton Dyudin
        0w0                           ::   77, ~rec, urbit.org
        0w0                           ::   78, ~meg, Tlon
        0w2L.tavpW.Lk4R-.elm7E.4KEqZ  ::   79, ~net, Anthony Martinez (oldkey)
        0w0                           ::   80, ~sec, Curtis Yarvin
        0w0                           ::   81, ~mul, Curtis Yarvin
        0w1F.Tqroo.wyq2m.cBaTM.9MbG-  ::   82, ~nym, Max Greer (oldkey)
        0w0                           ::   83, ~tev, Curtis Yarvin
        0w2x.~ldho.Oo7kE.QqNSx.XteFh  ::   84, ~web, Ar Vicco (oldkey)
        0w0                           ::   85, ~sum, Philip Monk
        0w0                           ::   86, ~mut, Philip Monk
        0w0                           ::   87, ~nyx, Philip Monk
        0w30.UUr19.iBPlD.wfyJD.2CWPv  ::   88, ~rex, Tlon Investor 12 (oldkey)
        0w0                           ::   89, ~teb, Michael Vassar
        0w0                           ::   90, ~fus, Tlon Corporation
        0w0                           ::   91, ~hep, urbit.org
        0w0                           ::   92, ~ben, urbit.org
        0w0                           ::   93, ~mus, urbit.org
        0w0                           ::   94, ~wyx, urbit.org
        0w0                           ::   95, ~sym, urbit.org
        0w0                           ::   96, ~sel, urbit.org
        0w0                           ::   97, ~ruc, urbit.org
        0w0                           ::   98, ~dec, urbit.org
        0w1L.NQ-5f.ABF9R.kVwVJ.zRfn2  ::   99, ~wex, Pax Dickinson (oldkey)
        0w0                           ::  100, ~syr, urbit.org
        0w0                           ::  101, ~wet, urbit.org
        0w0                           ::  102, ~dyl, urbit.org
        0w0                           ::  103, ~myn, urbit.org
        0w0                           ::  104, ~mes, urbit.org
        0w0                           ::  105, ~det, urbit.org
        0w0                           ::  106, ~bet, urbit.org
        0w0                           ::  107, ~bel, urbit.org
        0w0                           ::  108, ~tux, Tlon Investor 13
        0w1D.JV9n0.9z~YK.yAWyi.c9~Lu  ::  109, ~tug, Philip Monk (oldkey)
        0w0                           ::  110, ~myr, urbit.org
        0w0                           ::  111, ~pel, urbit.org
        0w0                           ::  112, ~syp, urbit.org
        0w0                           ::  113, ~ter, urbit.org
        0w0                           ::  114, ~meb, urbit.org
        0w0                           ::  115, ~set, urbit.org
        0w0                           ::  116, ~dut, urbit.org
        0w0                           ::  117, ~deg, urbit.org
        0w0                           ::  118, ~tex, urbit.org
        0w0                           ::  119, ~sur, urbit.org
        0w0                           ::  120, ~fel, urbit.org
        0w0                           ::  121, ~tud, urbit.org
        0w0                           ::  122, ~nux, urbit.org
        0w0                           ::  123, ~rux, urbit.org
        0w0                           ::  124, ~ren, urbit.org
        0w0                           ::  125, ~wyt, urbit.org
        0w0                           ::  126, ~nub, urbit.org
        0w0                           ::  127, ~med, urbit.org
        0w20.GGLXx.aqxaQ.w4Iob.wdmmr  ::  128, ~lyt, Arthur Breitman (oldkey)
        0w0                           ::  129, ~dus, urbit.org
        0w0                           ::  130, ~neb, urbit.org
        0w0                           ::  131, ~rum, urbit.org
        0w0                           ::  132, ~tyn, urbit.org
        0w0                           ::  133, ~seg, urbit.org
        0w0                           ::  134, ~lyx, urbit.org
        0w0                           ::  135, ~pun, Anton Dyudin
        0w0                           ::  136, ~res, urbit.org
        0w0                           ::  137, ~red, Alex Kravets
        0w3J.15iJA.0pbNk.mZXyh.A~uKb  ::  138, ~fun, Aaron Beckerman (oldkey)
        0w0                           ::  139, ~rev, urbit.org
        0w3m.Cqumo.ZC7-e.794A4.Bqhh8  ::  140, ~ref, Matt Brubeck (oldkey)
        0w0                           ::  141, ~mec, urbit.org
        0w0                           ::  142, ~ted, urbit.org
        0w2d.GLlYg.-MwtO.ZCPBE.OqGB9  ::  143, ~rus, Stephen Burnham (oldkey)
        0w0                           ::  144, ~bex, urbit.org
        0w0                           ::  145, ~leb, Justin LeBlanc
        0w0                           ::  146, ~dux, urbit.org
        0w0                           ::  147, ~ryn, urbit.org
        0w0                           ::  148, ~num, Tlon
        0w0                           ::  149, ~pyx, Katherine McFall
        0w2g.gLmg4.MtrHQ.A5VmH.WPk6G  ::  150, ~ryg, Dan Haffey (oldkey)
        0w0                           ::  151, ~ryx, Tlon
        0w0                           ::  152, ~fep, Tlon
        0w3x.y5stk.FMmvV.LQo3X.OCXkI  ::  153, ~tyr, Steven Dee
        0w0                           ::  154, ~tus, Tlon
        0w0                           ::  155, ~tyc, Tlon
        0w0                           ::  156, ~leg, Tlon
        0w0                           ::  157, ~nem, Jeremy Tunnell
        0w0                           ::  158, ~fer, Tlon
        0w0                           ::  159, ~mer, Tlon
        0w1E.bDeR7.cuQmt.Uc5CS.OSyMx  ::  160, ~ten, Tlon
        0w0                           ::  161, ~lus, Tlon
        0w0                           ::  162, ~nus, Tlon
        0w0                           ::  163, ~syl, Tlon
        0w0                           ::  164, ~tec, Tlon
        0w0                           ::  165, ~mex, Tlon
        0w24.TfzCX.qmRm9.ukapQ.It6tG  ::  166, ~pub, Tlon
        0w0                           ::  167, ~rym, Tlon
        0w0                           ::  168, ~tuc, Tlon
        0w0                           ::  169, ~fyl, Tlon
        0w0                           ::  170, ~lep, Tlon
        0w0                           ::  171, ~deb, Tlon
        0w0                           ::  172, ~ber, Tlon
        0w0                           ::  173, ~mug, Tlon
        0w0                           ::  174, ~hut, Tlon
        0w0                           ::  175, ~tun, Tlon
        0w0                           ::  176, ~byl, Tlon
        0wq.wmRvk.V8tZ5.1lz5q.dbrYx   ::  177, ~sud, Tlon
        0w3Q.0LYXy.LjTEu.~FjaT.OpTts  ::  178, ~pem, Tlon
        0wQ.uJzGo.M94yL.L7yw6.nGyfW   ::  179, ~dev, Tlon
        0w1B.LTLDt.3Yu~s.BuzYM.fjcS3  ::  180, ~lur, Tlon
        0w2B.9j6g-.q9DwV.tXzPz.DKEKq  ::  181, ~def, Tlon
        0w1m.CxbiW.SKF5E.11JQ6.yW~T4  ::  182, ~bus, Tlon
        0w0                           ::  183, ~bep, Tlon
        0w0                           ::  184, ~run, Tlon
        0w0                           ::  185, ~mel, Tlon
        0w0                           ::  186, ~pex, Tlon
        0w0                           ::  187, ~dyt, Tlon
        0w0                           ::  188, ~byt, Tlon
        0w0                           ::  189, ~typ, Anton Dyudin
        0w0                           ::  190, ~lev, Tlon
        0w0                           ::  191, ~myl, Tlon
        0w0                           ::  192, ~wed, Tlon
        0w0                           ::  193, ~duc, Tlon
        0w0                           ::  194, ~fur, Tlon
        0w0                           ::  195, ~fex, Tlon
        0w0                           ::  196, ~nul, Matthew Liston
        0w0                           ::  197, ~luc, Tlon
        0w0                           ::  198, ~len, Tlon
        0w0                           ::  199, ~ner, Tlon
        0wv.aixe9.7gG2w.7cJiy.i3Mg8   ::  200, ~lex, Michael Hartl (oldkey)
        0w0                           ::  201, ~rup, Owen Rescher
        0w0                           ::  202, ~ned, Tlon
        0w0                           ::  203, ~lec, Tlon
        0w0                           ::  204, ~ryd, Tlon
        0w1U.n361n.FC3jj.9cX26.V1Wif  ::  205, ~lyd, Adam Bliss (oldkey)
        0w0                           ::  206, ~fen, Tlon
        0w0                           ::  207, ~wel, Tlon
        0w0                           ::  208, ~nyd, Tlon
        0w0                           ::  209, ~hus, Tlon
        0w0                           ::  210, ~rel, Tlon
        0w0                           ::  211, ~rud, Tlon
        0w0                           ::  212, ~nes, Tlon
        0w16.~8NZV.VyMmf.4toMO.pui1R  ::  213, ~hes, Tlon Investor 14 (oldkey)
        0w0                           ::  214, ~fet, Tlon
        0w0                           ::  215, ~des, Tlon
        0w0                           ::  216, ~ret, Tlon
        0w0                           ::  217, ~dun, Tlon
        0w0                           ::  218, ~ler, Tlon
        0w10.w0AUz.QVdks.HCNvf.ja~TO  ::  219, ~nyr, Ivan Matosevic (oldkey)
        0w0                           ::  220, ~seb, Tlon
        0w0                           ::  221, ~hul, Tlon
        0w0                           ::  222, ~ryl, Tlon
        0w0                           ::  223, ~lud, Tlon
        0w0                           ::  224, ~rem, Tlon
        0w0                           ::  225, ~lys, Tlon
        0w3C.YXlEl.pFbYV.9pYWI.d7cla  ::  226, ~fyn, Stephen Burnham (oldkey)
        0w0                           ::  227, ~wer, Tlon
        0w0                           ::  228, ~ryc, Tlon
        0w0                           ::  229, ~sug, Tlon
        0w0                           ::  230, ~nys, Tlon
        0w0                           ::  231, ~nyl, Tlon
        0w0                           ::  232, ~lyn, Tlon
        0w0                           ::  233, ~dyn, Tlon
        0w0                           ::  234, ~dem, Tlon
        0w0                           ::  235, ~lux, Tlon Investor 15
        0w1O.Jq9xt.YFg7U.qg13U.WFNGJ  ::  236, ~fed, Iceman
        0w0                           ::  237, ~sed, Tlon
        0w0                           ::  238, ~bec, Tlon
        0w0                           ::  239, ~mun, Tlon
        0w0                           ::  240, ~lyr, Tlon
        0w0                           ::  241, ~tes, Tlon
        0w0                           ::  242, ~mud, Ian Rowan
        0w4.yybWD.F1BgE.ynzlF.47neH   ::  243, ~nyt, Byrne Hobart (oldkey)
        0w0                           ::  244, ~byr, Tlon
        0w0                           ::  245, ~sen, Tlon
        0w0                           ::  246, ~weg, Tlon
        0w28.bRVMq.Oi3tM.zOCNV.j00Yq  ::  247, ~fyr, Anton Dyudin (oldkey)
        0w0                           ::  248, ~mur, Tlon
        0w0                           ::  249, ~tel, Tlon
        0w3w.V54nF.e9eNv.1fLkl.PiUo-  ::  250, ~rep, Raymond Pasco
        0w0                           ::  251, ~teg, Tlon
        0w0                           ::  252, ~pec, Tlon
        0w0                           ::  253, ~nel, Tlon
        0w0                           ::  254, ~nev, Tlon
        0wY.a0HAU.7Lbkf.6V514.OsJBv   ::  255, ~fes, John Burnham (oldkey)
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
            chk=(cut 0 [3 20] mag)                      ::  checksum
            wix=(bex +((cut 0 [23 2] mag)))             ::  width of receiver
            vix=(bex +((cut 0 [25 2] mag)))             ::  width of sender
            tay=(cut 0 [27 5] mag)                      ::  message type
        ==
    ?>  =(protocol-version vez)
    ?>  =(chk (end 0 20 (mug bod)))
    :+  [(end 3 wix bod) (cut 3 [wix vix] bod)]
      (kins tay)
    (rsh 3 (add wix vix) bod)
  ::
  ++  kins  |=(tay=@ (snag tay `(list skin)`[%none %open %fast %full ~]))
  ++  ksin  |=(sin=skin `@`?-(sin %none 0, %open 1, %fast 2, %full 3))
  ++  spit                                              ::  cake to packet
    |=  kec=cake  ^-  @
    =+  wim=(met 3 p.p.kec)
    =+  dum=(met 3 q.p.kec)
    =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
    =+  qax=?:((lte dum 2) 0 ?:((lte dum 4) 1 ?:((lte dum 8) 2 3)))
    =+  wix=(bex +(yax))
    =+  vix=(bex +(qax))
    =+  bod=:(mix p.p.kec (lsh 3 wix q.p.kec) (lsh 3 (add wix vix) r.kec))
    =+  tay=(ksin q.kec)
    %+  mix
      %+  can  0
      :~  [3 protocol-version]
          [20 (mug bod)]
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
      |_  [our=ship saf=sufi]                           ::  per server
      ++  born                                          ::    born:as:go
        |=  [now=@da her=@p tic=@pG ges=gens pub=pass]  ::  register user
        ^-  [(unit wyll) _+>]
        ?.  =(our (sein:title her))  [~ +>.$]
        =+  nes=sen
        =+  ryt=(end 6 1 (shaf %tick (mix her (shax sec:ex:q.nes))))
        ?.  =(tic ryt)
          ~&  [%ames-wrong-ticket `@p`ryt]
          [~ +>.$]
        =+  rad=(~(get by hoc.saf) her)
        ?^  rad
          ?.  ?=(^ lew.wod.u.rad)
            $(hoc.saf (~(del by hoc.saf) her))          :: XX how can this be?
          ?.  =(pub r.q.i.lew.wod.u.rad)  [~ +>.$]
          [[~ lew.wod.u.rad] +>.$]
        =+  syp=[[0 [~ p.nes] her now] ges pub]
        =+  ded=[(sign:as:q.nes *code (shaf %meld (sham syp))) syp fak.ton]
        =+  wil=[ded law.saf]
        ?>  =(wil (grip wil ~))
        :-  [~ wil]
        +>.$(hoc.saf (~(put by hoc.saf) her [[~31337.1.1 ~ wil] ~ *clot]))
      ::
      ++  lax                                           ::    lax:as:go
        |_  [her=ship dur=dore]                         ::  per client
        ++  cluy                                        ::    cluy:lax:as:go
          ^-  [p=life q=gens r=acru]                    ::  client crypto
          ?~  lew.wod.dur  !!
          ?.  =(fak.ton r.i.lew.wod.dur)  ~|([%client-wrong-fake her] !!)
          :+  p.p.q.i.lew.wod.dur
            q.q.i.lew.wod.dur
          (com:nu:crub:crypto r.q.i.lew.wod.dur)
        ::
        ++  clon
          ^-  life
          ?~(lew.wod.dur 0 p.p.q.i.lew.wod.dur)
        ::
        ++  deng
          |=  law=wyll
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
        ++  pode                                        ::    pode:lax:as:go
          |=  now=@da                                   ::  timeout route
          ^+  +>
          ?:  (lth her 256)  +>(lun.wod.dur [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
          +>(lun.wod.dur ~)
        ::
        ++  kuch                                        ::    kuch:lax:as:go
          |=  had=hand                                  ::  hear key tag
          ^-  (unit [code _+>])
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
        ++  wasc                                        ::    wasc:lax:as:go
          |=  key=code                                  ::  hear foreign code
          ^+  +>
          =+  had=(shaf %hand key)
          %_  ..wasc
            yed.caq.dur  [~ had key]
            qim.caq.dur  (~(put by qim.caq.dur) had key)
          ==
        ::
        ++  wast                                        ::    wast:lax:as:go
          |=  ryn=lane                                  ::  set route
          ^+  +>
          %=    +>
              lun.wod.dur
            ?:  ?=([%ix *] ryn)
              ?:  ?|  ?=(~ lun.wod.dur)
                      ?=([%ix *] u.lun.wod.dur)
                      ?&  ?=([%if *] u.lun.wod.dur)
                          (gth p.ryn (add ~s10 p.u.lun.wod.dur))
                      ==
                  ==
                [~ ryn]
              lun.wod.dur
            [~ ryn]
          ==
        ::
        ++  wist                                        ::    wist:lax:as:go
          |=  $:  now=@da                                ::  route via
                  waz=(list @p)
                  ryn=(unit lane)
                  pac=rock
              ==
          ^-  (list boon)
          ?:  =(our her)  [[%ouzo *lane pac] ~]
          ?~  waz  ~
          =+  dyr=?:(=(her i.waz) dur (gur i.waz))
          ?.  ?&  !=(our i.waz)
                  ?=(^ lun.wod.dyr)
              ==
            $(waz t.waz)
          :_  ?:  ?=(%ix -.u.lun.wod.dyr)
                $(waz t.waz)
              ~
          :+  %ouzo  u.lun.wod.dyr
          ?:  &(=(i.waz her) =(~ ryn))  pac
          =+  mal=(jam `meal`[%fore her ryn pac])
          %-  spit
          ^-  cake
          :*  [our i.waz]
              ?~  yed.caq.dyr  [%none mal]
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dyr
              (en:crub:crypto q.u.yed.caq.dyr mal)
          ==
        ::
        ++  xeno                                        ::    xeno:lax:as:go
          ^-  (list ship)                               ::  foreign canon
          (saxo:title her)
        ::
        ++  xong                                        ::    xong:lax:as:go
          ^-  (list ship)                               ::  route unto
          =+  [fro=xen too=xeno]
          =+  ^=  oot  ^-  (list ship)
              =|  oot=(list ship)
              |-  ^+  oot
              ?~  too  ~
              ?:  (lien fro |=(a=ship =(a i.too)))  ~
              [i.too $(too t.too)]
          ::  ~&  [%xong-to [our her] (weld oot ?>(?=(^ fro) t.fro))]
          (weld oot ?>(?=(^ fro) t.fro))
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
            =+  wit=(met ?:(fak.ton 16 13) q.gim)
            ?<  =(0 wit)
            ?:  =(1 wit)
              =+  yup=(spit [our her] p.gim q.gim)
              [yup ~]
            =+  ruv=(rip ?:(fak.ton 16 13) q.gim)
            =+  gom=(shaf %thug q.gim)
            =+  inx=0
            |-  ^-  (list rock)
            ?~  ruv  ~
            =+  ^=  vie
                %+  spit
                  [our her]
                wasp(ham [%carp (ksin p.gim) inx wit gom i.ruv])
            :-  vie
            $(ruv t.ruv, inx +(inx))
          ::
          ++  wisp                                      ::  generate message
            ^-  [[p=skin q=@] q=_..wisp]
            ?:  =(%carp -.ham)
              [wasp ..wisp]
            ?:  !=(~ yed.caq.dur)
              ?>  ?=(^ yed.caq.dur)
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
            ::  =+  bil=`wyll`(pare wyl.dur law.saf)    ::  XX not set
            =+  bil=law.saf                             ::  XX send whole wyll
            =+  hom=(jam ham)
            ?:  =(~ lew.wod.dur)
              :-  %open
              %^    jam
                  [~ `life`p.yig]
                bil
              (sign:as:q.yig tuy hom)
            :-  %full
              =+  cay=cluy
              %^    jam
                  [`life`p.cay `life`p.yig]
                bil
              (seal:as:q.yig pub:ex:r.cay tuy hom)
          --                                            ::  --zuul:lax:as:go
        --                                              ::  --lax:as:go
      ::
      ++  gur                                           ::  default dore
        |=  her=ship
        ^-  dore
        =+  def=?.((lth her 256) ~ [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
        [[~2100.1.1 def ~] ~ *clot]
      ::
      ++  myx                                           ::  dore by ship
        |=  her=ship
        ^+  lax
        =+  fod=(~(get by hoc.saf) her)
        ~(. lax [her ?~(fod (gur her) u.fod)])
      ::
      ++  nux                                           ::  install dore
        |=  new=_lax
        ^+  +>
        +>(hoc.saf (~(put by hoc.saf) her.new dur.new))
      ::
      ++  sen                                           ::  current crypto
        ^-  [p=life q=acru]
        ?~(val.saf !! [p.i.val.saf r.i.val.saf])
      ::
      ++  sev                                           ::  crypto by life
        |=  mar=life
        ^-  [p=? q=acru]
        ?~  val.saf  !!
        ?:  =(mar p.i.val.saf)
          [& r.i.val.saf]
        ?>  (lth mar p.i.val.saf)
        :-  |
        |-  ^-  acru
        ?>  ?=(^ t.val.saf)
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
      ++  xen                                           ::  canon
        |-  ^-  (list ship)
        (saxo:title our)
      ::
      ++  yew                                           ::  best wyll for
        |=  her=ship
        ^-  wyll
        =+  gel=(~(get by hoc.saf) her)
        ?^  gel
          lew.wod.u.gel
        ?:((lth her 256) ~ $(her (sein:title her)))
      --                                                ::  --as:go
    ::
    ++  ha  !:                                          ::  adopt new license
      |=  [our=ship mac=mace wil=wyll]
      ^-  town
      ?>  !=(~ mac)
      ?>  ?=(^ wil)
      ::  ?>  =(our r.p.q.i.wil)
      ?>  =(wil (grip wil ~))
      ?>  (real mac wil)
      %_    ton
          fak  r.i.wil
          urb
        %+  ~(put by urb.ton)
          our
        :*  %-  flop
            |-  ^-  (list ship)
            ?:((lth our 256) ~ =+(seg=(sein:title our) [seg $(our seg)]))
        ::
            (turn mac |=([p=life q=ring] [p q (nol:nu:crub:crypto q)]))
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
      |=  now=@da
      ^-  town
      !!
    ::
    ++  us                                              ::  produce safe
      |=  our=ship
      ^-  (unit _as)
      =+  goh=(~(get by urb.ton) our)
      ?~  goh  ~
      [~ ~(. as [our u.goh])]
    --                                                ::  --go
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aF, packet pump             ::
  |%
  ++  pu                                                ::  packet pump
    |_  shed
    ++  abet  +<
    ++  ahoy                                            ::    ahoy:pu
      ^+  .                                             ::  initialize
      %_    .
          rtt  ~s1
          rto  ~s4
          rtn  ~
          rue  ~
          nus  0
          nif  0
          nep  0
          caw  2
          cag  64
          diq  ~
          pyz  ~
          puq  ~
      ==
    ::
    ++  bick                                            ::    bick:pu
      |=  [now=@da fap=flap]                            ::  ack by hash
      ^-  [[p=(unit soup) q=(list rock)] _+>]
      =+  sun=(~(get by diq) fap)
      ?~  sun
        ::  ~&  [%bick-none `@p`(mug fap)]              ::  not a real error
        [[~ ~] +>.$]
      ::  ~&  [%bick-good `@p`(mug fap) u.sun]
      =.  diq  (~(del by diq) fap)
      =^  gub  +>.$  (bock now u.sun)
      =^  yop  +>.$  (harv now)
      [[gub yop] +>.$]
    ::
    ++  bilk                                            ::    bilk:pu
      |=  now=@da                                       ::  inbound packet
      ^+  +>
      =+  trt=(mul 2 rtt)
      %=  +>.$
        rue  [~ now]
        rto  trt
        rtn  ?~(puq ~ [~ (add now trt)])
      ==
    ::
    ++  boom                                            ::    boom:pu
      |=  now=@da  ^-  ?                                ::  address timeout
      |(?=(~ rue) (gte (sub now u.rue) ~m1))
    ::
    ++  bust                                            ::    bust:pu
      ^-  ?                                             ::  not responding
      &(?=(^ rtn) (gte rto ~s16))
    ::
    ++  bike                                            ::    bike:pu
      ^+  .                                             ::  check stats
      ?>  .=  nif
          |-  ^-  @
          ?~  puq  0
          :(add !liv.q.n.puq $(puq l.puq) $(puq r.puq))
      .
    ::
    ++  beet                                            ::    beet:pu
      ^+  .                                             ::  advance unacked
      =-  +(nep ?~(foh nus u.foh))
      ^=  foh
      |-  ^-  (unit @ud)
      ?~  puq  ~
      ?:  (lte p.n.puq nep)  $(puq l.puq)
      =+  rig=$(puq r.puq)
      ?^(rig rig [~ p.n.puq])
    ::
    ++  bine                                            ::    bine:pu
      |=  [now=@da num=@ud]                             ::  apply ack
      ^-  [(unit soup) _+>]
      ?~  puq  !!
      ?.  =(num p.n.puq)
        ?:  (gth num p.n.puq)
          =+  lef=$(puq l.puq)
          [-.lef +.lef(puq [n.puq puq.lef r.puq])]
        =+  rig=$(puq r.puq)
        [-.rig +.rig(puq [n.puq l.puq puq.rig])]
      =:  rtt  ?.  &(liv.q.n.puq =(1 nux.q.n.puq))  rtt
               =+  gap=(sub now lys.q.n.puq)
               ::  ~&  [%bock-trip num (div gap (div ~s1 1.000))]
               (div (add (mul 2 rtt) gap) 3)
          nif  (sub nif !liv.q.n.puq)
        ==
      =+  lez=(dec (need (~(get by pyz) gom.q.n.puq)))
      =^  gub  pyz
          ?:  =(0 lez)
            [[~ gom.q.n.puq] (~(del by pyz) gom.q.n.puq)]
          [~ (~(put by pyz) gom.q.n.puq lez)]
      :-  gub
      +>.$(puq ~(nap to puq))
    ::
    ++  bock                                            ::    bock:pu
      |=  [now=@da num=@ud]                             ::  ack by sequence
      ^-  [(unit soup) _+>]
      =^  gym  +>  (bine now num)
      :-  gym
      ?:  (gth num nep)
        =+  cam=(max 2 (div caw 2))
        ::  ~&  [%bock-hole num nep cam]
        beet:(wept(nep num, cag cam, caw cam) nep num)
      =.  caw  ?:  (lth caw cag)  +(caw)
               (add caw !=(0 (mod (mug now) caw)))
      ?:  =(num nep)
        ::  ~&  [%bock-fine num nif caw cag]
        beet
      ::  ~&  [%bock-fill num nif caw cag]
      +>.$
    ::
    ++  harv                                            ::    harv:pu
      |=  now=@da                                       ::  harvest queue
      ^-  [(list rock) _+>]
      ?:  =(~ puq)  [~ +>(rtn ~)]
      ?.  (gth caw nif)  [~ +>]
      =+  wid=(sub caw nif)
      =|  rub=(list rock)
      =<  abet  =<  apse
      |%
      ++  abet
        ?~  rub  [~ +>.$]
        [(flop rub) +>.$(rtn [~ (add rto now)])]
      ::
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  =(0 wid)  .
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?:  =(0 wid)  .
        ?.  =(| liv.q.n.puq)  .
        ::  ~&  [%harv nux.q.n.puq p.n.puq]
        %_    .
          wid          (dec wid)
          rub          [pac.q.n.puq rub]
          nif          +(nif)
          liv.q.n.puq  &
          nux.q.n.puq  +(nux.q.n.puq)
          lys.q.n.puq  now
        ==
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  wack                                            ::    wack:pu
      |=  now=@da                                       ::  wakeup (timeout)
      ^-  [(list rock) _+>]
      ?.  &(!=(~ rtn) ?>(?=(^ rtn) (gte now u.rtn)))  [~ +>]
      ::  ~&  [%slow (div rto (div ~s1 1.000))]
      =.  +>  (wept 0 nus)
      ?>  =(0 nif)
      =+  oub=(gte rto ~s16)
      =:  caw  2
          rto  ;:  min
                 (mul 2 rto)
                 ~m2
                 (mul ~s16 ?~(rue 1 +((div (sub now u.rue) ~d1))))
               ==
        ==
      (harv now)
    ::
    ++  wept                                            ::    wept:pu
      |=  [fip=@ud lap=@ud]                             ::  fip thru lap-1
      =<  abet  =<  apse
      |%
      ++  abet  +>.$
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  (lth p.n.puq fip)  ?~(l.puq . left)
        ?:  (gte p.n.puq lap)  ?~(r.puq . rigt)
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?.(liv.q.n.puq . .(nif (dec nif), liv.q.n.puq |))
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  whap                                            ::    whap:pu
      |=  [now=@da gom=soup wyv=(list rock)]            ::  send a message
      ^-  [(list rock) _+>]
      =.  pyz  (~(put by pyz) gom (lent wyv))
      =.  +>
        |-  ^+  +>.^$
        ?~  wyv  +>.^$
        %=  $
          wyv  t.wyv
          nus  +(nus)
          diq  (~(put by diq) (shaf %flap i.wyv) nus)
          puq  (~(put to puq) [nus `soul`[gom 0 | ~2000.1.1 i.wyv]])
        ==
      (harv now)
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
        %-  ~(gas by *(map ship sufi))
        %+  turn
          ~(tap by urb.ton.fox)
        |=  [p=ship q=sufi]  ^-  [p=ship q=sufi]
        :-  p
        %=    q
            val
          (turn val.q |=([p=life q=ring r=acru] [p q (nol:nu:crub:crypto q)]))
        ==
      ==
    ++  come                                            ::    come:am
      |=  [ges=(unit @t) wid=@ bur=@ fak=?]            ::  instantiate pawn
      ^-  [p=[p=ship q=@uvG] q=fort]
      =+  loy=(pit:nu:crub:crypto wid bur)
      =+  rig=sec:ex:loy
      =+  our=`@p`fig:ex:loy
      =+  syp=[[0 ~ our now] [%en %pawn ges] pub:ex:loy]
      :-  [our pac:ex:loy]
      %_    fox
          ton
        %^    ~(ha go ton.fox)
            our
          `mace`[[0 rig] ~]
        `wyll`[[(sign:as:loy *@ (shaf %self (sham syp))) syp fak] ~]
          fak.ton
        fak
      ==
    ::
    ++  czar  !:                                        ::    czar:am
      |=  [our=ship ger=@uw fak=?]                      ::  instantiate emperor
      ^-  [p=(list boon) q=fort]
      =+  ^=  loy
          ?:  fak
            ::  fake uses carrier number as seed
            ::
            (pit:nu:crub:crypto 512 our)
          (pit:nu:crub:crypto 512 ger)
      =+  fim==(fig:ex:loy (zeno our))
      ?:  &(!fak !fim)  !!                              ::  not fake & bad fig
      =+  mac=`mace`[[0 sec:ex:loy] ~]
      =+  syp=`step`[`bray`[0 ~ our now] [%en %czar ~] pub:ex:loy]
      =+  ded=`deyd`[(sign:as:loy *@ (shaf %self (sham syp))) syp fak]
      =+  buq=`buck`[mac [ded ~]]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
          fak.ton.fox  fak
        ==
      [[[%beer our pac:ex:loy] ~] fox]
    ::
    ++  gnaw                                            ::    gnaw:am
      |=  [kay=cape ryn=lane pac=rock]                  ::  process packet
      ^-  [p=(list boon) q=fort]
      ?.  =(protocol-version (end 0 3 pac))  [~ fox]
      =+  kec=(bite pac)
      ?:  (goop p.p.kec)  [~ fox]
      ?.  (~(has by urb.ton.fox) q.p.kec)
        [~ fox]
      =<  zork
      =<  zank
      ::  ~&  [%hear p.p.kec ryn `@p`(mug (shaf %flap pac))]
      %-  ~(chew la:(ho:(um q.p.kec) p.p.kec) kay ryn %none (shaf %flap pac))
      [q.kec r.kec]
    ::
    ++  goop                                            ::  blacklist
      |=  him=ship
      |
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
      |=  [our=ship buq=buck]                           ::  acquire license
      ^-  [p=(list boon) q=fort]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
        ==
      [[[%beer our pac:ex:q:sen:(need (~(us go ton.fox) our))] ~] fox]
    ::
    ++  kick                                            ::    kick:am
      |=  hen=duct                                      ::  refresh net
      =+  aks=(turn ~(tap by urb.ton.fox) |=([p=ship q=sufi] p))
      |-  ^-  [p=(list boon) q=fort]
      ?~  aks  [~ fox]
      =^  buz  fox  zork:(kick:(um i.aks) hen)
      =^  biz  fox  $(aks t.aks)
      [(weld p.buz p.biz) fox]
    ::
    ++  rack                                            ::    rack:am
      |=  [soq=sock cha=path cop=coop]                  ::  e2e ack
      =+  oh=(ho:(um p.soq) q.soq)
      =^  gud  oh  (cook:oh cop cha ~)
      ?.  gud  oh
      (cans:oh cha)
    ::
    ++  wake                                            ::    wake:am
      |=  hen=duct                                      ::  harvest packets
      ^-  [p=(list boon) q=fort]
      =+  sox=hall
      =|  bin=(list boon)
      |-  ^-  [p=(list boon) q=fort]
      ?~  sox
        =^  ban  fox  (kick hen)
        [(weld bin p.ban) fox]
      =^  bun  fox  zork:zank:thaw:(ho:(um p.i.sox) q.i.sox)
      $(sox t.sox, bin (weld p.bun bin))
    ::
    ++  wise                                            ::    wise:am
      |=  [soq=sock hen=duct cha=path val=*]            ::  send a statement
      ^-  [p=(list boon) q=fort]
      zork:zank:(wool:(ho:(um p.soq) q.soq) hen cha val)
    ::
    ++  um                                              ::  per server
      |=  our=ship
      =+  gus=(need (~(us go ton.fox) our))
      =+  ^=  weg  ^-  corn
          =+  weg=(~(get by zac.fox) our)
          ?^(weg u.weg *corn)
      =|  bin=(list boon)
      |%
      ++  ho                                            ::    ho:um:am
        |=  her=ship                                    ::  per friend
        =+  diz=(myx:gus her)
        =+  bah=(~(get by wab.weg) her)
        =+  puz=?~(bah ahoy:pu %*(. pu +< sop.u.bah))
        =>  .(bah `bath`?~(bah [abet:puz ~ ~] u.bah))
        |%
        ++  busk                                        ::    busk:ho:um:am
          |=  [waz=(list ship) pax=(list rock)]         ::  send packets
          %_    +>
              bin
            |-  ^+  bin
            ?~  pax  bin
            $(pax t.pax, bin (weld (flop (wist:diz now waz ~ i.pax)) bin))
          ==
        ::
        ++  cans                                        ::    cans:ho:um:am
          |=  cha=path
          =+  rum=(need (~(get by raz.bah) cha))
          =.  rum
            %=  rum
              did  +(did.rum)
              mis  (~(del by mis.rum) did.rum)
            ==
          (coat cha rum)
        ::
        ++  coat                                        ::    coat:ho:um:am
          |=  [cha=path rum=race]                       ::  update input race
          ^+  +>
          =+  cun=(~(get by mis.rum) did.rum)
          ?:  |(!dod.rum ?=(~ cun))
            ::
            ::  if we have not yet received the current message,
            ::  or if we are not idle, just wait.
            ::
            +>.$(raz.bah (~(put by raz.bah) cha rum))
          ?.  =(%good p.u.cun)
            ::
            ::  if we are recording a failed message, acknowledge
            ::  it now, since it obviously won't be processed.
            ::
            ~&  [%fail-ack did.rum]
            =^  gud  +>.$
              (cook ``[%dead-message ~] cha `[q.u.cun r.u.cun])
            ?.  gud  +>.$
            %=    +>.$
                raz.bah
              %+  ~(put by raz.bah)  cha
              %=  rum
                did  +(did.rum)
                mis  (~(del by mis.rum) did.rum)
              ==
            ==
          ::
          ::  the message is good.  send it to be processed.
          ::
          ?>  ?=(^ s.u.cun)
          %=    +>.$
              raz.bah  (~(put by raz.bah) cha rum(dod |))
              bin
            :_  bin
            :^    %milk
                [our her]
              `soap`[[p:sen:gus clon:diz] cha did.rum]
            u.s.u.cun
          ==
        ::
        ++  cook                                        ::    cook:ho:um:am
          |=  [cop=coop cha=path ram=(unit [ryn=lane dam=flap])]
          ^-  [gud=? con=_+>]                        ::  acknowledgment
          ::  ~&  [%ames-cook cop cha ram]
          =+  rum=(need (~(get by raz.bah) cha))
          =+  lat=(~(get by mis.rum) did.rum)
          ?:  &(?=(~ lat) ?=(~ ram))
            ~&  %ack-late-or-redundant
            [%| +>.$]
          :-  %&
          =+  ^-  [ryn=lane dam=flap]
              ?^  ram  [ryn.u.ram dam.u.ram]
              ?<  ?=(~ lat)
              [q r]:u.lat
          =.  raz.bah
            ?^  ram  raz.bah
            %+  ~(put by raz.bah)  cha
            rum(dod &, bum ?~(cop bum.rum (~(put by bum.rum) did.rum u.cop)))
          =^  roc  diz  (zuul:diz now [%back cop dam ~s0])
          (busk(diz (wast:diz ryn)) xong:diz roc)
        ::
        ++  done                                        ::    done:ho:um:am
          |=  [cha=path num=@ud]                        ::  complete outgoing
          ^-  [(unit duct) _+>]
          ::  ~&  [%ames-done cha num]
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
          |_  [kay=cape ryn=lane aut=skin dam=flap]     ::  per packet
          ::
          ++  chew                                      ::    chew:la:ho:um:am
            |=  [sin=skin msg=@]                        ::  receive
            ^+  +>
            =<  apse
            |%
            ++  apse
              ^+  +>.$
              =+  oub=bust:puz
              =+  neg==(~ yed.caq.dur.diz)
              =.  +>.$  east
              =+  eng==(~ yed.caq.dur.diz)
              =+  bou=bust:puz
              =.  bin
                ?.  &(oub !bou)  bin
                :_(bin [%wine [our her] " is ok"])
              =.  bin
                ?.  &(neg !eng)  bin
                :_(bin [%wine [our her] " is your neighbor"])
              +>.$
            ::
            ++  east
              ^+  +>.$
              ?-    sin
                  %none
                ::  ~&  %chew-none
                =.  puz  (bilk:puz now)
                (chow ((hard meal) (cue msg)))
              ::
                  %fast
                ::  ~&  %chew-fast
                =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
                =+  dey=(kuch:diz mag)
                ?~  dey
                  ::  ~&  [%bad-key her mag]
                  +>.$                           ::  ignore unknown key
                =.  puz  (bilk:puz now)
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:q:sen:gus key bod))))
              ::
                  %full
                ::  ~&  %chew-full
                =+  mex=((hard ,[p=[p=life q=life] q=wyll r=@]) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  gey=(sev:gus p.p.mex)
                =+  mes=(need (tear:as:q.gey pub:ex:r.wug r.mex))
                =.  diz  (wasc:diz p.mes)
                =.  puz  (bilk:puz now)
                (west(msg q.mes))
              ::
                  %open
                ::  ~&  %chew-open
                =+  mex=((hard ,[p=[~ q=life] q=wyll r=@]) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  mes=(need (sure:as:r.wug *code r.mex))
                =.  puz  (bilk:puz now)
                (west(msg mes))
              ==
            ++  west
              |=  ~
              =+  vib=(cue msg)
              =+  mal=(meal vib)
              ?.  =(mal vib)
                ~&  [%bad-meal her]
                +>.^$
              (chow(aut sin) mal)
            --
          ::
          ++  chow                                      ::    chow:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            =.  diz  ?:(=(%none aut) diz (wast:diz ryn))
            (dine fud)
          ::
          ++  cock                                      ::    cock:la:ho:um:am
            ^+  .                                       ::  send new ack
            ::  ~&  [%back kay dam]
            =*  cop  `coop`?:(=(%good kay) ~ ``[%dead-packet ~])
            =^  pax  diz  (zuul:diz now [%back cop dam ~s0])
            +(+> (busk(diz (wast:diz ryn)) xong:diz pax))
          ::
          ++  deer                                      ::    deer:la:ho:um:am
            |=  [cha=path num=@ud dut=(unit)]           ::  interpret message
            ^+  +>
            =+  rum=(fall (~(get by raz.bah) cha) *race)
            ::  ~&  [%rx kay cha num [dod.rum did.rum] ?=(~ dut)]
            =*  bad  (~(has in bad.fox) her)
            =.  kay  ?.((~(has in bad.fox) her) kay ~&(%blocked %dead))
            %=    +>.$
                +>
              ?:  (lth num did.rum)
                ::
                ::  this message already acknowledged; repeat old ack,
                ::  or negative ack if this ship is blocked
                ::
                =*  cop  ^-  coop
                  %+  fall
                    (~(get by bum.rum) num)
                  ?:(bad ~ ``[%blocked ~])
                con:(cook (~(get by bum.rum) num) cha `[ryn dam])
              ::
              ::  insert this message in unprocessed set
              ::
              =.  mis.rum  (~(put by mis.rum) num [kay ryn dam dut])
              ::
              ::  if ship is blocked, advance pointer to latest message
              ::
              =.  did.rum  ?.(bad did.rum num)
              ::
              ::  process update
              ::
              (coat cha rum)
            ==
          ::
          ++  dine                                      ::    dine:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            ?-    -.fud
                %back
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish handshake
              +>(..la (tock p.fud q.fud r.fud))
            ::
                %bond
              ::  ~&  [%bond q.fud r.fud]
              ?>  =(p:sen:gus p.fud)
              (deer q.fud r.fud ?-(kay %dead ~, %good [~ s.fud]))
            ::
                %carp
              ::  =+  zol=(~(get by olz.weg) s.fud)
              ::  ?^  zol  cock(kay u.zol)
              =^  neb  nys.weg
                  =+  neb=(~(get by nys.weg) s.fud)
                  ?^  neb  [u.neb nys.weg]
                  =+  neb=`bait`[(kins p.fud) 0 r.fud ~]
                  [neb (~(put by nys.weg) s.fud neb)]
              ?>  (lth q.fud p.r.neb)
              ?>  =((kins p.fud) p.neb)
              ?>  =(r.fud p.r.neb)
              =+  doy=`(unit @)`(~(get by q.r.neb) q.fud)
              ?^  doy  cock
              =>  ^+  .   %=  .
                    q.r.neb  (~(put by q.r.neb) q.fud t.fud)
                    q.neb    +(q.neb)
                  ==
              ::  ~&  [%carp q.fud s.fud q.neb p.r.neb]
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) s.fud)
                ::  olz.weg  (~(put by olz.weg) s.fud kay)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  cock
              +>.$(nys.weg (~(put by nys.weg) s.fud neb))
            ::
                %fore
              =+  ^=  lyn  ^-  lane
                  ?~  q.fud  ryn
                  ?.  ?=(%if -.u.q.fud)  u.q.fud
                  [%ix +.u.q.fud]
                  ::  u.q.fud
              ?:  =(our p.fud)
                (emit %mead lyn r.fud)
              =+  zid=(myx:gus p.fud)
              (emir (wist:zid now xong:zid [~ lyn] r.fud))
            ==
          ::
          ++  emir                                      ::    emir:la:ho:um:am
            |=  ben=(list boon)                         ::  emit boons
            ^+  +>
            ?~(ben +> $(ben t.ben, bin [i.ben bin]))
          ::
          ++  emit                                      ::    emit:la:ho:um:am
            |=  bun=boon                                ::  emit a boon
            +>(bin [bun bin])
          ::
          ++  golf                                      ::    golf:la:ho:um:am
            |=  [sin=skin duv=dove]                     ::  assemble fragments
            ^+  +>
            %+  chew  sin
            =+  [nix=0 rax=*(list @)]
            |-  ^-  @
            ?:  =(p.duv nix)
              (can ?:(fak.ton.fox 16 13) (turn (flop rax) |=(a=@ [1 a])))
            $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
          --                                            ::  --la:ho:um:am
        ::
        ++  pong                                        ::    pong:ho:um:am
          |=  hen=duct                                  ::  test connection
          ^+  +>
          ?.  ?&  =(~ puq.puz)
                  ?|  bust:puz
                      ?=(~ rue.puz)
                      (gth now (add ~s32 u.rue.puz))
                      (lth u.rue.puz hop.fox)
                  ==
              ==
            +>.$
          (wool [/a/ping hen] /a/pi ~ |)
        ::
        ++  thaw                                        ::    thaw:ho:um:am
          ^+  .                                         ::  wakeup bomb
          =+  oub=bust:puz
          =^  yem  puz  (wack:puz now)
          =+  bou=bust:puz
          =.  bin
              ?.  &(bou !oub)  bin
              :_(bin [%wine [our her] " not responding still trying"])
          =.  diz  ?:((boom:puz now) (pode:diz now) diz)
          (busk xong:diz yem)
        ::
        ++  tock                                        ::    tock:ho:um:am
          |=  [cop=coop fap=flap cot=@dr]               ::  e2e ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$
              (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%cake [our her] [[p:sen:gus clon:diz] u.p.yoh] cop u.hud]
            ==
          (busk xong:diz q.yoh)
        ::
        ++  wind                                        ::    wind:ho:um:am
          |=  [gom=soup ham=meal]
          ::  ~&  [%wind her gom]
          ^+  +>
          =^  wyv  diz  (zuul:diz now ham)
          =^  feh  puz  (whap:puz now gom wyv)
          (busk xong:diz feh)
        ::
        ++  wool                                        ::    wool:ho:um:am
          |=  [hen=duct cha=path val=*]                 ::  send a statement
          ^+  +>
          =+  ^=  rol  ^-  rill
              =+  rol=(~(get by ryl.bah) cha)
              ?~(rol *rill u.rol)
          =+  sex=sed.rol
          ::  ~&  [%tx [our her] cha sex]
          =.  ryl.bah
              %+  ~(put by ryl.bah)  cha
              rol(sed +(sed.rol), san (~(put by san.rol) sex hen))
          =+  cov=[p=p:sen:gus q=clon:diz]
          %+  wind  [cha sex]
          [%bond q.cov cha sex val]
        ::
        ++  zest                                        ::    zest:ho:um:am
          :~  :~  :*  [%rtt rtt.sop.bah]
                      [%rto rto.sop.bah]
                      [%rtn rtn.sop.bah]
                      [%rue rue.sop.bah]
                  ==
                  :*  [%nus nus.sop.bah]
                      [%nif nif.sop.bah]
                      [%nep nep.sop.bah]
                      [%caw caw.sop.bah]
                      [%cag cag.sop.bah]
                  ==
                  =+  qup=~(tap to puq.sop.bah)
                  :-  %qup
                  %+  turn  qup
                  |=  [a=@ud b=soul]
                  :*  a
                      nux.b
                      liv.b
                      lys.b
                      `@p`(mug (shaf %flap pac.b))
                      gom.b
                  ==
              ==
          ::
              :-  %raz
              =+  zar=~(tap by raz.bah)
              %+  turn  zar
              |=  [a=path b=race]
              :+  a
                did.b
              =+  ciy=~(tap by mis.b)
              %+  turn  ciy
              |=  [c=@ud d=[p=cape q=lane r=flap s=(unit)]]
              [c p.d r.d]
          ::
              [%ryl ~(tap to ryl.bah)]
              [%lun lun.wod.dur.diz]
              [%caq caq.dur.diz]
              [%lew lew.wod.dur.diz]
          ==
        ::
        ++  zank                                        ::    zank:ho:um:am
          %=  +>.$                                      ::  resolve
            gus      (nux:gus diz)
            wab.weg  (~(put by wab.weg) her bah(sop abet:puz))
          ==
        --                                              ::  --ho:um:am
      ::
      ++  kick                                          ::    kick:um:am
        |=  hen=duct                                    ::  test connection
        ^+  +>
        =+  hoy=hoy.saf.gus
        |-  ^+  +>.^$
        ?~  hoy
          +>.^$
        $(hoy t.hoy, +>.^$ (pong i.hoy hen))
      ::
      ++  pals                                          ::    pals:um:am
        ^-  (list @p)                                   ::  active neighbors
        %+  turn
          %+  skim  ~(tap by wab.weg)
          |=  [a=ship b=bath]
          !(~(boom pu sop.b) now)
        |=([a=ship b=bath] a)
      ::
      ++  pong                                          ::    pong:um:am
        |=  [her=ship hen=duct]                         ::  test neighbor
        ^+  +>
        zank:(pong:(ho her) hen)
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
  .  ==
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aH, protocol vane            ::
  ::
  =|  $:  fox=fort                                      ::  kernel state
      ==                                                ::
  |=  [now=@da eny=@ ski=sley]                          ::  current invocation
  ^?                                                    ::  opaque core
  =<
    |%                                                  ::  vane interface
    ++  call                                            ::  handle request
      |=  $:  hen=duct
              hic=(hypo (hobo task:able))
          ==
      =>  %=    .                                       ::  XX temporary
              q.hic
            ^-  task:able
            ?:  ?=(%soft -.q.hic)
              ((hard task:able) p.q.hic)
            ?:  (~(nest ut -:!>(*task:able)) | p.hic)  q.hic
            ~&  [%ames-call-flub (@tas `*`-.q.hic)]
            ((hard task:able) q.hic)
          ==
      ^-  [p=(list move) q=_..^$]
      =^  duy  ..knob
        (knob hen q.hic)
      [duy ..^$]
    ::
    ++  doze
      |=  [now=@da hen=duct]
      =+  doz=`(unit @da)`[~ (add now ~s32)]
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
      (hunt lth doz rtn.sop.bah)
    ::
    ++  load
      |=  old=fort
      ~&  %ames-reload
      ..^$(fox old)
    ::
    ++  scry
      |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
      ^-  (unit (unit cage))
      ?.  ?=($& -.why)  [~ ~]
      ?~  tyl  [~ ~]
      =+  hun=(slaw %p i.tyl)
      ?~  hun  [~ ~]
      ?.  =(`@`0 ren)  ~
      ?+    lot  ~
          [$$ %ud @]
        (perm p.why u.hun q.p.lot [syd t.tyl])
      ::
          [$$ %da @]
        ?.  =(now q.p.lot)  ~
        (temp p.why u.hun [syd t.tyl])
      ==
    ::
    ++  stay  fox
    ++  take                                            ::  accept response
      |=  [tea=wire hen=duct hin=(hypo sign:able)]
      ^-  [p=(list move) q=_..^$]
      =^  duy  ..knap
        (knap tea hen q.hin)
      [duy ..^$]
    --
  |%
  ++  claw  |=(our=ship ^-(duct hen:(need (~(get by zac.fox) our))))
  ++  clop
    |=  [now=@da hen=duct bon=boon]
    ^-  [(list move) fort]
    ?-    -.bon
        %beer
      :_  fox(zac (~(put by zac.fox) p.bon `corn`[hen ~ ~ ~]))
      :*  [hen [%slip %c %init p.bon]]
          [hen [%give %init p.bon]]
          [hen [%slip %a %kick now]]
          [hen [%slip %e %init p.bon]]
          [hen [%slip %g %init p.bon]]
          [hen [%slip %d %init p.bon]]                  ::  must be after gall
          ~
      ==
    ::
        %cake
      ::  ~?  ?=(^ r.bon)  [%cake-woot-bad hen r.bon]
      :_  fox
      :~  [s.bon %give %woot q.p.bon r.bon]
      ==
    ::
        %mead  :_(fox [[hen [%give %hear p.bon q.bon]] ~])
        %milk
      ::  ~&  [%milk p.bon q.bon]
      ?>  ?=([@ @ *] q.q.bon)
      ?>  ?=(?(%a %c %e %g) i.q.q.bon)
      =+  pax=[(scot %p p.p.bon) (scot %p q.p.bon) q.q.bon]
      :_  fox  [hen %pass pax i.q.q.bon %west p.bon t.q.q.bon r.bon]~
    ::
        %ouzo
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))]
      :_  fox
      [[gad.fox [%give %send p.bon q.bon]] ~]
    ::
        %wine
      :_  fox
      =+  fom=~(rend co %$ %p q.p.bon)
      :~  :-  hen
          :+  %slip  %d
          :+  %flog  %text
          ;:  weld
            "; "
            fom
            q.bon
          ==
      ==
    ==
  ::
  ++  knap
    |=  [tea=wire hen=duct sih=sign:able]
    ^-  [(list move) _+>]
    ?-  +<.sih
        %crud  [[[hen [%slip %d %flog +.sih]] ~] +>]
        %mack  ?~  +>.sih  $(sih [%g %nice ~])          ::  XX using old code
               $(sih [%g %mean `[%mack +>+.sih]])
        %unto  [~ +>]
        %woot  [~ +>]
        ?(%mean %nice)                                  ::  XX obsolete
      ?:  ?=([%ye ~] tea)
        [~ +>.$]
      ?>  ?=([@ @ @ *] tea)
      =+  soq=[(slav %p i.tea) (slav %p i.t.tea)]
      =+  pax=t.t.tea
      =+  ^=  fuy
          =<  zork  =<  zank
          %^  ~(rack am [now fox])  soq  pax
          ::  ~&  [%knap-ack ?-(+<.sih %mean `p.+.sih, %nice ~)]
          ?-(+<.sih %mean `p.+.sih, %nice ~)
      =>  %_(. fox q.fuy)
      =|  out=(list move)
      |-  ^-  [p=(list move) q=_+>.^$]
      ?~  p.fuy
        [(flop out) +>.^$]
      =^  toe  fox  (clop now hen i.p.fuy)
      $(p.fuy t.p.fuy, out (weld (flop toe) out))
    ==
  ::
  ++  knob
    |=  [hen=duct kyz=task:able]
    ^-  [(list move) _+>]
    ?:  ?=(%crud -.kyz)
      [[[hen [%slip %d %flog kyz]] ~] +>]
    ?:  ?=(%west -.kyz)
      ?:  ?=([%pi ~] q.kyz)
        :_  +>.$
        [[hen %give %mack ~] ~]
      ?>  ?=([%ta ~] q.kyz)
      =+  gox=((hard ,[p=@p q=@pG r=gens s=pass]) r.kyz)
      =+  gus=(need (~(us go ton.fox) p.p.kyz))
      =^  wyl  gus  (born:gus now gox)
      =.  ton.fox  (~(su go ton.fox) gus)
      :_  +>.$
      =+  ^=  pax
          :+  (scot %p p.p.kyz)
            (scot %p q.p.kyz)
          q.kyz
      [hen %pass pax %g %deal p.kyz %hood %poke %will !>(wyl)]~
    ?:  ?=(%wegh -.kyz)
      ~&  %ames-weighing
      [[hen %give %mass wegh]~ +>]
    =+  ^=  fuy
        ^-  [p=(list boon) q=fort]
        ?-    -.kyz
            %barn
          [~ fox(gad hen)]
            %cash
          (~(have am [now fox]) p.kyz q.kyz)
        ::
            %hear
          (~(gnaw am [now fox]) %good p.kyz q.kyz)
        ::
            %halo
          (~(gnaw am [now fox]) %dead p.kyz q.kyz)
        ::
            %hole
          (~(gnaw am [now fox]) %dead p.kyz q.kyz)
        ::
            %junk
          [~ fox(any.ton (shax (mix any.ton.fox p.kyz)))]
        ::
            %kick
          (~(kick am [now fox(hop p.kyz)]) hen)
        ::
            %make
          =+  vun=(~(come am [now fox]) p.kyz (bex q.kyz) r.kyz s.kyz)
          [[[%beer p.vun] ~] q.vun]
        ::
            %sith
          (~(czar am [now fox]) p.kyz q.kyz r.kyz)
        ::
            %nuke
          :-  ~
          ?:  (~(has in bad.fox) p.kyz)
            ~&  [%unblock p.kyz]
            fox(bad (~(del in bad.fox) p.kyz))
          ~&  [%block p.kyz]
          fox(bad (~(put in bad.fox) p.kyz))
        ::
            %wake
          (~(wake am [now fox]) hen)
        ::
            %want
          (~(wise am [now fox]) p.kyz hen q.kyz r.kyz)
        ==
    =>  %_(. fox q.fuy)
    =|  out=(list move)
    |-  ^-  [p=(list move) q=_+>.^$]
    ?~  p.fuy
      [(flop out) +>.^$]
    =^  toe  fox  (clop now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld (flop toe) out))
  ::
  ++  perm
    |=  [our=ship his=ship mar=@ud tyl=path]
    ^-  (unit (unit cage))
    ?~  tyl  ~
    ?:  ?=([%name ~] tyl)
      =+  wul=$(tyl [%will ~])
      :+  ~  ~
      :-  %noun
      !>(?~(wul (scot %p his) (gnow:title his q.q.q:((hard deyd) -.u.wul))))
    ?:  ?=([%gcos ~] tyl)
      =+  wul=$(tyl [%will ~])
      ?~(wul ~ ``[%noun !>(`gcos`q.q.q:((hard deyd) -.u.wul))])
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      ?:  ?=([%will ~] tyl)
        =+  fod=(~(get by hoc.saf.u.gys) his)
        ?~  fod  ~
        %+  bind  (rick mar his lew.wod.u.fod)
        |=(a=wyll `[%noun !>(a)])
      ?:  ?=([%tick ~] tyl)
        ?.  =(our (sein:title his))  ~
        ``[%noun !>((end 6 1 (shaf %tick (mix his (shax sec:ex:q:sen:u.gys)))))]
      ~
    ?:  ?=([%buck ~] tyl)
      =+  muc=(rice mar sex:u.gys)
      =+  luw=(rick mar our law.saf.u.gys)
      ?.  &(?=(^ muc) ?=(^ luw))  ~
      ``[%noun !>(`buck`[u.muc u.luw])]
    ?:  ?=([%code ~] tyl)
      ``[%noun !>((end 6 1 (shaf %pass (shax sec:ex:q:sen:u.gys))))]
    ?:  ?=([%will ~] tyl)
      (bind (rick mar our law.saf.u.gys) |=(a=wyll `[%noun !>(a)]))
    ~
  ::
  ++  temp
    |=  [our=ship his=ship tyl=path]
    ^-  (unit (unit cage))
    ?:  ?=([?(%show %tell) *] tyl)
      ?^  t.tyl  [~ ~]
      =+  gys=(~(us go ton.fox) our)
      ?~  gys  [~ ~]
      =+  zet=zest:(ho:(~(um am [now fox]) our) his)
      ``[%noun ?:(=(%show i.tyl) !>(>zet<) !>(zet))]
    ?:  ?=([%pals ~] tyl)
      ?.  =(our his)
        ~
      ``[%noun !>(pals:(~(um am [now fox]) our))]
    ?.  ?=([%life ~] tyl)
      =+  muc=$(tyl [%life ~])
      (perm our his ?~(muc 0 (@ud u.muc)) tyl)
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      =+  fod=(~(get by hoc.saf.u.gys) his)
      ?~  fod  ~
      ?~  lew.wod.u.fod  ~
      ``[%noun !>(`@ud`p.p.q.i.lew.wod.u.fod)]
    ?~  val.saf.u.gys  ~
    ``[%noun !>(`@ud`p.i.val.saf.u.gys)]
  ::
  ++  wegh
    ^-  mass
    :-  %ames
    :-  %|
    :~  fox+[%& fox]
    ==
  --
