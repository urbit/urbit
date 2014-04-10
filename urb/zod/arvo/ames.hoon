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
    ?>  =((shaf %meld (sham q.new)) (need (sure:as:(haul r.q.old) *code p.new)))
    %&
  ::
  ++  melt                                              ::  proper connect
    |=  [new=deed old=deed]
    ^-  ?
    =+  rac=(clan r.p.q.new)
    ?&  ?~  q.p.q.new
          ?&  =(r.p.q.old r.p.q.new)
              &(!=(%earl rac) =(p.p.q.old (dec p.p.q.new)))
          ==
        ?&  &(!=(%pawn rac) !=(%czar rac))
            |(=(0 p.p.q.new) =(%earl rac))
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
        =+  loy=(haul r.q.wed)
        ?>  =(0 p.p.q.wed)
        ?>  =(fig:ex:loy ?+(rac !! %czar (zeno r.p.q.wed), %pawn r.p.q.wed))
        ?>  =((shaf %self (sham q.wed)) (need (sure:as:loy *code p.wed)))
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
                    =(r.q.i.law pub:ex:(weur q.i.mac))
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
    |=  [mar=life lag=ship law=will]
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
    ^-  (list ,@uw)
    :~  0wN.Kdp5k.p5ncD.4Wsih.bFQFu   ::    0, ~zod, Tlon (gleba)
        0w0                           ::    1, ~nec, Curtis Yarvin (sator)
        0w0                           ::    2, ~bud, Charles Songhurst (angelus)
        0w0                           ::    3, ~wes, Tamares Group (angelus)
        0w0                           ::    4, ~sev, Tamares Group (angelus)
        0wt.cKYxs.Yb5VZ.boSwm.l0yYc   ::    5, ~per, Jonathan Perlow (angelus)
        0w0                           ::    6, ~sut, A16Z(a) (angelus)
        0w0                           ::    7, ~let, A16Z(b) (angelus)
        0w0                           ::    8, ~ful, A16Z (angelus)
        0w0                           ::    9, ~pen, A16Z (angelus)
        0w0                           ::   10, ~syt, A16Z (angelus)
        0w0                           ::   11, ~dur, A16Z (angelus)
        0w0                           ::   12, ~wep, ~rislus-dopsym (fidelis)
        0w0                           ::   13, ~ser, Alex Morcos (angelus)
        0w3j.H0sty.jHa3F.JlD26.4LPwV  ::   14, ~wyl, Zimran Ahmed (donum)
        0w3F.QdvV-.toAsR.hvUNk.fHjW6  ::   15, ~sun, Colin Smith (fortuna)
        0w0                           ::   16, ~ryp, FF Angel (angelus)
        0w0                           ::   17, ~syx, FF Angel (angelus)
        0w0                           ::   18, ~dyr, FF Angel (angelus)
        0w0                           ::   19, ~nup, FF Angel (angelus)
        0w0                           ::   20, ~heb, FF Angel (angelus)
        0w0                           ::   21, ~peg, FF Angel (angelus)
        0w0                           ::   22, ~lup, FF Angel (angelus)
        0w0                           ::   23, ~dep, FF Angel (angelus)
        0w0                           ::   24, ~dys, Mike Gogulski (mercor)
        0w0                           ::   25, ~put, Suhas Daftuar (angelus)
        0w0                           ::   26, ~lug, Garth Partners (angelus)
        0w0                           ::   27, ~hec, Garth Partners (angelus)
        0w0                           ::   28, ~ryt, Garth Partners (angelus)
        0w0                           ::   29, ~tyv, Garth Partners (angelus)
        0w0                           ::   30, ~syd, Curtis Yarvin (sator)
        0wp.BgRGJ.kslnv.PLAqb.TRKbr   ::   31, ~nex, Prakhar Goel (fortuna)
        0w0                           ::   32, ~lun, Tim Draper (angelus)
        0w0                           ::   33, ~mep, Tim Draper (angelus)
        0w0                           ::   34, ~lut, Tim Draper (angelus)
        0w0                           ::   35, ~sep, Tim Draper (angelus)
        0w0                           ::   36, ~pes, Curtis Yarvin (sator)
        0w2J.WSHlR.t5VHN.X8GKE.DB-yz  ::   37, ~del, ~novrud-hanweb (fidelis)
        0w1w.KF-J1.5I63F.khFyv.h0n4J  ::   38, ~sul, John Burnham (donum)
        0w0                           ::   39, ~ped, Curtis Yarvin (sator)
        0w2.Mr2Id.SX8xI.MAs-j.5Y-1W   ::   40, ~tem, Bruce Schwartz (angelus)
        0w0                           ::   41, ~led, ~lontec-botrum (fidelis)
        0w0                           ::   42, ~tul, Curtis Yarvin (sator)
        0w0                           ::   43, ~met, Curtis Yarvin (sator)
        0w0                           ::   44, ~wen, Curtis Yarvin (sator)
        0w0                           ::   45, ~byn, Curtis Yarvin (sator)
        0w0                           ::   46, ~hex, ~bishus-namsum (fidelis)
        0w0                           ::   47, ~feb, Curtis Yarvin (sator)
        0wK.GoKEY.rMjfn.ZcvFQ.n4BmX   ::   48, ~pyl, Michael Hartl (donum)
        0w0                           ::   49, ~dul, Curtis Yarvin (sator)
        0w0                           ::   50, ~het, Curtis Yarvin (sator)
        0w0                           ::   51, ~mev, Curtis Yarvin (sator)
        0w0                           ::   52, ~rut, Curtis Yarvin (sator)
        0w2L.M6-o5.DDTFL.R4sFL.7Zuay  ::   53, ~tyl, Jaan Tallinn (angelus)
        0w0                           ::   54, ~wyd, Curtis Yarvin (sator)
        0w0                           ::   55, ~tep, Curtis Yarvin (sator)
        0w0                           ::   56, ~bes, Curtis Yarvin (sator)
        0w0                           ::   57, ~dex, Curtis Yarvin (sator)
        0w0                           ::   58, ~sef, Curtis Yarvin (sator)
        0w0                           ::   59, ~wyc, Curtis Yarvin (sator)
        0w0                           ::   60, ~bur, Curtis Yarvin (sator)
        0w0                           ::   61, ~der, Curtis Yarvin (sator)
        0w0                           ::   62, ~nep, Curtis Yarvin (sator)
        0w0                           ::   63, ~pur, Curtis Yarvin (sator)
        0w0                           ::   64, ~rys, Curtis Yarvin (sator)
        0w0                           ::   65, ~reb, Curtis Yarvin (sator)
        0wp.LslIa.IFSM9.mIp-z.KBIBh   ::   66, ~den  Michael Hartl (donum)
        0w0                           ::   67, ~nut, Curtis Yarvin (sator)
        0w0                           ::   68, ~sub, Curtis Yarvin (sator)
        0w0                           ::   69, ~pet, Curtis Yarvin (sator)
        0w0                           ::   70, ~rul, Curtis Yarvin (sator)
        0w0                           ::   71, ~syn, Curtis Yarvin (sator)
        0w0                           ::   72, ~reg, Curtis Yarvin (sator)
        0w0                           ::   73, ~tyd, Curtis Yarvin (sator)
        0w0                           ::   74, ~sup, Curtis Yarvin (sator)
        0w0                           ::   75, ~sem, ~boswed-nibnyd (fidelis)
        0w0                           ::   76, ~wyn, Curtis Yarvin (sator)
        0w0                           ::   77, ~rec, Curtis Yarvin (sator)
        0w0                           ::   78, ~meg, Curtis Yarvin (sator)
        0w2L.tavpW.Lk4R-.elm7E.4KEqZ  ::   79, ~net, ~hatteb-mitlyd (fidelis)
        0w0                           ::   80, ~sec, Curtis Yarvin (sator)
        0w0                           ::   81, ~mul, Curtis Yarvin (sator)
        0w0                           ::   82, ~nym, Curtis Yarvin (sator)
        0w0                           ::   83, ~tev, Curtis Yarvin (sator)
        0w2x.~ldho.Oo7kE.QqNSx.XteFh  ::   84, ~web, Ar Vicco (donum)
        0w0                           ::   85, ~sum, Curtis Yarvin (sator)
        0w0                           ::   86, ~mut, Curtis Yarvin (sator)
        0w0                           ::   87, ~nyx, urbit.org (civitas)
        0w30.UUr19.iBPlD.wfyJD.2CWPv  ::   88, ~rex, Ben Davenport (angelus)
        0w0                           ::   89, ~teb, urbit.org (civitas)
        0w0                           ::   90, ~fus, urbit.org (civitas)
        0w0                           ::   91, ~hep, urbit.org (civitas)
        0w0                           ::   92, ~ben, urbit.org (civitas)
        0w0                           ::   93, ~mus, urbit.org (civitas)
        0w0                           ::   94, ~wyx, urbit.org (civitas)
        0w0                           ::   95, ~sym, urbit.org (civitas)
        0w0                           ::   96, ~sel, urbit.org (civitas)
        0w0                           ::   97, ~ruc, urbit.org (civitas)
        0w0                           ::   98, ~dec, urbit.org (civitas)
        0w1L.NQ-5f.ABF9R.kVwVJ.zRfn2  ::   99, ~wex, Pax Dickinson (donum)
        0w0                           ::  100, ~syr, urbit.org (civitas)
        0w0                           ::  101, ~wet, urbit.org (civitas)
        0w0                           ::  102, ~dyl, urbit.org (civitas)
        0w0                           ::  103, ~myn, urbit.org (civitas)
        0w0                           ::  104, ~mes, urbit.org (civitas)
        0w0                           ::  105, ~det, urbit.org (civitas)
        0w0                           ::  106, ~bet, urbit.org (civitas)
        0w0                           ::  107, ~bel, urbit.org (civitas)
        0w0                           ::  108, ~tux, Chen Zheng (angelus)
        0w0                           ::  109, ~tug, urbit.org (civitas)
        0w0                           ::  110, ~myr, urbit.org (civitas)
        0w0                           ::  111, ~pel, urbit.org (civitas)
        0w0                           ::  112, ~syp, urbit.org (civitas)
        0w0                           ::  113, ~ter, urbit.org (civitas)
        0w0                           ::  114, ~meb, urbit.org (civitas)
        0w0                           ::  115, ~set, urbit.org (civitas)
        0w0                           ::  116, ~dut, urbit.org (civitas)
        0w0                           ::  117, ~deg, urbit.org (civitas)
        0w0                           ::  118, ~tex, urbit.org (civitas)
        0w0                           ::  119, ~sur, urbit.org (civitas)
        0w0                           ::  120, ~fel, urbit.org (civitas)
        0w0                           ::  121, ~tud, urbit.org (civitas)
        0w0                           ::  122, ~nux, urbit.org (civitas)
        0w0                           ::  123, ~rux, urbit.org (civitas)
        0w0                           ::  124, ~ren, urbit.org (civitas)
        0w0                           ::  125, ~wyt, urbit.org (civitas)
        0w0                           ::  126, ~nub, urbit.org (civitas)
        0w0                           ::  127, ~med, urbit.org (civitas)
        0w20.GGLXx.aqxaQ.w4Iob.wdmmr  ::  128, ~lyt, Arthur Breitman (mercor)
        0w0                           ::  129, ~dus, urbit.org (civitas)
        0w0                           ::  130, ~neb, urbit.org (civitas)
        0w0                           ::  131, ~rum, urbit.org (civitas)
        0w0                           ::  132, ~tyn, urbit.org (civitas)
        0w0                           ::  133, ~seg, urbit.org (civitas)
        0w0                           ::  134, ~lyx, urbit.org (civitas)
        0w0                           ::  135, ~pun, urbit.org (civitas)
        0w0                           ::  136, ~res, urbit.org (civitas)
        0w0                           ::  137, ~red, urbit.org (civitas)
        0w3J.15iJA.0pbNk.mZXyh.A~uKb  ::  138, ~fun, Aaron Beckerman (fortuna)
        0w0                           ::  139, ~rev, urbit.org (civitas)
        0w3m.Cqumo.ZC7-e.794A4.Bqhh8  ::  140, ~ref, Matt Brubeck (fortuna)
        0w0                           ::  141, ~mec, urbit.org (civitas)
        0w0                           ::  142, ~ted, urbit.org (civitas)
        0w2d.GLlYg.-MwtO.ZCPBE.OqGB9  ::  143, ~rus, Stephen Burnham (donum)
        0w0                           ::  144, ~bex, urbit.org (civitas)
        0w0                           ::  145, ~leb, ~nosryl-tarpem (fidelis)
        0w0                           ::  146, ~dux, urbit.org (civitas)
        0w0                           ::  147, ~ryn, urbit.org (civitas)
        0w0                           ::  148, ~num, urbit.org (civitas)
        0w0                           ::  149, ~pyx, ~racbes-solmun (fidelis)
        0w2g.gLmg4.MtrHQ.A5VmH.WPk6G  ::  150, ~ryg, Dan Haffey (fortuna)
        0w0                           ::  151, ~ryx, Tlon (gleba)
        0w0                           ::  152, ~fep, Tlon (gleba)
        0w3h.8OnVd.~cI9l.Y1-lK.82Lqb  ::  153, ~tyr, ~hobmed-hinrym (fidelis)
        0w0                           ::  154, ~tus, Tlon (gleba)
        0w0                           ::  155, ~tyc, Tlon (gleba)
        0w0                           ::  156, ~leg, Tlon (gleba)
        0w0                           ::  157, ~nem, Tlon (gleba)
        0w0                           ::  158, ~fer, Tlon (gleba)
        0w0                           ::  159, ~mer, Tlon (gleba)
        0w0                           ::  160, ~ten, Tlon (gleba)
        0w0                           ::  161, ~lus, Tlon (gleba)
        0w0                           ::  162, ~nus, Tlon (gleba)
        0w0                           ::  163, ~syl, Tlon (gleba)
        0w0                           ::  164, ~tec, Tlon (gleba)
        0w0                           ::  165, ~mex, Tlon (gleba)
        0w0                           ::  166, ~pub, Tlon (gleba)
        0w0                           ::  167, ~rym, Tlon (gleba)
        0w0                           ::  168, ~tuc, Tlon (gleba)
        0w0                           ::  169, ~fyl, Tlon (gleba)
        0w0                           ::  170, ~lep, Tlon (gleba)
        0w0                           ::  171, ~deb, Tlon (gleba)
        0w0                           ::  172, ~ber, Tlon (gleba)
        0w0                           ::  173, ~mug, Tlon (gleba)
        0w0                           ::  174, ~hut, Tlon (gleba)
        0w0                           ::  175, ~tun, Tlon (gleba)
        0w0                           ::  176, ~byl, Tlon (gleba)
        0w0                           ::  177, ~sud, Tlon (gleba)
        0w0                           ::  178, ~pem, Tlon (gleba)
        0w0                           ::  179, ~dev, Tlon (gleba)
        0w0                           ::  180, ~lur, Tlon (gleba)
        0w0                           ::  181, ~def, Tlon (gleba)
        0w0                           ::  182, ~bus, Tlon (gleba)
        0w0                           ::  183, ~bep, Tlon (gleba)
        0w0                           ::  184, ~run, Tlon (gleba)
        0w0                           ::  185, ~mel, Tlon (gleba)
        0w0                           ::  186, ~pex, Tlon (gleba)
        0w0                           ::  187, ~dyt, Tlon (gleba)
        0w0                           ::  188, ~byt, Tlon (gleba)
        0w0                           ::  189, ~typ, Tlon (gleba)
        0w0                           ::  190, ~lev, Tlon (gleba)
        0w0                           ::  191, ~myl, Tlon (gleba)
        0w0                           ::  192, ~wed, Tlon (gleba)
        0w0                           ::  193, ~duc, Tlon (gleba)
        0w0                           ::  194, ~fur, Tlon (gleba)
        0w0                           ::  195, ~fex, Tlon (gleba)
        0w0                           ::  196, ~nul, Tlon (gleba)
        0w0                           ::  197, ~luc, Tlon (gleba)
        0w0                           ::  198, ~len, Tlon (gleba)
        0w0                           ::  199, ~ner, Tlon (gleba)
        0wv.aixe9.7gG2w.7cJiy.i3Mg8   ::  200, ~lex, Michael Hartl (donum)
        0w0                           ::  201, ~rup, Tlon (gleba)
        0w0                           ::  202, ~ned, Tlon (gleba)
        0w0                           ::  203, ~lec, Tlon (gleba)
        0w0                           ::  204, ~ryd, Tlon (gleba)
        0w1U.n361n.FC3jj.9cX26.V1Wif  ::  205, ~lyd, Adam Bliss (fortuna)
        0w0                           ::  206, ~fen, Tlon (gleba)
        0w0                           ::  207, ~wel, Tlon (gleba)
        0w0                           ::  208, ~nyd, Tlon (gleba)
        0w0                           ::  209, ~hus, Tlon (gleba)
        0w0                           ::  210, ~rel, Tlon (gleba)
        0w0                           ::  211, ~rud, Tlon (gleba)
        0w0                           ::  212, ~nes, Tlon (gleba)
        0w16.~8NZV.VyMmf.4toMO.pui1R  ::  213, ~hes, Alex Moskalyuk (angelus)
        0w0                           ::  214, ~fet, Tlon (gleba)
        0w0                           ::  215, ~des, Tlon (gleba)
        0w0                           ::  216, ~ret, Tlon (gleba)
        0w0                           ::  217, ~dun, Tlon (gleba)
        0w0                           ::  218, ~ler, Tlon (gleba)
        0w10.w0AUz.QVdks.HCNvf.ja~TO  ::  219, ~nyr, Ivan Matosevic (fortuna)
        0w0                           ::  220, ~seb, Tlon (gleba)
        0w0                           ::  221, ~hul, Tlon (gleba)
        0w0                           ::  222, ~ryl, Tlon (gleba)
        0w0                           ::  223, ~lud, Tlon (gleba)
        0w0                           ::  224, ~rem, Tlon (gleba)
        0w0                           ::  225, ~lys, Tlon (gleba)
        0w3C.YXlEl.pFbYV.9pYWI.d7cla  ::  226, ~fyn, Stephen Burnham (donum)
        0w0                           ::  227, ~wer, Tlon (gleba)
        0w0                           ::  228, ~ryc, Tlon (gleba)
        0w0                           ::  229, ~sug, Tlon (gleba)
        0w0                           ::  230, ~nys, Tlon (gleba)
        0w0                           ::  231, ~nyl, Tlon (gleba)
        0w0                           ::  232, ~lyn, Tlon (gleba)
        0w0                           ::  233, ~dyn, Tlon (gleba)
        0w0                           ::  234, ~dem, Tlon (gleba)
        0w0                           ::  235, ~lux, Mark Zavislak (angelus)
        0w0                           ::  236, ~fed, Tlon (gleba)
        0w0                           ::  237, ~sed, Tlon (gleba)
        0w0                           ::  238, ~bec, Tlon (gleba)
        0w0                           ::  239, ~mun, Tlon (gleba)
        0w0                           ::  240, ~lyr, Tlon (gleba)
        0w0                           ::  241, ~tes, Tlon (gleba)
        0w0                           ::  242, ~mud, ~difryt-dapdeg (fidelis)
        0w4.yybWD.F1BgE.ynzlF.47neH   ::  243, ~nyt, Byrne Hobart (mercor)
        0w0                           ::  244, ~byr, Tlon (gleba)
        0w0                           ::  245, ~sen, Tlon (gleba)
        0w0                           ::  246, ~weg, Tlon (gleba)
        0w0                           ::  247, ~fyr, Tlon (gleba)
        0w0                           ::  248, ~mur, Tlon (gleba)
        0w0                           ::  249, ~tel, Tlon (gleba)
        0w0                           ::  250, ~rep, Tlon (gleba)
        0w0                           ::  251, ~teg, Tlon (gleba)
        0w0                           ::  252, ~pec, Tlon (gleba)
        0w0                           ::  253, ~nel, Tlon (gleba)
        0w0                           ::  254, ~nev, Tlon (gleba)
        0wY.a0HAU.7Lbkf.6V514.OsJBv   ::  255, ~fes, John Burnham (donum)
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
    ?>  =(2 vez)
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
      :~  [3 2]
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
    |_  ton=toun                                        ::  ames state
    ++  as                                              ::    as:go
      |_  [our=ship saf=sufi]                           ::  per server
      ++  born                                          ::    born:as:go
        |=  [now=@da her=@p tic=@pG ges=gens pub=pass]  ::  register user
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
        =+  syp=[[0 [~ p.nes] her now] ges pub]
        =+  ded=[(sign:as:q.nes *code (shaf %meld (sham syp))) syp]
        =+  wil=[ded law.saf]
        ?>  =(wil (grip wil ~))
        :-  [~ wil]
        +>.$(hoc.saf (~(put by hoc.saf) her [[~31337.1.1 ~ wil] ~ *cask]))
      ::
      ++  lax                                           ::    lax:as:go
        |_  [her=ship dur=door]                         ::  per client
        ++  cluy                                        ::    cluy:lax:as:go
          ^-  [p=life q=gens r=acru]                    ::  client crypto
          ?~  lew.wod.dur  !!
          :+  p.p.q.i.lew.wod.dur
            q.q.i.lew.wod.dur
          (haul r.q.i.lew.wod.dur)
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
        ++  pode                                        ::    pode:lax:as:go
          |=  now=@da                                   ::  timeout route
          ^+  +>
          ?:  (lth her 256)  +>
          +>(lun.wod.dur ~)
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
                          ?|  !=(q.ryn p.u.lun.wod.dur)
                              !=(r.ryn q.u.lun.wod.dur)
                          ==
                      ==
                  ==
                [~ ryn]
              lun.wod.dur
            [~ ryn]
          ==
        ::
        ++  wist                                        ::    wist:lax:as:go
          |=  $:  now=@da                               ::  route via
                  waz=(list ,@p)
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
              (en:crya q.u.yed.caq.dyr mal)
          ==
        ::
        ++  xeno                                        ::    xeno:lax:as:go
          ^-  (list ship)                               ::  foreign canon
          (saxo her)
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
            =+  wit=(met 13 q.gim)
            ?<  =(0 wit)
            ?:  =(1 wit)
              =+  yup=(spit [our her] p.gim q.gim)
              [yup ~]
            =+  ruv=(rip 13 q.gim)
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
            =+  ^=  gom
                %^    jam
                    `life`p.yig
                  ::  `will`(pare wyl.dur law.saf)      ::  XX not set
                  law.saf                               ::  XX send whole will
                ham
            ?:  =(~ lew.wod.dur)
              [%open gom]
            :-  %full
            =+  cay=cluy
            (jam p.cay (seal:as:q.yig pub:ex.r.cay tuy gom))
          --                                            ::  --zuul:lax:as:go
        --                                              ::  --lax:as:go
      ::
      ++  gur                                           ::  default door
        |=  her=ship
        ^-  door
        =+  def=?.((lth her 256) ~ [~ %if 0 (mix her .0.0.1.0)])
        [[~2100.1.1 def ~] ~ *cask]
      ::
      ++  myx                                           ::  door by ship
        |=  her=ship
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
        (saxo our)
      ::
      ++  yew                                           ::  best will for
        |=  her=ship
        ^-  will
        =+  gel=(~(get by hoc.saf) her)
        ?^  gel
          lew.wod.u.gel
        ?:((lth her 256) ~ $(her (sein her)))
      --                                                ::  --as:go
    ::
    ++  ha                                              ::  adopt new license
      |=  [our=ship mac=mace wil=will]
      ^-  toun
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
            |-  ^-  (list ship)
            ?:((lth our 256) ~ =+(seg=(sein our) [seg $(our seg)]))
        ::
            (turn mac |=([p=life q=ring] [p q (weur q)]))
            wil
            ~
            ~
        ==
      ==
    ::
    ++  su                                              ::  install safe
      |=  new=_as
      ^-  toun
      ton(urb (~(put by urb.ton) our.new saf.new))
    ::
    ++  ti                                              ::  expire by time
      |=  [now=@da]
      ^-  toun
      !!
    ::
    ++  us                                              ::  produce safe
      |=  our=ship
      ^-  (unit ,_as)
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
      |-  ^-  (unit ,@ud)
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
    |_  [now=@da fox=furt]                              ::  protocol engine
    ++  boot                                            ::    boot:am
      ^-  furt                                          ::  restore from noun
      %=    fox
          urb.ton
        %-  ~(gas by *(map ship sufi))
        %+  turn
          (~(tap by urb.ton.fox) ~)
        |=  [p=ship q=sufi]  ^-  [p=ship q=sufi]
        :-  p
        %=    q
            val
          (turn val.q |=([p=life q=ring r=acru] [p q (weur q)]))
        ==
      ==
    ++  come                                            ::    come:am
      |=  [ges=(unit ,@t) wid=@ bur=@]                  ::  instantiate pawn
      ^-  [p=[p=ship q=@uvG] q=furt]
      =+  loy=(bruw wid bur)
      =+  rig=sec:ex:loy
      =+  our=`@p`fig:ex:loy
      =+  syp=[[0 ~ our now] [%en %pawn ges] pub:ex:loy]
      :-  [our pac:ex:loy]
      %_    fox
          ton
        %^    ~(ha go ton.fox)
            our
          `mace`[[0 rig] ~]
        `will`[[(sign:as:loy _@ (shaf %self (sham syp))) syp] ~]
      ==
    ::
    ++  czar                                            ::    czar:am
      |=  [our=ship ger=@uw]                            ::  instantiate emperor
      ^-  [p=(list boon) q=furt]
      =+  loy=(bruw 2.048 ger)
      ?>  =(fig:ex:loy (zeno our))
      =+  mac=`mace`[[0 sec:ex:loy] ~]
      =+  syp=`step`[`bray`[0 ~ our now] [%en %czar ~] pub:ex:loy]
      =+  ded=`deed`[(sign:as:loy _@ (shaf %self (sham syp))) syp]
      =+  buq=`buck`[mac [ded ~]]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
        ==
      [[[%beer our pac:ex:loy] ~] fox]
    ::
    ++  gnaw                                            ::    gnaw:am
      |=  [kay=cape ryn=lane pac=rock]                  ::  process packet
      ^-  [p=(list boon) q=furt]
      ?.  =(2 (end 0 3 pac))  [~ fox]
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
    ++  goop
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
      ^-  [p=(list boon) q=furt]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
        ==
      [[[%beer our pac:ex:q:sen:(need (~(us go ton.fox) our))] ~] fox]
    ::
    ++  kick                                            ::    kick:am
      |=  hen=duct                                      ::  refresh net
      =+  aks=(turn (~(tap by urb.ton.fox) ~) |=([p=ship q=sufi] p))
      |-  ^-  [p=(list boon) q=furt]
      ?~  aks  [~ fox]
      =^  buz  fox  zork:(kick:(um i.aks) hen)
      =^  biz  fox  $(aks t.aks)
      [(weld p.buz p.biz) fox]
    ::
    ++  wake                                            ::    wake:am
      |=  hen=duct                                      ::  harvest packets
      ^-  [p=(list boon) q=furt]
      =+  sox=hall
      =|  bin=(list boon)
      |-  ^-  [p=(list boon) q=furt]
      ?~  sox
        =^  ban  fox  (kick hen)
        [(weld bin p.ban) fox]
      =^  bun  fox  zork:zank:thaw:(ho:(um p.i.sox) q.i.sox)
      $(sox t.sox, bin (weld p.bun bin))
    ::
    ++  wash                                            ::    wash:am
      |=  [soq=sock sup=soap ham=meal]                  ::  dispatch and send
      ^-  [p=(list boon) q=furt]
      zork:zank:(wind:(ho:(um p.soq) q.soq) [q.sup r.sup] ham)
    ::
    ++  wise                                            ::    wise:am
      |=  [soq=sock hen=duct cha=path val=*]             ::  send a statement
      ^-  [p=(list boon) q=furt]
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
        ++  done                                        ::    done:ho:um:am
          |=  [cha=path num=@ud]                        ::  complete outgoing
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
                  ~&  [%bad-key her mag]
                  +>.$                           ::  ignore unknown key
                =.  puz  (bilk:puz now)
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:q:sen:gus key bod))))
              ::
                  %full
                ::  ~&  %chew-full
                =+  mex=((hard ,[p=life q=@]) (cue msg))
                =+  gey=(sev:gus p.mex)
                =+  hey=sen:gus
                =+  mes=(need (tear:as:q.gey pub:ex.q.hey q.mex))
                =.  diz  (wasc:diz p.mes)
                =.  puz  (bilk:puz now)
                (west(msg q.mes) p.mes)
              ::
                  %open
                ::  ~&  %chew-open
                =.  puz  (bilk:puz now)
                (west *code)
              ==
            ++  west
              |=  key=code
              =+  ^=  mex
                  %.  (cue msg)
                  (hard ,[p=life q=will r=*])
              =.  diz  (deng:diz q.mex)
              =+  wug=cluy:diz
              ?>  =(p.mex p.wug)
              =+  mal=(meal r.mex)
              ?.  =(mal r.mex)
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
            ^+  .                                       ::  acknowledgment
            ::  ~&  [%back kay dam]
            =^  pax  diz  (zuul:diz now [%back kay dam ~s0])
            +(+> (busk(diz (wast:diz ryn)) xong:diz pax))
          ::
          ++  coot                                      ::    coot:la:ho:um:am
            |=  [cha=path rum=race]                     ::  update input race
            ^+  +>
            =+  cun=(~(get by mis.rum) did.rum)
            ?~  cun
              +>.$(raz.bah (~(put by raz.bah) cha rum))
            =.  +>.$  cock(kay p.u.cun, dam q.u.cun)
            =.  +>.$  ?.  =(%good p.u.cun)  +>.$
                      ?>  ?=(^ r.u.cun)
                      %-  emit
                      ^-  boon
                      :^    %milk
                          [our her]
                        `soap`[[p:sen:gus clon:diz] cha did.rum]
                      u.r.u.cun
            %=  $
              mis.rum  (~(del by mis.rum) did.rum)
              did.rum  +(did.rum)
            ==
          ::
          ++  dear                                      ::    dear:la:ho:um:am
            |=  [cha=path num=@ud dut=(unit)]           ::  interpret message
            ^+  +>
            =+  ^=  rum  ^-  race
                =+  rum=(~(get by raz.bah) cha)
                ?~(rum *race u.rum)
            ?.  (gte num did.rum)
              cock                                      ::  always ack a dup
            (coot cha rum(mis (~(put by mis.rum) num [kay dam dut])))
          ::
          ++  dine                                      ::    dine:la:ho:um:am
            |=  fud=meal                                ::  interpret meal
            ^+  +>
            ?-    -.fud
                %back
              ::  ~&  [%back aut her ryn `@p`(mug dam)]
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish key exch
              +>(..la (tuck p.fud q.fud r.fud))
            ::
                %bond
              ::  ~&  [%bond q.fud r.fud]
              ?>  =(p:sen:gus p.fud)
              (dear q.fud r.fud ?-(kay %dead ~, %good [~ s.fud]))
            ::
                %carp
              =+  zol=(~(get by olz.weg) s.fud)
              ?^  zol  cock(kay u.zol)
              =^  neb  nys.weg
                  =+  neb=(~(get by nys.weg) s.fud)
                  ?^  neb  [u.neb nys.weg]
                  =+  neb=`bait`[(kins p.fud) 0 r.fud ~]
                  [neb (~(put by nys.weg) s.fud neb)]
              ?>  (lth q.fud p.r.neb)
              ?>  =((kins p.fud) p.neb)
              ?>  =(r.fud p.r.neb)
              =+  doy=`(unit ,@)`(~(get by q.r.neb) q.fud)
              ?^  doy  cock
              =>  ^+  .   %=  .
                    q.r.neb  (~(put by q.r.neb) q.fud t.fud)
                    q.neb    +(q.neb)
                  ==
              ::  ~&  [%carp q.fud s.fud q.neb p.r.neb]
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) s.fud)
                    olz.weg  (~(put by olz.weg) s.fud kay)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  cock
              +>.$(nys.weg (~(put by nys.weg) s.fud neb))
            ::
                %fore
              =+  ^=  lyn  ^-  lane
                  ?~  q.fud  ryn
                  ?.  ?=(%if -.u.q.fud)  u.q.fud
                  [%ix now +.u.q.fud]
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
            =+  [nix=0 rax=*(list ,@)]
            |-  ^-  @
            ?:  =(p.duv nix)
              (can 13 (turn (flop rax) |=(a=@ [1 a])))
            $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
          ::
          ++  wait                                      ::    wait:la:ho:um:am
            |=  [sin=skin msg=@]                        ::  receive indirect
            ^+  +>
            =+  pay=((hard ,[p=@ud q=@uvH r=@]) (cue msg))
            =.  nys.weg  (~(put by nys.weg) q.pay [sin 0 p.pay ~])
            (dine [%carp (ksin sin) 0 p.pay q.pay r.pay])
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
              ==  +>.$
          (wool [/a hen] /q/pi ~)
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
        ++  tuck                                        ::    tuck:ho:um:am
          |=  [kay=cape fap=flap cot=@dr]               ::  ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$  (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%coke [our her] [[p:sen:gus clon:diz] u.p.yoh] kay u.hud]
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
          (wind [cha sex] [%bond q.cov cha sex val])
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
                  =+  qup=(~(tap to puq.sop.bah) ~)
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
              =+  zar=(~(tap by raz.bah) ~)
              %+  turn  zar
              |=  [a=path b=race]
              :+  a
                did.b
              =+  ciy=(~(tap by mis.b) ~)
              %+  turn  ciy
              |=  [c=@ud d=[p=cape q=flap r=(unit)]]
              [c p.d q.d]
          ::
              [%ryl (~(tap to ryl.bah) ~)]
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
        ^-  (list ,@p)                                  ::  active neighbors
        %+  turn
          %+  skim  (~(tap by wab.weg) ~)
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
        ^-  [p=(list boon) q=furt]                      ::  resolve
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
  =|  $:  fox=furt                                      ::  kernel state
      ==                                                ::
  |=  [now=@da eny=@ sky=$+(* (unit))]                  ::  current invocation
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
      |=  [sam=? old=vase]
      ^-  vane
      (load old)
    ::
    ++  doze
      |=  [now=@da hen=duct]
      =+  doz=`(unit ,@da)`[~ (add now ~s32)]
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
      (hunt doz rtn.sop.bah)
    ::
    ++  load
      |=  new=vase
      ^-  vane
      ?.  (~(nest ut -:!>(fox)) & p.new)  ~|(%load-type-fail !!)
      ..^$(fox ~(boot am [now (furt q.new)]))
    ::
    ++  raze
      ^-  vane
      ..$(fox *furt)
    ::
    ++  scry
      |=  [our=ship ren=@tas his=ship syd=disc lot=coin tyl=path]
      ^-  (unit)
      ?.  =(0 ren)  ~
      ?+    lot  ~
          [%$ %ud @]
        (perm our his q.p.lot [syd tyl])
      ::
          [%$ %da @]
        ?.  =(now q.p.lot)  ~
        (temp our his [syd tyl])
      ==
    ::
    ++  stay  `vase`!>(fox)
    ++  vern  [164 0]
    --
  |%
  ++  claw  |=(our=ship ^-(duct hen:(need (~(get by zac.fox) our))))
  ++  clop
    |=  [wru=(unit writ) now=@da hen=duct bon=boon]
    ^-  [(list move) furt]
    ?-    -.bon
        %beer
      :_  fox(zac (~(put by zac.fox) p.bon `corn`[hen ~ ~ ~]))
      :*  [[~ %gold p.bon] [/c hen] [%init p.bon]]
          [[~ %gold p.bon] hen [%init p.bon]]
          [[~ %gold p.bon] [/a hen] [%kick now]]
          [[~ %gold p.bon] [/e hen] [%init p.bon]]
          ~
          ::  =+  bos=(sein p.bon)
          ::  =.  bos  ?.(=(bos p.bon) bos ~zod)
          ::  ?:  =(~zod p.bon)  ~
          ::  :~  [[~ %iron p.bon] [/c hen] [%pull bos %main ~[%main]]]
          ::      [[~ %iron p.bon] [/c hen] [%pull bos %spec ~[%spec]]]
          ::      [[~ %iron p.bon] [/c hen] [%pull bos %try ~[%try]]]
          ::      [[~ %iron p.bon] [/c hen] [%pull bos %arvo ~[%arvo]]]
          ::  ==
      ==
    ::
        %coke
      ::  ~&  [%tz p.bon q.bon r.bon]
      :_  fox
      :~  [[~ %iron p.p.bon] s.bon [%went q.p.bon r.bon]]
      ==
    ::
        %mead  :_(fox [[wru hen [%hear p.bon q.bon]] ~])
        %milk
      ::  ~&  [%rx p.bon q.bon]
      ?>  ?=([@ *] q.q.bon)
      ?:  ?=(%r i.q.q.bon)
        ?>  ?=([@ @ *] t.q.q.bon)
        :_  fox
        =+  [cak=i.t.q.q.bon ven=i.t.t.q.q.bon]
        :~  :-  [~ %iron p.p.bon]
            =+  neh=(claw p.p.bon)
            ?>  ?=(^ neh)
            ::  ~&  [%milk-waft [[ven `path`t.t.t.q.q.bon] t.neh]]
            :-  ?:  =(%c ven)
                  ?>  =(%re cak)
                  [[%c `path`t.t.t.q.q.bon] hen]
                ?:  =(%e ven)
                  ?>  =(%pr cak)
                  [[%e `path`t.t.t.q.q.bon] hen]
                [[ven `path`t.t.t.q.q.bon] t.neh]
            `card`[%waft q.p.bon r.bon]
        ==
      ?>  ?=(%q i.q.q.bon)
      ?>  ?=([@ *] t.q.q.bon)
      ?+    i.t.q.q.bon
        :_  fox
        :~  :+  [~ %iron p.p.bon]
              (claw p.p.bon)
            `card`[%wart q.p.bon i.t.q.q.bon t.t.q.q.bon r.bon]
        ==
      ::
          %pi                                           ::  ping
        $(bon [%wine p.bon " sent a ping at {(scow %da now)}"])
      ::
          %pr                                           ::    %pr
        :_  fox
        :~  :-  [~ %iron p.p.bon]
            [[/e hen] `card`[%wart q.p.bon i.t.q.q.bon t.t.q.q.bon r.bon]]
        ==
      ::
          %ta
        =+  gox=((hard ,[p=@p q=@pG r=gens s=pass]) r.bon)
        =+  gus=(need (~(us go ton.fox) p.p.bon))
        =^  wyl  gus  (born:gus now gox)
        =.  ton.fox  (~(su go ton.fox) gus)
        :_  fox
        :~  :+  [~ %iron p.p.bon]
              [/a /a hen]
            `card`[%want q.p.bon [%r %ta t.t.q.q.bon] `(unit will)`wyl]
        ==
          %re                                           ::    %re
        :_  fox
        :~  :-  [~ %iron p.p.bon]
            [[/c hen] `card`[%wart q.p.bon i.t.q.q.bon t.t.q.q.bon r.bon]]
        ==
      ::
          %ye                                           ::    %ye
        ::  ~&  [%ye bon]
        ?>  =(p.p.bon (sein q.p.bon))
        =+  ^=  paz  ^-  (list ,@p)
            %+  skim  pals:(~(um am [now fox]) p.p.bon)
            |=(a=@p =(p.p.bon (sein a)))
        :_  fox
        %+  turn  paz
        |=  him=ship
        :+  [~ %iron p.p.bon]
          [/a /a hen]
        [%want him /q/yu [q.p.bon r.bon]]
      ==
    ::
        %ouzo
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))]
      :_  fox
      [[wru hen [%send p.bon q.bon]] ~]
    ::
        %wine
      :_  fox
      =+  nym=(temp p.p.bon q.p.bon /name)
      =+  fom=~(rend co %$ %p q.p.bon)
      :~  :+  wru  [/d hen]
          :+  %flog  %text
          ;:  weld
            "; "
            ?:  |(?=(~ nym) =(%$ u.nym))  fom
            :(weld fom " " (trip ((hard ,@) u.nym)))
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
    =+  ^=  fuy  ^-  [p=(list boon) q=furt]
        ?+    -.fav
          [~ fox]
        ::
            %cash
          (~(have am [now fox]) p.fav q.fav)
        ::
            %hear
          (~(gnaw am [now fox]) %good p.fav q.fav)
        ::
            %hole
          (~(gnaw am [now fox]) %dead p.fav q.fav)
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
          (~(czar am [now fox]) p.fav q.fav)
        ::
            %want
          ?>  ?=(^ wru)
          (~(wise am [now fox]) [q.u.wru p.fav] hen q.fav r.fav)
        ::
            %wake
          (~(wake am [now fox]) hen)
        ==
    =>  %_(. fox q.fuy)
    =|  out=(list move)
    |-  ^-  [p=(list move) q=_+>.^$]
    ?~  p.fuy
      [(flop out) +>.^$]
    =^  toe  fox  (clop wru now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld (flop toe) out))
  ::
  ++  perm
    |=  [our=ship his=ship mar=@ud tyl=path]
    ^-  (unit)
    ?~  tyl  ~
    ?:  ?=([%name ~] tyl)
      =+  wul=$(tyl [%will ~])
      [~ ?~(wul (scot %p his) (gnow his q.q.q:((hard deed) -.u.wul)))]
    ?:  ?=([%gcos ~] tyl)
      =+  wul=$(tyl [%will ~])
      [~ ?~(wul ~ [~ `gcos`q.q.q:((hard deed) -.u.wul)])]
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
      ?.  =(our (sein u.hur))  ~
      [~ (end 6 1 (shaf %tick (mix u.hur (shax sec:ex:q:sen:u.gys))))]
    ?:  ?=([%will ~] tyl)
      (rick mar our law.saf.u.gys)
    ~
  ::
  ++  temp
    |=  [our=ship his=ship tyl=path]
    ^-  (unit)
    ?:  ?=([?(%show %tell) *] tyl)
      ?^  t.tyl  ~
      =+  gys=(~(us go ton.fox) our)
      ?~  gys  ~
      =+  zet=zest:(ho:(~(um am [now fox]) our) his)
      [~ ?:(=(%show i.tyl) >zet< zet)]
    ?.  ?=([%life ~] tyl)
      =+  muc=$(tyl [%life ~])
      (perm our his ?~(muc 0 (,@ud u.muc)) tyl)
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      =+  fod=(~(get by hoc.saf.u.gys) his)
      ?~  fod  ~
      ?~  lew.wod.u.fod  ~
      [~ `@ud`p.p.q.i.lew.wod.u.fod]
    ?~  val.saf.u.gys  ~
    [~ `@ud`p.i.val.saf.u.gys]
  --
