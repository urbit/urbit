
!:  ::  %eyre, http servant
!?  164
::::
|=  pit=vase
^-  vane                                                ::  kernel instrument
=>  =~
|%                                                      ::  structures
++  ju                                                  ::  jug engine
  |/  a=(jug)
  +-  get
    |*  b=*
    =+  c=(~(get by a) b)
    ?~(c ~ u.c)
  ::
  +-  has
    |*  [b=* c=*]
    ^-  ?
    (~(has in (get(+< a) b)) c)
  ::
  +-  put
    |*  [b=* c=*]
    ^+  a
    =+  d=(get(+< a) b)
    (~(put by a) b (~(put in d) c))
  --
++  bolo                                                ::  eyre state
  $:  gub=@t                                            ::  random identity
      hov=(unit ,@p)                                    ::  master for remote
      ged=duct                                          ::  client interface
      ney=@uvI                                          ::  rolling entropy
      dop=(map host ship)                               ::  host aliasing
      own=(map ship serf)                               ::  domestic servers
      fon=(map ship rote)                               ::  foreign servers
      ask=[p=@ud q=(map ,@ud ,[p=duct q=hiss])]         ::  outgoing by number
      kes=(map duct ,@ud)                               ::  outgoing by duct
      lor=(map duct dual)                               ::  incoming by duct
  ==                                                    ::
++  clue                                                ::  console
  $:  ino=@ud                                           ::  input sequence
      ono=@ud                                           ::  (lent out)
      voy=(map ,@ud (list ,@ud))                        ::  waiters (q.rey)
      out=(list json)                                   ::  output commands
  ==                                                    ::
++  cyst                                                ::  client session
  $:  ced=cred                                          ::  credential
      cow=(map ,@ud clue)                               ::  consoles
      cug=(list ,@t)                                    ::  unacked cookies
      lax=@da                                           ::  last used
      rey=[p=@ud q=(map ,@ud pimp)]                     ::  live requests
  ==                                                    ::
++  dual  ,[p=@ud q=(each ,[p=ship q=hole] ship)]       ::  request handle
++  dude  ,[p=@tas q=@]                                 ::  client identity
++  loco  ,[p=? q=(unit ,@tas) r=path]                  ::  logical construct
++  pest                                                ::  request in progress
  $|  $?  %new                                          ::  virgin
          %way                                          ::  waiting
      ==                                                ::
  $%  [%err p=@ud q=(list tank)]                        ::  error report
      [%fin p=love]                                     ::  ready to send
      [%haz p=riot]                                     ::  clay responded
      [%raw p=hiss]                                     ::  wild url
      [%who p=@tas q=@ta]                               ::  awaiting auth
  ==                                                    ::
++  pimp                                                ::  traced request
  $:  ful=?                                             ::  | === HEAD
      hen=duct                                          ::  event trace
      som=seam                                          ::  logical request
      pez=pest                                          ::  request state
  ==                                                    ::
++  rote                                                ::  remote server
  $:  cnt=@ud                                           ::  number served
      sor=@p                                            ::  home sponsor
      rem=[p=@ud q=(map ,@ud duct)]                     ::  active requests
  ==                                                    ::
++  serf                                                ::  local server
  $:  pef=@t                                            ::  server prefix
      wup=(map hole cyst)                               ::  secure sessions
      cah=(map cash vase)                               ::  compilation cache
  ::  wez=(map duct root)                               ::  all routes
  ==                                                    ::
--                                                      ::
|%
++  colt                                                ::  prune to save
  |=  bol=bolo
  %_(bol own (~(run by own.bol) |=(a=serf a(cah ~))))
::
++  coss                                                ::  cookie search
  |=  [nam=@t mah=math]
  ^-  (unit hole)
  =+  ^=  cok  ^-  (list ,@t)
      =+  cok=(~(get by mah) 'cookie')
      ?~(cok ~ u.cok)
  |-  ^-  (unit hole)
  ?~  cok  ~
  =+  mar=`(unit (list ,[p=@t q=@t]))`(rush i.cok cock:epur)
  ?~  mar  $(cok t.cok)
  |-  ^-  (unit hole)
  ?~  u.mar  ^$(cok t.cok)
  ?:(=(nam p.i.u.mar) [~ q.i.u.mar] $(u.mar t.u.mar))
::
++  ecco                                                ::  eat headers
  |=  hed=(list ,[p=@t q=@t])
  =+  mah=*math
  |-  ^-  math
  ?~  hed  mah
  =+  cus=(cass (rip 3 p.i.hed))
  =+  zeb=(~(get by mah) cus)
  $(hed t.hed, mah (~(put by mah) cus ?~(zeb [q.i.hed ~] [q.i.hed u.zeb])))
::
++  lofi                                                ::  insert scripts
  |=  [mog=(list manx) luv=love]
  ^-  love
  ?:  =(~ mog)  luv
  ?+    -.luv  luv
      %mid
    =+  str=(trip q.q.luv)
    =+  scr=|-(^-(tape ?~(mog "</body>" (xmlt & i.mog $(mog t.mog)))))
    =+  rep=(need (repg "</body>" str scr))
    [%mid p.luv (tact rep)]
  ==
::
++  loft                                                ::  love to response
  |=  luv=love
  ^-  httr
  ?-  -.luv
    %mid  [200 ~[content-type/(moon p.luv)] [~ q.luv]]
    %ham  [200 ~[content-type/'text/html'] [~ (tact (xmlt | p.luv ~))]]
    %raw  p.luv
    %wan  :+  200
            ~[content-type/'text/plain']
          :-  ~
          %-  taco
          %+  rap  3
          |-  ^-  (list ,@)
          ?~(p.luv ~ [i.p.luv 10 $(p.luv t.p.luv)])
    %zap  :+  p.luv
            ~[content-type/'text/plain']
          :-  ~
          %-  tell
          |-  ^-  wall
          ?~  q.luv  ~
          (weld (~(win re i.q.luv) 0 120) $(q.luv t.q.luv))
  ==
--
|%                                                      ::  functions
++  ye                                                  ::  per event
  =|  $:  $:  $:  wru=(unit writ)                       ::  event authority
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          bolo                                          ::  all vane state
      ==                                                ::
  =*  bol  ->
  |%
  ++  abet
    ^-  [(list move) bolo]
    [(flop mow) bol]
  ::
  ++  adit
    .(ney (mix eny ney))
  ::
  ++  apex
    |-  ^+  +
    ?+    -.fav
      +.$(mow [[wru hen fav] mow])
    ::
        %born  +(ged hen)                               ::  register external
        %init                                           ::  register ownership
      %_    +.$
          hov  ?~(hov [~ p.fav] [~ (min u.hov p.fav)])
          own
        %+  ~(put by own)
          p.fav
        ^-  serf
        :*  (cat 3 gub (rsh 3 1 (scot %p p.fav)))
            ~
            ~
        ==
      ==
    ::
        %that                                           ::  outbound response
      ?>  ?=([@ @ @ ~] tea)                             ::
        =+  :*  our=(slaw %p i.tea)                     ::  ship
                ses=i.t.tea                             ::  session
                num=(slaw %ud i.t.t.tea)                ::  request in session
          ==                                            ::
      !!
    ::
        %them                                           ::  outbound request
      ?~  p.fav
        =+  sud=(need (~(get by kes) hen))
        %=  +.$
          mow    :_(mow [~ ged [%thus sud ~]])
          q.ask  (~(del by q.ask) sud)
          kes    (~(del by kes) hen)
        ==
      %=  +.$
        mow    :_(mow [~ ged [%thus p.ask p.fav]])
        p.ask  +(p.ask)
        q.ask  (~(put by q.ask) p.ask hen u.p.fav)
        kes    (~(put by kes) hen p.ask)
      ==
    ::
        %they                                           ::  inbound response
      =+  kas=(need (~(get by q.ask) p.fav))
      %=  +.$
        mow    :_(mow [~ p.kas [%thou q.fav]])
        q.ask  (~(del by q.ask) p.kas)
      ==
    ::
        %this                                           ::  inbound request
      =*  sec  p.fav    ::  ?                           ::  https bit
      =*  heq  r.fav    ::  httq                        ::  request content
      =+  ryp=`quri`(rash q.heq zest:epur)
      =+  mah=(ecco r.heq)
      =+  ^=  pul  ^-  purl
          ?-  -.ryp
            &  ?>(=(sec p.p.p.ryp) p.ryp)
            |  =+  hot=(~(get by mah) %host)
               ?>  ?=([~ @ ~] hot)
               [[sec (rash i.u.hot thor:epur)] p.ryp q.ryp]
          ==
      =.  p.p.pul  |(p.p.pul =([& /localhost] r.p.pul))
      (hell pul +.fav [p.heq mah s.heq])
    ::
        %thud                                           ::  cancel request
      =+  dul=(~(get by lor) hen)
      ?~  dul  +.$
      =.  lor  (~(del by lor) hen)
      ?-  -.q.u.dul
        &  =+  boy=(myth p.p.q.u.dul q.p.q.u.dul)
           ?~(boy +.$ abet:(idle:u.boy p.u.dul))
        |  (hops p.q.u.dul p.u.dul)
      ==
    ::
        %thou                                           ::  remote return
      ?>  ?=([@ *] tea)
      (hajj (need (slaw %p i.tea)) t.tea p.fav)
    ::
        %wart                                           ::  remote request
      ?>  ?=(^ wru)
      ?+    q.fav
        ~&  [%strange-wart p.fav q.fav]
        +.$
      ::
          %pr  (hare r.fav p.fav s.fav)
          %pc  (here q.u.wru p.fav s.fav)
      ==
    ::
        %waft
      ?.  ?=([%hork @ ~] tea)
        +.$
      (gosh p.fav (need (slaw %ud i.t.tea)) ((hard httr) q.fav))
    ::
        %went
      +.$
    ::
        %writ
      ?.  ?=([%hoot @ @ ~] tea)
        +.$
      ?>  ?=(^ wru)
      (gout q.u.wru i.t.tea (need (slaw %ud i.t.t.tea)) p.fav)
    ::
        ?(%helo %tell %text %talk %warn)
      ?.  ?=([%cons @ @ ~] tea)
        +.$
      ?>  ?=(^ wru)
      (goat q.u.wru i.t.tea (need (slaw %ud i.t.t.tea)) fav)
    ==
  ::
  ++  doss                                              ::  host to ship
    |=  hot=host
    ^-  (unit ship)
    =+  gow=(~(get by dop) hot)
    ?^  gow  gow
    ?.  &(?=(& -.hot) ?=(^ p.hot))  ~
    (rush -:(flop p.hot) fed:ag)
  ::
  ++  fail                                              ::  request failed
    |=  [sas=@ud str=tape]
    ^+  +>
    %-  muff
    :-  %thou
    ^-  httr
    [sas ~[content-type/'text/plain'] [~ (tact str)]]
  ::
  ++  goat
    |=  [our=ship ses=hole num=@ud fav=card]
    =+  suf=(~(get by own) our)
    ?~  suf  +>.$
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  +>.$
    abet:work:(~(dodo ya [our ses] u.suf u.cuz) num fav)
  ::
  ++  gosh                                              ::  receive %pr response
    |=  [him=ship num=@ud har=httr]
    ^+  +>
    =+  ryt=(~(get by fon) him)
    ?~  ryt  +>.$
    =+  zur=(~(get by q.rem.u.ryt) num)
    ?~  zur  +>.$
    %_  +>.$
      mow  :_(mow [wru u.zur [%thou har]])
      fon  (~(put by fon) him u.ryt(q.rem (~(del by q.rem.u.ryt) num)))
    ==
  ::
  ++  gout
    |=  [our=ship ses=hole num=@ud rot=riot]
    ^+  +>
    =+  suf=(~(get by own) our)
    ?~  suf  +>.$
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  +>.$
    abet:work:(~(iota ya [our ses] u.suf u.cuz) num rot)
  ::
  ++  haji                                              ::  send %pc login
    |=  [him=ship ses=hole]
    ^+  +>
    +>.$(mow :_(mow [wru [/a /e hen] [%want him [%r %pc ~] ses]]))
  ::
  ++  hajj                                              ::  send %pr response
    |=  [him=ship tus=path har=httr]
    ^+  +>
    +>.$(mow :_(mow [wru [/a /e hen] [%want him [%r %pr tus] har]]))
  ::
  ++  hare                                              ::  receive request
    |=  [tus=path him=ship hor=*]
    ^+  +>
    =+  hux=((hard (unit httx)) hor)
    %_    +>.$
        mow
      :_  mow
      [wru [/e [%e (scot %p him) tus] hen] ?~(hux [%thud ~] [%this u.hux])]
    ==
  ::
  ++  here                                              ::  receive auth
    |=  [our=ship him=ship hez=*]
    ^+  +>
    =+  boy=(myth our (need ((sand %ta) ((hard ,@) hez))))
    ?~(boy +>.$ abet:(auth:u.boy him))
  ::
  ++  hell                                              ::  request, no ship
    |=  [pul=purl hyx=httx moh=moth]
    ^+  +>
    =^  wiq  q.q.pul
        ?~  q.q.pul  [~ ~]
        =+  nam=(cat 3 '~' i.q.q.pul)
        =+  gow=(rush i.q.q.pul fed:ag)
        ^-  [(unit ship) (list ,@t)]
        ?~(gow [~ q.q.pul] [gow t.q.q.pul])
    =+  oar=`(unit ship)`?^(wiq wiq (doss r.p.pul))
    ?~  oar
      (horn pul q.hyx moh)
    ?.  (home u.oar)
      (hork u.oar hyx)
    (huff u.oar ?=(@ wiq) q.hyx pul moh)
  ::
  ++  home                                              ::  do we own?
    |=  who=ship
    ^-  ?
    ?:  (~(has by own) who)  &
    ?:  (~(has by fon) who)  |
    !=(~ (sky /a/(scot %p who)/buck/(scot %da now)))
  ::
  ++  hoot                                              ::  clay request
    |=  [our=ship num=@ud ses=hole rif=riff]
    %_    +>
        mow
      :_  mow
      :+  [~ %gold our]
        [/c [%e %hoot ses (scot %ud num) ~] hen]
      [%warp our rif]
    ==
  ::
  ++  hops                                              ::  cancel remote
    |=  [him=ship num=@]
    ^+  +>
    =+  mun=(scot %ud num)
    =+  rot=(need (~(get by fon) him))
    %_    +>.$
        mow
      :_  mow
      :+  [~ %gold sor.rot]
        [/a [%e %hork mun ~] hen]
      [%want him [%q %pr %e %hork mun ~] ~]
    ==
  ::
  ++  hork                                              ::  remote request
    |=  [him=ship hyx=httx]
    ^+  +>
    =+  ^=  sur  ^-  (unit ship)
        ?^  hov  hov
        ?^  own  [~ p.n.own]
        ~
    ?~  sur  (fail 500 "no vessel available to proxy {<him>}")
    ?.  (gth (met 3 him) (met 3 u.sur))                 ::  very permissive
      (fail 500 "<u.sur> cannot proxy for <him>")
    =+  ^=  rot  ^-  rote
        =+  rut=(~(get by fon) him)
        ?^  rut  u.rut
        [0 u.sur [0 ~]]
    =+  num=p.rem.rot
    =+  mun=(scot %ud num)
    %_    +>.$
        lor  (~(put by lor) hen num [%| him])
        mow
      :_  mow
      :+  [~ %gold sor.rot]
        [/a [%e %hork mun ~] hen]
      [%want him [%q %pr %e %hork mun ~] [~ hyx]]
    ::
        fon
      %+  ~(put by fon)  him
      %_  rot
        cnt  +(cnt.rot)
        p.rem  +(p.rem.rot)
        q.rem  (~(put by q.rem.rot) num hen)
      ==
    ==
  ::
  ++  horn                                              ::  irregular request
    |=  [pul=purl cip=clip moh=moth]
    ^+  +>
    =-  ?:  &(=(/favicon q.q.pul) ?=([~ ?(%ico %png)] p.q.pul))
          %-  muff
          :-  %thou
          ^-  httr
          [200 ~[content-type/'image/png'] [~ (taco fac)]]
        ?:  &(=(/robots q.q.pul) ?=([~ %txt] p.q.pul))
          %-  muff
          :-  %thou
          ^-  httr
          [200 ~[content-type/'text/plain'] [~ (taco rob)]]
        (fail 400 "urbit: url {<pul>} does not match a vessel")
    :*
        ^=  rob
      %-  role
      :~  'User-agent: *'
          'Disallow: /'
      ==
    ::
        ^=  fac
      0w89.wgGV4.jAl90.00003.sV4OG.IJjfa.1vYpi.gRxB9.3m6kA.dopig.
      RxB93.m6kAd.opigR.xB93m.6kAdo.pigRx.B93m6.kAdop.igRxB.93m6k.
      Adopi.gRxBf.vGSfy.m8hQj.T-DiD.7kqvH.vEpA3.3vH-C.in~Tq.l8U0n.
      1FVhj.w9E1A.NIF6w.4j9v~.VZ0~B.9flkB.IY90B.-ieSV.Ky8Q~.4~07s.
      JcXFC.DtI-1.GGz-1.V-olV.g3wsv.ZCQM1.BJbVj.Vwiv0.uo7Gh.4qsxA.
      92ZYU.tJ5uH.yiIzV.FwvJR.UUq6z.cpKIG.Hck9v.qGDm1.PY2rM.itxLB.
      fn0Bo.5DO8x.oO7KE.kYh-P.NiKp1.HT88j.Mu3ZK.ciKsU.TnlkV.0Zo77.
      12ciy.nY3dM.7nDnY.GVgGh.ZllpO.SFHFb.p1Ae0.uUpXV.eqFvS.pkBRl.
      jv0MP.ilRHP.1HwtK.GFptt.2KdpP.RsYqI.wRHEG.j~LZQ.I06qJ.fP0Pp.
      77qjo.s0PU0.rGGg6.lgNvc.~CZE~.bSp9j.EGHF~.UqYB6.l4Y~Z.P~GGE.
      LwrJs.ZvYV-.U4Wh4.04dws.6HeuZ.2ZF7A.y4MN5.3vsCj.QHzjW.4lflk.
      WU6X0.AmMws.vbMfB.3e1s~.aeE7W.0hQPH.ODvMf.cvgzb.Y15Ah.01384.
      YwVPT.KzILB.PlaqN.pNlvw.fdQ79.~mPpo.YaHqw.fnWGB.QYM4F.w3E0b.
      0o~n-.faydD.zlllI.0nU3D.w5rlI.4nrSG.VkhPM.NTkpa.eoXzw.9AEYN.
      auZGt.99gxL.8RlsI.aXMXX.tFVhX.V4kj8.yczjh.faRI3.JTg1H.-0uZM.
      JA6rR.z0~pO.uXiSg.rvU27.A58MU.TBijQ.23F1J.CCIYE.IO8w-.cMlMA.
      hvKh4.zY16M.gjRlk.v--9h.TNNRR.HhIGo.8kZXk.Wb74j.faHlk.6V-Vw.
      jMan8.yb37R.Q2h42.Or3Nw.Pp39w.jZ--3.-jwZH.U~3Za.Uu0u6.bNAOP.
      U2jux.Jqo2R.O8x1~.ecZvL.30ug~.qpoFw.vwtqD.Vb6EI.cZQyO.EN-xl.
      nlsLC.dT099.apOh5.SEeDz.07-GE.xFzZk.KcmCl.SJWF5.v3u1x.Uq1Cj.
      tV~hG.YuGGb.SgpdR.xHaBh.S3eEv.q0mSg.RZh8s.wxhnk.EcNvW.GccZQ.
      yO0Jb.n18hs.BLFx2.iigqf.AhsKS.LWqby.TUEmv.gmmhR.6DW3w.uLR0Y.
      QQBC8.YoQ63.g8m8i.iq3B-.SxwLn.jLbh3.l7cq3.eVQmV.5O2df.SXBkv.
      Y3LLb.denQq.GvR0R.P3Gh4.2iiq2.h-srW.o0ZZ-.HIrdj.npm5n.pnv07.
      vyT77.43WGP.Bciiq.zt1cI.7A4xB.zK9xm.-tV6x.ZdA6P.pheXQ.aSz4X.
      Zj2bS.C1UPx.~c1dS.xwF3b.6jZ-M.WI2eQ.e69Qw.DGFly.tTze-.GGbZU.
      qJ-m-.fD8yI.Adktz.oqTsF.F7ltA.6no6T.~fWJU.0gRsp.-P88x.a9I9b.
      Adkvz.ory8J.Ouhfu.H8c-U.2HLgE.Wi4xH.3AEGK.VjkS-.Z5hMx.UN5o~.
      Y~EWp.7LGox.IQxpt.cgONH.CEyKJ.jjTdM.GJ9HL.RloJZ.xuRtL.JZ7jg.
      ZZj6w.2AOoM.CENdS.xxegZ.RzTdh.i-1hZ.N1HPF.EqHU0.XzN6K.mBedG.
      uvBiL.HqpmY.Bl9z2.qzqA8.WzKqz.h~S1J.K2QHQ.Dy-CM.7RO0l.QksW3.
      mpFnx.fy-Pa.p7xhW.SboOd.fOBon.mCgSX.Z38Qe.dMHUC.79wje.wziG5.
      6Xtn7.ksEHO.xkBrO.e7yFe.vNaYx.FgDsI.BS9y8.AELs-.C9-DB.FAZI-.
      wKt2N.8qQhA.Apxm7.O5yIB.X51l9.Kduxm.SRA5N.UYi6I.MrySX.RZXrT.
      8UcY2.zUAfu.SOcUK.vZrDL.vBAHb.eOo~N.7J3sR.eJhSo.4~YE1.5k0h5.
      51RqS.b0jyR.RfhON.4Dt07.idahL.5isLK.eeBv3.znQxC.9LXkE.xKghP.
      Ia-R0.AgmB5.pGGIA.slCGu.CtR5q.NrzHh.1bscz.8CsWC.KH4it.LLrWm.
      UlRdr.lUGji.W76xr.kVAmO.6oAYS.7nXX~.kfeM2.TSS2m.JOCAb.sFFWg.
      4xH3C.MDKh4.FZso1.tXwUJ.Taq5K.8yS24.xHr4M.Kvu~E.HTpka.-Zg3f.
      KEXFS.qCKwh.l1KRN.c9H8A.HFcSw.rePCF.Iy93m.njkMZ.IEyiq.lFq3y.
      gRFzg.uL9tz.zP8du.Y1ZWP.PtQ6G.gzIt5.K8hNz.UAdpM.Q43L6.IMHx5.
      N8qPh.EfX8G.UC~68.S93ms.d18Vh.adkOx.GLkTI.khFcL.ZWG5G.Adoeh.
      hx~As.hci6I.Uq2pG.ykqHO.yUAdq.gQ7FD.4sOjn.IwGGw.UAdqo.Q4jVN.
      eJP8c.xQlm~.8nJ1y.gRF3g.oSPAM.fuqE0.M~23y.gRHyo.gngjF.ceM3n.
      V~uQy.93m-9.xa-3N.T80~v.GzR-g.HqBGA.mi4xH.3AMOL.mCjT5.Blqab.
      60ruw.HDV~k.Tj~fX.Swx8u.ZFOoi.m1GUF.Gs4-q.0kfxh.H8yjt.OCXGL.
      PYGTY.23LgI.Wl4x6.8bI3e.MXeVb.h6rL9.DXWyt.8wJ8W.HalWR.itqp3.
      pkrSC.8bQSM.HLV2J.G7sCj.QtGEi.AkSwI.A4P0J.gJ85j.MuMUY.nkT45.
      -rkqv.BFBFU.KGd98.qRs~A.iblOv.mVKWx.Z19cs.AxHc6.UIKJc.NIHW8.
      EnOEy.fygRG.29bbR.FBDVL.Ter6T.SBKat.MFBPE.AfuO9.kBHV~.QstE-.
      VaYNV.qpfhL.sFHj0.eFphG.U6Hw6.EsVox.7kpks.N6bRk.GMLY~.HWBVj.
      Snx6X.0GY2b.GhzmW.udfRF.jTgLC.uPWGL.fIwM6.16Ah4.NFZjz.Ftln7.
      KQ-k-.0SO8H.xrqcw.MXZG9.6BZsJ.zULJU.NPDy3.aewMa.3auiA.Ysei3.
      YQJGB.PlCAQ.S5YPU.uGEtI.wQrw1.cy8Sd.bFYuX.GGWZS.DSq1Y.O8ELq.
      cR6kA.dopig.RxB93.m6kAd.opigR.xB93m.6kAdo.pigRx.B93m6.kAdop.
      igRxB.93m6k.Adopi.gRxB9.3m6kA.doSsI.1Tves.7Fb5l.hneus.VDLsZ.
      ~P3DD.D~CpI.BALPA.rxSTT.fuXa4.gP3Yv.sPKOY.KSMKP.balqk.xjbEH.
      idkbq.Elo0N.dHjkM.vEBiq.BC-Rb.IKMiB.JiaoS.x3mLy.Jr6P5.ToiS2.
      gAz4y.qNHiI.k7WIl.9EJGb.iJ2Tp.NQ5H5.VpSni.By-OX.TfvYs.plRic.
      rpPJD.7xkgk.h9BMw.001EY.XFJDs.CYKpn.1xoTd.HrCAK.tTtT0.6lOon.
      tQpCZ.jt5x5.t1A00.01UCO.x20ts.d003n.3g00s.RB8s0.A0002.8p0xY.
      20w82.5h9gD.c4000.0l9ny.s0000.0o8p0.0006g.0001i.h4x93.g0000.
      Eq2wR.7jB29
    ==
  ::
  ++  huff                                              ::  request by ship
    |=  [our=ship hey=? cip=clip pul=purl moh=moth]
    =*  sec  p.p.pul
    =+  ^=  sef  ^-  serf
        =+  suf=(~(get by own) our)
        ?^  suf  u.suf
        =+  sef=*serf
        sef(pef (cat 3 gub (rsh 3 1 (scot %p our))))    ::  XX transitional
    =+  ^=  saw  ^-  [p=hole q=cyst]
        =+  lig=(coss pef.sef q.moh)
        ?^  lig
          =+  cyz=(need (~(get by wup.sef) u.lig))
          [u.lig cyz(cug ~)]
        =+  ses=(rsh 3 1 (scot %p (end 6 1 ney)))
        :-  ses
        ^-  cyst
        :*  ^-  cred
            :*  [sec hey q.p.pul r.p.pul]
                ~
                (rsh 3 1 (scot %p (end 6 1 (shaf %oryx ses))))
            ::
                =+  lag=(~(get by q.moh) %accept-language)
                ?~(lag ~ ?~(u.lag ~ [~ i.u.lag]))
            ::
                cip
                ~
            ==
        ::
            ~
        ::
            :_  ~
            %^  cat  3
              (cat 3 (cat 3 pef.sef '=') ses)
            ::  (cat 3 '; HttpOnly' ?.(sec '' '; Secure'))
            '; Path=/; HttpOnly'
        ::
            now
            [1 ~]
        ==
    abet:work:(~(into ya [our p.saw] sef q.saw) pul moh)
  ::
  ++  muff                                              ::  return card
    |=  fav=card
    +>(mow :_(mow [wru hen fav]))
  ::
  ++  myth                                              ::  load session
    |=  [our=ship ses=hole]
    =+  suf=(~(get by own) our)
    ?~  suf  ~
    =+  cuz=(~(get by wup.u.suf) ses)
    ?~  cuz  ~
    [~ u=~(. ya [our ses] u.suf u.cuz)]
  ::
  ++  noon                                              ::  login page
    |=  [our=ship whu=(unit ship) rul=tape ruf=tape]
    ^-  manx
    =+  ^=  sic  ^-  manx
      ;script
        ;
        ; var seal = {
        ;   who: goal,
        ;   url: burl,
        ;   pas: null
        ; }
        ; var hist = []
        ; var hind = 0
        ; $(
        ;   function() {
        ;     $input = $('#input .line')
        ;     $prompt = $('#input .prompt')
        ;     $prompt.addClass('prefix')
        ;     $output = $('#output')
        ;     $input.focus()
        ;     $('body').click(function() { $input.focus() })
        ;     ctrl = false;
        ;
        ;     start = function(ship) {
        ;       $prompt.text('vessel: ~')
        ;       $input.attr('placeholder', 'ship-name')
        ;       if(ship) {
        ;         $input.val(ship)
        ;       }
        ;     }
        ;
        ;     ident = function() {
        ;       seal.who = $input.val()
        ;
        ;       if( (seal.who.length != 13) &&
        ;           (seal.who.length != 6) &&
        ;           (seal.who.length != 3) )
        ;       {
        ;         $output.text('not a ship name - try again.');
        ;         return false;
        ;       }
        ;
        ;       if(seal.who !== host) {
        ;         var foreign = {oth: host, ses: session};
        ;         var all = $.extend({}, seal, foreign);
        ;
        ;         console.log('redirect')
        ;         window.location="http://"+seal.who+".urbit.org/gul"
        ;                         + $.params(all);
        ;         return false;
        ;       }
        ;
        ;       $output.text($prompt.text() + " " + seal.who)
        ;       $input.val('')
        ;       $input.attr('placeholder', 'ronber-bacnub-hanmev-labnyd')
        ;       $prompt.text('secret: ~')
        ;
        ;       return true;
        ;     }
        ;
        ;     login = function() {
        ;       seal.pas = $input.val()
        ;
        ;       output = $output.html()
        ;       console.log($output.html())
        ;       $output.html(output.replace('sorry. please try again.<br>',''))
        ;
        ;       $.post(form, seal, function(data,xhr,status) {
        ;         if(data.ok == true) {
        ;           document.location = "/goc/0"
        ;         } else {
        ;           $output.prepend('sorry. please try again.<br>')
        ;         }
        ;       })
        ;     }
        ;
        ;     steps = [ident,login]
        ;     step = 0
        ;     start(seal.who)
        ;     if(seal.who) {
        ;       ident()
        ;       step++
        ;     }
        ;
        ;     $input.on('keydown', function(e) {
        ;       if(e.keyCode == 17) {
        ;         ctrl = true
        ;         return;
        ;       }
        ;
        ;       if(e.keyCode == 68 &&
        ;         ctrl == true &&
        ;         step == 1) {
        ;         $output.text('')
        ;         step = 0
        ;         start(null)
        ;         return;
        ;       }
        ;
        ;       if(e.keyCode == 13) {
        ;         if(steps[step]() && step < steps.length-1)
        ;           step++
        ;         return;
        ;       }
        ;     });
        ;
        ;     $input.on('keyup', function(e) {
        ;       if(e.keyCode == 17) {
        ;         ctrl = false
        ;       }
        ;     });
        ;   })
      ==
    =+  ^=  cof
      ;=
        ; var host = '{(trip (rsh 3 1 (scot %p our)))}';
        ; var goal = '{?~(whu ~ (trip (rsh 3 1 (scot %p u.whu))))}';
        ; var burl = '{rul}';
        ; var form = '{ruf}';
      ==
    =>  .(+.sic (weld `marl`cof `marl`+.sic))
    =+  jqu="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
    =+  ^=  hed
      ;head
        ;title: urbit login
        ;script(type "text/javascript", src jqu);
        ;style
          ; body {
          ;   margin: 60px 120px;
          ;   font: normal 12px "Menlo" monospace;
          ;   background-color: #000;
          ;   color: #fff;
          ; }
          ;
          ; #output {
          ;
          ; }
          ;
          ; #input .prompt {
          ;   display: inline-block;
          ;   margin-right: 12px;
          ; }
          ;
          ; #input .line {
          ;   outline: none;
          ;   width: 80%;
          ;   border: 0;
          ;   background-color: transparent;
          ;   color: #fff;
          ;   font: normal 12px "Menlo" monospace;
          ; }
        ==
      ==
    =+  ^=  bod  ^-  manx
      ;body
        ;div@output;
        ;div@input
          ;div/prompt;
          ;input/line(type "text");
        ==
      ==
    =.  +.bod  (weld `marl`+.bod `marl`~[sic])
    ;html
      ;+  hed
      ;+  bod
    ==
  ::
  ++  ya                                                ::  session engine
    =|  [[our=ship ses=hole] serf cyst]
    =*  sef  ->-
    =*  cyz  ->+
    |%
    ++  abet                                            ::  resolve engine
      ^+  ..ya
      %=    ..ya
          own
        (~(put by own) our sef(wup (~(put by wup) ses cyz)))
      ==
    ::
    ++  auth                                            ::  remote authorize
      |=  him=ship
      %_(+> aut.ced (~(put ju aut.ced) %$ (scot %p him)))
    ::
    ++  dodo                                            ::  console s->c
      |=  [con=@ud fav=card]
      ^+  +>
      =+  cal=(need (~(get by cow) con))
      =+  ^=  jon  ^-  json
          ?:  ?=(%helo -.fav)
            %+  joba  %helo
            %-  jobe
            :~  [%path [%a `(list jval)`(turn p.fav |=(a=@ta [%s a]))]]
                [%prod ~[%a [%s p.q.fav] (jape q.q.fav) (jape r.q.fav)]]
            ==
          %+  joba  %text
          :-  %a  ^-  (list jval)
          ?+  -.fav  ~|(-.fav !!)
            %tell  (turn p.fav |=(a=@t [%s a]))
            %text  [%s (crip p.fav)]~
            %talk  (turn (~(win re p.fav) [0 80]) |=(a=tape [%s (crip a)]))
            %warn  [%s (crip '!' p.fav)]~
          ==
      =+  ^=  yov  ^-  (list ,@ud)
          =+  yov=(~(get by voy.cal) ono.cal)
          ?~(yov ~ u.yov)
      =:  voy.cal  (~(del by voy.cal) ono.cal)
          ono.cal  +(ono.cal)
          out.cal  [jon out.cal]
        ==
      =.  cow  (~(put by cow) con cal)
      ?~(yov +>.$ (dove ~[%a (jone ono.cal) (jone ino.cal) jon] yov))
    ::
    ++  dove                                            ::  console waiting
      |=  [jon=json yov=(list ,@ud)]
      ^+  +>
      =+  noz=`pest`[%fin %mid /text/json (tact (pojo jon))]
      |-  ^+  +>.^$
      ?~  yov  +>.^$
      =+  pup=(~(get by q.rey) i.yov)
      ?~  pup  $(yov t.yov)
      ?>  ?=(%way pez.u.pup)
      $(yov t.yov, q.rey (~(put by q.rey) i.yov u.pup(pez noz)))
    ::
    ++  fake
      ^-  (unit seam)
      :+  ~  %app
      ;html
        ;head  ;title: not an application
        ==
        ;body
          ;p: Not an application, on {<our>}, with {<aut.ced>}
        ==
      ==
    ::
    ++  flea                                            ::  permissive decimal
      |=  txt=@t
      ^-  (unit ,@ud)
      =+  zac=(slay txt)
      ?:  ?=([~ %$ %ud @] zac)
        [~ q.p.u.zac]
      =+  soc=(rush txt dim:ag)
      ?~(soc ~ [~ u.soc])
    ::
    ++  foal                                            ::  url from query
      |=  [nam=@t yaq=(map ,@t ,@t)]
      ^-  (unit purl)
      =+  uru=(~(get by yaq) %url)
      ?~  uru  ~
      (rush u.uru auri:epur)
    ::
    ++  folk                                            ::  seal from query
      |=  quy=quay
      ^-  (unit seal)
      =+  yaq=(~(gas by *(map ,@t ,@t)) quy)
      =+  pyl=(foal %url yaq)
      =+  wit==(%yes (fall (~(get by yaq) %wit) %no))
      =+  huw=(~(get by yaq) %who)
      =+  whu=?~(huw ~ (rush u.huw fed:ag))
      =+  pus=(~(get by yaq) %pas)
      =+  tuh=(~(get by yaq) %oth)
      =+  thu=?~(tuh ~ (rush u.tuh fed:ag))
      =+  sus=(~(get by yaq) %ses)
      =+  foy=?~(sus ~ ?~(thu ~ [~ u.thu u.sus]))
      ?~  pyl  ~
      ?:  &(wit |(=(~ whu) =(~ ses)))  ~
      ~&  [%folk-quay whu u.pyl wit foy pus]
      [~ whu u.pyl wit foy pus]
    ::
    ++  fool                                            ::  domestic login get
      |=  quy=quay
      ^-  (unit seam)
      (bind (folk quy) |=(a=seal [%lon a]))
    ::
    ++  foom                                            ::  domestic login post
      |=  moh=moth
      ^-  (unit seam)
      ?.  ?&  ?=(^ r.moh)
              ::  .=  [~ 'application/x-www-form-urlencoded' ~]
              ::  (~(get by q.moh) 'content-type')
          ==  ~
      =+  yuq=(rush q.u.r.moh yquy:epur)
      ?~(yuq ~ (fool u.yuq))
    ::
    ++  flub                                            ::  console request
      |=  [paw=(list ,@t) muh=(unit moth)]
      ^-  (unit seam)
      ?:  ?=([@ ~] paw)
        ?^  muh  ~
        =+  fee=(flea i.paw)
        ?~  fee  ~
        [~ %con u.fee]
      ?.  ?=([@ @ ~] paw)  ~
      =+  [fee=(flea i.paw) fum=(flea i.t.paw)]
      ?.  &(?=(^ fee) ?=(^ fum))  ~
      ?:  |(?=(~ muh) ?=(~ r.u.muh))
        [~ %cog u.fee u.fum]
      ?.  =([~ 'text/json' ~] (~(get by q.u.muh) 'content-type'))
        ~
      =+  jun=`(unit json)`(rush q.u.r.u.muh apex:poja)
      ?~  jun
        ~
      [~ %cop u.fee u.fum u.jun]
    ::
    ++  flux                                            ::  credential caboose
      |=  [nep=@tas quy=quay]
      ^-  coin
      :*  %many
          [%$ %tas nep]
          [%blob ced]
          |-  ^-  (list coin)
          ?~  quy  ~
          [[%$ %t p.i.quy] [%$ %t q.i.quy] $(quy t.quy)]
      ==
    ::
    ++  foin                                            ::  version request
      |=  [fur=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      ?.  ?&  ?=(~ fur)
              ?=(~ quy)
              ?=([@ @ ~] paw)
              ((sane %tas) i.t.paw)
          ==  ~
      %+  bind
        ^-  (unit moat)
        =+  soy=(slay i.paw)
        ?~  soy  ~
        ?+    u.soy
          ~
        ::
            [%$ ?(%da %ud %tas) @]
          [~ (case p.u.soy) (case p.u.soy)]
        ::
            [%many [%$ ?(%da %ud %tas) @] [%$ ?(%da %ud %tas) @] ~]
          [~ (case i.p.u.soy) (case i.t.p.u.soy)]
        ==
      |=  mot=moat
      `seam`[%det i.t.paw mot]
    ::
    ++  funk                                            ::  functional request
      |=  [nep=@tas fur=(unit term) paw=(list ,@t) quy=quay]
      ^-  (unit seam)
      =+  won==(%n (rsh 3 2 nep))
      %+  bind
        ^-  (unit ,[logo tube])
        =+  ^=  zac  ^-  (unit ,[p=@ta q=path])
            ?:  won
              [~ (scot %da now) paw]
            ?~  paw  ~
            =+  zac=(slay i.paw)
            ?.  ?=([~ %$ ?(%ud %da %tas) *] zac)  ~
            [~ i.paw t.paw]
        ?:  ?|  ?=(~ zac)
                ?=(~ q.u.zac)
                !(levy t.q.u.zac (sane %ta))
            ==  ~
        :+  ~  ?~(fur %html u.fur)
        ^-  tube
        :*  (scot %p our)
            i.q.u.zac
            p.u.zac
            t.q.u.zac
        ==
      |=  [for=logo toe=tube]
      ^-  seam
      :^  %fun  for
        toe(s (weld s.toe `path`~[~(rent co (flux nep quy))]))
      ?.  won  ~
      :_  ~
      =-  =+  pey=(cat 3 (end 3 2 nep) %v)
          =+  ven=+((,@ (need (sky %cw p.toe q.toe r.toe ~))))
          =+  ^=  cal  :/
              "path='".
              "/{(trip (rsh 3 1 p.toe))}".
              "/{(trip pey)}".
              "/{(scow %ud ven)}".
              "/{(trip q.toe)}';"
          [-.sac [cal +.sac]]
      ^=  sac
      ;script
        ; 
        ; tries = 0;
        ; call = function() {
        ;   xhr = new XMLHttpRequest();
        ;   xhr.open('GET', path, true);
        ;   xhr.addEventListener('load', function() {
        ;     if(this.status !== 200) {
        ;       return keep();
        ;     }
        ;     document.location.reload();
        ;   });
        ;   xhr.addEventListener('error', keep);
        ;   xhr.addEventListener('abort', keep);
        ;   xhr.send();
        ; }
        ; keep = function() {
        ;   setTimeout(call,1000*tries);
        ;   tries++;
        ; }
        ; call();
      ==
    ::
    ++  holt                                            ::  login redirect
      |=  [whu=(unit ship) pul=purl]
      ^-  (unit seam)
      :+  ~
        %red
      ::  :+  [& q.p.pul r.p.pul]
      %+  earl  our
      :+  [p.p.pul q.p.pul r.p.pul]
        [~ /gul]
      :-  [%url (crip (urle (earn (earl our pul))))]
      ?~  whu  ~
      [%who (rsh 3 1 (scot %p u.whu))]~
    ::
    ++  holy                                            ::  structured request
      |=  [pul=purl moh=moth]
      ^-  (unit seam)
      ?~  q.q.pul  ~
      =*  nep  i.q.q.pul
      =*  paw  t.q.q.pul
      =+  [one=(end 3 1 nep) two=(cut 3 [1 1] nep) tri=(cut 3 [2 1] nep)]
      ?.  ?&  ?-  p.moh
                %conn  |                                ::  connect
                %delt  |                                ::  delete
                %get   =(%g one)                        ::  get
                %head  =(%g one)                        ::  head
                %opts  |                                ::  options
                %post  =(%p one)                        ::  post
                %put   =(%t one)                        ::  put
                %trac  |                                ::  trace
              ==
          ::
              ?+  two  |
                %e  &                                   ::  stranger
                %u  p.p.pul                             ::  guest
                %i  p.p.pul                             ::  neighbor
                %o  p.p.pul                             ::  identified
              ==
          ::
              ?=  $?  %p                                ::  application
                      %c                                ::  console
                      %f                                ::  functional
                      %v                                ::  version
                      %l                                ::  local login
                      %m                                ::  remote login
                      %n                                ::  now
                  ==
                  tri
          ::
              !&(=(%c tri) !=(%o two))
              =(3 (met 3 nep))
          ==
        ~
      ?:  &(=(%i two) =(~ aut.ced))
        (holt ~ pul)
      ?:  &(=(%o two) !(~(has ju aut.ced) %$ (scot %p our)))
        (holt [~ our] pul)
      ?+    one  ~
          %g
        ?+  tri  ~
          ?(%f %n)  (funk nep p.q.pul paw r.pul)
          %v        (foin p.q.pul paw r.pul)
          %c        (flub paw ~)
          %l        (fool r.pul)
          %p        fake
        ==
      ::
          %p
        ?+  tri  ~
          %l  (foom moh)
        ==
      ::
          %t
        ?+  tri  ~
          %c  (flub paw [~ moh])
        ==
      ==
    ::
    ++  idle                                            ::  cancel request
      |=  num=@ud
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup  +>.$
      =.  q.rey  (~(del by q.rey) num)
      ?.  ?=(%way pez.u.pup)  +>.$
      ?.  ?=(%det -.som.u.pup)  +>.$
      +>.$(..ya (hoot our num ses `riff`[p.som.u.pup ~]))
    ::
    ++  into                                            ::  introduce
      |=  [pul=purl moh=moth]
      ^+  +>
      =+  num=p.rey
      %=    +>.$
          lor    (~(put by lor) hen num [%& our ses])
          p.rey  +(num)
          q.rey
        %+  ~(put by q.rey)  num
        ^-  pimp
        :*  !?=(%head p.moh)
            hen
            *seam
            `pest`[%raw pul moh]
        ==
      ==
    ::
    ++  iota                                            ::  change response
      |=  [num=@ud rot=riot]
      ^+  +>
      =+  pup=(~(get by q.rey) num)
      ?~  pup
        ~&  [%iota-lost ses num rot]
        +>.$
      ?>  ?=(%way pez.u.pup)
      +>.$(q.rey (~(put by q.rey) num u.pup(pez [%haz rot])))
    ::
    ++  lace                                            ::  load and execute
      |=  [pax=path sam=vase]
      ^-  [gank _+>]
      =^  hum  +>.$  (lack pax)
      :_  +>.$
      =+  mud=(need hum)
      ?:  ?=(| -.mud)  mud
      =+  typ=(~(play ut [%cell p.p.mud p.sam]) [%cncl [~ 2] [~ 3]])
      =+  ton=(mong [q.p.mud q.sam] sky)
      ?-  -.ton
        %0  [%& typ p.ton]
        %1  [%| (turn p.ton |=(a=* (smyt (path a))))]
        %2  [%| p.ton]
      ==
    ::
    ++  lack                                            ::  probe/load
      |=  pax=path
      ^-  [(unit gank) _+>]
      =+  ans=(sky %cz pax)
      ?~  ans  [~ +>.$]
      =+  ank=((hard ankh) u.ans)
      ?~  q.ank  [~ +>.$]
      =+  huc=(~(get by cah.sef) p.u.q.ank)
      ?^  huc
        [[~ %& u.huc] +>.$]
      =+  mud=(much pax q.u.q.ank)
      :-  [~ mud]
      ?:  ?=(| -.mud)  +>.$
      +>.$(cah.sef (~(put by cah.sef) p.u.q.ank p.mud))
    ::
    ++  lass                                            ::  load secret
      ^-  @ta
      %^  rsh  3  1
      (scot %p (,@ (need (sky %a (scot %p our) %code (scot %da now) ~))))
    ::
    ++  lend                                            ::  load directory node
      |=  pax=path
      ^-  arch
      ((hard arch) (need (sky %cy pax)))
    ::
    ++  liar                                            ::  load file as vase
      |=  pax=path
      ^-  vase
      =+  fil=(lick pax)
      :_(fil ?^(fil [%cell %noun %noun] [%atom %$]))
    ::
    ++  lich                                            ::  simple directory
      |=  pax=path
      ^-  (list ,@tas)
      (turn (~(tap by r:(lend pax)) ~) |=([a=@tas b=~] a))
    ::
    ++  lick                                            ::  load file
      |=  pax=path
      (need (sky %cx pax))
    ::
    ++  lily                                            ::  translation targets
      |=  [pre=path for=@tas]
      ^-  (list ,@tas)
      (lich :(weld pre `path`/tan `path`/[for]))
    ::
    ++  lion                                            ::  translation graph
      |=  [too=@tas pre=path fro=(list ,@tas)]
      ^-  (unit (list ,@tas))
      =|  war=(set ,@tas)
      =<  -:(apex fro)
      |%
      ++  apex
        |=  rof=(list ,@tas)
        ^-  [(unit (list ,@tas)) _+>]
        ?~  rof
          [~ +>]
        =^  orf  +>  (apse i.rof)
        ?^(orf [orf +>.$] $(rof t.rof))
      ::
      ++  apse
        |=  for=@tas
        ^-  [(unit (list ,@tas)) _+>]
        ?:  =(for too)  [[~ [too ~]] +>]
        ?:  (~(has in war) for)  [~ +>]
        =.  war  (~(put in war) for)
        =^  orf  +>.$  (apex (lily pre for))
        :_  +>.$
        ?~(orf ~ [~ [for u.orf]])
      --
    ::
    ++  link                                            ::  translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [(unit gank) _+>]
      ?:  =(for too)  [[~ %& vax] +>.$]
      =+  wuy=(lion too pre [for ~])
      ?~  wuy  [~ +>.$]
      ?>  ?=(^ u.wuy)
      ?>  =(for i.u.wuy)
      |-  ^-  [(unit gank) _+>.^$]
      ?~  t.u.wuy  [[~ %& vax] +>.^$]
      =^  mud  +>.^$  (lite i.t.u.wuy pre for vax)
      ?:  ?=(| -.mud)  [[~ mud] +>.^$]
      $(t.u.wuy t.t.u.wuy, for i.t.u.wuy, vax p.mud)
    ::
    ++  lino                                            ::  translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [gank _+>]
      =^  gun  +>.$  (link too pre for vax)
      :_  +>.$
      ?^  gun  u.gun
      [%| [[%leaf "can't make {<too>} from {<for>}"] ~]]
    ::
    ++  lite                                            ::  step translate
      |=  [too=@tas pre=path for=@tas vax=vase]
      ^-  [gank _+>]
      (lace :(weld pre `path`/tan `path`/[for] `path`/[too] `path`/hoon) vax)
    ::
    ++  loan                                            ::  normalize vase
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  mof  +>.$  (lack :(weld pre `path`/nor `path`/[for] `path`/hoon))
      :_  +>.$
      ?~  mof  [%& vax]
      ?:  ?=(| -.u.mof)  u.mof
      =+  pud=(mule |.((~(play ut `type`p.p.u.mof) [%cnzy %$])))
      ?:  ?=(| -.pud)  pud
      ?:  (~(nest ut `type`p.pud) | p.vax)
        [%& vax]
      (mule |.((slam `vase`p.u.mof vax)))
    ::
    ++  lobo                                            ::  vase to love
      |=  [for=logo pre=path vax=vase]
      ^-  [(each love (list tank)) _+>]
      =^  mud  +>.$  (lino %mime pre for vax)
      :_  +>.$
      ?:  ?=(| -.mud)  mud
      [%& %mid (mite -.q.p.mud) (octs +.q.p.mud)]
    ::
    ++  loch                                            ::  validate vase
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  wav  +>.$  (lack :(weld pre `path`/val `path`/[for] `path`/hoon))
      :_  +>.$
      ?~  wav  [%& vax]
      ?:  ?=(| -.u.wav)  u.wav
      (mule |.((slam `vase`p.u.wav vax)))
    ::
    ++  loot                                            ::  load extension tree
      |=  [pax=path one=(unit logo)]
      ^-  (list path)
      =|  [tex=path all=(list path)]
      |-  ^-  (list path)
      ?^  one
        =+  don=`path`[u.one ~]
        =+  arc=(lend (weld pax don))
        ?~(q.arc ~ [[u.one tex] ~])
      =+  arc=(lend pax)
      =+  ryx=(~(tap by r.arc) ~)
      =-  ?~(q.arc orz [tex orz])
      ^=  orz
      |-  ^-  (list path)
      ?~  ryx  all
      %=  ^$
        one  [~ %hoon]
        pax  (weld pax `path`[p.i.ryx ~])
        tex  [p.i.ryx tex]
        all  $(ryx t.ryx)
      ==
    ::
    ++  lope                                            ::  normalize/validate
      |=  [for=@tas pre=path vax=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (loan for pre vax)
      ?:  ?=(| -.mud)  [mud +>.$]
      (loch for pre p.mud)
    ::
    ++  loth                                            ::  direct hard
      |=  [for=logo pre=path pax=path]
      ^-  [gank _+>]
      (lope for pre (liar pax))
    ::
    ++  loti                                            ::  translated soft
      |=  [too=logo for=logo pre=path pax=path sam=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (loto for pre pax sam)
      ?:  ?=(| -.mud)  [mud +>.$]
      (lino too pre for p.mud)
    ::
    ++  loto                                            ::  direct soft
      |=  [for=logo pre=path pax=path sam=vase]
      ^-  [gank _+>]
      =^  mud  +>.$  (lace pax sam)
      ?:  ?=(| -.mud)  [mud +>.$]
      (lope for pre p.mud)
    ::
    ++  lots                                            ::  translated hard
      |=  [too=logo for=logo pre=path pax=path]
      ^-  [gank _+>]
      =^  mud  +>.$  (lope for pre (liar pax))
      ?:  ?=(| -.mud)  [mud +>.$]
      (lino too pre for p.mud)
    ::
    ++  loud                                            ::  synthesis search
      |=  [syn=? for=logo pre=path mid=path]
      ^-  (list ,[p=path q=path r=loco])
      =|  suf=path
      |-  ^-  (list ,[p=path q=path r=loco])
      =+  pax=(weld pre (flop mid))
      =+  lot=(loot pax ?:(syn ~ [~ for]))
      =-  ?^  tol  tol
          ?~  mid  ~
          $(mid t.mid, suf [i.mid suf])
      ^=  tol
      |-  ^-  (list ,[p=path q=path r=loco])
      ?~  lot  ~
      =+  mor=$(lot t.lot)
      ?~  i.lot  mor
      =+  axp=(weld pax `path`(flop i.lot))
      ?:  &(syn ?=([%hoon @ ~] i.lot))
        :_(mor [mid suf | ?:(=(for i.t.i.lot) ~ [~ i.t.i.lot]) axp])
      ?:  ?=([@ ~] i.lot)
        :_(mor [mid suf & ?:(=(for i.i.lot) ~ [~ i.i.lot]) axp])
      mor
    ::
    ++  loup                                            ::  weak synthesis
      |=  [for=logo pre=path mid=path]
      ^-  [(unit gank) _+>]
      =+  syt=(weld pre `path`[%syn ~])
      =+  ^=  luc  ^-  (list ,[p=path q=path r=loco])
          =+  luc=(loud | for pre mid)
          ?.  ?=(~ luc)  luc
          (loud & for syt mid)
      ?:  =(~ luc)  [~ +>.$]
      =+  ^=  waz
          |-  ^-  $:  p=(list ,[p=path q=path r=path])
                      q=(list ,[p=path q=path r=path])
                      r=(list ,[p=path q=path r=[p=@tas q=path]])
                      s=(list ,[p=path q=path r=[p=@tas q=path]])
                  ==
          ?~  luc  [~ ~ ~ ~]
          =+  mor=$(luc t.luc)
          ?-  -.r.i.luc
            &  ?~  q.r.i.luc
                 [[[p.i.luc q.i.luc r.r.i.luc] p.mor] q.mor r.mor s.mor]
               :+  p.mor  q.mor
               [[[p.i.luc q.i.luc u.q.r.i.luc r.r.i.luc] r.mor] s.mor]
            |  ?~  q.r.i.luc
                 [p.mor [[p.i.luc q.i.luc r.r.i.luc] q.mor] r.mor s.mor]
               :+  p.mor  q.mor
               [r.mor [[p.i.luc q.i.luc u.q.r.i.luc r.r.i.luc] s.mor]]
          ==
      =^  mud  +>.$
        ?^  p.waz                                       ::  direct hard
          (loth for pre r.i.p.waz)
        ?^  q.waz                                       ::  direct soft
          %-  loto
          :*  for
              pre
              r.i.q.waz
              !>([for pre p.i.q.waz q.i.q.waz])
          ==
        ?^  r.waz                                       ::  translated hard
          (lots for p.r.i.r.waz pre q.r.i.r.waz)
        ?^  s.waz                                       ::  translated soft
          %-  loti
          :*  for
              p.r.i.s.waz
              pre
              q.r.i.s.waz
              !>([for pre p.i.s.waz q.i.s.waz])
          ==
        !!
      [[~ mud] +>.$]
    ::
    ++  lude                                            ::  functional synth
      |=  [for=logo toe=tube]
      ^-  [(unit (each love (list tank))) _+>]
      =+  [pre mid]=[`path`[p.toe q.toe r.toe ~] `path`(flop s.toe)]
      =^  gun  +>.$  (loup for pre mid)
      ?~  gun  [~ +>.$]
      ?:  ?=(| -.u.gun)  :_(+>.$ [~ %| p.u.gun])
      =^  mun  +>.$  (lobo for pre p.u.gun)
      [[~ mun] +>.$]
    ::
    ++  step                                            ::  step in work
      |-  ^+  +
      =^  zib  +.$
          =+  yub=q.rey
          |-  ^-  [(list ,[p=@ud q=pimp]) _+.^$]
          ?~  yub  [~ +.^$]
          =^  sin  +.^$  $(yub l.yub)
          =^  dex  +.^$  $(yub r.yub)
          =^  top  +.^$  (wink n.yub)
          =+  pot=`(list ,[p=@ud q=pimp])`?~(top ~ [[p.n.yub u.top] ~])
          [:(weld pot dex sin) +.^$]
      +.$(q.rey (~(gas by `_q.rey`~) zib))
    ::
    ++  wink                                            ::  advance request
      |=  [num=@ud pip=pimp]
      ^-  [(unit pimp) _+>]
      ?-    pez.pip
          %way  [[~ pip] +>.$]
          %new
        ?-    -.som.pip
            %app                                        ::  XX a mere stub
          :_(+>.$ [~ pip(pez [%fin %ham p.som.pip])])
        ::
            %con
          :_  +>.$
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          =+  ^=  obj
              %-  jobe
              :~  sent/(jone ino.cal)
                  recv/(jone ono.cal)
                  ownr/[%s (rsh 3 1 (scot %p our))]
              ==
          =+  sez=:/("seq={(pojo obj)}")
          =+  jqu="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
          =+  ^=  sac
              ;script
                ; 
                ; var hist = []
                ; var hind = 0
                ; $(
                ;   function() {
                ;     $input = $('#input .line')
                ;     $prompt = $('#input .prompt')
                ;     $output = $('#output')
                ;     $input.focus()
                ;
                ;     $('body').click(function() {
                ;       $input.focus()
                ;     })
                ;
                ;     send = function(com) {
                ;       if(com.line !== undefined &&
                ;         com.line.indexOf('error') != -1) {
                ;         com = {error:true}
                ;       }
                ;       console.log('sending')
                ;       console.log(com)
                ;       $.ajax('/'+seq.ownr+'/toc/0/'+seq.sent, {
                ;         type: 'PUT',
                ;         contentType: 'text/json',
                ;         data: JSON.stringify(com),
                ;         success: function(data,status,xhr) {
                ;           seq.sent++
                ;         },
                ;         error: function(data,status,xhr) {
                ;           if(data.responseJSON.lines !== undefined)
                ;             addLines(data.responseJSON.lines)
                ;           seq.sent++
                ;         }
                ;       })
                ;     }
                ;
                ;     recv = function() {
                ;       $.ajax('/'+seq.ownr+'/goc/0/'+seq.recv, {
                ;         type:'GET',
                ;         success: function(data,status,xhr) {
                ;           console.log(data);
                ;           seq.recv = data[0];
                ;           seq.send = data[1];
                ;           msg = data[2];
                ;           if(msg.text !== undefined)
                ;             addLines(msg.text)
                ;           if(msg.helo !== undefined)
                ;             changePrompt(msg.helo)
                ;           recv()
                ;         },
                ;         error: function(data,status,xhr) {
                ;           console.log('error')
                ;           seq.recv++
                ;         }
                ;       })
                ;     }
                ;
                ;     recv()
                ;
                ;     send({hail:true})
                ;
                ;     addLines = function(lines) {
                ;       $output.append(lines.join("<br />")+"<br />")
                ;     }
                ;
                ;     changePrompt = function(helo) {
                ;       $prompt.text(helo.prod[1]);
                ;     }
                ;
                ;     ctrl = false
                ;
                ;     $input.on('keydown', function(e) {
                ;       console.log('keydown')
                ;       console.log(e.keyCode)
                ;       if(e.keyCode == 17) {
                ;         ctrl = true
                ;         return;
                ;       }
                ;       if(e.keyCode == 88 && ctrl == true) {
                ;         console.log('ling')
                ;         send({ling:true})
                ;         return;
                ;       }
                ;
                ;       if(e.keyCode == 69 && ctrl == true) {
                ;         console.log('^e')
                ;         $input[0].selectionStart = $input[0].selectionEnd = $input.val().length
                ;         return
                ;       }
                ;       if(e.keyCode == 65 && ctrl == true) {
                ;         console.log('^a')
                ;         $input[0].selectionStart = $input[0].selectionEnd = 0
                ;         return
                ;       }
                ;
                ;
                ;       if(e.keyCode == 40) {
                ;         if(hist.length > 1) {
                ;           hind++
                ;           if(hind > hist.length-1) {
                ;             hind = hist.length-1
                ;           }
                ;         }
                ;       }
                ;       if(e.keyCode == 38) {
                ;         if(hist.length > 1) {
                ;           hind--
                ;           if(hind < 0) {
                ;             hind = 0
                ;           }
                ;         }
                ;       }
                ;       if(e.keyCode == 38 ||
                ;       e.keyCode == 40) {
                ;         console.log('set from hist')
                ;         $input.val(hist[hind])
                ;         return;
                ;       }
                ;
                ;       if(e.keyCode == 13) {
                ;         val = $input.val()
                ;         send({line:val})
                ;         $output.append($('.prompt').text()+" "+val+"<br>")
                ;         hind = hist.length-1
                ;         if(hind<0)
                ;           hind = 0
                ;         hist[hind] = val
                ;         hist.push('')
                ;         hind = hist.length-1
                ;         $input.val('')
                ;         $('body').scrollTop($('html').height() - $('.line').offset().top)
                ;         return;
                ;       }
                ;       if(hind == hist.length-1)
                ;         hist[hind] = $input.val()
                ;     });
                ;
                ;     $input.on('keyup', function(e) {
                ;       if(e.keyCode == 17) {
                ;         ctrl = false
                ;       }
                ;     });
                ;   }
                ; )
              ==
          =+  ^=  ham
              ;html
                ;head
                  ;title: urbit {<our>}/{<p.som.pip>}
                  ;script(type "text/javascript", src jqu);
                  ;style
                    ; body {
                    ;   margin: 60px 120px;
                    ;   font: normal 12px "Menlo" monospace;
                    ;   background-color: #000;
                    ;   color: #fff;
                    ; }
                    ;
                    ; #output {
                    ;   line-height: 18px;
                    ; }
                    ;
                    ; #input .prompt {
                    ;   display: inline-block;
                    ;   margin-right: 12px;
                    ; }
                    ;
                    ; #input .line {
                    ;   outline: none;
                    ;   width: 80%;
                    ;   border: 0;
                    ;   background-color: transparent;
                    ;   color: #fff;
                    ;   font: normal 12px "Menlo" monospace;
                    ; }
                  ==
                ==
                ;body
                  ;div@output;
                  ;div@input
                    ;div/prompt;
                    ;input/line(type "text");
                  ==
                  ;+  [-.sac `marl`[sez +.sac]]
                ==
              ==
          [~ pip(pez [%fin %ham ham])]
        ::
            %cog
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          ?.  (lth q.som.pip ono.cal)
            :-  [~ pip(pez %way)]
            %=  +>.$  cow
              %+  ~(put by cow)
                p.som.pip
              =+  val=(~(get by voy.cal) q.som.pip)
              cal(voy (~(put by voy.cal) q.som.pip ?~(val [num ~] [num u.val])))
            ==
          :_  +>.$
          =+  ^=  jon  ^-  json
              :~  %a
                (jone ono.cal)
                (jone ino.cal)
                (snag (sub ono.cal q.som.pip) out.cal)
              ==
          [~ pip(pez [%fin %mid /text/json (tact (pojo jon))])]
        ::
            %cop
          =+  cal==+(cal=(~(get by cow) p.som.pip) ?^(cal u.cal *clue))
          ?.  =(q.som.pip ino.cal)
            =.  cow  (~(put by cow) p.som.pip cal)
            :_  +>.$
            [~ pip(pez [%err 500 [%leaf "cop: {<q.som.pip>}, {<ino.cal>}."]~])]
          =+  ^=  fuv  ^-  (unit card)
              ?.  ?=(%o -.r.som.pip)  ~
              =+  lin=(~(get by p.r.som.pip) %line)
              ?^  lin  ?.(?=(%s -.u.lin) ~ [~ %line p.u.lin])
              =+  syc=(~(get by p.r.som.pip) %hail)
              ?^  syc  [~ %hail ~]
              =+  lig=(~(get by p.r.som.pip) %ling)
              ?^  lig  [~ %ling ~]
              ~
          :_  %_    +>.$
                  cow  (~(put by cow) p.som.pip cal(ino +(ino.cal)))
                  mow
                ?~  fuv  mow
                :_  mow
                :+  [~ %gold our]
                  [/b [%e %cons ses (scot %ud p.som.pip) ~] ~ ~]
                u.fuv
              ==
          [~ `pimp`pip(pez `pest`[%fin %raw 200 ~ ~])]
        ::
            %det
          :-  [~ pip(pez %way)]
          =+  rif=`riff`[p.som.pip ~ [%| q.som.pip]]
          +>.$(..ya (hoot our num ses rif))
        ::
            %fun
          =^  syt  +>.$  (lude p.som.pip q.som.pip)
          :_  +>.$
          :-  ~
          %=    pip
              pez
            ^-  pest
            ?~  syt
              [%err 404 [[%leaf "{<p.som.pip>} not found"] ~]]
            ?-  -.u.syt
              |  [%err 500 (flop p.u.syt)]
              &  [%fin (lofi r.som.pip p.u.syt)]
              ::  &  [%fin p.u.syt]
            ==
          ==
        ::
            %lon
          ?^  pus.p.som.pip
            ~&  [%pass lass u.pus.p.som.pip]
            ?.  =(lass u.pus.p.som.pip)
              $(pus.p.som.pip ~)
            =.  aut.ced  (~(put ju aut.ced) %$ (scot %p (need whu.p.som.pip)))
            ?~  foy.p.som.pip
              ~&  [%redirect pul.p.som.pip]
              $(som.pip [%red pul.p.som.pip])
            =.  ..ya   (haji u.foy.p.som.pip)
            %=    $
                som.pip
              :-  %red
              ^-  purl
              :+  ^-  hart
                  :+  &   ~
                  [%& ~[%org %urbit (rsh 3 1 (scot %p p.u.foy.p.som.pip))]]
                ^-  pork
                [~ /gul]
              ^-  quay
              :~  [%who (rsh 3 1 (scot %p (need whu.p.som.pip)))]
                  [%url (crip (earn pul.p.som.pip))]
                  [%wit %yes]
              ==
            ==
          :_  +>.$
          ?:  wit.p.som.pip
            [~ pip(pez [%who %$ (scot %p (need whu.p.som.pip))])]
          =+  rul=(earn pul.p.som.pip)
          =+  ruf=(earn (earl our pul.p.som.pip(q.q /pul, r ~)))
          =+  ham=(noon our whu.p.som.pip rul ruf)
          [~ pip(pez [%fin %ham ham])]
        ::
            %red
          :_  +>.$
          :-  ~
          %=    pip
              pez
            :-  %fin
            :-  %raw
            :+  301
              [%location (crip (earn p.som.pip))]~
            ~
          ==
        ==
      ::
          [%err *]
        [~ +>.$(..ya (muff(hen hen.pip) [%thou (loft `love`[%zap +.pez.pip])]))]
      ::
          [%fin *]
        =+  har=(loft p.pez.pip)
        =.  q.har  (weld (turn cug |=(a=@t ['set-cookie' a])) q.har)
        [~ +>.$(..ya (muff(hen hen.pip) [%thou har]))]
      ::
          [%haz *]
        :_  +>.$
        [~ pip(pez [%fin %wan 'Hello, world' ~])]
      ::
          [%raw *]
        :_  +>.$
        ^-  (unit pimp)
        :-  ~
        =+  hoy=(holy p.pez.pip)
        ?~  hoy
          pip(pez [%err 404 [[%leaf "invalid request"] ~]])
        pip(som u.hoy, pez %new)
      ::
          [%who *]
        :_  +>.$
        ?.((~(has ju aut.ced) p.pez.pip q.pez.pip) [~ pip] [~ pip(pez %new)])
      ==
    ::
    ++  work
      |-  ^+  +
      =+  sez=step
      ?:  =(rey.sez rey)  sez
      $(+ sez)
    --
  --
--
.  ==
=|  bolo
=*  bol  -
|=  [now=@da eny=@ sky=$+(* (unit))]                    ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  =>  .(fav ((hard card) fav))
  =.  ney  (shax :(mix (shax now) +(eny) ney))          ::  XX!!  shd not need
  ?:  ?=(%crud -.fav)
    [[[wru [/d hen] %flog fav] ~] ..^$]
  ^-  [p=(list move) q=vane]
  =.  gub  ?.(=(0 gub) gub (cat 3 (rsh 3 1 (scot %p (end 6 1 eny))) '-'))
  =^  mos  bol
    abet:apex:~(adit ye [[wru tea hen fav] [now eny sky] ~] bol)
  [mos ..^$]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`bolo`+>-.^$)) | p.old)
    ~&  %eyre-reset
    ..^$
  ..^$(+>- (bolo q.old))
::
++  raze
  ^-  vane
  ..$(+>- *bolo)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay
  `vase`!>((colt `bolo`+>-.$))
++  vern  [164 0]
--
