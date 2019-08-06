/+  *test
::
/=  ames-raw  /:  /===/sys/vane/ames
              /!noun/
=/  type-spear  -:!>(ames-raw)
::
=/  test-pit=vase  !>(.)
=/  ames-gate  (ames-raw test-pit)
::
|%
::  tests that %ames asks for private keys on %init
::
++  test-init
  =^  results1  ames-gate
    =/  =duct  [/ /term/1 / ~]
    =/  =wire  /our/~nul
    %:  ames-call
      ames-gate
      now=~1234.5.6
      call-args=[duct type=*type %soft %init ~nul]
      expected-moves=[[duct %pass wire %k %private-keys ~] [duct %pass / %k %turf ~] ~]
    ==
  ::
  results1
::
::  tests that %ames sends a message to itself
::
++  test-send
  =/  now  ~1234.5.6
  =/  =duct  [/ /term/1 / ~]
  =/  =wire  /our/~nul
  =/  pact1
    0wHfb.1hdCh.0oxed.Ta7-f.4IIDV.4ku6J.PoJe7.AiyMS.w~mfu.V04ja.iXj8d.E3nq7.
    gcW-a.0II6T.vb5zH.FHEkp.J7wgT.XTnuu.KaUiu.xZ6dg.qgWSH.3ovaO.dETNQ.5YAOR.
    Lw8Mj.iQCrM.-TcjY.gFysP.XCfdx.52ack.MN~yA.0CNFU.0eL1M.Un-ey.CZyf9.Omk2p.
    -Wbar.-w2bs.02sNg.340cg.okHUP
  =/  pact2
        0w78EWp.7898D.odZ3b.7iLvr.vyjzn.XBNaN.vxTZj.b4BFp.EHHvW.IjvpB.j0~87.
    t06D0.SbrGK.QlIeE.1Xj1v.CX~YY.c9cAE.eUPSb.gj8-M.e15TJ.EPPXN.efms-.8y9og.
    IdyLr.lkZJ5.KMB-F.S7mwd.t5rmo.CEYCp.3zC4n.HYh2T.RgVI8.0eT1z.Jxj9c.m1Sm5.
    SaYrP.0LKO3.-w2cA.02sNg.340cg.oi9Nj
  =/  vein-data
    [life=1 (my [1 sec:ex:(pit:nu:crub:crypto 512 ~nul)] ~)]
  ::
  =^  results1  ames-gate
    %:  ames-call
      ames-gate
      now
      call-args=[duct type=*type %soft [%barn ~]]
      expected-moves=[[duct %give %turf ~] ~]
    ==
  ::
  =.  now  (add ~m1 now)
  :: ~&  [%fox1 now fox.ames-gate]
  =^  results2  ames-gate
    %:  ames-take
      ames-gate
      now
      take-args=[wire duct -:!>([%k %private-keys vein-data]) [%k %private-keys vein-data]]
      expected-moves=~
    ==
  ::
  =.  now  (add ~m1 now)
  :: ~&  [%fox2 now fox.ames-gate]
  =^  results3  ames-gate
    %:  ames-call
      ames-gate
      now
      call-args=[duct type=*type %soft [%want ~nul /foo 1]]
      :~  [duct %pass /pubs/~nul %k %public-keys (silt ~nul ~)]
          [duct %give %send *lane:ames pact1]
          ::  XX why ~s4 ??
          ::
          [duct %pass /ames %b %wait (add ~s4 now)]
      ==
    ==
  ::
  =.  now  (add ~m1 now)
  :: ~&  [%fox3 now fox.ames-gate]
  =^  results4  ames-gate
    %:  ames-call
      ames-gate
      now
      call-args=[duct type=*type %soft [%want ~nul /foo 2]]
      expected-moves=[[duct %give %send *lane:ames pact2] ~]
    ==
  ::
  =.  now  (add ~m1 now)
  :: ~&  [%fox4 now fox.ames-gate]
  =^  results5  ames-gate
    %:  ames-take
      ames-gate
      now
      take-args=[wire duct -:!>([%b %wake ~]) [%b %wake ~]]
      :~  [duct %give %send *lane:ames pact1]
          [duct %give %send *lane:ames pact2]
          [duct %pass /ames %b %wait (add ~s8 now)]
      ==
    ==
  ::
  :: ~&  [%fox5 now fox.ames-gate]
  :(weld results1 results2 results3 results4 results5)
::
++  ames-scry
  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ?:  =(%turf q.beam)
    (some (some %noun !>(~)))
  ::
  ?:  ?&  =(%life q.beam)
          =(/~nul s.beam)
      ==
    (some (some %atom !>(1)))
  ::
  ?:  ?&  =(%saxo q.beam)
          =(/~nul s.beam)
      ==
    (some (some %noun !>([~nul ~])))
  ::
  ?:  ?&  =(%sein q.beam)
          =(/~nul s.beam)
      ==
    (some (some %atom !>(~nul)))
  ::
  ?:  ?&  =(%deed q.beam)
          =(/1/~nul s.beam)
      ==
    =/  =deed:ames
      [life=1 pub:ex:(pit:nu:crub:crypto 512 ~nul) ~]
    (some (some %noun !>(deed)))
  ::
  ~&  [%ames-scry-fail +<]
  ~
::
++  ames-call
  |=  $:  ames-gate=_ames-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:ames-gate))]
          expected-moves=(list move:ames-gate)
      ==
  ^-  [tang _ames-gate]
  ::
  =/  ames  (ames-gate our=~nul now=now eny=`@`0xdead.beef ames-scry)
  ::
  =^  moves  ames-gate
    (call:ames call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output ames-gate]
::
++  ames-take
  |=  $:  ames-gate=_ames-gate
          now=@da
          take-args=[=wire =duct wrapped-sign=(hypo sign:ames-gate)]
          expected-moves=(list move:ames-gate)
      ==
  ^-  [tang _ames-gate]
  ::
  =/  ames  (ames-gate our=~nul now=now eny=`@`0xdead.beef ames-scry)
  ::
  =^  moves  ames-gate
    (take:ames take-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output ames-gate]
--
