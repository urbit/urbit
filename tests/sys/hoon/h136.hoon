::  h136: test type-of-type migration
::
/+  *test
::
|%
++  test-cell-vase
  =/  old  ;;  vase:h136  %-  cue
    ::  ^-  @uw  %-  jam  !>  =>  ~  [123 456]
    0wV1w.ZY72z.IzHU7.qTKz3.Y1tzo.OIvM5
  %+  expect-eq
    !>  !.
    !>  =>  ~  [123 456]
  !>((next-vase:h136 old))
::
++  test-core-vase
  =/  old  ;;  vase:h136  %-  cue
    ::  ^-  @uw  %-  jam  !>  =>  ~  |%  ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
    0w2x.1xKPz.JD7xU.te3E3.3sf1m.In1yV.gRIf1.PJPiX.~0nAW.dz7Y1.NY5D5.MWs78.TcbDY.1Ns5C.n3MND.xxP39.1Or8c.A5sn3.5JzoW.ITM5s.adOdz.uP~0Y.Kj8Y1.BFTs7.qTKz3.Y1sHA.TIvM5
  %+  expect-eq
    !>  !.
    !>  =>  ~  |%  ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
  !>((next-vase:h136 old))
::
++  test-core-docs-vase
  =/  old  ;;  vase:h136  %-  cue
    ::  ^-  @uw  %-  jam  !>  =>  ~  |%
    ::  ::  +a: blah
    ::  ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
    0wa466X.eeSsu.7xQUe.wcdJ3.2ScnM.6MYdr.sWITM.5u3oO.J7M5O.KzuTv.0sf1m.In1yV.gRIf1.PJPiX.~0nAW.dz7Y1.NY5D5.MWs78.TcbDY.1Ns5C.n3MND.xxP39.1Or8c.A5sn3.5JzoW.ITM5s.adOdz.uP~0Y.Kj8Y1.BFTs7.qTKz3.Y1sHA.TIvM5
  %+  expect-eq
    !>  !.
    !>  =>  ~  |%
    ::  +a: blah
    ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
  !>((next-vase:h136 old))
::
++  test-gate-vase
  =/  old  ;;  vase:h136  %-  cue
    ::  ^-  @uw  %-  jam  !>  =>  ~  |=  a=@  ~_  leaf+"decrement-underflow"  ?<  =(0 a)  =+  b=0  |-  ^-  @  ?:  =(a +(b))  b  $(b +(b))
    0w6c8.uocdN.x3Nx3.E9V3x.GAdso.gTCWd.P7Y1x.jAdIn.1PEUr.Pr7hA.-0VOs.dLYUq.X6dQt.~wde2.6Vndx.oLwbM.q7hH-.0Y6xO.oLwea.D3Ns7.CWezn.Y1uro.VKDM6.MY7eT.dbLY1.IzHU7.8TcbD.Y1urE.WcDM5.ScXEX.~0oYk.fEQJP.nY1Yp.xYS5Q.u0WSd.LsLwa.UCdDs.~wcIO.XqEsc.G74JM.Zxq8c.xAw-m.ErVqx.P3Vxb.sbIeb.11Ydg.S23z2.6X4Sb.sbIcc.G73ax.O6i3t.-3LUe.PwVK3.OUenw.Ve3KU.fnwSQ.7FMtT.1OY7r.MsL1V.s77Ms.L1Os7.75wwV.bYepx.pmPU6.byXC5.Brvwb.p1Jzo.WITM5.sadOd.zuP~0.YKj8Y.1BNx3.Ts6s4.dFSJX.EM~0s.f1OIr.2Pv0n.oScH7.Y1sHA.TIvM5
  %+  expect-eq
    !>  !.
    !>  =>  ~  |=  a=@  ~_  leaf+"decrement-underflow"  ?<  =(0 a)  =+  b=0  |-  ^-  @  ?:  =(a +(b))  b  $(b +(b))
  !>((next-vase:h136 old))
::
++  test-core-context-vase
  =/  old  ;;  vase:h136  %-  cue
    ::  ^-  @uw  %-  jam  !>  =>  ~
    ::  =>  |%  ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
    ::  |%  ++  x  +(a)  ++  y  [a x]  --
    0w5.5j3t4.kdYYt.VUWYs.6eR3m.el3od.kdYY5.oVkfC.Sez9Y.1Ys5C.mEC7G.2xKyr.8cA6o.nD4ds.adwh3.vf7uu.f3MWs.7mC6U.u2JoK.37qxH.ou3Dr.CBT-0.L9Qr6.fU3zU.bebxQ.UehKo.nfU3y.UbcK7.xzij3.C6i3A.Sgp8a.UK6br.6NRpL.wa-Qr.Ar6ZD.-1VsC.hU3bj.KUeRL.t67U2.PdsHA.TIvM5
  %+  expect-eq
    !>  !.
    !>  =>  ~
    =>  |%  ++  a  'a'  ++  b  'b'  ++  c  [a b]  --
    |%  ++  x  +(a)  ++  y  [a x]  --
  !>((next-vase:h136 old))
--
