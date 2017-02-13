|%
++  results  (map mark (each vase tang))
++  show-results
  =,  format
  |=  a/results  ^-  json
  :-  %o
  %-  ~(run by a)
  |=  b/(each vase tang)
  ?-  -.b
    $&  (tape:enjs (text p.b))
    $|  (tape:enjs (of-wall (wush 160 (flop p.b))))
  ==
++  wush
  |=  {wid/@u tan/tang}  ^-  wall
  (zing (turn tan |=(a/tank (wash 0^wid a))))
--
::
=,  gall
=,  ford
|_  {bowl $~}
++  peek  _~
++  peer-scry-x
  |=  path
  [[ost %exec /all-marks our `build-marks]~ +>]
::
++  made-all-marks
  |=  {path @uvH a/gage}
  :_  +>.$
  ?>  ?=($tabl -.a)
  =;  res/results
    [ost %diff [%json (show-results res)]]~
  %-  malt
  %+  turn  p.a
  |=  {k/gage v/gage}  ^-  {mark (each vase tang)}
  :-  ?>(?=({$& $mark * @tas} k) q.q.p.k)
  ?-  -.v
    $tabl  !!
    $&  [%& q.p.v]
    $|  v
  ==
::
++  build-marks
  ^-  {beak silk}
  :-  now-beak
  :-  %tabl
  %+  turn  (weld list-marks list-sub-marks)
  |=  {a/mark $~}  ^-  {silk silk}
  :-  [%$ %mark !>(a)]
  [%bunt a]
::
++  poke-noun
  |=  *
  ~&  have+list-marks
  `+>
::
++  now-beak  %_(byk r [%da now])
++  list-marks
  =,  space:userlib
  =,  format
  =+  .^(arch %cy (en-beam now-beak /mar))
  %+  skim  ~(tap by dir)
  |=  {a/mark $~}
  ?=(^ (file (en-beam now-beak /hoon/[a]/mar)))
::
++  list-sub-marks
  =,  space:userlib
  =,  format
  ^-  (list {mark $~})
  %-  zing  ^-  (list (list {mark $~}))
  =/  top  .^(arch %cy (en-beam now-beak /mar))
  %+  turn  ~(tap by dir.top)
  |=  {sub/knot $~}
  =+  .^(arch %cy (en-beam now-beak /[sub]/mar))
  %+  murn  ~(tap by dir)
  |=  {a/mark $~}  ^-  (unit {mark $~})
  ?~  (file (en-beam now-beak /hoon/[a]/[sub]/mar))  ~
  `[(rap 3 sub '-' a ~) ~]
--
