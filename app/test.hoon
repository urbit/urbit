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
=,  format
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
++  made-a-core
  |=  {a/spur @uvH b/gage}
  :_  +>.$
  ?>  ?=([%tabl ^ ~] b)
  =/  cur  p.i.p.b
  %-  %-  slog
      ?+  -.cur  !!
        %|  (flop p.cur)
        %&  ~ ::[(sell q.p.cur)]~
      ==
  =/  nex/(list spur)
    =<(p ;;(,[%& %cont * p=(list spur)] q.i.p.b))
  ?~  nex  ~
  [ost (build-core nex)]~
::
++  build-core
  |=  [a=spur b=(list spur)]
  ~&  >>  (flop a)
  :^  %exec  a-core+a  our
  %-  some
  ^-  bilk
  :-  now-beak
  :~  %tabl
    [[%core now-beak a] [%$ %cont !>(b)]]
  ==
::   =;  res/results
::     [ost %diff [%json (show-results res)]]~
::   %-  malt
::   %+  turn  p.a
::   |=  {k/gage v/gage}  ^-  {mark (each vase tang)}
::   :-  ?>(?=({$& $mark * @tas} k) q.q.p.k)
::   ?-  -.v
::     $tabl  !!
::     $&  [%& q.p.v]
::     $|  v
::  ==
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
  |=  a/path
  :_  +>
  ?:  [dry=|]
    ~&((list-cores a) ~)
  [ost (build-core [- +]:(list-cores a))]~
::
++  list-cores
  |=  a/path  ^-  (list spur)
  =/  sup  (flop a)
  |-  ^-  (list spur)
  %-  zing
  %+  turn
    =-  (sort ~(tap by -) aor)
    dir:.^(arch %cy (en-beam now-beak sup))
  |=  [a=knot ~]  ^-  (list spur)
  =.  sup  [a sup]
  ?~  [fil:.^(arch %cy (en-beam now-beak [%hoon sup]))]
    ^$
  ~&  (flop sup)
  [sup ^$]
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
