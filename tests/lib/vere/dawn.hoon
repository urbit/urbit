/+  *test, *vere
|%
::  example point for ~zod
::
++  pot
  ^-  point:azimuth-types
  :+  [0x0 0x0 0x0 0x0]
    :*  ~
        life=1
         pass=2.448.360.348.730.164.860.814.441.775.703.143.856.915.192.920.
        639.124.529.297.987.279.849.833.790.775.864.413.949.853.880.667.744.
        188.597.545.066.664.466.963.044.328.182.155.965.137.512.758.548.384.
        637.214.562
        continuity-number=0
        sponsor=[& ~zod]
        escape=~
    ==
  [~ u=[spawn-proxy=0x0 spawned=~]]
::  secret key for ~zod
::
++  sec
  ^-  ring
  0w8.Ugyke.eUOf2.NcHRo.tZA7r.P8vP6.DGKp4.yn-BI.etdzb.ucv75.WgRob.H1-7n.
  4qCje.gc7z7.1i-3T.9~8UR.IGkGH.6NWR2
::  Azimuth contract address
::
++  azimuth
  %-  crip
  %+  weld  "0x"
  (render-hex-bytes:ethereum 20 `@`azimuth:contracts:^azimuth)
::
++  test-give-czar
  =/  zar  czar:give:dawn
  ~!  zar
  %+  expect-eq
    !>  &
    !>  ?=(^ zar)
::
++  test-veri-good
  =/  fed  [~zod 1 sec ~]
  %+  expect-eq
    !>  &+[[%2 ~] ~zod 0 [1 sec]~]
    !>  (veri:dawn ~zod fed pot ~)
::
++  test-veri-not-spawned
  =/  fed  [~zod 1 sec ~]
  %+  expect-eq
    !>  |+[%not-keyed ~]
    !>  (veri:dawn ~zod fed =>(pot .(net ~)) ~)
::
++  test-veri-wrong-key
  =/  fed  [~zod 1 sec:ex:(pit:nu:crub:crypto 24 %foo) ~]
  %+  expect-eq
    !>  |+[%key-mismatch ~]
    !>  (veri:dawn ~zod fed pot ~)
::
++  test-veri-life-mismatch
  =/  fed  [~zod 2 sec ~]
  %+  expect-eq
    !>  |+[%life-mismatch ~]
    !>  (veri:dawn ~zod fed pot ~)
::
++  test-veri-bad-multikey
  =/  fed=feed:jael
    :-  [%1 ~]
    :-  ~zod
    :~  [1 sec:ex:(pit:nu:crub:crypto 24 %foo)]
        [2 sec]
    ==
  %+  expect-eq
    !>  |+[%key-mismatch %life-mismatch ~]
    !>  (veri:dawn ~zod fed pot ~)
::
++  test-veri-none-multikey
  %+  expect-eq
    !>  |+[%no-key ~]
    !>  (veri:dawn ~zod [[%1 ~] ~zod ~] pot ~)
::
++  test-veri-already-booted
  =/  fed  [~zod 1 sec ~]
  ;:  weld
    %+  expect-eq
      !>  |+[%already-booted ~]
      !>  (veri:dawn ~zod fed pot `[1 |])
  ::
    %+  expect-eq
      !>  |+[%already-booted ~]
      !>  (veri:dawn ~zod fed pot `[2 &])
  ==
::
++  test-veri-earl-good
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who  ~simtel-mithet-dozzod-dozzod
  =/  fed
    =/  sig
      %-  sign:as:(nol:nu:crub:crypto sec)
      (shaf %earl (sham who 1 pub:ex:cub))
    [[%2 ~] who 0 [1 sec:ex:cub]~]
  %+  expect-eq
    !>  &+fed
    !>  (veri:dawn who fed pot ~)
::
++  test-veri-earl-parent-not-keyed
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who  ~simtel-mithet-dozzod-dozzod
  =/  fed
    =/  sig
      %-  sign:as:(nol:nu:crub:crypto sec)
      (shaf %earl (sham who 1 pub:ex:cub))
    [[%2 ~] who 0 [1 sec:ex:cub]~]
  %+  expect-eq
    !>  &+fed
    !>  (veri:dawn who fed =>(pot .(net ~)) ~)
::
++  test-veri-pawn-good
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  fed  [who 1 sec:ex:cub ~]
  %+  expect-eq
    !>  &+[[%2 ~] who 0 [1 sec:ex:cub]~]
    !>  (veri:dawn who fed *point:azimuth-types ~)
::
++  test-veri-pawn-key-mismatch
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 1 sec:ex:(pit:nu:crub:crypto 24 %bar) ~]
  %+  expect-eq
    !>  |+[%key-mismatch ~]
    !>  (veri:dawn who sed *point:azimuth-types ~)
::
++  test-veri-pawn-invalid-life
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 2 sec:ex:cub ~]
  %+  expect-eq
    !>  |+[%invalid-life ~]
    !>  (veri:dawn who sed *point:azimuth-types ~)
::
++  test-veri-pawn-already-booted
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 1 sec:ex:cub ~]
  %+  expect-eq
    !>  |+[%already-booted ~]
    !>  (veri:dawn who sed *point:azimuth-types `[1 |])
--
