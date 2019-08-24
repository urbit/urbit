/+  *test
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
++  test-give-bloq
  =/  oct
    %-  as-octs:mimes:html
    '{"params":[],"id":"0","jsonrpc":"2.0","method":"eth_blockNumber"}'
  %+  expect-eq
    !>  oct
    !>  bloq:give:dawn
:: this produces a 1000+ line payload, so we just check that it doesn't crash
::
++  test-give-czar
  =/  zar  czar:give:dawn
  ~!  zar
  %+  expect-eq
    !>  &
    !>  ?=(^ zar)
::
++  test-give-point
  =/  oct
    %-  as-octs:mimes:html
    %+  rap  3
    :~  '{"params":[{"to":"'  azimuth  '","data":"'
        '0x63fa9a87'
        '0000000000000000000000000000000000000000000000000000000000000000'
        '"},"0x0"],"id":"0","jsonrpc":"2.0","method":"eth_call"}'
    ==
  %+  expect-eq
    !>  oct
    !>  (point:give:dawn 0 ~zod)
::
++  test-give-turf
  =/  oct
    %-  as-octs:mimes:html
    %+  rap  3
    :~  '[{"params":[{"to":"'  azimuth  '","data":"'
        '0xeccc8ff1'
        '0000000000000000000000000000000000000000000000000000000000000000'
        '"},"0x0"],"id":"turf-0","jsonrpc":"2.0","method":"eth_call"},'
        '{"params":[{"to":"'  azimuth  '","data":"'
        '0xeccc8ff1'
        '0000000000000000000000000000000000000000000000000000000000000001'
        '"},"0x0"],"id":"turf-1","jsonrpc":"2.0","method":"eth_call"},'
        '{"params":[{"to":"'  azimuth  '","data":"'
        '0xeccc8ff1'
        '0000000000000000000000000000000000000000000000000000000000000002'
        '"},"0x0"],"id":"turf-2","jsonrpc":"2.0","method":"eth_call"}]'
    ==
  %+  expect-eq
    !>  oct
    !>  (turf:give:dawn 0)
::
++  test-take-bloq
  =/  oct
    %-  as-octs:mimes:html
    '{"id":"0","jsonrpc":"2.0","result":"0x20"}'
  =/  boq  32
  %+  expect-eq
    !>  [~ boq]
    !>  (bloq:take:dawn oct)
::
++  test-take-czar
  =/  oct
    %-  as-octs:mimes:html
    %+  rap  3
    :~  '[{"id":"gal-0","jsonrpc":"2.0","result":"'
        '0xb69b6818b17b7cc22f8e0a2291f58e4aa840cbf44cb2f1c94dc3d71e3cda0d94'
          '3defb87516f42ce4327820b588002aa53e52527af8d23bee4aa215fa296bdf5f'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000002'
        '"},{"id":"gal-1","jsonrpc":"2.0","result":"'
        '0xb727e38d031162e50913b2e37a2e29d4ba457eff4f7fd4ac47dc68fcb54260d3'
          'b8bfe4789483c171f7fa359438cdcc8d268d40fe08d6c1d8b36267748d2139f8'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000003'
        '"},{"id":"gal-2","jsonrpc":"2.0","result":"'
        '0x0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000004'
        '"}]'
    ==
  =/  kyz
    ^-  [zod=pass nec=pass]
    :-  2.448.360.348.730.164.860.814.441.775.703.143.856.915.192.920.639.124.
      529.297.987.279.849.833.790.775.864.413.949.853.880.667.744.188.597.545.
      066.664.466.963.044.328.182.155.965.137.512.758.548.384.637.214.562
    ::
      2.455.718.000.840.284.920.492.237.722.671.938.413.341.955.411.945.312.
    638.361.167.187.097.711.481.986.932.531.569.955.478.938.087.263.286.158.
    823.313.739.767.009.446.819.531.923.255.637.798.148.055.143.938.146
  %+  expect-eq
    !>  :-  ~
        %-  ~(gas by *(map ship [=rift =life =pass]))
        [[~zod 2 1 zod.kyz] [~nec 3 1 nec.kyz] [~bud 4 1 'b'] ~]
    !>  (czar:take:dawn oct)
::
++  test-take-point
  =/  oct
    %-  as-octs:mimes:html
    %+  rap  3
    :~  '{"jsonrpc":"2.0","result":"'
        '0xb69b6818b17b7cc22f8e0a2291f58e4aa840cbf44cb2f1c94dc3d71e3cda0d94'
          '3defb87516f42ce4327820b588002aa53e52527af8d23bee4aa215fa296bdf5f'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000000'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000001'
          '0000000000000000000000000000000000000000000000000000000000000000'
        '"}'
    ==
  %+  expect-eq
    !>  [~ pot]
    !>  (point:take:dawn ~zod oct)
::
++  test-take-turf
  =/  oct
    %-  as-octs:mimes:html
    %+  rap  3
    :~  '[{"id":"turf-0","jsonrpc":"2.0","result":"'
        '0x0000000000000000000000000000000000000000000000000000000000000020'
          '0000000000000000000000000000000000000000000000000000000000000009'
          '75726269742e6f72670000000000000000000000000000000000000000000000'
        '"},{"id":"turf-1","jsonrpc":"2.0","result":"'
        '0x0000000000000000000000000000000000000000000000000000000000000020'
          '0000000000000000000000000000000000000000000000000000000000000009'
          '75726269742e6f72670000000000000000000000000000000000000000000000'
        '"},{"id":"turf-2","jsonrpc":"2.0","result":"'
        '0x0000000000000000000000000000000000000000000000000000000000000020'
          '0000000000000000000000000000000000000000000000000000000000000009'
          '75726269742e6f72670000000000000000000000000000000000000000000000'
        '"}]'
    ==
  %+  expect-eq
    !>  [~ [/org/urbit ~]]
    !>  (turf:take:dawn oct)
::
++  test-veri-good
  =/  sed  [~zod 1 sec ~]
  %+  expect-eq
    !>  ~
    !>  (veri:dawn sed pot ~)
::
++  test-veri-not-spawned
  =/  sed  [~zod 1 sec ~]
  %+  expect-eq
    !>  `%not-keyed
    !>  (veri:dawn sed =>(pot .(net ~)) ~)
::
++  test-veri-wrong-key
  =/  sed  [~zod 1 sec:ex:(pit:nu:crub:crypto 24 %foo) ~]
  %+  expect-eq
    !>  `%key-mismatch
    !>  (veri:dawn sed pot ~)
::
++  test-veri-life-mismatch
  =/  sed  [~zod 2 sec ~]
  %+  expect-eq
    !>  `%life-mismatch
    !>  (veri:dawn sed pot ~)
::
++  test-veri-already-booted
  =/  sed  [~zod 1 sec ~]
  ;:  weld
    %+  expect-eq
      !>  `%already-booted
      !>  (veri:dawn sed pot `[1 |])
  ::
    %+  expect-eq
      !>  `%already-booted
      !>  (veri:dawn sed pot `[2 &])
  ==
::
++  test-veri-earl-good
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who  ~simtel-mithet-dozzod-dozzod
  =/  sed
    =/  sig
      %-  sign:as:(nol:nu:crub:crypto sec)
      (shaf %earl (sham who 1 pub:ex:cub))
    [who 1 sec:ex:cub `sig]
  %+  expect-eq
    !>  ~
    !>  (veri:dawn sed pot ~)
::
++  test-veri-earl-parent-not-keyed
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who  ~simtel-mithet-dozzod-dozzod
  =/  sed
    =/  sig
      %-  sign:as:(nol:nu:crub:crypto sec)
      (shaf %earl (sham who 1 pub:ex:cub))
    [who 1 sec:ex:cub `sig]
  %+  expect-eq
    !>  ~
    !>  (veri:dawn sed =>(pot .(net ~)) ~)
::
++  test-veri-pawn-good
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 1 sec:ex:cub ~]
  %+  expect-eq
    !>  ~
    !>  (veri:dawn sed *point:azimuth-types ~)
::
++  test-veri-pawn-key-mismatch
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 1 sec:ex:(pit:nu:crub:crypto 24 %bar) ~]
  %+  expect-eq
    !>  `%key-mismatch
    !>  (veri:dawn sed *point:azimuth-types ~)
::
++  test-veri-pawn-invalid-life
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 2 sec:ex:cub ~]
  %+  expect-eq
    !>  `%invalid-life
    !>  (veri:dawn sed *point:azimuth-types ~)
::
++  test-veri-pawn-already-booted
  =/  cub  (pit:nu:crub:crypto 24 %foo)
  =/  who=ship  `@`fig:ex:cub
  =/  sed  [who 1 sec:ex:cub ~]
  %+  expect-eq
    !>  `%already-booted
    !>  (veri:dawn sed *point:azimuth-types `[1 |])
--
