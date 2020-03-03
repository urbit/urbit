/+  *test, pinto
::
=/  scry=sley
  |=  [* (unit (set monk)) tem=term bem=beam]
  ^-  (unit (unit cage))
  =-  (~(get by -) tem bem)
  %-  ~(gas by *(map [term beam] (unit cage)))
  :~  :-  cx+[[~nul %home da+~1234.5.6] /hoon/foo/lib]
      `hoon+!>('%bar')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/baz/lib]
      `hoon+!>('!:  [12 13]  !:  -')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/hoon/sys]
      `hoon+!>('=<  |=(* ~)  |%  ++  one  1  --')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/arvo/sys]
      `hoon+!>('|%  ++  is  one  --')
  ::
      :-  cx+[[~nul %home da+~1234.5.6] /hoon/zuse/sys]
      `hoon+!>('|%  ++  aint  is  --')
  ==
=/  ford  ((pinto) ~nul %home ~1234.5.6 scry)
=/  ca  (by-clock:contain hoon-cache-key:ford vase)
=/  hoon-cache  (clock hoon-cache-key:ford vase)  ::  TODO: broken import?
=|  =hoon-cache
|%
++  test-make-ride  ^-  tang
  ::
  =/  m  (fume:ford ,cage)
  =/  out=output:m
    ((make:ford %ride $+noun+!>([%foo 17]) (ream '-')) ~ hoon-cache)
  ::
  ;:  welp
    %+  expect-eq
      !>  %foo
      ?>(?=(%done -.next.out) q.value.next.out)
  ::
    %+  expect-eq
      !>  `(list @tas)`(turn ~(tap in ~(key by lookup.s.out)) head)
      !>  `(list @tas)`~[%ride %slim]
  ==
++  test-run-root-build-load-synchronous  ^-  tang
  =/  [=product:ford =build-state:ford =^hoon-cache]
    %:  run-root-build:ford
      ^-  build:ford
      :*  live=%.n
          desk=%home
          case=da+~1234.5.6
          plan=[%load %x /lib/foo/hoon]
      ==
    ::
      ^-  build-state:ford
      [fum=~ cur=~ pre=~]
    ::
      ^-  (unit (unit cage))
      ~
    ==
  ::
  ;:  welp
    %+  expect-eq
      !>  %hoon
      ?>(?=([~ %& *] product) !>(p.p.u.product))
  ::
    %+  expect-eq
      !>  '%bar'
      ?>(?=([~ %& *] product) q.p.u.product)
  ::
    %+  expect-eq
      !>  `build-state:ford`build-state(cur ~)
      !>  `build-state:ford`[fum=~ cur=~ pre=~]
  ::
    %+  expect-eq
      !>  `(list spar:ford)`~(tap in ~(key by cur.build-state))
      !>  `(list spar:ford)`[%x /lib/foo/hoon]~
  ==
++  test-make-call  ^-  tang
  =/  m  (fume:ford ,cage)
  =/  out=output:m
    ((make:ford %call $+noun+!>(dec) $+noun+!>(17)) ~ hoon-cache)
  ?>  ?=(%done -.next.out)
  ::
  ;:  welp
    %+  expect-eq
      !>  %noun
      !>  p.value.next.out
  ::
    %+  expect-eq
      !>  16
      q.value.next.out
  ==
++  test-run-root-build-grok  ^-  tang
  =/  [=product:ford =build-state:ford =^hoon-cache]
    %:  run-root-build:ford
      ^-  build:ford
      :*  live=%.n
          desk=%home
          case=da+~1234.5.6
          plan=[%grok /lib/baz/hoon]
      ==
    ::
      ^-  build-state:ford
      [fum=~ cur=~ pre=~]
    ::
      ^-  (unit (unit cage))
      ~
    ==
  ?>  ?=([~ %& *] product)
  ::
  ;:  welp
    %+  expect-eq
      !>  %noun
      !>  p.p.u.product
  ::
    %+  expect-eq
      !>  12
      =+  !<(=pile:ford q.p.u.product)
      =/  hoons  (turn pile |=(pike:ford ?>(?=(%'/~' +<-) +<+)))
      =|  sut=vase
      |-  ^+  sut
      ?~  hoons  sut
      =.  sut  (slap sut i.hoons)
      $(hoons t.hoons)
  ::
    %+  expect-eq
      !>  `build-state:ford`build-state(cur ~)
      !>  `build-state:ford`[fum=~ cur=~ pre=~]
  ::
    %+  expect-eq
      !>  `(list spar:ford)`~(tap in ~(key by cur.build-state))
      !>  `(list spar:ford)`[%x /lib/baz/hoon]~
  ==
++  test-run-root-build-file  ^-  tang
  =/  [=product:ford =build-state:ford =^hoon-cache]
    %:  run-root-build:ford
      ^-  build:ford
      :*  live=%.n
          desk=%home
          case=da+~1234.5.6
          plan=[%file /lib/baz/hoon]
      ==
    ::
      ^-  build-state:ford
      [fum=~ cur=~ pre=~]
    ::
      ^-  (unit (unit cage))
      ~
    ==
  ~|  product
  ?>  ?=([~ %& *] product)
  ;:  welp
    %+  expect-eq
      !>  %noun
      !>  p.p.u.product
  ::
    %+  expect-eq
      !>  12
      q.p.u.product
  ::
    %+  expect-eq
      !>  `build-state:ford`build-state(cur ~)
      !>  `build-state:ford`[fum=~ cur=~ pre=~]
  ::
    %+  expect-eq
      !>  %-  ~(gas in *(set spar:ford))
          :~  [%x /sys/arvo/hoon]  [%x /lib/baz/hoon]
              [%x /sys/hoon/hoon]  [%x /sys/zuse/hoon]
          ==
      !>  ~(key by cur.build-state)
  ==
--
