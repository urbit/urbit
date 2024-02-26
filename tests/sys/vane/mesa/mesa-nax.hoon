::  test %peek for %naxplanation
::
/+  *test, v=test-mesa-gall
=+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
|%
++  dbug  `?`&
++  make-roof
  |=  [pax=path val=cage]
  ^-  roof
  |=  [lyc=gang pov=path vis=view bem=beam]
  ^-  (unit (unit cage))
  ?.  &(=(s.bem pax) |(=(vis %x) =(vis [%$ %x]) =(vis [%g %x])))  [~ ~]
  ``val
::
++  nec-scry-roof
  scry:(ames.nec now=~1111.1.1 eny=`@`0xdead.beef *roof)
::
++  bud-scry-roof
  scry:(ames.bud now=~1111.1.1 eny=`@`0xdead.beef *roof)
::
++  test-watch
  %-  run-chain
  |.  :-  %|
  ::
  =/  poke-plea  [%x /ge/pok [%0 %m noun/0]]
  =/  poke-path  /flow/0/~nec/poke/~bud/for/1
  =/  ack-path   /flow/0/~bud/ack/~nec/bak/1
  =/  nax-path   /flow/0/~bud/nax/~nec/bak/1
  ::
  =/  ack-wire   /flow/int/for/~bud/0/0/1
  =/  vane-wire  /flow/out/bak/~nec/0/0/1
  =/  nax-wire   /flow/ext/for/~bud/0/0/1
  ::
  =/  make-poke=[%make-poke spar:ames path]
    [%make-poke [~bud ack-path] poke-path]
  =/  bex-roof  (make-roof poke-path message+!>(plea/poke-plea))
  ::  preamble
  ::
  =^  *  ames.nec
    (mesa-call:v ames.nec [~[/poke] [%plea ~bud poke-plea] bex-roof])
  ::
  =^  *  ames.nec
    (mesa-call:v ames.nec ~[ack-wire /poke] make-poke bex-roof)
  ::
  =/  message=mess:mesa
    [%poke [~bud ack-path] [~nec poke-path] auth=&+*@uxJ page=message/poke-plea]
  ::
  ~?  >  dbug  'place %naxplanation payload in namespace'
  =^  *  ames.bud
    %:  mesa-call-with-dude:v  ames.bud
      :^    *goof
          ~[//unix]
        [%mess lane=`*@ux message]
      bex-roof
    ==
  ::
  =/  make-peek  [%make-peek ~bud (weld /publ/0//x/1/[%$] nax-path)]
  =^  error-moves  ames.nec
    %:    mesa-check-take:v  ames.nec  [now=~1111.1.1 eny=`@`0xdead.beef bex-roof]
      :+  ack-wire  ~[/poke]
      [%mesa %response %page ~bud^ack-path *(each @uxJ @uxI) `page`message/error=&]
    ::
      :~  :-  ~[/poke]
          [%pass nax-wire %m make-peek]
      ==
    ==
  ::
  :-  error-moves  |.  :-  %|
  ::  start
  ::
  ~?  >  dbug  'naxplanation payload is accesible at /flow/0/~bud/nax/~nec/bak/1'
  =/  moves-1
    %+  expect-eq
    !>  nax/*goof
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        %-  need   %-  need
        %-  scry:(ames.bud ~1111.1.10 `@`0xdead.beef *roof)
        [[~ ~] / %x [[~bud %$ ud+1] nax-path]]
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec starts %peeking for the naxplanation on ~bud'
  ::
  =/  naxplanation=*  *error:mesa
  =/  bex-roof        (make-roof nax-path message+!>(naxplanation))
  :::
  =^  moves-x  ames.nec
    (mesa-call:v ames.nec [~[nax-wire /poke] make-peek bex-roof])
  ~?  >  dbug  '~bud gives ~nec the first fragment'
  =^  moves-y  ames.bud     (mesa-reply:v ames.bud ~[/unix-pact] moves-x bex-roof)
  ~?  >  dbug  '~nec hears complete message'
  =^  moves-page  ames.nec  (mesa-reply:v ames.nec ~[/unix-pact] moves-y bex-roof)
  ::
  :-  (mesa-expect-msg:v moves-page naxplanation)  |.  :-  %|
  ~?  >  dbug  '~nec takes %naxplanation from ~bud, given by the packet layer'
  =^  moves-3  ames.nec
    %:    mesa-check-take:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
      ?>  ?=([[^ [%give %response *]] *] moves-page)
      :+  wire=i.duct.i.moves-page
        duct=t.duct.i.moves-page
      [%mesa gift.card.i.moves-page]
    ::
      :~  :-  ~[/poke]
          [%give %done `*goof]
      ==
    ==
  :-  moves-3  |.  :-  %&
    %+  expect-eq
    !>  0
    !>  =<  next-bone.ossuary
        %:  mesa-scry-peer:v
          ames.bud
          [~1111.1.10 0xdead.beef *roof]
          [~bud ~nec]
        ==
--
