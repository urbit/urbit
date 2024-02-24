::  test %peek for %naxplanation
::
/+  *test, v=test-mesa-gall
|%
++  dbug  `?`&
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  =/  poke-plea   [%x /ge/pok [%0 %m noun/0]]
  =/  poke-path   /~nec/poke/~bud/flow/0/for/1
  =/  ack-path    /~bud/ack/~nec/flow/0/for/1
  =/  ack-wire    /flow/int/for/~bud/0/0/1
  =/  vane-wire   /flow/out/bak/~nec/0/0/1
  =/  make-poke=[%make-poke spar:ames path]
    [%make-poke [~bud ack-path] poke-path]
  ::  preamble
  ::
  =^  *  ames.nec
    (mesa-call:v ames.nec [~[/poke] [%plea ~bud poke-plea] *roof])
  ::
  =^  *  ames.nec
    (mesa-call:v ames.nec ~[ack-wire /poke] make-poke *roof)
  =/  message=mess:mesa
    [%poke [~bud ack-path] [~nec poke-path] auth=&+*@uxJ page=message/poke-plea]
  ::
  ~?  >  dbug  'place %naxplanation payload in namespace'
  =^  *  ames.bud
    %:  mesa-call-with-dude:v  ames.bud
      :^    *goof
          ~[//unix]
        [%mess lane=`*@ux message]
      *roof
    ==
  ::
  =^  *  ames.nec
    %:    mesa-take:v  ames.nec  ack-wire
      ~[/poke]
      [%mesa %response %page ~bud^ack-path *(each @uxJ @uxI) `page`message/&]
      *roof
    ==
  ::  start
  ::
  =/  nax-path  /~bud/nax/~nec/flow/0/bak/1  :: XX refactor bak/for directions
  ~?  >  dbug  'naxplanation payload is accesible at /~nec/nax/~bud/flow/0/for/1'
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
  ~?  >  dbug  '~nec hears %naxplanation from ~bud'
  =^  moves-2  ames.nec
    %:    mesa-check-take:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
      :+  /flow/ext/for/~bud/0/0/1
        ~[/poke]
      [%mesa %response %page ~bud^nax-path *(each @uxJ @uxI) `page`message/*goof]
    ::
      :~  :-  ~[/poke]
          [%give %done `*goof]
      ==
    ==
  :-  moves-2  |.  :-  %&
    %+  expect-eq
    !>  0
    !>  =<  next-bone.ossuary
        %:  mesa-scry-peer:v
          ames.bud
          [~1111.1.10 0xdead.beef *roof]
          [~bud ~nec]
        ==
--
