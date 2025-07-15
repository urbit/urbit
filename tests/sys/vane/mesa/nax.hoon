::  test %peek for %naxplanation
::
/+  *test, v=test-mesa-gall
=+  [nec-life=2 bud-life=3]
=+  (ames-nec-bud:v [nec-life bud-life] nec=0 bud=0)
|%
++  dbug  `?`&
++  make-roof
  |=  [pax=path val=cage]
  ^-  roof
  |=  [lyc=gang pov=path vis=view bem=beam]
  ^-  (unit (unit cage))
  ?.  &(=(s.bem pax) |(=(vis %x) =(vis [%$ %x]) =(vis [%g %x]) =(vis [%a %x])))
    [~ ~]
  ``val
::
++  test-watch
  ::  uncomment to turn on verbose debug output
  ::
  =^  *  nec
    (ames-call:v nec ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  =^  *  bud
    (ames-call:v bud ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  ::
  %-  run-chain
  |.  :-  %|
  ::
  =/  key-1  =<  symmetric-key
    ^-  fren-state:ames
    %:  ames-scry-peer:v
      nec
      [~1111.1.10 0xdead.beef *roof]
      [~nec ~bud]
    ==
  =/  key  `@uv`49.444.113.421.508.228.869.460.528.530.323.207.394.851.818.486.403.699.510.004.444.292.293.148.898.490
  ~&  [key-1 key]
  =/  =space:ames
    [%chum server-life=bud-life client=~nec client-life=nec-life key]
  =/  poke-plea  [%x /ge/pok [%0 %m noun/0]]
  =/  poke-path  /flow/0/poke/for/~bud/1
  =/  ack-path   /flow/0/ack/bak/~nec/1
  =/  nax-path   /flow/0/naxp/bak/~nec/1
  ::
  =/  ack-wire   /mesa/flow/ack/for/~bud/0/0
  =/  vane-wire  /bone/~nec/0/1
  =/  nax-wire   /mesa/flow/nax/for/~bud/0/0
  ::
  =/  moke=[%moke space:ames =spar:ames =path]
    [%moke space [~bud %a %x '1' %$ ack-path] %a %x '1' %$ poke-path]
  =/  poke-roof  (make-roof poke-path message+!>(plea/poke-plea))
  ::  preamble
  ::
  =^  *  nec
    (ames-call:v nec [~[/poke] [%plea ~bud poke-plea] *roof])
  ::
  =^  *  nec
    (ames-call:v nec ~[[%ames ack-wire] /poke] moke poke-roof)
  ::
  =/  message=mess:ames
    [%poke [~bud ack-path] [~nec poke-path] page=[%message plea/poke-plea]]
  ::
  ~?  >  dbug  'place %naxplanation payload in namespace'
  =^  *  bud
    %:  ames-call-with-dude:v  bud
      :^    *goof
          ~[//unix]
        [%mess message]
      poke-roof
    ==
  ::
  =/  =space:ames
    [%chum our-life=bud-life her=~nec her-life=nec-life key]
  =/  full-nax-path=path  [%a %x '1' %$ nax-path]
  =/  meek=[%meek space:ames =spar:ames]
    [%meek space ~bud full-nax-path]
  ~?  >  dbug  '~nec hears %nack, produces %meek request'
  =^  error-moves  nec
    %:    ames-check-take:v  nec  [now=~1111.1.1 eny=`@`0xdead.beef poke-roof]
      :+  ack-wire  ~[/poke]
      [%ames %sage ~bud^[%a %x '1' %$ ack-path] `page`message/[%ack error=&]]
    ::
      :~  :-  ~[/poke]
          [%pass nax-wire %a meek]
      ==
    ==
  ::
  :-  error-moves  |.  :-  %|
  ::  start
  ::
  ~?  >  dbug  'naxplanation payload is accesible at /flow/0/nax/bak/~nec/1'
  =/  naxplanation=*  *error:ames
  =/  nax-roof        (make-roof nax-path message+!>(nax/naxplanation))
  =/  moves-1
    %+  expect-eq
    !>  nax/*goof
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        (ames-scry-payload:v bud ~nec ~bud %a %x '1' %$ nax-path)
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec starts %peeking for the naxplanation on ~bud'
  ::
  =^  moves-x  nec
    (ames-call:v nec [~[[%ames nax-wire] /poke] meek nax-roof])
  ~?  >  dbug  '~bud gives ~nec the first fragment'
  =^  moves-y  bud     (ames-reply:v bud ~[/unix-pact] moves-x nax-roof)
  ~?  >  dbug  '~nec hears complete message'
  =^  moves-page  nec  (ames-reply:v nec ~[/unix-pact] moves-y nax-roof)
  ::
  :-  (ames-expect-msg:v moves-page nax/naxplanation)  |.  :-  %|
  ~?  >  dbug  '~nec takes %naxplanation from ~bud, given by the packet layer'
  =^  moves-3  nec
    %:    ames-check-take:v  nec
        [~1111.1.1 0xdead.beef *roof]
      ?>  ?=([[[[%ames *] *] [%give %sage *]] *] moves-page)
      ::  remove reentrant %ames wire; this would be done by %arvo to route it
      ::  to the %ames vane
      ::
      [wire=t.i.duct duct=t.duct %ames p.card]:i.moves-page
    ::
      :~  :-  ~[/poke]
          [%give %done `*error:ames]
      ==
    ==
  :-  moves-3  |.  :-  %&
    %+  expect-eq
    !>  0
    !>  =<  next-bone.ossuary
        %:  ames-scry-peer:v
          bud
          [~1111.1.10 0xdead.beef *roof]
          [~bud ~nec]
        ==
--
