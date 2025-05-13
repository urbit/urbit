::  test send %poke %boon
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
  %-  run-chain
  |.  :-  %|
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
  =/  poke-space
    ?>  ?=(%chum -.space)
    ::  lifes need to be switched since for %pokes,
    ::  this is a payload in our namespace
    ::
    %_  space
      server-life  client-life.space
      client-life  server-life.space
      client       ~bud
    ==
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  =/  poke-path  /flow/0/poke/for/~bud/1
  =/  ack-path   /flow/0/ack/bak/~nec/1
  ::
  =/  ack-wire   /mesa/flow/ack/for/~bud/0/0
  =/  vane-wire  /bone/~nec/0/1
  ::
  =/  ack-full-path=path  (make-space-path.nec space %a %x '1' %$ ack-path)
  =/  moke=[%moke space:ames =spar:ames =path]
    [%moke space [~bud %a %x '1' %$ ack-path] %a %x '1' %$ poke-path]
  =/  mage=[%mage space:ames =spar:ames]
    [%mage space [~nec %a %x '1' %$ ack-path]]
  =/  poke-roof  (make-roof poke-path message+!>(poke-plea))
  ::  preamble
  ::
  =^  *  nec
    (ames-call:v nec [~[/poke] [%plea ~bud poke-plea] *roof])
  ::
  =^  *  nec
    (ames-call:v nec ~[ack-wire /poke] moke poke-roof)
  ::
  =/  message=mess:ames
    [%poke [~bud ack-path] [~nec poke-path] page=[%message plea/poke-plea]]
  ::
  =^  *  bud
    (ames-call:v bud ~[//unix] [%mess message] *roof)
  ::
  =^  *  bud
    (ames-take:v bud vane-wire ~[//unix] [%gall %done ~] *roof)
  ::
  =^  *  nec
    %:    ames-take:v  nec  ack-wire
      ~[/poke]
      [%ames %sage ~bud^[%a %x '1' %$ ack-path] `page`message/[%ack error=|]]
      *roof
    ==
  ::  start
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
    [%chum server-life=nec-life client=~bud client-life=bud-life key]
  =/  boon-space
    ?>  ?=(%chum -.space)
    ::  lifes need to be switched since for %pokes,
    ::  this is a payload in our namespace
    ::
    %_  space
      server-life  client-life.space
      client-life  server-life.space
      client       ~nec
    ==
  =/  poke-boon  [%x ~]  :: %kick
  =/  boon-path  /flow/0/poke/bak/~nec/1
  =/  ack-path   /flow/0/ack/for/~bud/1
  =/  ack-wire   /mesa/flow/ack/bak/~nec/0/0
  =/  ack-full-path=path  (make-space-path.nec space %a %x '1' %$ ack-path)
  =/  bon-full-path=path
    (make-space-path.nec boon-space %a %x '1' %$ boon-path)
  =/  moke=[%moke space:ames =spar:ames =path]
    [%moke space [~nec %a %x '1' %$ ack-path] %a %x '1' %$ boon-path]
  ~?  >  dbug  'send %poke-boon to ~nec'
  =^  moves-1  bud
    %:    ames-check-take:v  bud
        [~1111.1.1 0xdead.beef *roof]
    ::
      [vane-wire ~[/poke] %ames %boon `*`poke-boon]
    ::
      :~  [~[/poke] [%pass ack-wire %a moke]]
      ==
    ==
::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~bud makes $pact and sends it'
  =/  bon-roof  (make-roof boon-path message+!>(poke-boon))
  =^  moves-2  bud
    %:    ames-check-call:v  bud
        [~1111.1.1 0xdead.beef bon-roof]
    ::
      [~[[%ames ack-wire] /poke] moke]
    ::
      =/  blob=@
        %:   ames-make-pact:v  bud
          `spar:ames`[~nec ack-full-path]
          bon-full-path
          rift=0
          bon-roof
        ==
      :~  [~[//unix] %give %push lanes=~[0x0] blob]
      ==
    ==
  ::
  ~?  >  dbug  'boon payload accesible at /flow/0/poke/bak/~nec/1'
  :-  moves-2  |.  :-  %|
  =/  moves-3
    %+  expect-eq
    !>  boon/poke-boon
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        (ames-scry-payload:v bud ~nec ~bud %a %x '1' %$ boon-path)
  ::
  :-  moves-3  |.  :-  %|
  ~?  >  dbug  '~nec hears %poke-boon from ~bud'
  =/  message=mess:ames
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :*  %poke
        [~nec ack-path]
        [~bud boon-path]
        page=[%message boon/poke-boon]
    ==
  ::
  =/  mage=[%mage space:ames =spar:ames]
    [%mage space [~bud %a %x '1' %$ ack-path]]
  =^  moves-4  nec
    %:  ames-check-call:v  nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%mess message]
    ::
      :~  :-  ~[/ames]
          [%pass /make-page %a mage]  ::  send ack for %boon
      ::
          :-  ~[/poke]
          [%give %boon poke-boon]     :: give %boon to vane
      ==
    ==
  ::
  :-  moves-4  |.  :-  %|
  ~?  >  dbug  '~nec %ames acks the boon'
  =/  moves-5
    %+  expect-eq
    !>  1
    !>  =/  flows  =<  flows
          %:  ames-scry-peer:v
            nec
            [~1111.1.10 0xdead.beef *roof]
            [~nec ~bud]
          ==
        last-acked.rcv:(~(got by flows) 0 %for)
  ::
  :-  moves-5  |.  :-  %|
  ~?  >  dbug  '~bud hears %ack from ~nec, clears timers'
  =^  moves-6  bud
    %:    ames-check-take:v  bud
        [~1111.1.1 0xdead.beef *roof]
      :+  ack-wire
        ~[/poke]
      [%ames %sage ~nec^[%a %x '1' %$ ack-path] `page`message/[%ack error=|]]
    ::
      ~
    ==
  ::
  :-  moves-6  |.  :-  %|
  ~?  >  dbug  '~bud %ames removes the boon payload after ack'
  =/  moves-6
    %+  expect-eq
    !>  ~
    !>  =/  flows  =<  flows
          %:  ames-scry-peer:v
            bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        loads.snd:(~(got by flows) 0 %bak)
  ::
  :-  moves-6  |.  :-  %&
    %+  expect-eq
    !>  0
    !>  =<  next-bone.ossuary
        %:  ames-scry-peer:v
          bud
          [~1111.1.10 0xdead.beef *roof]
          [~bud ~nec]
        ==
--
