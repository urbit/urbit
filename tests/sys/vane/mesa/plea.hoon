::  test send %poke
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
  ::
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  =/  poke-path  /flow/0/poke/for/~bud/1
  =/  ack-path   /flow/0/ack/bak/~nec/1
  ::
  =/  ack-wire   /mesa/flow/ack/for/~bud/0/0
  =/  vane-wire  /bone/~nec/0/1
  =/  ack-full-path=path  (make-space-path.nec space %a %x '1' %$ ack-path)
  =/  pok-full-path=path
    (make-space-path.nec poke-space %a %x '1' %$ poke-path)
  =/  moke=[%moke space:ames =spar:ames =path]
    [%moke space [~bud %a %x '1' %$ ack-path] %a %x '1' %$ poke-path]
  =/  mage=[%mage space:ames =spar:ames]
    [%mage space [~nec %a %x '1' %$ ack-path]]
  ::
  ~?  >  dbug  'send %poke-plea to ~bud'
  =^  moves-1  nec
    %:    ames-check-call:v  nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/poke] %plea ~bud poke-plea]
    ::
      :~  :-  ~[/poke]
          [%pass ack-wire %a moke]
      ==
    ==
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec makes $pact and sends it'
  =/  poke-roof  (make-roof poke-path message+!>(poke-plea))
  =^  moves-2  nec
    %:    ames-check-call:v  nec
        [~1111.1.1 0xdead.beef poke-roof]
    ::
      [~[[%ames ack-wire] /poke] moke]
    ::
      =/  blob=@
        %:   ames-make-pact:v  nec
          `spar:ames`[~bud ack-full-path]
          pok-full-path
          rift=0
          poke-roof
        ==
      :~  [~[//unix] %give %push lanes=~[0x0] blob]
      ==
    ==
  ::
  :-  moves-2  |.  :-  %|
  ~?  >  dbug  '~bud hears %poke-plea from ~nec'
  =/  message=mess:ames
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :*  %poke
        [~bud ack-path]
        [~nec poke-path]
        page=[%message plea/poke-plea]
    ==
  ::
  =^  moves-3  bud
    %:  ames-check-call:v  bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%mess message]
    ::
      :~  :-  ~[//unix]
          [%pass vane-wire %g %plea ~nec poke-plea]
      ==
    ==
  ::
  :-  moves-3  |.  :-  %|
  ~?  >  dbug  '~bud %ames has flow=[0 %bak] backwards flow with a pending ack'
  =/  moves-4
    %+  expect-eq
    !>  %.y
    !>  =/  flows  =<  flows
          ^-  fren-state:ames
          %:  ames-scry-peer:v
            bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        ?&  (~(has by flows) 0 %bak)
            pending-ack.rcv:(~(got by flows) 0 %bak)
        ==
  ::
  :-  moves-4  |.  :-  %|
  ~?  >  dbug  '%gall gives %done to %ames'
  =^  moves-5  bud
    %:  ames-check-take:v  bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  vane-wire
        ~[//unix]
      [%gall %done ~]
    ::
      :~  :-  ~[/ames]
          [%pass /make-page %a mage]
      ==
      :: XX emit ack to unix
      :: :~  :-  ~[//unix]
      ::     :*  %give  %send  [%& ~nec]
      ::         0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
      ::         2d9d.076e.6d00.0100.0223.9ae9.5004
      :: ==  ==
    ==
  ::
  :-  moves-5  |.  :-  %|
  ~?  >  dbug  '~bud %ames acks the %poke'
  =/  moves-6
    %+  expect-eq
    !>  1
    !>  =/  flows  =<  flows
          ^-  fren-state:ames
          %:  ames-scry-peer:v
            bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        last-acked.rcv:(~(got by flows) 0 %bak)
  ::
  ~?  >  dbug  'poke payload is accesible at /flow/0/poke/for/~bud/1'
  :-  moves-6  |.  :-  %|
  =/  moves-7
    %+  expect-eq
    !>  plea/poke-plea
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        (ames-scry-payload:v nec ~bud ~nec %a %x '1' %$ poke-path)
  ::
  :-  moves-7  |.  :-  %|
  ~?  >  dbug  '~nec hears %ack from ~bud, gives to gall'
  =^  moves-8  nec
    %:    ames-check-take:v  nec
        [~1111.1.1 0xdead.beef *roof]
      :+  ack-wire
        ~[/poke]
      [%ames %sage ~bud^[%a %x '1' %$ ack-path] `page`message/[%ack error=|]]
    ::
      :~  :-  ~[/poke]
          [%give %done ~]
      ==
    ==
  :-  moves-8  |.  :-  %|
  ~?  >  dbug  '~nec %ames removes the payload for the poke after ack'
  =/  moves-9
    %+  expect-eq
    !>  ~
    !>  =/  flows  =<  flows
          ^-  fren-state:ames
          %:  ames-scry-peer:v
            nec
            [~1111.1.10 0xdead.beef *roof]
            [~nec ~bud]
          ==
        loads.snd:(~(got by flows) 0 %for)
  ~?  >  dbug  '~nec %ames next bone is 1'
  :-  moves-9  |.  :-  %&
    %+  expect-eq
    !>  4
    !>  =<  next-bone.ossuary
        ^-  fren-state:ames
        %:  ames-scry-peer:v
          nec
          [~1111.1.10 0xdead.beef *roof]
          [~nec ~bud]
        ==
--
