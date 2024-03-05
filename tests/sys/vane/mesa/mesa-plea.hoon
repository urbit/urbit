::  test send %poke
::
/+  *test, v=test-mesa-gall
=+  [nec-life=2 bud-life=3]
=+  (nec-bud:v [nec-life bud-life] nec=0 bud=0)
|%
++  dbug  `?`&
++  make-roof
  |=  [pax=path val=cage]
  ^-  roof
  |=  [lyc=gang pov=path vis=view bem=beam]
  ^-  (unit (unit cage))
  ~&  s.bem^pax
  ?.  &(=(s.bem pax) |(=(vis %x) =(vis [%$ %x]) =(vis [%g %x])))  [~ ~]
  ``val
::
++  test-watch
  %-  run-chain
  |.  :-  %|
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  =/  poke-path  //x/1//flow/0/~nec/poke/~bud/for/1
  =/  ack-path   //x/1//flow/0/~bud/ack/~nec/bak/1
  ::
  =/  ack-wire   /flow/int/for/~bud/0/0/1
  =/  vane-wire  /flow/out/bak/~nec/0/0/1
  =/  make-poke=[%make-poke namespace:mesa =spar:ames =path]
    [%make-poke %publ^life=bud-life [~bud ack-path] poke-path]
  ::
  ~?  >  dbug  'send %poke-plea to ~bud'
  =^  moves-1  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/poke] %plea ~bud poke-plea]
    ::
      :~  [~[/poke] [%pass /~bud/0/0 %b %wait ~1111.1.1..00.00.30]]
        ::
          :-  ~[/poke]
          [%pass ack-wire %m make-poke]
      ==
    ==
  ::
  =/  ack-full-path  (weld /publ/[(scot %ud bud-life)] ack-path)
  =/  pok-full-path  (weld /publ/[(scot %ud nec-life)] poke-path)
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec makes $pact and sends it'
  =/  poke-roof  (make-roof /flow/0/~nec/poke/~bud/for/1 message+!>(poke-plea))
  =^  moves-2  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef poke-roof]
    ::
      [~[/flow/int/for/~bud/0/0/1 /poke] make-poke]
    ::
      =/  blob=@
        %:   mesa-make-pact:v  ames.nec
          ~bud^ack-full-path
          poke-path
          rift=0
          publ/bud-life
        ==
      :~  [~[//unix] %give %send lanes=~ blob]
      ==
    ==
  ::
  :-  moves-2  |.  :-  %|
  ~?  >  dbug  '~bud hears %poke-plea from ~nec'
  =/  message=mess:mesa
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :*  %poke
        [~bud ack-path]
        [~nec poke-path]
        auth=&+*@uxJ
        page=message/poke-plea
    ==
  ::
  =^  moves-3  ames.bud
    %:  mesa-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%mess lane=`*@ux message]
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
          %:  mesa-scry-peer:v
            ames.bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        ?&  (~(has by flows) 0 %bak)
            pending-ack:(~(got by flows) 0 %bak)
        ==
  ::
  :-  moves-4  |.  :-  %|
  ~?  >  dbug  '%gall gives %done to %ames'
  =^  moves-5  ames.bud
    %:  mesa-check-take:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  vane-wire
        ~[//unix]
      [%gall %done ~]
    ::
      ~  :: XX emit ack to unix
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
          %:  mesa-scry-peer:v
            ames.bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        last-acked:(~(got by flows) 0 %bak)
  ::
  ~?  >  dbug  'poke payload is accesible at /~nec/poke/~bud/flow/0/for/1'
  :-  moves-6  |.  :-  %|
  =/  moves-7
    %+  expect-eq
    !>  plea/poke-plea
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        (mesa-scry-payload:v ames.nec ~nec poke-path)
  ::
  :-  moves-7  |.  :-  %|
  ~?  >  dbug  '~nec hears %ack from ~bud, gives to gall'
  =^  moves-8  ames.nec
    %:    mesa-check-take:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
      :+  ack-wire
        ~[/poke]
      [%mesa %response %page ~bud^ack-path *(each @uxJ @uxI) `page`message/|]
    ::
      :~  :-  ~[/ames]
          [%pass /~bud/0/0 %b %rest ~1111.1.1..00.00.30]
        ::
        :-  ~[/poke]
        [%give %done ~]
      ==
    ==
  :-  moves-8  |.  :-  %|
  ~?  >  dbug  '~nec %ames removes the payload for the poke after ack'
  =/  moves-9
    %+  expect-eq
    !>  ~
    !>  =/  flows  =<  flows
          %:  mesa-scry-peer:v
            ames.nec
            [~1111.1.10 0xdead.beef *roof]
            [~nec ~bud]
          ==
        loads:(~(got by flows) 0 %for)
  ~?  >  dbug  '~nec %ames next bone is 1'
  :-  moves-9  |.  :-  %&
    %+  expect-eq
    !>  1
    !>  =<  next-bone.ossuary
        %:  mesa-scry-peer:v
          ames.nec
          [~1111.1.10 0xdead.beef *roof]
          [~nec ~bud]
        ==
--
