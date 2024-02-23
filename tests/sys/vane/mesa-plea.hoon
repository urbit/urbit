::  test send %poke
::
/+  *test, v=test-mesa-gall
|%
++  dbug  `?`&
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  =/  poke-path  /~nec/poke/~bud/flow/0/for/1
  =/  ack-path   /~bud/ack/~nec/flow/0/bak/1
  =/  make-poke=[%make-poke spar:ames path]
    [%make-poke [~bud [%$ %x '1' %$ ack-path]] [%$ %x '1' %$ poke-path]]
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
          [%pass /flow/int/for/~bud/0/0/1 %m make-poke]
      ==
    ==
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec makes $pact and sends it'
  =^  moves-2  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/flow/int/for/~bud/0/0/1 /poke] make-poke]
    ::
      :~  [~[//unix] %give %send lanes=~ blob=0] :: XX TODO
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
          [%pass /flow/out/bak/~nec/0/0/1 %g %plea ~nec poke-plea]
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
      :+  /flow/out/bak/~nec/0/0/1
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
  ~?  >  dbug  '~bud %ames acks the packet'
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
        %-  need   %-  need
        %-  scry:(ames.nec ~1111.1.10 `@`0xdead.beef *roof)
        [[~ ~] / %x [[~nec %$ ud+1] poke-path]]
  ::
  :-  moves-7  |.  :-  %|
  ~?  >  dbug  '~nec hears %ack from ~bud, gives to gall'
  ~&  ~bud^ack-path
  =^  moves-8  ames.nec
    %:    mesa-check-take:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
      :+  /flow/int/for/~bud/0/0/1
        ~[/poke]
      [%mesa %response %page ~bud^ack-path *(each @uxJ @uxI) `page`message/|]
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
