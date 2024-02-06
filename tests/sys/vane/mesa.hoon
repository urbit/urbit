::  test send %poke sema vane
::
/+  *test, v=test-mesa-gall
|%
++  dbug  `?`&
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  ~?  >  dbug  'send %poke-plea to ~bud'
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  ::
  =/  make-poke=[%make-poke spar:ames path]
    :+  %make-poke
      ^-  spar:ames  [~bud /~bud/ack/~nec/flow/0/1]
    /~nec/poke/~bud/flow/0/1
  ::
  =^  moves-1  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/poke] %plea ~bud poke-plea]
    ::
      :~  [~[/poke] [%pass /~bud/0/0 %b %wait ~1111.1.1..00.00.30]]
        ::
          :-  ~[/poke]
          [%pass /flow/~bud/0/0/1/int %a make-poke]
      ==
    ==
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~nec makes $pact and sends it'
  =^  moves-2  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/flow/~bud/0/0/1/int /poke] make-poke]
    ::
      :~  [~[//unix] %give %send lanes=~ blob=0] :: XX TODO
      ==
    ==
  ::
  :-  moves-2  |.  :-  %|
  ~?  >  dbug  '~bud hears %poke-plea from ~nec'
  =/  message
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :^    %poke
        [~bud /ax/~bud//1/mess/0/~bud/ack/~nec/flow/0/1]
      [~nec /ax/~bud//1/mess/0/~nec/poke/~bud/flow/0/1]
    page=message/poke-plea
  ::
  =^  moves-3  ames.bud
    %:  mesa-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%sink lane=`*@ux |+message]
    ::
      :~  :-  ~[//unix]
          [%pass /flow/~nec/0/1/1/out %g %plea ~nec poke-plea]
      ==
    ==
  ::
  :-  moves-3  |.  :-  %|
  ~?  >  dbug  '~bud %ames owns bone 1'
  =/  moves-4
    %+  expect-eq
    !>  %.y
    !>  =/  flows  =<  flows
          %:  mesa-scry-peer:v
            ames.bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        (~(has by flows) 1)  :: XX check that pending-ack is flows is [~ 1]
  ::
  :-  moves-4  |.  :-  %|
  ~?  >  dbug  '%gall gives %done to %ames'
  =^  moves-5  ames.bud
    %:  mesa-check-take:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /flow/~nec/0/1/1/out  ~[//unix]
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
       last-acked:(~(got by flows) 1)
  ::
  :-  moves-6  |.  :-  %&
  ~?  >  dbug  '~nec %ames next bone is 2'
  %+  expect-eq
  !>  2
  !>  =<  next-bone.ossuary
      %:  mesa-scry-peer:v
        ames.nec
        [~1111.1.10 0xdead.beef *roof]
        [~nec ~bud]
      ==
--
