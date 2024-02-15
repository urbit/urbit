::  test send %poke sema vane
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
  =/  ack-path   /~bud/ack/~nec/flow/0/for/1
  =/  make-poke=[%make-poke spar:ames path]
    [%make-poke [~bud ack-path] poke-path]
  ::  preamble
  ::
  =^  *  ames.nec
    (mesa-call:v ames.nec [~[/poke] [%plea ~bud poke-plea] *roof])
  ::
  =^  *  ames.nec
    %:    mesa-call:v  ames.nec
      [~[/flow/int/for/~bud/0/0/1 /poke] make-poke *roof]
    ==
  =/  message=mess:mesa
    [%poke [~bud ack-path] [~nec poke-path] auth=&+*@uxJ page=message/poke-plea]
  ::
  =^  *  ames.bud
    %:  mesa-call:v  ames.bud
    ::
      ~[//unix]
      [%mess lane=`*@ux message]
      *roof
    ==
  ::
  =^  *  ames.bud
    %:  mesa-take:v  ames.bud
      /flow/out/bak/~nec/0/0/1
      ~[//unix]
      [%gall %done ~]
      *roof
    ==
  ::
  =^  *  ames.nec
    %:    mesa-take:v  ames.nec
    ::
      /flow/int/for/~bud/0/0/1
      ~[/poke]
      [%mesa %response %page ~bud^ack-path *(each @uxJ @uxI) `page`message/|]
      *roof
    ==
  ::  start
  ::
  =/  poke-boon  [%x ~]  :: %kick
  =/  poke-path  /~bud/poke/~nec/flow/0/bak/1
  =/  ack-path   /~nec/ack/~bud/flow/0/bak/1
  =/  make-poke=[%make-poke spar:ames path]
    [%make-poke [~nec ack-path] poke-path]
  ~?  >  dbug  'send %poke-boon to ~nec'
  =^  moves-1  ames.bud
    %:    mesa-check-take:v  ames.bud
        [~1111.1.1 0xdead.beef *roof]
    ::
      [/flow/out/bak/~nec/0/0/1 ~[/poke] %mesa %boon `*`poke-boon]
    ::
      :~  [~[/poke] [%pass /~nec/0/0 %b %wait ~1111.1.1..00.00.30]]
        ::
          :-  ~[/poke]
          [%pass /flow/int/bak/~nec/0/0/1 %m make-poke]
      ==
    ==
::
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~bud makes $pact and sends it'
  =^  moves-2  ames.bud
    %:    mesa-check-call:v  ames.bud
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/flow/int/bak/~nec/0/0/1 /poke] make-poke]
    ::
      :~  [~[//unix] %give %send lanes=~ blob=0] :: XX TODO
      ==
    ==
  ::
  :-  moves-2  |.  :-  %|
  ~?  >  dbug  '~nec hears %poke-boon from ~bud'
  =/  message=mess:mesa
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :*  %poke
        [~nec ack-path]
        [~bud poke-path]
        auth=&+*@uxJ
        page=message/poke-boon
    ==
  ::
  =^  moves-3  ames.nec
    %:  mesa-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%mess lane=`*@ux message]
    ::
      :~  [~[/poke] [%give %boon poke-boon]]
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
