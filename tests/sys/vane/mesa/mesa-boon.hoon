::  test send %poke %boon
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
  ?.  &(=(s.bem pax) |(=(vis %x) =(vis [%$ %x]) =(vis [%g %x])))  [~ ~]
  ``val
::
++  test-watch
  %-  run-chain
  |.  :-  %|
  =/  poke-plea   [%g /ge/pok [%0 %m noun/0]]
  =/  poke-path   //x/1//flow/0/~nec/poke/~bud/for/1
  =/  ack-path    //x/1//flow/0/~bud/ack/~nec/bak/1
  ::
  =/  ack-wire    /flow/int/for/~bud/0/0/1
  =/  vane-wire   /flow/out/bak/~nec/0/0/1
  =/  make-poke=[%make-poke space:mesa spar:ames path]
    [%make-poke publ/bud-life [~bud ack-path] poke-path]
  =/  poke-roof  (make-roof /flow/0/~nec/poke/~bud/for/1 message+!>(poke-plea))
  ::  preamble
  ::
  =^  *  mesa.nec
    (mesa-call:v mesa.nec [~[/poke] [%plea ~bud poke-plea] *roof])
  ::
  =^  *  mesa.nec
    (mesa-call:v mesa.nec ~[ack-wire /poke] make-poke poke-roof)
  =/  message=mess:mesa
    [%poke [~bud ack-path] [~nec poke-path] auth=&+*@uxJ page=message/poke-plea]
  ::
  =^  *  mesa.bud
    (mesa-call:v mesa.bud ~[//unix] [%mess lane=`*@ux message] *roof)
  ::
  =^  *  mesa.bud
    (mesa-take:v mesa.bud vane-wire ~[//unix] [%gall %done ~] *roof)
  ::
  =^  *  mesa.nec
    %:    mesa-take:v  mesa.nec  ack-wire
      ~[/poke]
      [%mesa %response %page ~bud^ack-path &+0x0 `page`message/[%ack error=|]]
      *roof
    ==
  ::  start
  ::
  =/  poke-boon  [%x ~]  :: %kick
  =/  boon-path  //x/1//flow/0/~bud/poke/~nec/bak/1
  =/  ack-path   //x/1//flow/0/~nec/ack/~bud/for/1
  =/  ack-wire   /flow/int/bak/~nec/0/0/1
  =/  make-poke=[%make-poke space:mesa spar:ames path]
    [%make-poke publ/nec-life [~nec ack-path] boon-path]
  ~?  >  dbug  'send %poke-boon to ~nec'
  =^  moves-1  mesa.bud
    %:    mesa-check-take:v  mesa.bud
        [~1111.1.1 0xdead.beef *roof]
    ::
      [vane-wire ~[/poke] %mesa %boon `*`poke-boon]
    ::
      :~  [~[/poke] [%pass /~nec/0/0 %b %wait ~1111.1.1..00.00.30]]
          [~[/poke] [%pass /flow/int/bak/~nec/0/0/1 %m make-poke]]
      ==
    ==
::
  =/  ack-full-path   (weld /publ/[(scot %ud nec-life)] ack-path)
  =/  boon-full-path  (weld /publ/[(scot %ud bud-life)] boon-path)
  :-  moves-1  |.  :-  %|
  ~?  >  dbug  '~bud makes $pact and sends it'
  =^  moves-2  mesa.bud
    %:    mesa-check-call:v  mesa.bud
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/flow/int/bak/~nec/0/0/1 /poke] make-poke]
    ::
      =/  blob=@
        %:   mesa-make-pact:v  mesa.bud
          ~nec^ack-full-path
          boon-path
          rift=0
          publ/nec-life
        ==
      :~  [~[//unix] %give %send lanes=~ blob]
      ==
    ==
  ::
  ~?  >  dbug  'boon payload is accesible at /~bud/poke/~nec/flow/0/for/1'
  :-  moves-2  |.  :-  %|
  =/  moves-3
    %+  expect-eq
    !>  boon/poke-boon
    !>  !<  page
        =<  ?>  ?=(%message p)  q
        (mesa-scry-payload:v mesa.bud ~bud boon-path)
  ::
  :-  moves-3  |.  :-  %|
  ~?  >  dbug  '~nec hears %poke-boon from ~bud'
  =/  message=mess:mesa
    ::  XX  the message layer should only get the inner path (from rift onwards)
    ::
    :*  %poke
        [~nec ack-path]
        [~bud boon-path]
        auth=&+*@uxJ
        page=message/poke-boon
    ==
  ::
  =^  moves-4  mesa.nec
    %:  mesa-check-call:v  mesa.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%mess lane=`*@ux message]
    ::
      :~  [~[/poke] [%give %boon poke-boon]]
      ==
    ==
  ::
  :-  moves-4  |.  :-  %|
  ~?  >  dbug  '~nec %ames acks the boon'
  =/  moves-5
    %+  expect-eq
    !>  1
    !>  =/  flows  =<  flows
          %:  mesa-scry-peer:v
            mesa.nec
            [~1111.1.10 0xdead.beef *roof]
            [~nec ~bud]
          ==
        last-acked:(~(got by flows) 0 %for)
  ::
  :-  moves-5  |.  :-  %|
  ~?  >  dbug  '~bud hears %ack from ~nec, clears timers'
  =^  moves-6  mesa.bud
    %:    mesa-check-take:v  mesa.bud
        [~1111.1.1 0xdead.beef *roof]
      :+  ack-wire
        ~[/poke]
      [%mesa %response %page ~nec^ack-path &+0x0 `page`message/[%ack error=|]]
    ::
      :~  :-  ~[/ames]
          [%pass /~nec/0/0 %b %rest ~1111.1.1..00.00.30]
      ==
    ==
  ::
  :-  moves-6  |.  :-  %|
  ~?  >  dbug  '~bud %ames removes the boon payload after ack'
  =/  moves-6
    %+  expect-eq
    !>  ~
    !>  =/  flows  =<  flows
          %:  mesa-scry-peer:v
            mesa.bud
            [~1111.1.10 0xdead.beef *roof]
            [~bud ~nec]
          ==
        loads:(~(got by flows) 0 %bak)
  ::
  :-  moves-6  |.  :-  %&
    %+  expect-eq
    !>  0
    !>  =<  next-bone.ossuary
        %:  mesa-scry-peer:v
          mesa.bud
          [~1111.1.10 0xdead.beef *roof]
          [~bud ~nec]
        ==
--
