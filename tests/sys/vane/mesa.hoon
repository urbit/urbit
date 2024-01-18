::  test sema vane
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
  =/  pact
    :^    %poke
        [~bud /ax/~bud//1/mess/0/~bud/ack/~nec/flow/0/1/0 13 s=num=0]
      [~nec /ax/~bud//1/mess/0/~nec/poke/~bud/flow/0/1/0 13 s=num=0]
    [tot=1 aut=0x0 dat=(jam poke-plea)]
  ::
  =^  moves-1  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/poke] %plea ~bud poke-plea]
    ::
      :~  [~[//unix] [%give %pres lanes=~ pact]]
          [~[/poke] [%pass /mess/~bud/0/0 %b %wait ~1111.1.1..00.00.30]]
      ==
    ==
  ::
  :-  moves-1  |.  :-  %&
  ~?  >  dbug  '~bud hears %poke-plea from ~nec'
  =/  message
    :^    %poke
        [~bud /ax/~bud//1/mess/0/~bud/ack/~nec/flow/0/1/0]
      [~nec /ax/~bud//1/mess/0/~nec/poke/~bud/flow/0/1/0]
    noun/poke-plea
  =^  moves-2  ames.bud
    %:  mesa-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      [%sink lane=`*@ux |+message]
    ::
      :~  ::  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~nec is your neighbor"]
          :-  ~[//unix]
          [%pass /mess/flow/~nec/0/0 %g %plea ~nec poke-plea]
      ==
    ==
  moves-2
--
