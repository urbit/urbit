::  test sema vane
::
/+  *test, v=test-mesa-gall
|%
++  dbug  `?`|
++  test-watch
  %-  run-chain
  |.  :-  %&
  =+  (nec-bud:v [nec=2 bud=3] nec=0 bud=0)
  ~?  >  debug  'send %poke-plea to ~bud'
  =/  poke-plea  [%g /ge/pok [%0 %m noun/0]]
  =/  message
    :^    %poke
        [~bud /ax/~bud//1/mess/0/~bud/ack/~nec/flow/0/1/0 13 s=num=0]
      [~nec /ax/~bud//1/mess/0/~bud/poke/~nec/flow/0/1/0 13 s=num=0]
    [tot=1 aut=0x0 dat=(jam poke-plea) ]
  ::
  =^  moves-1  ames.nec
    %:    mesa-check-call:v  ames.nec
        [~1111.1.1 0xdead.beef *roof]
    ::
      [~[/poke] %plea ~bud poke-plea]
    ::
      :~  [~ [%give %pres lanes=~ message]]
          [~[/poke] [%pass /mess/~bud/0 %b %wait ~1111.1.1..00.00.30]]
      ==
    ==
  :: moves-1
  ::
  :-  moves-1  |.  :-  %|
  ~?  >  debug  '~bud hears %poke-plea from ~nec'
  =^  moves-2  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0xae59.5b29.277b.22c1.20b7.a8db.9086.46df.31bd.f9bc.
          2633.7300.17d4.f5fc.8be5.8bfe.5c9d.36d9.2ea1.7cb3.
          8a00.0200.0132.8fd4.f004
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~nec is your neighbor"]
          :-  ~[//unix]
          [%pass /bone/~nec/0/1 %g %plea ~nec %g /ge/pub [%0 %s /foo]]
      ==
    ==



  :: ~&  >  moves-1/moves-1
  :: %+  expect-eq
  ::   !>  2
  :: !>  2
--
