::  test gall request queue fix, which implicates ames and gall
::
/+  *test, v=test-ames-gall
|%
++  test-setup-nec-bud
  %-  run-chain
  |.  :-  %|
  =+  nec-bud:v
  =/  =task:gall  [%deal [~nec ~nec] %sub %poke watch+!>(~bud)]
  =^  t1  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/foo] task]
      :~  ^-  move:gall-bunt:v
          :*  ~[/foo]  %give  [%unto %poke-ack ~]
          ==
          ^-  move:gall-bunt:v
          :*  ~[/init]  %pass  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
              [%g %deal [~nec ~bud] %pub %watch /foo]
      ==  ==
    ==
  :-  t1  |.  :-  %|
  =^  t2  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/init] [%deal [~nec ~bud] %pub %watch /foo]]
      :~  :-  ~[/init]  [%pass /sys/lag %a %heed ~bud]
          :-  ~[/init]  [%pass /sys/era %j %public-keys (sy ~bud ~)]
          :-  ~[/init]
          [%pass /sys/way/~bud/pub %a %plea ~bud %g /ge/pub [%0 %s /foo]]
      ==
    ==
  :-  t2  |.  :-  %|
  =/  t3  ~
::  =^  t3  gall.nec
::    %:  gall-check-take:v  gall.nec
::      [~1111.1.2 0xdead.beef *roof]
::      :+  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
::        ~[/init]
::      [%g %deal [~nec ~bud] %pub %watch /foo]
::      ~
::    ==
  :-  t3  |.  :-  %&  ~
--
