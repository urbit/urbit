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
  =^  t3  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/sys/way/~bud/pub /init] %plea ~bud %g /ge/pub %pub [%0 %s /foo]]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0x79ca.8314.59aa.4ee3.b4bf.9f31.8a0b.1ba1.e19d.7b83.
                84a0.209b.2000.193c.d818.2da8.9309.759f.9e8c.b88e.
                1cb8.8400.0200.0132.e478.8000
          ==
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  :-  t3  |.  :-  %|
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0x79ca.8314.59aa.4ee3.b4bf.9f31.8a0b.1ba1.e19d.7b83.
            84a0.209b.2000.193c.d818.2da8.9309.759f.9e8c.b88e.
            1cb8.8400.0200.0132.e478.8000
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~nec is your neighbor"]
          :-  ~[//unix]
          [%pass /bone/~nec/0/1 %g %plea ~nec %g /ge/pub %pub [%0 %s /foo]]
      ==
    ==
::  =^  t3  gall.nec
::    %:  gall-check-take:v  gall.nec
::      [~1111.1.2 0xdead.beef *roof]
::      :+  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
::        ~[/init]
::      [%g %deal [~nec ~bud] %pub %watch /foo]
::      ~
::    ==
  :-  t4  |.  :-  %&  ~
--
