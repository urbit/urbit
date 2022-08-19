::  test gall request queue fix, which implicates ames and gall
::
/+  *test, v=test-ames-gall
|%
++  test-watch
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
      [~[/sys/way/~bud/pub /init] %plea ~bud %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0xae59.5b29.277b.22c1.20b7.a8db.9086.46df.31bd.f9bc.
                2633.7300.17d4.f5fc.8be5.8bfe.5c9d.36d9.2ea1.7cb3.
                8a00.0200.0132.8fd4.f000
          ==
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  :-  t3  |.  :-  %|
  =^  t4  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0xae59.5b29.277b.22c1.20b7.a8db.9086.46df.31bd.f9bc.
            2633.7300.17d4.f5fc.8be5.8bfe.5c9d.36d9.2ea1.7cb3.
            8a00.0200.0132.8fd4.f000
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~nec is your neighbor"]
          :-  ~[//unix]
          [%pass /bone/~nec/0/1 %g %plea ~nec %g /ge/pub [%0 %s /foo]]
      ==
    ==
  :-  t4  |.  :-  %|
  =^  t5  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[/bone/~nec/0/1 //unix]
      [%plea ~nec %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[/init]  [%pass /sys/lag %a %heed ~nec]
          :-  ~[/init]  [%pass /sys/era %j %public-keys (sy ~nec ~)]
          :-  ~[/bone/~nec/0/1 //unix]
          [%pass /sys/req/~nec/pub %g %deal [~nec ~bud] %pub %watch /foo]
      ==
    ==
  :-  t5  |.  :-  %|
  =^  t6  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
      [%deal [~nec ~bud] %pub %watch /foo]
      :~  :-  ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
          [%give %unto %watch-ack ~]
      ==
    ==
  :-  t6  |.  :-  %|
  =^  t7  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /sys/req/~nec/pub  ~[/bone/~nec/0/1 //unix]
      [%gall %unto %watch-ack ~]
      :~  :-  ~[/bone/~nec/0/1 //unix]  [%give %done ~]
      ==
    ==
  :-  t7  |.  :-  %|
  =^  t8  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /bone/~nec/0/1  ~[//unix]
      [%gall %done ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%| `@`'lane-bar']
              0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
                2d9d.076e.6d00.0100.0223.9ae9.5000
      ==  ==
    ==
  :-  t8  |.  :-  %|
  =^  t9  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.3 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
            2d9d.076e.6d00.0100.0223.9ae9.5000
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~bud is your neighbor"]
          :-  ~[/sys/way/~bud/pub /init]  [%give %done ~]
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  :-  t9  |.  :-  %&  ~
--
