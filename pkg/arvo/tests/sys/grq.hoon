::  test gall request queue fix, which implicates ames and gall
::
/+  *test, v=test-ames-gall
|%
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  nec-bud:v
  ::  poke %sub to tell it to subscribe
  =/  =task:gall  [%deal [~nec ~nec] %sub %poke watch+!>(~bud)]
  =^  t1  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/foo] task]
      :~  :-  ~[/foo]  [%give %unto %poke-ack ~]
          :-  ~[/init]
          :*  %pass  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
              [%g %deal [~nec ~bud] %pub %watch /foo]
      ==  ==
    ==
  :-  t1  |.  :-  %|
  ::  handle gall passing the %watch to itself, which passes to ames
  =^  t2  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
      [%deal [~nec ~bud] %pub %watch /foo]
      :~  :-  ~[/init]  [%pass /sys/lag %a %heed ~bud]
          :-  ~[/init]  [%pass /sys/era %j %public-keys (sy ~bud ~)]
          :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
          [%pass /sys/way/~bud/pub %a %plea ~bud %g /ge/pub [%0 %s /foo]]
      ==
    ==
  :-  t2  |.  :-  %|
  ::  pass %plea to ames, which gives a packet to vere
  =^  t3  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      :-  ~[/sys/way/~bud/pub /use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
      [%plea ~bud %g /ge/pub [%0 %s /foo]]
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
  ::  send packet across the network
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
  ::  handle pass from ames to gall, which passes the %watch to itself
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
  ::  gall runs %pub with %watch, gives ack to itself
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
  ::  gall gives ack to ames
  =^  t7  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /sys/req/~nec/pub  ~[/bone/~nec/0/1 //unix]
      [%gall %unto %watch-ack ~]
      :~  :-  ~[/bone/~nec/0/1 //unix]  [%give %done ~]
      ==
    ==
  :-  t7  |.  :-  %|
  ::  ames hears ack from gall, sends over the network
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
  ::  ames hears watch-ack packet, gives to gall
  =^  t9  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.3 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
            2d9d.076e.6d00.0100.0223.9ae9.5000
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~bud is your neighbor"]
          :-  ~[/sys/way/~bud/pub /use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
          [%give %done ~]
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  :-  t9  |.  :-  %|
  ::  gall gives watch-ack to itself
  =^  t10  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.3 0xdead.beef *roof]
      :+  /sys/way/~bud/pub
        ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
      [%ames %done ~]
      :~  :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub /init]
          [%give %unto %watch-ack ~]
      ==
    ==
  :-  t10  |.  :-  %|
  ::  gall delivers %watch-ack to %sub
  =^  t11  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.3 0xdead.beef *roof]
      :+  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub
        ~[/init]
      [%gall %unto %watch-ack ~]
      ~
    ==
  ::  start the clog and kick process; give clog to publisher gall
  :-  t11  |.  :-  %|
  =^  t12  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.4 0xbeef.dead *roof]
      :+  /sys/lag  ~[/init]
      [%ames %clog ~nec]
      :~  :-  ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
          [%give %unto %kick ~]
      ==
    ==
  :-  t12  |.  :-  %|
  ::  gall gives %kick %boon to ames
  =^  t13  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.4 0xbeef.dead *roof]
      :+  /sys/req/~nec/pub  ~[/bone/~nec/0/1 //unix]
      [%gall %unto %kick ~]
      :~  :-  ~[/bone/~nec/0/1 //unix]  [%give %boon %x ~]
      ==
    ==
  :-  t13  |.  :-  %|
  ::  ames gives kick over the network
  =^  t14  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.4 0xbeef.dead *roof]
      :+  /bone/~nec/0/1  ~[//unix]
      [%gall %boon %x ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%| `@`'lane-bar']
              0xa1fc.cd35.c730.9a00.07e0.90a2.f87c.3657.935e.
                4ca0.801d.3ddc.d400.0100.0223.bc18.1000
          ==
          :-  ~[/ames]  [%pass /pump/~nec/1 %b %wait ~1111.1.4..00.00.01]
      ==
    ==
  :-  t14  |.  :-  %&  ~
--
