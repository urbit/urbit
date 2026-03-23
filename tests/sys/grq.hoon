::  test gall subscription nonce incrementation and ames flow killing
::
/+  *test, v=test-ames-gall
|%
++  dbug  `?`&
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud-zod:v life=[nec=2 bud=3 zod=1] rift=[nec=0 bud=0 zod=0])
  ::  uncomment to turn on verbose debug output
  :: =^  *  ames.nec
  ::  (ames-call:v ames.nec ~[/none] [%spew ~[%msg %snd %rcv %odd %rot]] *roof)
  :: =^  *  ames.bud
  ::  (ames-call:v ames.bud ~[/none] [%spew ~[%msg %snd %rcv %odd %rot]] *roof)
  ::
  =^  *  ames.nec  (ames-call:v ames.nec ~[/none] [%load %ames] *roof)
  =^  *  ames.bud  (ames-call:v ames.bud ~[/none] [%load %ames] *roof)
  ::  poke %sub to tell it to subscribe
  ~?  >  dbug  'poke %sub to tell it to subscribe'
  =/  =task:gall  [%deal [~nec ~nec /] %sub %poke watch+!>(~bud)]
  =^  t1  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/foo] task]
      :~  :-  ~[/foo]  [%give %unto %poke-ack ~]
          :-  ~[/init]
          :*  %pass  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
              [%g %deal [~nec ~bud /gall/sub] %pub %watch /foo]
      ==  ==
    ==
  :-  t1  |.  :-  %|
  ::  handle gall passing the %watch to itself, which passes to ames
  ~?  >  dbug  'handle gall passing the %watch to itself, which passes to ames'
  =^  t2  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.1 0xdead.beef *roof]
      :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
      [%deal [~nec ~bud /] %pub %watch /foo]
      :~  :-  ~[/init]  [%pass /sys/era %j %public-keys (sy ~bud ~)]
          :-  ~[/init]  [%pass /sys/flu/~bud %a %plea ~bud %g /gf [%0 ~]]
          :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
          [%pass /sys/way/~bud/pub %a %plea ~bud %g /ge/pub [%0 %s /foo]]
      ==
    ==
  :-  t2  |.  :-  %|
  ::  subscriber ames handles %plea from gall, gives a packet to vere
  ~?  >  dbug  'subscriber ames handles %plea from gall, gives a packet to vere'
  =^  t3  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      :-  :~  /sys/way/~bud/pub
              /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
              /init
          ==
      [%plea ~bud %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~bud]
              0xae59.5b29.277b.22c1.20b7.a8db.9086.46df.31bd.f9bc.
              2633.7300.17d4.f5fc.8be5.8bfe.5c9d.36d9.2ea1.7cb3.
              8a00.0200.0132.8fd4.f000
          ==
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  :-  t3  |.  :-  %|
  ::  publisher ames hears %watch, passes to gall
  ~?  >  dbug  'publisher ames hears %watch, passes to gall'
  =^  t4  ames.bud
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
  :-  t4  |.  :-  %|
  ::  publisher gall hears %watch from ames, passes to itself
  ~?  >  dbug  'publisher gall hears %watch from ames, passes to itself'
  =^  t5  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[/bone/~nec/0/1 //unix]
      [%plea ~nec %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[/init]  [%pass /sys/era %j %public-keys (sy ~nec ~)]
          :-  ~[/bone/~nec/0/1 //unix]
          [%pass /sys/req/~nec/pub %g %deal [~nec ~bud /] %pub %watch /foo]
      ==
    ==
  :-  t5  |.  :-  %|
  ::  publisher gall runs %pub with %watch, gives ack to itself
  ~?  >  dbug  'publisher gall runs %pub with %watch, gives ack to itself'
  =^  t6  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
      [%deal [~nec ~bud /] %pub %watch /foo]
      :~  :-  ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
          [%give %unto %watch-ack ~]
      ==
    ==
  :-  t6  |.  :-  %|
  ::  gall gives ack to ames
  ~?  >  dbug  'gall gives ack to ames'
  =^  t7  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /sys/req/~nec/pub  ~[/bone/~nec/0/1 //unix]
      [%gall %unto %watch-ack ~]
      :~  :-  ~[/bone/~nec/0/1 //unix]  [%give %done ~]
      ==
    ==
  :-  t7  |.  :-  %|
  ::  publisher ames hears ack from gall, sends over the network
  ~?  >  dbug  'publisher ames hears ack from gall, sends over the network'
  =^  t8  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.2 0xbeef.dead *roof]
      :+  /bone/~nec/0/1  ~[//unix]
      [%gall %done ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
              2d9d.076e.6d00.0100.0223.9ae9.5004
      ==  ==
    ==
  :-  t8  |.  :-  %|
  ::  subscriber ames hears watch-ack packet, gives to gall
  ~?  >  dbug  'subscriber ames hears watch-ack packet, gives to gall'
  =^  t9  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.3 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0x2.0219.8100.0485.5530.3c88.9068.3cc6.484e.
          2d9d.076e.6d00.0100.0223.9ae9.5004
      ==
      :~  :-  ~[//unix]  [%pass /qos %d %flog %text "; ~bud is your neighbor"]
          :-  :~  /sys/way/~bud/pub
                  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
                  /init
              ==
          [%give %done ~]
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  :-  t9  |.  :-  %|
  ::  gall gives %done to itself
  ~?  >  dbug  'gall gives %done to itself'
  =^  t10  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.3 0xdead.beef *roof]
      :+  /sys/way/~bud/pub
        ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
      [%ames %done ~]
      :~  :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
          [%give %unto %watch-ack ~]
      ==
    ==
  :-  t10  |.  :-  %|
  ::  gall gives watch-ack to itself
  ~?  >  dbug  'gall gives watch-ack to itself'
  =^  t11  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.3 0xdead.beef *roof]
      :+  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
        ~[/init]
      [%gall %unto %watch-ack ~]
      ~
    ==
  :-  t11  |.  :-  %|
  ::  gall gives %kick %boon to ames
  ~?  >  dbug  'gall gives %kick %boon to ames'
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
  ~?  >  dbug  'ames gives kick over the network'
  =^  t14  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.4 0xbeef.dead *roof]
      :+  /bone/~nec/0/1  ~[//unix]
      [%gall %boon %x ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0xa1fc.cd35.c730.9a00.07e0.90a2.f87c.3657.935e.
              4ca0.801d.3ddc.d400.0100.0223.bc18.1000
          ==
          :-  ~[/ames]  [%pass /pump/~nec/1 %b %wait ~1111.1.4..00.00.01]
      ==
    ==
  :-  t14  |.  :-  %|
  ::  subscriber ames receives kick, gives to gall and gives ack to unix
  ~?  >  dbug  'subscriber ames receives kick, gives to gall and gives ack to unix'
  =^  t15  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.5 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0xa1fc.cd35.c730.9a00.07e0.90a2.f87c.3657.935e.
          4ca0.801d.3ddc.d400.0100.0223.bc18.1000
      ==
      :~  :-  :~  /sys/way/~bud/pub
                  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
                  /init
              ==
          [%give %boon %x ~]
          :-  ~[//unix]
          :*  %give  %send  [%& ~bud]
              0xfe.e208.da00.0491.bf7f.9594.2ddc.0948.
              9de0.3906.b678.6e00.0200.0132.e55d.5004
      ==  ==
    ==
  :-  t15  |.  :-  %|
  ::  subscriber gall receives kick %boon from ames, gives to self
  ~?  >  dbug  'subscriber gall receives kick %boon from ames, gives to self'
  =^  t16  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.5 0xdead.beef *roof]
      :+  /sys/way/~bud/pub
        ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
      [%ames %boon %x ~]
      :~  :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
          [%give %unto %kick ~]
          :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
          [%pass /sys/way/~bud/pub %a %cork ~bud]
      ==
    ==
  ::  subscriber gall receives %kick from itself
  ~?  >  dbug  'subscriber gall receives %kick from itself'
  =^  t17  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.5 0xdead.beef *roof]
      :+  /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
        ~[/init]
      [%gall %unto %kick ~]
      :~  :-  ~[/init]
          :*  %pass  /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
              [%g %deal [~nec ~bud /gall/sub] %pub %watch /foo]
      ==  ==
    ==
  :-  t17  |.  :-  %|
  ::  gall receives %deal %watch from itself, passes to ames
  ~?  >  dbug  'gall receives %deal %watch from itself, passes to ames'
  =^  t18  gall.nec
    %:  gall-check-call:v  gall.nec
      [~1111.1.5 0xdead.beef *roof]
      :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud /init]
      [%deal [~nec ~bud /] %pub %watch /foo]
      :~  :-  ~[/use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud /init]
          [%pass /sys/way/~bud/pub %a %plea ~bud %g /ge/pub [%0 %s /foo]]
      ==
    ==
  :-  t18  |.  :-  %|
  ::  subscriber ames sends new %watch
  ~?  >  dbug  'subscriber ames sends new %watch'
  =^  t19  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.5 0xdead.beef *roof]
      :-  :~  /sys/way/~bud/pub
              /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
              /init
          ==
      [%plea ~bud %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~bud]
              0xfe.9174.6d7c.e042.4ea7.cf3c.08da.3acf.68ec.3bd1.1f2c.abfe.f500.
              1897.c42e.a3ec.2159.86d6.e2f1.b344.9d06.b600.0200.0132.ebe7.8800
          ==
          :-  ~[/ames]  [%pass /pump/~bud/4 %b %wait ~1111.1.5..00.00.01]
      ==
    ==
  :-  t19  |.  :-  %|
  ::  subscriber ames sends %cork
  ~?  >  dbug  'subscriber ames sends %cork'
  =^  t20  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.5 0xdead.beef *roof]
      :-  :~  /sys/way/~bud/pub
              /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
              /init
          ==
      [%cork ~bud]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~bud]
              0xb.130c.ab37.ca24.49cd.aecb.23ba.70f1.6f1c.4d00.124e.c9a5.
              3413.3843.d81c.47c4.7040.6e62.3700.0200.0132.e1ab.9000
          ==
          :-  ~[/ames]  [%pass /pump/~bud/0 %b %wait ~1111.1.5..00.02.00]
      ==
    ==
  ::  publisher ames hears %kick ack
  ~?  >  dbug  'publisher ames hears %kick ack'
  :-  t20  |.  :-  %|
  =^  t21  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.6 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0xfe.e208.da00.0491.bf7f.9594.2ddc.0948.
          9de0.3906.b678.6e00.0200.0132.e55d.5004
      ==
      :~  :-  ~[/ames]  [%pass /pump/~nec/1 %b %rest ~1111.1.4..00.00.01]
      ==
    ==
  ::  publisher ames hears new %watch
  ~?  >  dbug  'publisher ames hears new %watch'
  :-  t21  |.  :-  %|
  =^  t22  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.7 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0xfe.9174.6d7c.e042.4ea7.cf3c.08da.3acf.68ec.3bd1.1f2c.abfe.f500.
          1897.c42e.a3ec.2159.86d6.e2f1.b344.9d06.b600.0200.0132.ebe7.8804
      ==
      :~  :-  ~[//unix]
          [%pass /bone/~nec/0/5 %g %plea ~nec %g /ge/pub [%0 %s /foo]]
      ==
    ==
  ::  publisher gall hears new %watch, passes to self
  ~?  >  dbug  'publisher gall hears new %watch, passes to self'
  :-  t22  |.  :-  %|
  =^  t23  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.7 0xbeef.dead *roof]
      :-  ~[/bone/~nec/0/5 //unix]
      [%plea ~nec %g /ge/pub [%0 %s /foo]]
      :~  :-  ~[/bone/~nec/0/5 //unix]
          [%pass /sys/req/~nec/pub %g %deal [~nec ~bud /] %pub %watch /foo]
      ==
    ==
  ::  publisher gall runs :pub's +on-watch, gives ack to self
  ~?  >  dbug  'publisher gall runs :pub\'s +on-watch, gives ack to self'
  :-  t23  |.  :-  %|
  =^  t24  gall.bud
    %:  gall-check-call:v  gall.bud
      [~1111.1.7 0xbeef.dead *roof]
      :-  ~[/sys/req/~nec/pub /bone/~nec/0/5 //unix]
      [%deal [~nec ~bud /] %pub %watch /foo]
      :~  :-  ~[/sys/req/~nec/pub /bone/~nec/0/5 //unix]
          [%give %unto %watch-ack ~]
      ==
    ==
  ::  publisher gall hears %watch-ack, gives to ames
  ~?  >  dbug  'publisher gall hears %watch-ack, gives to ames'
  :-  t24  |.  :-  %|
  =^  t25  gall.bud
    %:  gall-check-take:v  gall.bud
      [~1111.1.7 0xbeef.dead *roof]
      :+  /sys/req/~nec/pub  ~[/bone/~nec/0/5 //unix]
      [%gall %unto %watch-ack ~]
      :~  :-  ~[/bone/~nec/0/5 //unix]  [%give %done ~]
      ==
    ==
  ::  publisher ames hears done from gall, sends over the network
  ~?  >  dbug  'publisher ames hears done from gall, sends over the network'
  :-  t25  |.  :-  %|
  =^  t26  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.7 0xbeef.dead *roof]
      :+  /bone/~nec/0/5  ~[//unix]
      [%gall %done ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0x5f5.c27c.c400.0587.8b0d.0a5d.eb8e.39fa.
              49f4.4848.bfa6.f600.0100.0223.c98c.8804
      ==  ==
    ==
  ::  publisher ames hears %cork, passes to itself
  ~?  >  dbug  'publisher ames hears %cork, passes to itself'
  :-  t26  |.  :-  %|
  =^  t27  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.8 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0xb.130c.ab37.ca24.49cd.aecb.23ba.70f1.6f1c.4d00.124e.c9a5.
          3413.3843.d81c.47c4.7040.6e62.3700.0200.0132.e1ab.9004
      ==
      :~  :-  ~[//unix]  [%pass /bone/~nec/0/1 %a %deep %cork ~nec 1]
      ==
    ==
  :-  t27  |.  :-  %|
  ::  publisher ames hear cork plea from self, give %done to self
  ~?  >  dbug  'publisher ames hear cork plea from self, give %done to self'
  =^  t28  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.8 0xbeef.dead *roof]
      :-  ~[/bone/~nec/0/1 //unix]
      [%deep %cork ~nec 1]
      :~  :-  ~[/bone/~nec/0/1 //unix]  [%give %done ~]
      ==
    ==
  ::  publisher ames hears cork done from self, sends ack and $cork to self
  ~?  >  dbug  'publisher ames hears cork done from self, sends ack and $cork to self'
  :-  t28  |.  :-  %|
  =^  t29  ames.bud
    %:  ames-check-take:v  ames.bud
      [~1111.1.8 0xbeef.dead *roof]
      :+  /bone/~nec/0/1
        ~[//unix]
      [%ames %done ~]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~nec]
              0x5f.f966.8e00.0449.bdec.9006.c7e5.1237.
              1d87.53fe.d7bb.ad00.0100.0223.c6a8.5804
      ==  ==
    ==
  ::  subscriber ames hears %watch-ack, gives to gall
  ~?  >  dbug  'subscriber ames hears %watch-ack, gives to gall'
  :-  t29  |.  :-  %|
  =^  t30  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.9 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0x5f5.c27c.c400.0587.8b0d.0a5d.eb8e.39fa.
          49f4.4848.bfa6.f600.0100.0223.c98c.8804
      ==
      :~  :-  :~  /sys/way/~bud/pub
                  /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
                  /init
              ==
          [%give %done ~]
          :-  ~[/ames]  [%pass /pump/~bud/4 %b %rest ~1111.1.5..00.00.01]
      ==
    ==
  ::  subscriber gall hears new %watch-ack from ames, gives to self
  ~?  >  dbug  'subscriber gall hears new %watch-ack from ames, gives to self'
  :-  t30  |.  :-  %|
  =^  t31  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.9 0xdead.beef *roof]
      :+  /sys/way/~bud/pub
        :~  /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
            /init
        ==
      [%ames %done ~]
      :~  :-  :~  /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
                  /init
              ==
          [%give %unto %watch-ack ~]
      ==
    ==
  ::  subscriber gall hears new %watch-ack from self, tells :sub
  ~?  >  dbug  'subscriber gall hears new %watch-ack from self, tells :sub'
  :-  t31  |.  :-  %|
  =^  t32  gall.nec
    %:  gall-check-take:v  gall.nec
      [~1111.1.9 0xdead.beef *roof]
      :+  /use/sub/0w1.d6Isf/out/~bud/pub/2/sub-foo/~bud
        ~[/init]
      [%gall %unto %watch-ack ~]
      ~
    ==
  ::  subscriber ames hears %cork ack, sends $kill to self
  ~?  >  dbug  'subscriber ames hears %cork ack, sends $kill to self'
  :-  t32  |.  :-  %|
  =^  t33  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.10 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0x5f.f966.8e00.0449.bdec.9006.c7e5.1237.
          1d87.53fe.d7bb.ad00.0100.0223.c6a8.5804
      ==
      :~  :-  ~[//unix]
          [%pass /bone/~bud/0/0 %a %deep %kill ~bud 0]
        ::
          :-  ~[/ames]
          [%pass /pump/~bud/0 %b %rest ~1111.1.5..00.02.00]
      ==
    ==
  ::  subscriber ames hears $kill from self, deletes the flow
  ~?  >  dbug  'subscriber ames hears $kill from self, deletes the flow'
  :-  t33  |.  :-  %|
  =^  t34  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.10 0xdead.beef *roof]
      :-  ~[/bone/~bud/0/0 //unix]
      [%deep %kill ~bud 0]
      ~
    ==
  :-  t34  |.  :-  %&
  ;:  weld
    %+  expect-eq
      !>  (sy 0 ~)
      !>  =<  corked
          %:  ames-scry-peer:v
            ames.nec
            [~1111.1.10 0xdead.beef *roof]
            [~nec ~bud]
          ==
  ::
    %+  expect-eq
      !>  (sy 1 ~)
      !>  =<  corked
          %:  ames-scry-peer:v
            ames.bud
            [~1111.1.8 0xbeef.dead *roof]
            [~bud ~nec]
          ==
  ::
    %+  expect-eq
      !>  2
      !>  %:  gall-scry-nonce:v
            gall.nec
            [~1111.1.10 0xdead.beef *roof]
            ~nec  %sub
            [~bud %pub /sub-foo/~bud]
          ==
  ==
--
