::  test: %leave enqueued before %kick, %pub suspended before %leave arrives
::        %kick gets handled when %leave is outstanding on %gall,
::        triggers %cork; both %leave and %cork are in-flight, un-acked
::        the %cork arrives first; receiver drops it since it has num > last-acked
::                               ; %leave still pending
::
::   Current:
::
::          - agent is suspended
::          - %leave arrives; get's flubbed:
::            - we roll back to last-ack; drop pending-vane-ack
::          - agent is revived; remote %spur is sent; %goad resumes flow:
::            - %leave is sent and handled:
::            - %cork is sent and handled
::          - flow is corked
::
::   TODO:
::          - %ack for %cork arrives first, flows gets deleted
::          - %boon %flub arrives:
::             - the flow then gets halted and the remote %flub is sent,
::               but when it's handled the %flow is closed:
::          crash!
::
::
/+  *test, v=test-ames-gall, test-pub, test-sub
|%
++  find-blob
  |=  mvs=(list move:ames-bunt:v)
  |-  ^-  blob:ames
  ?>  ?=(^ mvs)
  ?.  ?=([* %give %send * @ux] i.mvs)
    $(mvs t.mvs)
  blob.p.card.i.mvs
::
++  dbug  `?`&
++  test-watch
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud-zod:v life=[nec=2 bud=3 zod=1] rift=[nec=0 bud=0 zod=0])
  ::  uncomment to turn on verbose debug output
  =^  *  ames.nec
   (ames-call:v ames.nec ~[/none] [%spew ~[%msg %snd %rcv %odd %rot]] *roof)
  =^  *  ames.bud
   (ames-call:v ames.bud ~[/none] [%spew ~[%msg %snd %rcv %odd %rot]] *roof)

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
  ::
  ::  agent subscription established; start /gf flow subscription
  ::
  ~?  >  dbug  'send /gf plea'
  =^  t-flub  ames.nec
    %:  ames-call:v  ames.nec
      ~[/sys/flu/~bud /init]
      ^-  task:ames  [%plea ~bud %g /gf %0 ~]
      *roof
    ==
  ::  inject /gf %plea
  ::
  ~?  >  dbug  'inject /gf %plea'
  =^  t-flub-to-gall  ames.bud
    %:  ames-call:v  ames.bud
      ~[//unix]
      :*  %hear  [%& ~nec]
          (find-blob t-flub)
      ==
      *roof
    ==
  ~?  >  dbug  'give /gf %plea to gall'
  =^  t-flub-ack  gall.bud
    %+  gall-call:v  gall.bud
    :+  ~[/bone/~nec/0/5 //unix]
      [%plea ~nec %g /gf payload=[0 0]]
    *roof
  ::
  ~?  >  dbug  'give /gf %plea %done to %ames; produce ack'
  =^  t-flub-ack-send  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/5  ~[//unix]
      [%gall %done error=~]
      *roof
    ==
    ~?  >  dbug  'ingest /gf plea ack; give to %gall'
  =^  t-flub-get-ack  ames.nec
    %:  ames-call:v  ames.nec
      ~[//unix]
      :*  %hear  [%& ~bud]
          (find-blob t-flub-ack-send)
      ==
      *roof
    ==

  =^  t-flub-done  gall.nec
    %+  gall-take:v  gall.nec
    :*  /sys/flu/~bud  [/init]~
        [%ames %done error=~]
        *roof
    ==
  ::
  ::  subscription flow is establish; let's use it after the flow has been corked
  ::
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
  ::  before hearing the %kick, the ~nec %leaves the subscription
  ::
  ~?  >  dbug  '~nec: poke %sub with %leave'
  =^  *  gall.nec
    %:  gall-call:v  gall.nec
        ~[/foo]
        [%deal [~nec ~nec /] %sub %poke leave+!>(~bud)]
        *roof
    ==
  ::  l2: gall.nec routes %deal %leave internally
  ::  duct uses the ap-nonce-wire for the existing subscription (nonce=1)
  ::
  ~?  >  dbug  '~nec: route %leave deal'
  =^  t-leave  gall.nec
    %:  gall-call:v  gall.nec
        ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
        [%deal [~nec ~bud /gall/sub] %pub %leave ~]
        *roof
    ==
  =^  t-leave-out  ames.nec
    %:  ames-call:v  ames.nec
        :~  /sys/way/~bud/pub
            /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
            /init
        ==
        [%plea ~bud %g /ge/pub [%0 %u ~]]
        *roof
    ==
  ::  now gall.nec should have an outstanding leave in its unacked queue
  ::
  ::  ~bud suspends %pub BEFORE the %leave packet arrives
  ::
  ~?  >  dbug  '~bud suspends %pub'
  =^  t-idle  gall.bud
    %:  gall-call:v  gall.bud
        ~[/init]
        [%idle %pub]
        *roof
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
      ~  :: since we left the subscription it doesn't exist anymore so we don't trigger the new %watch
    ==
  :-  t17  |.  :-  %|
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
          :*  %give  %send  [%& ~bud]  ::  this is message num=3 (since the %leave is still outstanding)
               0x51.201d.35c6.5c33.5fe4.af83.861f.bc5e.5c6c.7600.12f0.
               d0b9.c6ef.9f14.169d.b17e.3b3c.64b7.4600.0200.0132.ab71.5800
          ==
          ::  %leave still outstanding
          ::
          :: :-  ~[/ames]  [%pass /pump/~bud/0 %b %wait ~1111.1.5..00.02.00]  :: XX why not?
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
  ::  publisher ames hears %cork, passes to itself
  ~?  >  dbug  'publisher ames hears %cork, %leave pending; drop'
  :-  t21  |.  :-  %|
  =^  t27  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.8 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0x51.201d.35c6.5c33.5fe4.af83.861f.bc5e.5c6c.7600.12f0.
          d0b9.c6ef.9f14.169d.b17e.3b3c.64b7.4600.0200.0132.ab71.5800
      ==
      :: :~  :-  ~[//unix]  [%pass /bone/~nec/0/1 %a %deep %cork ~nec 1]
      :: ==
      ~
    ==
  ::  the %leave arrives now, but the agent is not running
  ::
  ~?  >  dbug  'inject %leave %plea; trigger %boon %flub'
  =^  t-remote-flub  ames.bud
    %:  ames-call:v  ames.bud
      ~[//unix]
      :*  %hear  [%& ~nec]
          (find-blob t-leave-out)
      ==
      *roof
    ==
  =^  t-flub-done  gall.bud
    %+  gall-call:v  gall.bud
    :*  ~[/bone/~nec/0/1 //unix]
        [%plea ship=~nec plea=[vane=%g path=/ge/pub payload=[0 117 0]]]
        *roof
    ==
  ~?  >  dbug  'give %flub to %ames'
  =^  t-remote-flub  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/1  ~[//unix]
      [%gall %flub [blocked=%.n dap=[~ %pub]]]
      *roof
    ==
  ~?  >  dbug  'handle %deep %halt'
  =^  t-deep-flub  ames.bud
    %:  ames-call:v  ames.bud
      ~[/flub //unix]
      [%deep deep=[%halt ship=~nec agent=%pub bone=1]]
      *roof
    ==
  ~?  >  dbug  'pass %halt to %gall'
  =^  t-halt-flow  gall.bud
    %+  gall-call:v  gall.bud
    :*  ~[/flub //unix]
        [%halt ship=~nec agent=%pub bone=0]
        *roof
    ==
  ~?  >  dbug  'give remote %boon %flub to %ames'
  =^  t-remote-flub  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/5  ~[//unix]
      [%gall %boon payload=[0 1.651.862.630 6.452.592 8]]
      *roof
    ==
  ~?  >  dbug  'get remote %flub'
  =^  t-get-flub  ames.nec
    %:  ames-call:v  ames.nec
      ~[//unix]
      :*  %hear  [%& ~nec]
          (find-blob t-remote-flub)
      ==
      *roof
    ==
  ::  ingest %ack for remote flub
  ::
  ~?  >  dbug  'ingest %ack for remote flub'
  =^  t-deep-flub  ames.bud
    %:  ames-call:v  ames.bud
      ~[//unix]
      :*  %hear  [%& ~nec]
          (find-blob t-get-flub)
      ==
      *roof
    ==
  =^  t-flub-received  gall.nec
    %+  gall-take:v  gall.nec
    :*  /sys/flu/~bud  [/init]~
        [%ames %boon payload=[0 1.651.862.630 6.452.592 8]]
        *roof
    ==
  =^  t-halt-flow  ames.nec
    %:  ames-call:v  ames.nec
      ~[/remote-flub //unix]
      [%halt ship=~bud agent=%pub bone=0]
      *roof
    ==
  ::  at this point the flow is halted on both ends; %leave and %cork are outstanding
  ::  %cork is stored in .live-messages
  ::
  ::  now we unhalt the flow by reviving the agent
  ::
  ~?  >  dbug  '~bud revives %pub'
  =^  t-revive  gall.bud
    %+  gall-call:v  gall.bud
    [~[/load] load/[[%pub [~bud %base da+~1111.1.1] test-pub]~] *roof]
  ::  ames gets a %spur to un-halt the backward flow, locally
  ::  and a %boon %spur to un-halt the forward flow, over the wire
  ::
  ~?  >  dbug  'give remote %boon %spur to %ames'
  =^  t-remote-spur  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/5  ~[//unix]  :: /gf flow
      [%gall %boon payload=[0 1.920.299.123 6.452.592]]
      *roof
    ==
  =^  *  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/1  ~[//unix]  ::  agent flow
      [%gall %spur ~]
      *roof
    ==
  ~?  >  dbug  'ingest remote %spur'
  =^  t-handle-spur  ames.nec
    %:  ames-call:v  ames.nec
      ~[//unix]
      :*  %hear  [%& ~bud]
          (find-blob t-remote-spur)
      ==
      *roof
    ==
  =^  t-spur-received  gall.nec
    %+  gall-take:v  gall.nec
    :*  /sys/flu/~bud  [/init]~
        [%ames %boon payload=[0 1.920.299.123 6.452.592]]
        *roof
    ==
  ::
  =^  t-handle-goad  ames.nec
    %:  ames-call:v  ames.nec
      :~  /sys/way/~bud/pub
          /use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud
          /init
      ==
      [%goad ~bud]
      *roof
    ==
  =^  t-get-packets  ames.bud
    %:  ames-call:v  ames.bud
      ~[//unix]
      :*  %hear  [%& ~nec]
          (find-blob t-handle-goad)
      ==
      *roof
    ==
  ::  inject %flub plea and handle it
  ::
  =^  t-leave-done  gall.bud
    %+  gall-call:v  gall.bud
    :*  ~[/bone/~nec/0/1 //unix]
        [%plea ship=~nec plea=[vane=%g path=/ge/pub payload=[0 117 0]]]
        *roof
    ==
  ::  give %done, produce %ack for %leave, handle %deal %leave
  ::
  =^  *  gall.bud
    %:  gall-call:v  gall.bud
        ~[/sys/req/~nec/pub /bone/~nec/0/1 //unix]
        [%deal [~nec ~bud /] %pub %leave ~]
        *roof
    ==
  =^  t-flub-ack  ames.bud
    %:  ames-take:v  ames.bud
      /bone/~nec/0/1  ~[//unix]  ::  agent flow
      [%gall %done ~]
      *roof
    ==
  ::   get ack for %leave, set %wait for %cork
  ::
  =^  t-get-flub  ames.nec
    %:  ames-call:v  ames.nec
      ~[//unix]
      :*  %hear  [%& ~bud]
          (find-blob t-flub-ack)
      ==
      *roof
    ==
  =^  t-get-done  gall.nec
    %+  gall-take:v  gall.nec
    :*  /sys/way/~bud/pub   ~[/use/sub/0w1.d6Isf/out/~bud/pub/1/sub-foo/~bud /init]
        [%ames %done ~]
        *roof
    ==
  ::  this would enqueue a new %cork into %ames, but the flow is in closing
  ::  and a cork is already outstanding
  ::
  ::  publisher ames hears %cork, passes to itself
  ~?  >  dbug  'publisher ames hears again %cork,; handle'
  =^  t-recork  ames.bud
    %:  ames-check-call:v  ames.bud
      [~1111.1.8 0xbeef.dead *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~nec]
          0x51.201d.35c6.5c33.5fe4.af83.861f.bc5e.5c6c.7600.12f0.
          d0b9.c6ef.9f14.169d.b17e.3b3c.64b7.4600.0200.0132.ab71.5800
      ==
      :~  :-  ~[//unix]  [%pass /bone/~nec/0/1 %a %deep %cork ~nec 1]
      ==
    ==
  ::
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
              0xb5.a401.9900.0436.7af9.f42f.63db.1ecb.
              f14f.4558.d63f.1c00.0100.0223.b32f.e804
      ==  ==
    ==
  ::  subscriber ames hears %cork ack, sends $kill to self
  ~?  >  dbug  'subscriber ames hears %cork ack, sends $kill to self'
  :-  t29  |.  :-  %|
  =^  t33  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.10 0xdead.beef *roof]
      :-  ~[//unix]
      :*  %hear  [%& ~bud]
          0xb5.a401.9900.0436.7af9.f42f.63db.1ecb.
          f14f.4558.d63f.1c00.0100.0223.b32f.e804
      ==
      :~  :-  ~[//unix]
          [%pass /bone/~bud/0/0 %a %deep %kill ~bud 0]
        ::
          :-  ~[/ames]
          ::  this the timer for the %leave
          ::
          [%pass /pump/~bud/0 %b %rest ~1111.01.01..00.02.00]
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
  ::
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
  ==
--
