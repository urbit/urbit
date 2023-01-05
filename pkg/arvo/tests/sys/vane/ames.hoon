/+  *test
/=  ames  /sys/vane/ames
/=  jael  /sys/vane/jael
::  construct some test fixtures
::
=/  nec     (ames ~nec)
=/  bud     (ames ~bud)
=/  marbud  (ames ~marbud)
::
=/  our-comet   ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud
=/  our-comet2  ~togdut-rosled-fadlev-siddys--botmun-wictev-sapfus-marbud
=/  comet   (ames our-comet)
=/  comet2  (ames our-comet2)
::
=.  now.nec        ~1111.1.1
=.  eny.nec        0xdead.beef
=.  life.ames-state.nec  2
=.  rof.nec  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.nec  (pit:nu:crub:crypto 512 (shaz 'nec'))
=/  nec-pub  pub:ex:crypto-core.ames-state.nec
=/  nec-sec  sec:ex:crypto-core.ames-state.nec
::
=.  now.bud        ~1111.1.1
=.  eny.bud        0xbeef.dead
=.  life.ames-state.bud  3
=.  rof.bud  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.bud  (pit:nu:crub:crypto 512 (shaz 'bud'))
=/  bud-pub  pub:ex:crypto-core.ames-state.bud
=/  bud-sec  sec:ex:crypto-core.ames-state.bud
::
=.  now.marbud        ~1111.1.1
=.  eny.marbud        0xbeef.beef
=.  life.ames-state.marbud  4
=.  rof.marbud  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.marbud  (pit:nu:crub:crypto 512 (shaz 'marbud'))
=/  marbud-pub  pub:ex:crypto-core.ames-state.marbud
=/  marbud-sec  sec:ex:crypto-core.ames-state.marbud
::
=.  now.comet        ~1111.1.1
=.  eny.comet        0xbeef.cafe
=.  life.ames-state.comet  1
=.  rof.comet  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.comet
  %-  nol:nu:crub:crypto
  0w9N.5uIvA.Jg0cx.NCD2R.o~MtZ.uEQOB.9uTbp.6LHvg.0yYTP.
  3q3td.T4UF0.d5sDL.JGpZq.S3A92.QUuWg.IHdw7.izyny.j9W92
=/  comet-pub  pub:ex:crypto-core.ames-state.comet
=/  comet-sec  sec:ex:crypto-core.ames-state.comet
::
=.  now.comet2        ~1111.1.1
=.  eny.comet2        0xcafe.cafe
=.  life.ames-state.comet2  1
=.  rof.comet2  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.comet2  (pit:nu:crub:crypto 512 0v1eb4)
=/  comet2-pub  pub:ex:crypto-core.ames-state.comet2
=/  comet2-sec  sec:ex:crypto-core.ames-state.comet2
::
=/  nec-sym  (derive-symmetric-key:ames bud-pub nec-sec)
=/  bud-sym  (derive-symmetric-key:ames nec-pub bud-sec)
?>  =(nec-sym bud-sym)
=/  nec-marbud-sym  (derive-symmetric-key:ames marbud-pub nec-sec)
::
=/  marbud-sym  (derive-symmetric-key:ames marbud-pub comet-sec)
=/  marbud2-sym  (derive-symmetric-key:ames marbud-pub comet2-sec)
=/  bud-marbud-sym  (derive-symmetric-key:ames bud-pub marbud-sec)
::
=/  comet-sym  (derive-symmetric-key:ames bud-pub comet-sec)
::
=.  peers.ames-state.nec
  %+  ~(put by peers.ames-state.nec)  ~bud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=bud-sym
        life=3
        rift=0
        public-key=bud-pub
        sponsor=~nec
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%& ~nec]]
  [%known peer-state]
::
=.  peers.ames-state.nec
  %+  ~(put by peers.ames-state.nec)  ~marbud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=nec-marbud-sym
        life=5
        rift=0
        public-key=marbud-pub
        sponsor=~bud
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
::
=.  peers.ames-state.bud
  %+  ~(put by peers.ames-state.bud)  ~nec
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=nec-sym
        life=2
        rift=0
        public-key=nec-pub
        sponsor=~nec
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
::
=.  peers.ames-state.comet
  %+  ~(put by peers.ames-state.comet)  ~marbud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=marbud-sym
        life=5
        rift=0
        public-key=marbud-pub
        sponsor=~bud
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
=.  peers.ames-state.comet
  %+  ~(put by peers.ames-state.comet)  ~bud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=bud-marbud-sym
        life=3
        rift=0
        public-key=bud-pub
        sponsor=~bud
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
=.  peers.ames-state.comet2
  %+  ~(put by peers.ames-state.comet2)  ~marbud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=marbud2-sym
        life=5
        rift=0
        public-key=marbud-pub
        sponsor=~bud
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
=.  peers.ames-state.comet2
  %+  ~(put by peers.ames-state.comet2)  ~bud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=bud-marbud-sym
        life=3
        rift=0
        public-key=bud-pub
        sponsor=~bud
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
::  metamorphose
::
=>  .(nec +:(call:(nec) ~[//unix] ~ %born ~))
=>  .(bud +:(call:(bud) ~[//unix] ~ %born ~))
=>  .(comet +:(call:(comet) ~[//unix] ~ %born ~))
=>  .(comet2 +:(call:(comet2) ~[//unix] ~ %born ~))
::  helper core
::
=>
|%
++  move-to-packet
  |=  =move:ames
  ^-  [=lane:ames =blob:ames]
  ::
  ?>  ?=([%give %send *] +.move)
  [lane blob]:+>+.move
::
++  is-move-send
  |=  =move:ames
  ^-  ?
  ?=([%give %send *] card.move)
::
++  snag-packet
  |=  [index=@ud moves=(list move:ames)]
  ^-  [=lane:ames =blob:ames]
  ::
  %-  move-to-packet
  %+  snag  index
  (skim moves is-move-send)
::
++  call
  |=  [vane=_nec =duct =task:ames]
  ^-  [moves=(list move:ames) _nec]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (call:vane-core duct ~ task)
::
++  take
  |=  [vane=_nec =wire =duct =sign:ames]
  ^-  [moves=(list move:ames) _nec]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (take:vane-core wire duct ~ sign)
--
::  test core
::
|%
++  test-packet-encoding  ^-  tang
  ::
  =/  =packet:ames
    :*  [sndr=~nec rcvr=~bud]
        sndr-tick=0b10
        rcvr-tick=0b11
        origin=~
        content=0xdead.beef
    ==
  ::
  =/  encoded  (encode-packet:ames packet)
  =/  decoded  (decode-packet:ames encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-origin-encoding  ^-  tang
  ::
  =/  =packet:ames
    :*  [sndr=~nec rcvr=~bud]
        sndr-tick=0b10
        rcvr-tick=0b11
        origin=`0xbeef.cafe.beef
        content=0xdead.beef
    ==
  ::
  =/  encoded  (encode-packet:ames packet)
  =/  decoded  (decode-packet:ames encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-shut-packet-encoding  ^-  tang
  ::
  =/  =shut-packet:ames
    :+  bone=17  message-num=18
    [%& num-fragments=1 fragment-num=1 fragment=`@`0xdead.beef]
  ::
  =/  =packet:ames
    (encode-shut-packet:ames shut-packet nec-sym ~marnec ~marbud-marbud 3 17)
  ::
  =/  decoded  (decode-shut-packet:ames packet nec-sym 3 17)
  ::
  %+  expect-eq
    !>  shut-packet
    !>  decoded
::
::  Crypto failures are now non-deterministic
::
::  ++  test-shut-packet-associated-data  ^-  tang
::    ::
::    =/  =shut-packet:ames
::      :+  bone=17  message-num=18
::      [%& num-fragments=1 fragment-num=1 fragment=`@`0xdead.beef]
::    ::
::    =/  =packet:ames
::      (encode-shut-packet:ames shut-packet nec-sym ~marnec ~marbud-marbud 3 1)
::    ::
::    %-  expect-fail
::    |.((decode-shut-packet:ames packet nec-sym 3 17))
::
++  test-alien-encounter  ^-  tang
  ::
  =/  lane-foo=lane:ames  [%| `@ux``@`%lane-foo]
  ::
  =/  =plea:ames  [%g /talk [%first %post]]
  ::
  =/  =shut-packet:ames
    :*  bone=1
        message-num=1
        [%& num-fragments=1 fragment-num=0 (jam plea)]
    ==
  ::
  =/  =packet:ames
    %:  encode-shut-packet:ames
      shut-packet
      nec-sym
      ~bus
      ~bud
      sndr-life=4
      rcvr-life=3
    ==
  ::
  =/  =blob:ames   (encode-packet:ames packet)
  =^  moves1  bud  (call bud ~[//unix] %hear lane-foo blob)
  =^  moves2  bud
    =/  =point:ames
      :*  rift=0
          life=4
          keys=[[life=4 [crypto-suite=1 `@`nec-pub]] ~ ~]
          sponsor=`~bus
      ==
    %-  take
    :^  bud  /public-keys  ~[//unix]
    ^-  sign:ames
    [%jael %public-keys %full [n=[~bus point] ~ ~]]
  =^  moves3  bud  (call bud ~[//unix] %hear lane-foo blob)
  ::
  ;:  weld
    %+  expect-eq
      !>  [~[//unix] %pass /public-keys %j %public-keys [~bus ~ ~]]~
      !>  moves1
  ::
    %+  expect-eq
      !>  %-  sy
          :~  :^  ~[//unix]  %pass  /bone/~bus/0/1
              [%g %plea ~bus %g /talk [%first %post]]
          ::
              :^  ~[//unix]  %pass  /qos
              [%d %flog %text "; ~bus is your neighbor"]
          ==
      !>  (sy ,.moves3)
  ==
::
++  test-message-flow  ^-  tang
  ::  ~nec -> %plea -> ~bud
  ::
  =^  moves1  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves2  bud  (call bud ~[//unix] %hear (snag-packet 0 moves1))
  ::  ~bud -> %done -> ~nec
  ::
  =^  moves3  bud  (take bud /bone/~nec/0/1 ~[//unix] %g %done ~)
  =^  moves4  nec  (call nec ~[//unix] %hear (snag-packet 0 moves3))
  ::  ~bud -> %boon -> ~nec
  ::
  =^  moves5  bud  (take bud /bone/~nec/0/1 ~[//unix] %g %boon [%post 'first1'])
  =^  moves6  nec  (call nec ~[//unix] %hear (snag-packet 0 moves5))
  ::  ~nec -> %done -> ~bud (just make sure ~bud doesn't crash on ack)
  ::
  =^  moves7  bud  (call bud ~[//unix] %hear (snag-packet 0 moves6))
  ::
  ;:  weld
    %+  expect-eq
      !>  :~  [~[//unix] %pass /qos %d %flog %text "; ~nec is your neighbor"]
              :^  ~[//unix]  %pass  /bone/~nec/0/1
              [%g %plea ~nec %g /talk [%get %post]]
          ==
      !>  moves2
  ::
    %+  expect-eq
      !>  %-  sy
          :~  [~[/ames] %pass /pump/~bud/0 %b %rest ~1111.1.1..00.00.02]
              [~[//unix] %pass /qos %d %flog %text "; ~bud is your neighbor"]
              [~[/g/talk] %give %done error=~]
          ==
      !>  (sy ,.moves4)
  ::
    %+  expect-eq
      !>  [~[/g/talk] %give %boon [%post 'first1']]
      !>  (snag 0 `(list move:ames)`moves6)
  ==
::  +test-comet-message-flow: galaxy<->comet comms
::
::    same as test-message-flow, but ~nec will send a sendkeys packet to
::    request comet's self-attestation directly
::
++  test-comet-message-flow  ^-  tang
  ::=^  *       nec   (call nec ~[//nemo] %spew ~[%snd %rcv %odd %msg])
  ::=^  *     comet   (call comet ~[//nemo] %spew ~[%snd %rcv %odd %msg])
  ::
  =^  moves0  nec    (call nec ~[/g/talk] %plea our-comet %g /talk [%get %post])
  =^  moves1  comet  (call comet ~[//unix] %hear (snag-packet 0 moves0))
  =^  moves2  comet
    =/  =point:ames
      :*  rift=1
          life=2
          keys=[[life=2 [crypto-suite=1 `@`nec-pub]] ~ ~]
          sponsor=`~nec
      ==
    %-  take
    :^  comet  /public-keys  ~[//unix]
    ^-  sign:ames
    [%jael %public-keys %full [n=[~nec point] ~ ~]]
  ::  give comet's self-attestation to ~nec; at this point, we have
  ::  established a channel, and can proceed as usual
  ::
  =/  post  [%post 'first1!!']
  =^  moves3  nec    (call nec ~[//unix] %hear (snag-packet 0 moves2))
  %+  weld
    %-  expect-fail  |.
    (call nec ~[//unix] %hear (snag-packet 1 moves2))
  ::
  =^  moves4  comet  (call comet ~[//unix] %hear (snag-packet 0 moves3))
  =^  moves5  comet  (take comet /bone/~nec/1/1 ~[//unix] %g %done ~)
  =^  moves6  nec    (call nec ~[//unix] %hear (snag-packet 0 moves5))
  =^  moves7  comet  (take comet /bone/~nec/1/1 ~[//unix] %g %boon post)
  =^  moves8  nec    (call nec ~[//unix] %hear (snag-packet 0 moves7))
  ::
  ;:  weld
    %+  expect-eq
      !>  =-  [~[//unix] %pass /qos %d %flog %text -]
              "; ~nec is your neighbor"
      !>  (snag 0 `(list move:ames)`moves4)
  ::
    %+  expect-eq
      !>  =-  [~[//unix] %pass /qos %d %flog %text -]
              "; {<our-comet>} is your neighbor"
      !>  (snag 0 `(list move:ames)`moves6)
  ::
    %+  expect-eq
      !>  [~[/g/talk] %give %boon post]
      !>  (snag 0 `(list move:ames)`moves8)
  ==
::
++  test-comet-comet-message-flow  ^-  tang
  ::  same as test-message-flow, but the comets need to exchange
  ::  self-attestations to establish a channel
  ::
  =^  moves0  comet   (call comet ~[/g/talk] %plea our-comet2 %g /talk [%get %post])
  =^  moves1  comet2  (call comet2 ~[//unix] %hear (snag-packet 0 moves0))
  =^  moves2  comet   (call comet ~[//unix] %hear (snag-packet 0 moves1))
  ::  channel is now established; comet also emitted a duplicate
  ::  self-attestation, which we ignore
  ::
  =^  moves3  comet2  (call comet2 ~[//unix] %hear (snag-packet 0 moves2))
  =^  moves4  comet2  (call comet2 ~[//unix] %hear (snag-packet 1 moves2))
  =^  moves5  comet2  (take comet2 /bone/(scot %p our-comet)/0/1 ~[//unix] %g %done ~)
  =^  moves6  comet2  (take comet2 /bone/(scot %p our-comet)/0/1 ~[//unix] %g %boon [%post 'first1!!'])
  =^  moves7  comet   (call comet ~[//unix] %hear (snag-packet 0 moves5))
  =^  moves8  comet   (call comet ~[//unix] %hear (snag-packet 0 moves6))
  ::
  ;:  weld
    %+  expect-eq
      !>  [~[//unix] %pass /qos %d %flog %text "; {<our-comet>} is your neighbor"]
      !>  (snag 0 `(list move:ames)`moves4)
  ::
    %+  expect-eq
      !>  [~[//unix] %pass /qos %d %flog %text "; {<our-comet2>} is your neighbor"]
      !>  (snag 0 `(list move:ames)`moves7)
  ::
    %+  expect-eq
      !>  [~[/g/talk] %give %boon [%post 'first1!!']]
      !>  (snag 0 `(list move:ames)`moves8)
  ==
::
++  test-nack  ^-  tang
  ::  ~nec -> %plea -> ~bud
  ::
  =^  moves1  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves2  bud  (call bud ~[//unix] %hear (snag-packet 0 moves1))
  ::  ~bud -> nack -> ~nec
  ::
  =/  =error:ames  [%flub [%leaf "sinusoidal repleneration"]~]
  =^  moves3  bud  (take bud /bone/~nec/0/1 ~[/bud] %g %done `error)
  =^  moves4  nec  (call nec ~[//unix] %hear (snag-packet 0 moves3))
  ::  ~bud -> nack-trace -> ~nec
  ::
  =^  moves5  nec  (call nec ~[//unix] %hear (snag-packet 1 moves3))
  ::  ~nec -> ack nack-trace -> ~bud
  ::
  =^  moves6  bud  (call bud ~[//unix] %hear (snag-packet 0 moves5))
  ::
  %+  expect-eq
    !>  [~[/g/talk] %give %done `error]
    !>  (snag 1 `(list move:ames)`moves5)
::
++  test-old-ames-wire  ^-  tang
  =^  moves0  bud  (call bud ~[/g/hood] %spew [%odd]~)
  =^  moves1  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves2  bud  (call bud ~[//unix] %hear (snag-packet 0 moves1))
  =^  moves3  bud  (take bud /bone/~nec/1 ~[//unix] %g %done ~)
  %+  expect-eq
    !>  1
    !>  (lent `(list move:ames)`moves3)
::
++  test-dangling-bone  ^-  tang
  =^  moves0  bud  (call bud ~[/g/hood] %spew [%odd]~)
  ::  ~nec -> %plea -> ~bud
  ::
  =^  moves1  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves2  bud  (call bud ~[//unix] %hear (snag-packet 0 moves1))
  ::  ~bud receives a gift from %jael with ~nec's new rift
  ::
  =^  moves3  bud
    %-  take
    :^  bud  /public-keys  ~[//unix]
    ^-  sign:ames
    [%jael %public-keys %diff who=~nec %rift from=0 to=1]
  ::  %gall has a pending wire with the old rift, so sending a gift to
  ::  %ames on it will drop that request, not producing any moves
  ::
  =^  moves3  bud  (take bud /bone/~nec/0/1 ~[//unix] %g %done ~)
  ::
  %+  expect-eq
    !>  ~
    !>  (sy ,.moves3)
::
++  test-ames-flow-with-new-rift  ^-  tang
  ::  ~nec receives a gift from %jael with ~bud's new rift
  ::
  =^  moves1  nec
    %-  take
    :^  nec  /public-keys  ~[//unix]
    ^-  sign:ames
    [%jael %public-keys %diff who=~bud %rift from=0 to=1]
  ::  now we try a normal message flow using the new rift in the wire
  ::  ~nec -> %plea -> ~bud
  ::
  =^  moves2  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves3  bud  (call bud ~[//unix] %hear (snag-packet 0 moves2))
  ::  ~bud -> %done -> ~nec
  ::
  =^  moves4  bud  (take bud /bone/~nec/1/1 ~[//unix] %g %done ~)
  =^  moves5  nec  (call nec ~[//unix] %hear (snag-packet 0 moves4))
  ::  ~bud -> %boon -> ~nec
  ::
  =^  moves6  bud  (take bud /bone/~nec/1/1 ~[//unix] %g %boon [%post '¡hola!'])
  =^  moves7  nec  (call nec ~[//unix] %hear (snag-packet 0 moves6))
  ::  ~nec -> %done -> ~bud (just make sure ~bud doesn't crash on ack)
  ::
  =^  moves8  bud  (call bud ~[//unix] %hear (snag-packet 0 moves7))
  ::
  ;:  weld
    %+  expect-eq
      !>  :~  [~[//unix] %pass /qos %d %flog %text "; ~nec is your neighbor"]
              :^  ~[//unix]  %pass  /bone/~nec/0/1
              [%g %plea ~nec %g /talk [%get %post]]
          ==
      !>  moves3
  ::
    %+  expect-eq
      !>  %-  sy
          :~  [~[/ames] %pass /pump/~bud/0 %b %rest ~1111.1.1..00.00.03]
              [~[//unix] %pass /qos %d %flog %text "; ~bud is your neighbor"]
              [~[/g/talk] %give %done error=~]
          ==
      !>  (sy ,.moves5)
  ::
    %+  expect-eq
      !>  [~[/g/talk] %give %boon [%post '¡hola!']]
      !>  (snag 0 `(list move:ames)`moves7)
  ==
--
