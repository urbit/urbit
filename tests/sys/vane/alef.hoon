/+  *test
/=  alef  /:  /===/sys/vane/alef
          /!noun/
::  construct some test fixtures
::
=/  vane  (alef !>(..zuse))
::
=/  alice  vane
=/  bob    vane
::
=.  our.alice  ~nec
=.  now.alice  ~1111.1.1
=.  eny.alice  0xdead.beef
::
=.  our.bob    ~doznec-doznec
=.  now.bob    ~1111.1.1
=.  eny.bob    0xbeef.dead
::
=.  crypto-core.ames-state.alice  (pit:nu:crub:crypto 512 (shaz 'alice'))
=.  crypto-core.ames-state.bob    (pit:nu:crub:crypto 512 (shaz 'bob'))
::
=/  alice-pub  pub:ex:crypto-core.ames-state.alice
=/  alice-sec  sec:ex:crypto-core.ames-state.alice
=/  bob-pub    pub:ex:crypto-core.ames-state.bob
=/  bob-sec    sec:ex:crypto-core.ames-state.bob
::
=/  alice-sym  (derive-symmetric-key:alef bob-pub alice-sec)
=/  bob-sym    (derive-symmetric-key:alef alice-pub bob-sec)
::
?>  =(alice-sym bob-sym)
::
=.  life.ames-state.alice  2
=.  peers.ames-state.alice
  %+  ~(put by peers.ames-state.alice)  ~doznec-doznec
  =|  =peer-state:alef
  =.  -.peer-state
    :*  symmetric-key=bob-sym
        life=3
        public-key=bob-pub
        sponsor=~marzod
    ==
  =.  route.peer-state  `[direct=%.y `lane:alef`[%& ~nec]]
  [%known peer-state]
::
=.  life.ames-state.bob  3
=.  peers.ames-state.bob
  %+  ~(put by peers.ames-state.bob)  ~nec
  =|  =peer-state:alef
  =.  -.peer-state
    :*  symmetric-key=alice-sym
        life=2
        public-key=alice-pub
        sponsor=~nec
    ==
  =.  route.peer-state  `[direct=%.y `lane:alef`[%| `@`%lane-bar]]
  [%known peer-state]
::
|%
++  move-to-packet
  |=  =move:alef
  ^-  [=lane:alef =blob:alef]
  ::
  ?>  ?=([%give %send *] +.move)
  [lane blob]:+>+.move
::
++  is-move-send
  |=  =move:alef
  ^-  ?
  ?=([%give %send *] card.move)
::
++  snag-packet
  |=  [index=@ud moves=(list move:alef)]
  ^-  [=lane:alef =blob:alef]
  ::
  %-  move-to-packet
  %+  snag  index
  (skim moves is-move-send)
--
|%
++  test-packet-encoding  ^-  tang
  ::
  =/  =packet:alef
    :*  [sndr=~nec rcvr=~doznec-doznec]
        encrypted=%.n
        origin=~
        content=[12 13]
    ==
  ::
  =/  encoded  (encode-packet:alef packet)
  =/  decoded  (decode-packet:alef encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-alien-encounter  ^-  tang
  ::
  =/  =packet:alef
    :*  [sndr=~nec rcvr=~doznec-doznec]
        encrypted=%.y
        origin=~
        content=%double-secret
    ==
  =/  =blob:alef  (encode-packet:alef packet)
  ::
  =/  event-args
    [our=~doznec-doznec eny=0xdead.beef now=~2222.2.2 *sley]
  ::
  =/  lane-foo=lane:alef  [%| `@ux``@`%lane-foo]
  ::
  =/  res
    (call:(vane event-args) ~[//unix] *type %hear lane-foo blob)
  ::
  ;:  weld
    %+  expect-eq
      !>  [~[//unix] %pass /alien %j %public-keys ~nec]~
      !>  -.res
  ::
    %+  expect-eq
      !>  [%alien [lane-foo packet]~ ~ ~]
      !>  (~(got by peers.ames-state.+.res) ~nec)
  ==
::
++  test-message-flow  ^-  tang
  ::
  =^  moves1  alice
    (call alice ~[/alice] %memo ~doznec-doznec /g/talk [%get %post])
  ::
  =^  moves2  bob    (call bob ~[/bob] %hear (snag-packet 0 moves1))
  =^  moves3  bob    (take bob /bone/~nec/1 ~[/bob] %g %done ~)
  =^  moves4  alice  (call alice ~[/alice] %hear (snag-packet 0 moves3))
  =^  moves5  bob
    (take bob /bone/~nec/1 ~[/bob] %g %memo /g/talk [%post 'first1!!'])
  ::
  =^  moves6  alice  (call alice ~[/alice] %hear (snag-packet 0 moves5))
  =^  moves7  bob    (call bob ~[/bob] %hear (snag-packet 0 moves6))
  ::
  ;:  weld
    %+  expect-eq
      !>  :~  :+  ~[/alice]  %give  [%done error=~]
              :+  ~[/alice]  %pass
              [/pump/~doznec-doznec/0 %b %rest ~1111.1.1..00.00.06]
          ==
      !>  moves4
  ::
    %+  expect-eq
      !>  [~[/alice] %give %memo /g/talk %post 'first1!!']
      !>  (snag 1 `(list move:alef)`moves6)
  ==
::
++  test-nack  ^-  tang
  =^  moves1  alice
    (call alice ~[/alice] %memo ~doznec-doznec /g/talk [%get %post])
  ::
  =^  moves2  bob    (call bob ~[/bob] %hear (snag-packet 0 moves1))
  =/  =error:alef    [%flub [%leaf "sinusoidal repleneration"]~]
  =^  moves3  bob    (take bob /bone/~nec/1 ~[/bob] %g %done `error)
  =^  moves4  alice  (call alice ~[/alice] %hear (snag-packet 0 moves3))
  =^  moves5  alice  (call alice ~[/alice] %hear (snag-packet 1 moves3))
  =^  moves6  bob    (call bob ~[/bob] %hear (snag-packet 0 moves5))
  ::
  %+  expect-eq
    !>  [~[/alice] %give %done `error]
    !>  (snag 1 `(list move:alef)`moves5)
::
++  call
  |=  [vane=_alice =duct =task:alef]
  ^-  [moves=(list move:alef) _alice]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (call:vane-core duct ** task)
::
++  take
  |=  [vane=_alice =wire =duct =sign:alef]
  ^-  [moves=(list move:alef) _alice]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (take:vane-core wire duct ** sign)
--
