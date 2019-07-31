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
=.  our.alice        ~nec
=.  now.alice        ~1111.1.1
=.  eny.alice        0xdead.beef
=.  scry-gate.alice  |=(* ``[%noun !>(*(list turf))])
::
=.  our.bob          ~doznec-doznec
=.  now.bob          ~1111.1.1
=.  eny.bob          0xbeef.dead
=.  scry-gate.bob    |=(* ``[%noun !>(*(list turf))])
::
=.  crypto-core.ames-state.alice  (pit:nu:crub:crypto 512 (shaz 'alice'))
=.  crypto-core.ames-state.bob    (pit:nu:crub:crypto 512 (shaz 'bob'))
::
=/  alice-pub  pub:ex:crypto-core.ames-state.alice
=/  alice-sec  sec:ex:crypto-core.ames-state.alice
=/  bob-pub    pub:ex:crypto-core.ames-state.bob
=/  bob-sec    sec:ex:crypto-core.ames-state.bob
::
=/  alice-sym  (derive-symmetric-key:vane bob-pub alice-sec)
=/  bob-sym    (derive-symmetric-key:vane alice-pub bob-sec)
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
::  metamorphose
::
=>  .(alice +:(call:(alice) ~[//unix] ** %born ~))
=>  .(bob +:(call:(bob) ~[//unix] ** %born ~))
::  helper core
::
=>
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
::  test core
::
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
  =/  encoded  (encode-packet:vane packet)
  =/  decoded  (decode-packet:vane encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-alien-encounter  ^-  tang
  ::
  =/  lane-foo=lane:alef  [%| `@ux``@`%lane-foo]
  ::
  =/  =plea:ames  [%g /talk [%first %post]]
  ::
  =/  =shut-packet:alef
    :*  sndr-life=4
        rcvr-life=3
        bone=1
        message-num=1
        [%& num-fragments=1 fragment-num=0 (jam plea)]
    ==
  ::
  =/  =packet:alef
    :*  [sndr=~bus rcvr=~doznec-doznec]
        encrypted=%.y
        origin=~
        content=(encrypt:vane alice-sym shut-packet)
    ==
  ::
  =/  =blob:alef   (encode-packet:vane packet)
  =^  moves1  bob  (call bob ~[//unix] %hear lane-foo blob)
  =^  moves2  bob
    =/  =point:alef
      :*  rift=1
          life=4
          keys=[[life=4 [crypto-suite=1 `@`alice-pub]] ~ ~]
          sponsor=`~bus
      ==
    %-  take
    :^  bob  /public-keys  ~[//unix]
    ^-  sign:alef
    [%k %public-keys %full [n=[~bus point] ~ ~]]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~[//unix] %pass /public-keys %k %public-keys [~bus ~ ~]]~
      !>  moves1
  ::
    %+  expect-eq
      !>  [~[//unix] %pass /bone/~bus/1 %g %plea ~bus %g /talk [%first %post]]~
      !>  moves2
  ==
::
++  test-message-flow  ^-  tang
  ::
  =^  moves1  alice
    (call alice ~[/alice] %plea ~doznec-doznec %g /talk [%get %post])
  ::
  =^  moves2  bob    (call bob ~[/bob] %hear (snag-packet 0 moves1))
  =^  moves3  bob    (take bob /bone/~nec/1 ~[/bob] %g %done ~)
  =^  moves4  alice  (call alice ~[/alice] %hear (snag-packet 0 moves3))
  =^  moves5  bob
    (take bob /bone/~nec/1 ~[/bob] %g %boon [%post 'first1!!'])
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
      !>  [~[/alice] %give %boon [%post 'first1!!']]
      !>  (snag 1 `(list move:alef)`moves6)
  ==
::
++  test-nack  ^-  tang
  =^  moves1  alice
    (call alice ~[/alice] %plea ~doznec-doznec %g /talk [%get %post])
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
--
