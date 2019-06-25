/+  *test
/=  alef  /:  /===/sys/vane/alef
          /!noun/
::
=/  vane  (alef !>(..zuse))
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
++  test-send-rcv-message  ^-  tang
  ::
  =/  alice  vane
  =/  bob    vane
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
    :-  %known
    ^-  peer-state:alef
    :-  :*  symmetric-key=bob-sym
            life=3
            public-key=bob-pub
            sponsor=~marzod
        ==
    :*  [~ direct=%.y `lane:alef`[%& ~nec]]
        *ossuary:alef
        *(map bone:alef message-pump-state:alef)
        *(map bone:alef message-still-state:alef)
        *(set [bone:alef message-num:alef])
    ==
  ::
  =.  life.ames-state.bob  3
  =.  peers.ames-state.bob
    %+  ~(put by peers.ames-state.bob)  ~nec
    :-  %known
    ^-  peer-state:alef
    :-  :*  symmetric-key=alice-sym
            life=2
            public-key=alice-pub
            sponsor=~nec
        ==
    :*  [~ direct=%.y `lane:alef`[%| `@`%lane-bar]]
        *ossuary:alef
        *(map bone:alef message-pump-state:alef)
        *(map bone:alef message-still-state:alef)
        *(set [bone:alef message-num:alef])
    ==
  ::
  =/  alice-core  (alice ~nec 0xdead.beef ~2222.2.2 *sley)
  ::
  =/  res1
    %-  call:alice-core
    [~[/alice] ** %memo ~doznec-doznec /g/talk [%get %post]]
  ::
  ::~&  res1=-.res1
  ::
  =+  ^-  [=lane:alef =blob:alef]
      =-  ?>  ?=([%give %send *] ->)
          [lane blob]:->+>
      %+  snag  0
      %+  skim  -.res1
      |=  [duct card=*]
      ^-  ?
      ?=([%give %send *] card)
  ::
  =/  bob-core  (bob ~doznec-doznec 0xbeef.dead ~2222.2.3 *sley)
  ::
  =/  res2
    %-  call:bob-core
    [~[/bob] ** %hear lane blob]
  ::
  ::~&  res2=-.res2
  ::
  =.  bob-core  (+.res2 ~doznec-doznec 0xbeef.dead ~2222.2.4 *sley)
  ::
  =/  res3
    %-  take:bob-core
    [/bone/~nec/1 ~[/bob] ** %g %done ~]
  ::
  ::~&  res3=-.res3
  ::
  =.  alice-core  (+.res1 ~nec 0xdead.beef ~2222.2.5 *sley)
  ::
  =+  ^-  [=lane:alef =blob:alef]
      =-  ?>  ?=([%give %send *] ->)
          [lane blob]:->+>
      %+  snag  0
      %+  skim  -.res3
      |=  [duct card=*]
      ^-  ?
      ?=([%give %send *] card)
  ::
  =/  res4
    %-  call:alice-core
    [~[/alice] ** %hear lane blob]
  ::
  =.  bob-core  (+.res3 ~doznec-doznec 0xbeef.dead ~2222.2.6 *sley)
  ::
  =/  res5
    %-  take:bob-core
    [/bone/~nec/1 ~[/bob] ** %g %memo /g/talk [%post 'first1!!']]
  ::
  ~&  res5=-.res5
  ::
  =.  alice-core  (+.res4 ~nec 0xdead.beef ~2222.2.7 *sley)
  ::
  =+  ^-  [=lane:alef =blob:alef]
      =-  ?>  ?=([%give %send *] ->)
          [lane blob]:->+>
      %+  snag  0
      %+  skim  -.res5
      |=  [duct card=*]
      ^-  ?
      ?=([%give %send *] card)
  ::
  =/  res6
    %-  call:alice-core
    [~[/alice] ** %hear lane blob]
  ::
  ~&  res6=-.res6
  ::
  =.  bob-core  (+.res5 ~doznec-doznec 0xbeef.dead ~2222.2.8 *sley)
  ::
  =+  ^-  [=lane:alef =blob:alef]
      =-  ?>  ?=([%give %send *] ->)
          [lane blob]:->+>
      %+  snag  0
      %+  skim  -.res6
      |=  [duct card=*]
      ^-  ?
      ?=([%give %send *] card)
  ::
  =/  res7
    %-  call:bob-core
    [~[/bob] ** %hear lane blob]
  ::
  ~&  res7=-.res7
  ::
  %+  expect-eq
    !>  :~  :+  ~[/alice]  %give  [%done error=~]
            :+  ~[/alice]  %pass
            [/pump/~doznec-doznec/0 %b %rest ~2222.2.2..00.00.05]
        ==
    !>  -.res4
::
++  test-nack  ^-  tang
  ::
  =/  alice  vane
  =/  bob    vane
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
    :-  %known
    ^-  peer-state:alef
    :-  :*  symmetric-key=bob-sym
            life=3
            public-key=bob-pub
            sponsor=~marzod
        ==
    :*  [~ direct=%.y `lane:alef`[%& ~nec]]
        *ossuary:alef
        *(map bone:alef message-pump-state:alef)
        *(map bone:alef message-still-state:alef)
        *(set [bone:alef message-num:alef])
    ==
  ::
  =.  life.ames-state.bob  3
  =.  peers.ames-state.bob
    %+  ~(put by peers.ames-state.bob)  ~nec
    :-  %known
    ^-  peer-state:alef
    :-  :*  symmetric-key=alice-sym
            life=2
            public-key=alice-pub
            sponsor=~nec
        ==
    :*  [~ direct=%.y `lane:alef`[%| `@`%lane-bar]]
        *ossuary:alef
        *(map bone:alef message-pump-state:alef)
        *(map bone:alef message-still-state:alef)
        *(set [bone:alef message-num:alef])
    ==
  ::
  =/  alice-core  (alice ~nec 0xdead.beef ~2222.2.2 *sley)
  ::
  =/  res1
    %-  call:alice-core
    [~[/alice] ** %memo ~doznec-doznec /g/talk [%get %post]]
  ::
  ::~&  res1=-.res1
  ::
  =+  ^-  [=lane:alef =blob:alef]
      %-  move-to-packet
      %+  snag  0
      (skim -.res1 is-move-send)
  ::
  =/  bob-core  (bob ~doznec-doznec 0xbeef.dead ~2222.2.3 *sley)
  ::
  =/  res2
    %-  call:bob-core
    [~[/bob] ** %hear lane blob]
  ::
  ::~&  res2=-.res2
  ::
  =.  bob-core  (+.res2 ~doznec-doznec 0xbeef.dead ~2222.2.4 *sley)
  ::
  =/  =error:alef  [%flub [%leaf "sinusoidal repleneration"]~]
  ::
  =/  res3
    %-  take:bob-core
    [/bone/~nec/1 ~[/bob] ** %g %done `error]
  ::
  ~&  res3=-.res3
  ::
  =/  pac3-0
    %-  move-to-packet
    %+  snag  0
    (skim -.res3 is-move-send)
  ::
  =/  pac3-1
    %-  move-to-packet
    %+  snag  1
    (skim -.res3 is-move-send)
  ::
  =.  alice-core  (+.res1 ~nec 0xdead.beef ~2222.2.5 *sley)
  ::
  =/  res4
    %-  call:alice-core
    [~[/alice] ** %hear pac3-0]
  ::
  ~&  res4=-.res4
  ::
  =.  alice-core  (+.res4 ~nec 0xdead.beef ~2222.2.6 *sley)
  ::
  =/  res5
    %-  call:alice-core
    [~[/alice] ** %hear pac3-1]
  ::
  ~&  res5=-.res5
  ::
  =.  bob-core  (+.res3 ~doznec-doznec 0xbeef.dead ~2222.2.7 *sley)
  ::
  =/  pac5
    %-  move-to-packet
    %+  snag  0
    (skim -.res5 is-move-send)
  ::
  =/  res6
    %-  call:bob-core
    [~[/bob] ** %hear pac5]
  ::
  ~&  res6=-.res6
  ::
  ~
--
