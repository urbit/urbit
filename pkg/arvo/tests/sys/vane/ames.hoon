/+  *test
/=  ames  /sys/vane/ames
::  construct some test fixtures
::
=/  nec  ^$:((ames ~nec))
=/  bud  ^$:((ames ~bud))
=/  comet  (ames ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud)
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
=.  now.comet        ~1111.1.1
=.  eny.comet        0xbeef.cafe
=.  rof.comet  |=(* ``[%noun !>(*(list turf))])
=.  crypto-core.ames-state.comet
  %-  nol:nu:crub:crypto
  0w9N.5uIvA.Jg0cx.NCD2R.o~MtZ.uEQOB.9uTbp.6LHvg.0yYTP.
  3q3td.T4UF0.d5sDL.JGpZq.S3A92.QUuWg.IHdw7.izyny.j9W92
=/  comet-pub  pub:ex:crypto-core.ames-state.comet
=/  comet-sec  sec:ex:crypto-core.ames-state.comet

=/  nec-sym  (derive-symmetric-key:ames bud-pub nec-sec)
=/  bud-sym  (derive-symmetric-key:ames nec-pub bud-sec)
?>  =(nec-sym bud-sym)
::
=/  comet-sym  (derive-symmetric-key:ames bud-pub comet-sec)  
::
=.  peers.ames-state.nec
  %+  ~(put by peers.ames-state.nec)  ~bud
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=bud-sym
        life=3
        public-key=bud-pub
        sponsor=~nec
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%& ~nec]]
  [%known peer-state]
::
=.  peers.ames-state.bud
  %+  ~(put by peers.ames-state.bud)  ~nec
  =|  =peer-state:ames
  =.  -.peer-state
    :*  symmetric-key=nec-sym
        life=2
        public-key=nec-pub
        sponsor=~nec
    ==
  =.  route.peer-state  `[direct=%.y `lane:ames`[%| `@`%lane-bar]]
  [%known peer-state]
::  metamorphose
::
=>  .(nec +:(call:(nec) ~[//unix] ~ %born ~))
=>  .(bud +:(call:(bud) ~[//unix] ~ %born ~))
::  helper core
::
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
++  scry
  |=  [vane=_nec car=term bem=beam]
  =/  vane-core  (vane)
  (scry:vane-core ~ car bem)
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
  =/  encoded  (encode-packet:ames & packet)
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
  =/  encoded  (encode-packet:ames & packet)
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
++  test-shut-packet-associated-data  ^-  tang
  ::
  =/  =shut-packet:ames
    :+  bone=17  message-num=18
    [%& num-fragments=1 fragment-num=1 fragment=`@`0xdead.beef]
  ::
  =/  =packet:ames
    (encode-shut-packet:ames shut-packet nec-sym ~marnec ~marbud-marbud 3 1)
  ::
  %-  expect-fail
  |.((decode-shut-packet:ames packet nec-sym 3 17))
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
  =/  =blob:ames   (encode-packet:ames & packet)
  =^  moves1  bud  (call bud ~[//unix] %hear lane-foo blob)
  =^  moves2  bud
    =/  =point:ames
      :*  rift=1
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
          :~  :^  ~[//unix]  %pass  /bone/~bus/1
              [%g %plea ~bus %g /talk [%first %post]]
          ::
              :^  ~[//unix]  %pass  /qos
              [%d %flog %text "; ~bus is your neighbor"]
          ==
      !>  (sy ,.moves3)
  ==
::
++  test-comet-encounter  ^-  tang
  ::
  =/  lane-foo=lane:ames  [%| `@ux``@`%lane-foo]
  ::
  =/  =open-packet:ames
    :*  public-key=`@`comet-pub
        sndr=our.comet
        sndr-life=1
        rcvr=~bud
        rcvr-life=3
    ==
  =/  packet
    ~!  ames
    (encode-open-packet:ames open-packet crypto-core.ames-state.comet)
  =/  blob  (encode-packet:ames & packet)
  ::
  =^  moves0  bud  (call bud ~[//unix] %hear lane-foo blob)
  ::
  =/  =plea:ames  [%g /talk [%first %post]]
  =/  =shut-packet:ames
    :*  bone=1
        message-num=1
        [%& num-fragments=1 fragment-num=0 (jam plea)]
    ==
  =/  =packet:ames
    %:  encode-shut-packet:ames
      shut-packet
      comet-sym
      our.comet
      ~bud
      sndr-life=1
      rcvr-life=3
    ==
  =/  blob  (encode-packet:ames & packet)
  =^  moves1  bud  (call bud ~[//unix] %hear lane-foo blob)
  ::
  ;:  weld
    %+  expect-eq
      !>  ~
      !>  moves0
  ::
    %+  expect-eq
      !>  :~  :*  ~[//unix]  %pass  /qos  %d  %flog  %text
                  "; {<our.comet>} is your neighbor"
              ==
              :*  ~[//unix]  %pass  /bone/(scot %p our.comet)/1
                  %g  %plea  our.comet  plea
          ==  ==
      !>  moves1
  ==
::
++  test-message-flow  ^-  tang
  ::  ~nec -> %plea -> ~bud
  ::
  =^  moves1  nec  (call nec ~[/g/talk] %plea ~bud %g /talk [%get %post])
  =^  moves2  bud  (call bud ~[//unix] %hear (snag-packet 0 moves1))
  ::  ~bud -> %done -> ~nec
  ::
  =^  moves3  bud  (take bud /bone/~nec/1 ~[//unix] %g %done ~)
  =^  moves4  nec  (call nec ~[//unix] %hear (snag-packet 0 moves3))
  ::  ~bud -> %boon -> ~nec
  ::
  =^  moves5  bud  (take bud /bone/~nec/1 ~[//unix] %g %boon [%post 'first1!!'])
  =^  moves6  nec  (call nec ~[//unix] %hear (snag-packet 0 moves5))
  ::  ~nec -> %done -> ~bud (just make sure ~bud doesn't crash on ack)
  ::
  =^  moves7  bud  (call bud ~[//unix] %hear (snag-packet 0 moves6))
  ::
  ;:  weld
    %+  expect-eq
      !>  :~  [~[//unix] %pass /qos %d %flog %text "; ~nec is your neighbor"]
              [~[//unix] %pass /bone/~nec/1 %g %plea ~nec %g /talk [%get %post]]
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
      !>  [~[/g/talk] %give %boon [%post 'first1!!']]
      !>  (snag 0 `(list move:ames)`moves6)
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
  =^  moves3  bud  (take bud /bone/~nec/1 ~[/bud] %g %done `error)
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
++  test-fine-request
  ^-  tang
  =/  want=path  /cz/~bud/kids/1/sys
  =/  net=path   /cz/kids/1/sys
  =^  moves1  nec  (call nec ~[/g/talk] %keen want)
  =/  req=hoot:ames
    %+  snag  0
    %+  murn  ;;((list move:ames) moves1)
    |=  =move:ames 
    ^-  (unit hoot:ames)
    ?.  ?=(%give -.card.move)    ~
    ?.  ?=(%send -.p.card.move)  ~
    `;;(@uxhoot blob.p.card.move)
  =/  [is-ames=? =packet:ames]  (decode-packet:ames `@ux`req)
  ?>  ?=(%| is-ames)
  =/  twit
   (decode-request:ames `@ux`content.packet)
  ~&  twit
  (expect-eq !>(1) !>(1))
::
++  test-fine-response
  ^-  tang
  =/  datum=@ux  (fil 5 32 0xdead.beef)
  =/  want=path  /cz/~bud/kids/1/sys
  =/  net=path   /cz/~bud/kids/1/sys
  =.  rof.bud
    |=(* ``noun+!>(datum))
  =/  =beam  [[~bud %$ da+now:bud] (welp /fine/message net)]
  =/  [=mark =vase]  (need (need (scry bud %x beam)))
  =+  !<(=song:ames vase)
  =/   partial=(list have:ames)
    %-  head 
    %^  spin  song  1
    |=  [blob=@ux num=@ud]
    ^-  [have:ames _num]
    :_  +(num)
    =/  [is-ames=? =packet:ames]  (decode-packet:ames `@ux`blob)
    ?>  ?=(%| is-ames)
    =/  [=peep:ames =purr:ames]  (decode-request-info:ames `@ux`content.packet)
    =/  rawr  (decode-response-packet:ames `@ux`purr)
    ~&  rawr-sig/`@ux`sig.rawr
    ~&  rawr-siz/`@ux`siz.rawr
    ~&  rawr-wid/`@ux`wid.rawr
    ~&  rawr-dat/`@ux`dat.rawr
    [num rawr]
  ::
  =/  num-frag=@ud  (lent partial)
  =/   =roar:ames
    (decode-response-msg:ames num-frag (flop partial))
  %+  welp
    =/  dat
      ?>  ?=(^ dat.roar)
      ;;(@ux q.dat.roar)
    (expect-eq !>(dat) !>(datum))
  =/  event-core
    ~!  nec
    =/   foo  [*@da *@ rof.nec]
    (per-event:(nec foo) [*@da *@ rof.nec] *duct ames-state.nec)
  %-  zing
  %+  turn  partial
  |=  [fra=@ud sig=@ siz=@ud byts] 
  %+  expect-eq  !>(%.y) 
  !>((veri-fra:keys:fine:event-core ~bud life.ames-state.bud want fra dat sig))

--
