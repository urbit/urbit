::  test %pact
::
/+  *test, v=test-mesa-gall
::
=+  thres=5
::
=/  [life=@ud rift=@ud bone=@ud msg=@ud]
  [(dec (bex thres)) (dec (bex thres)) (dec (bex thres)) (dec (bex thres))]
=+  lifes=[comet-a=1 comet-b=1 moon-a=life moon-b=life planet-a=life planet-b=life galaxy-a=1 galaxy-b=1]
=+  rifts=[comet-a=0 comet-b=0 moon-a=rift moon-b=rift planet-a=rift planet-b=rift galaxy-a=0 galaxy-b=0]
=+  (ames-comets-moons-planets-galaxies:v lifes rifts)
::
=>  |%
    ++  dbug  `?`&
    ++  make-roof
      |=  [pax=path val=cage]
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?.  ?&(=(s.bem pax) |(=(vis %x) =(vis [%g %x]) =(vis [%a %x]) =(vis %gx) =(vis %ax)))
        [~ ~]
      ``val
    ::
    --
|%
::  +check-mesa-ns: verify poke packet size for arbitrary sender/receiver rank pair
::
++  check-mesa-ns
  |=  $:  sender=ames-gate:v
          receiver=ames-gate:v
          sender-life=@ud
          receiver-life=@ud
          sender-rift=@ud
          receiver-rift=@ud
      ==
  ^-  tang
  =/  iters=(list @ud)  (gulf 1 1.500)
  =/  ack-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            sender
            [~1111.1.10 0xdead.beef *roof]
            [our:sender our:receiver]
        ==
    [%chum server-life=receiver-life client=our:sender client-life=sender-life key]
  =/  pok-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            receiver
            [~1111.1.10 0xdead.beef *roof]
            [our:receiver our:sender]
        ==
    [%chum server-life=sender-life client=our:receiver client-life=receiver-life key]
  =/  pok-path=path
    /flow/(scot %ud bone)/poke/for/(scot %p our:receiver)/(scot %ud msg)
  =/  ack-path=path
    /flow/(scot %ud bone)/ack/bak/(scot %p our:sender)/(scot %ud msg)
  =/  ack-wire
    /mesa/flow/ack/for/(scot %p our:receiver)/(scot %ud receiver-rift)/(scot %ud bone)
  =/  vane-wire
    /bone/(scot %p our:sender)/(scot %ud bone)/(scot %ud msg)
  =/  ack-full=path
    %+  make-space-path.sender  ack-space
      :: :*  %publ
      ::     receiver-life
      :: ==
    [%a %x '1' %$ ack-path]
  =/  pok-full=path
    %+  make-space-path.receiver  pok-space
      :: :*  %publ
      ::     sender-life
      :: ==
    [%a %x '1' %$ pok-path]
  =|  over-mtus=(list [page=@ud pact=@ud i=@ud])
  =;  mts=_over-mtus
    %+  expect-eq
      !>  ~
      !>  mts
  |-  ^+  over-mtus
  ?~  iters
    over-mtus
  =/  bex-roof
    %-  make-roof
    :-  pok-path  ^-  cage
    :-  %atom  !>
    :*  vane=%g
        path=/ge/hood
        payload=[%0 %m %helm-hi `*`(crip "{(reap i.iters 'a')}")]
    ==
  =/  res
    %-  scry:(sender ~1111.1.10 `@`0xdead.beef bex-roof)
    =-  [~ / %x [[our:sender %$ ud+1] -]]
    (weld /mess/(scot %ud sender-rift)/pact/13/etch/init pok-full)
  ?~  res
    ~&  >>>  %nope-1
    ~
  ?~  u.res
    ~&  >>>  %nope-2
    ~
  =+  !<([blob=@ pairs=(list @ux) proof=@ux] q.u.u.res)
  =/  page=pact:pact:sender  (parse-packet:sender blob)
  ?>  ?=(%page +<.page)
  =/  poke=pact:pact:sender
    :*  hop=0
        %poke
        ack=[[our:receiver sender-rift] [13 ~] ack-full]
        pok=[[our:sender receiver-rift] [13 ~] pok-full]
        data.page
    ==
  =/  [=bloq =step]  (met:plot (en:pact:ames poke))
  ?>  =(3 bloq)
  =+  dat-size=(met 3 dat.data.page)
  ?.  (gth step 1.472)
    $(iters t.iters)
  ::  if we go over the MTU, scry again for the %auth fragment
  ::
  =/  res
    %-  scry:(sender ~1111.1.10 `@`0xdead.beef bex-roof)
    =-  [~ / %x [[our:sender %$ ud+1] -]]
    (weld /mess/(scot %ud sender-rift)/pact/13/etch/auth/0 pok-full)
  ?~  res
    ~&  >>>  %nope-1
    ~
  ?~  u.res
    ~&  >>>  %nope-2
    ~
  =+  !<([blob=@ pairs=(list @ux) proof=@ux] q.u.u.res)
  =/  page=pact:pact:sender  (parse-packet:sender blob)
  ?>  ?=(%page +<.page)
  =/  poke=pact:pact:sender
    :*  hop=0
        %poke
        ack=[[our:receiver sender-rift] [13 ~] ack-full]
        pok=[[our:sender receiver-rift] [13 [%auth 0]] pok-full]
        data.page
    ==
  =/  [=^bloq =^step]  (met:plot (en:pact:ames poke))
  =?  over-mtus  (gth step 1.472)
    [[dat-size step i.iters] over-mtus]
  $(iters t.iters)
::
++  test-mesa-ns-comet-moon
  %:  check-mesa-ns
      comet-a  moon-a
      comet-a.lifes  moon-a.lifes
      comet-a.rifts  moon-a.rifts
  ==
::
++  test-mesa-ns-moon-comet
  %:  check-mesa-ns
      moon-a  comet-a
      moon-a.lifes  comet-a.lifes
      moon-a.rifts  comet-a.rifts
  ==
::
++  test-mesa-ns-comet-comet
  %:  check-mesa-ns
      comet-a  comet-b
      comet-a.lifes  comet-b.lifes
      comet-a.rifts  comet-b.rifts
  ==
::
++  test-mesa-ns-moon-moon
  %:  check-mesa-ns
      moon-a  moon-b
      moon-a.lifes  moon-b.lifes
      moon-a.rifts  moon-b.rifts
  ==
::
++  test-mesa-ns-planet-comet
  %:  check-mesa-ns
      planet-a  comet-b
      planet-a.lifes  comet-b.lifes
      planet-a.rifts  comet-b.rifts
  ==
::
++  test-mesa-ns-galaxy-comet
  %:  check-mesa-ns
      galaxy-a  comet-b
      galaxy-a.lifes  comet-b.lifes
      galaxy-a.rifts  comet-b.rifts
  ==
::
++  test-mesa-ns-planet-planet
  %:  check-mesa-ns
      planet-a  planet-b
      planet-a.lifes  planet-b.lifes
      planet-a.rifts  planet-b.rifts
  ==
::
++  test-mesa-ns-galaxy-galaxy
  %:  check-mesa-ns
      galaxy-a  galaxy-b
      galaxy-a.lifes  galaxy-b.lifes
      galaxy-a.rifts  galaxy-b.rifts
  ==
::
++  test-mesa-ns-galaxy-planet
  %:  check-mesa-ns
      galaxy-a  planet-b
      galaxy-a.lifes  planet-b.lifes
      galaxy-a.rifts  planet-b.rifts
  ==
::
++  test-mesa-ns-galaxy-moon
  %:  check-mesa-ns
      galaxy-a  moon-b
      galaxy-a.lifes  moon-b.lifes
      galaxy-a.rifts  moon-b.rifts
  ==
::
++  test-mesa-ns-planet-galaxy
  %:  check-mesa-ns
      planet-a  galaxy-b
      planet-a.lifes  galaxy-b.lifes
      planet-a.rifts  galaxy-b.rifts
  ==
::
++  test-mesa-ns-planet-moon
  %:  check-mesa-ns
      planet-a  moon-b
      planet-a.lifes  moon-b.lifes
      planet-a.rifts  moon-b.rifts
  ==
::
++  test-mesa-ns-moon-galaxy
  %:  check-mesa-ns
      moon-a  galaxy-b
      moon-a.lifes  galaxy-b.lifes
      moon-a.rifts  galaxy-b.rifts
  ==
::
++  test-mesa-ns-moon-planet
  %:  check-mesa-ns
      moon-a  planet-b
      moon-a.lifes  planet-b.lifes
      moon-a.rifts  planet-b.rifts
  ==
::
++  test-mesa-ns-comet-galaxy
  %:  check-mesa-ns
      comet-a  galaxy-b
      comet-a.lifes  galaxy-b.lifes
      comet-a.rifts  galaxy-b.rifts
  ==
::
++  test-mesa-ns-comet-planet
  %:  check-mesa-ns
      comet-a  planet-b
      comet-a.lifes  planet-b.lifes
      comet-a.rifts  planet-b.rifts
  ==
::
++  test-mesa-mtu-flow
  ^-  tang
  %.  :*  comet-a  comet-b
          comet-a.lifes  comet-b.lifes
          comet-a.rifts  comet-b.rifts
      ==
  |=  $:  sender=ames-gate:v
          receiver=ames-gate:v
          sender-life=@ud
          receiver-life=@ud
          sender-rift=@ud
          receiver-rift=@ud
      ==
  =/  ack-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            sender
            [~1111.1.10 0xdead.beef *roof]
            [our:sender our:receiver]
        ==
    [%chum server-life=receiver-life client=our:sender client-life=sender-life key]
  =/  pok-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            receiver
            [~1111.1.10 0xdead.beef *roof]
            [our:receiver our:sender]
        ==
    [%chum server-life=sender-life client=our:receiver client-life=receiver-life key]
  =/  pok-path=path
    /flow/(scot %ud bone)/poke/for/(scot %p our:receiver)/(scot %ud msg)
  =/  ack-path=path
    /flow/(scot %ud bone)/ack/bak/(scot %p our:sender)/(scot %ud msg)
  =/  ack-wire
    /mesa/flow/ack/for/(scot %p our:receiver)/(scot %ud receiver-rift)/(scot %ud bone)
  =/  vane-wire
    /bone/(scot %p our:sender)/(scot %ud bone)/(scot %ud msg)
  =/  =noun
    :*  vane=%g
        path=/ge/hood
        payload=[%0 %m %helm-hi `*`(crip "{(reap 900 'a')}")]
    ==
  =/  bex-roof  (make-roof pok-path atom/!>(noun))
  ::
  =^  *  sender
   (ames-call:v sender ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  =^  *  receiver
   (ames-call:v receiver ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  =^  moves-1  sender
    %-  ames-call:v
    :*  sender
        ~[/ames /test]
      ::
        :^    %moke
            ack-space
          `spar:ames`[our:receiver `path`[%a %x '1' %$ ack-path]]
        `path`[%a %x '1' %$ pok-path]
      ::
        bex-roof
    ==
  =^  moves-2  receiver  (ames-reply:v receiver ~[//unix] moves-1 bex-roof) :: "ok, here is the %auth fragment"
  =+  meek=(snag 1 moves-2)
  ?>  ?=([duct=^ %pass wire=^ *] meek)
  ?>  ?=([%meek *] |4.meek)
  =^  moves-3  receiver  (ames-call:v receiver ~[/ames /test] |4.meek bex-roof)
  =^  moves-4  sender    (ames-reply:v sender ~[//unix] moves-3 bex-roof)     :: "ok, give me the %data frag"
  =^  moves-5  receiver  (ames-reply:v receiver ~[//unix] moves-4 bex-roof)   :: "ok, here is the %data frag"
  (ames-expect-msg:v moves-5 noun)
::
++  test-mesa-mtu-flow-auth-first
  ^-  tang
  %.  :*  comet-a  comet-b
          comet-a.lifes  comet-b.lifes
          comet-a.rifts  comet-b.rifts
      ==
  |=  $:  sender=ames-gate:v
          receiver=ames-gate:v
          sender-life=@ud
          receiver-life=@ud
          sender-rift=@ud
          receiver-rift=@ud
      ==
  =/  ack-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            sender
            [~1111.1.10 0xdead.beef *roof]
            [our:sender our:receiver]
        ==
    [%chum server-life=receiver-life client=our:sender client-life=sender-life key]
  =/  pok-space=space:ames
    =/  key  =<  symmetric-key
        ^-  fren-state:ames
        %:  ames-scry-peer:v
            receiver
            [~1111.1.10 0xdead.beef *roof]
            [our:receiver our:sender]
        ==
    [%chum server-life=sender-life client=our:receiver client-life=receiver-life key]
  =/  pok-path=path
    /flow/(scot %ud bone)/poke/for/(scot %p our:receiver)/(scot %ud msg)
  =/  ack-path=path
    /flow/(scot %ud bone)/ack/bak/(scot %p our:sender)/(scot %ud msg)
  =/  ack-wire
    /mesa/flow/ack/for/(scot %p our:receiver)/(scot %ud receiver-rift)/(scot %ud bone)
  =/  vane-wire
    /bone/(scot %p our:sender)/(scot %ud bone)/(scot %ud msg)
  =/  pok-full=path
      %+  make-space-path.receiver  pok-space
        :: :*  %publ
        ::     sender-life
        :: ==
      [%a %x '1' %$ pok-path]

  =/  =noun
    :*  vane=%g
        path=/ge/hood
        payload=[%0 %m %helm-hi `*`(crip "{(reap 900 'a')}")]
    ==
  =/  bex-roof  (make-roof pok-path atom/!>(noun))
  ::
  =^  *  sender
   (ames-call:v sender ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  =^  *  receiver
   (ames-call:v receiver ~[/none] [%spew ~[%msg %snd %rcv %odd %rot %fin]] *roof)
  =^  moves-1  receiver
    %-  ames-call:v
    :*  receiver
        ~[/ames /test]
      ::
        :+  %meek
          none/~
        :-  our:sender
        ^-  path
        :: (weld /mess/(scot %ud sender-rift)/pact/13/etch/auth/0 pok-full)
        pok-full
      ::
        bex-roof
    ==
  =.  moves-1
    %+  skim  moves-1
    |=  =move:sender
    ?=([* [%give [%push *]]] move)
  ?>  ?=([[* [%give [%push *]]] *] moves-1)
  =/  peek=pact:pact:sender
    (parse-packet:sender q.p.card.i.moves-1)
  ~|  peek/peek
  ?>  ?=(%peek +<.peek)
  =.  wan.name.peek  [%auth 0]
  =+  blob=p:(fax:plot (en:pact:ames peek))
  =^  moves-2  sender
    (ames-call:v sender ~[//unix] [%heer *lane:pact:ames blob] bex-roof)     :: "ok, give me the %auth fragment"
  =^  moves-3  receiver  (ames-reply:v receiver ~[//unix] moves-2 bex-roof) :: "ok, here is the %auth frag"
  =^  moves-4  sender  (ames-reply:v sender ~[//unix] moves-3 bex-roof)     :: "ok, give me the %data frag"
  =^  moves-5  receiver  (ames-reply:v receiver ~[//unix] moves-4 bex-roof) :: "ok, here is the %data frag"
  (ames-expect-msg:v moves-5 noun)
::
--
