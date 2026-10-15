::  test fine, remote-scry request and response
::
/+  *test, v=test-ames-gall
/*  kelvin  %hoon  /sys/kelvin
=>  |%
    ++  dbug  `?`|
    ++  kelvin-roof
      ^-  roof
      ::
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?>  =(s.bem /sys/kelvin)
      ?+  vis  ~
          %cp
        =/  black=dict:clay
          %*(. *dict:clay mod.rul %black)
        ``noun+!>([black black])
      ::
          %cx  ``hoon+!>(kelvin)
      ==
    ::
    ++  bex-roof
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ?>  =(s.bem //some/data/atom)
      ?+  vis  ~
        %gx  ``atom+!>((bex (bex 14)))
      ==
    ::
    ::  agent that sends a secret %keen to whoever pokes it
    ::
    ++  keen-agent
      ^-  agent:gall
      |_  =bowl:gall
      ++  on-init   `..on-init
      ++  on-save   !>(~)
      ++  on-load   |=(vase `..on-init)
      ++  on-poke
        |=  [=mark =vase]
        =+  !<(=ship vase)
        :_  ..on-init
        [%pass /keen %keen & ship /g/x/0/keen//1/foo]~
      ++  on-watch  |=(path !!)
      ++  on-leave  |=(path `..on-init)
      ++  on-peek   |=(path ~)
      ++  on-agent  |=([wire sign:agent:gall] !!)
      ++  on-arvo   |=([wire sign-arvo] `..on-init)
      ++  on-fail   |=([term tang] `..on-init)
      --
    ::  roof that blocks on everything
    ::
    ++  block-roof
      ^-  roof
      |=  [lyc=gang pov=path vis=view bem=beam]
      ^-  (unit (unit cage))
      ~
    ::
    ++  etch-request-content
      |=  [our=@p =path num=@ud]
      ^-  @
      ?>  (lth num (bex 32))
      =+  pat=(spat path)
      =+  wid=(met 3 pat)
      %+  can  3
      :~  1^0         ::  tag byte
          4^num       ::  fragment number
          2^wid       ::  path size
          wid^`@`pat  ::  namespace path
      ==
    --
::
|%
++  test-fine
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud-zod:v life=[nec=1 bud=1 zod=1] rift=[nec=1 bud=1 zod=1])
  ::  uncomment to turn on verbose debug output
  :: =^  *  ames.nec
  ::  (ames-call:v ames.nec ~[/none] [%spew ~[%msg %snd %rcv %odd %fin]] *roof)
  :: =^  *  ames.bud
  ::  (ames-call:v ames.bud ~[/none] [%spew ~[%msg %snd %rcv %odd %fin]] *roof)
  =/  scry-path=path       /c/x/1/kids/sys/kelvin
  =/  fine-behn-wire=wire  (weld /fine/behn/wake/~bud scry-path)
  =/  future-path=path     /c/x/5/kids/sys/kelvin
  =/  future-behn=wire     (weld /fine/behn/wake/~bud future-path)
  =/  =task:ames           [%keen ~ ~bud scry-path]
  ::
  =/  request=shot:ames
    :*  [sndr=~nec rcvr=~bud]
        req=&  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=(etch-request-content ~nec (weld /~bud/1/1 scry-path) 1)
    ==
  ~?  >  dbug  'poke requester %ames with a %keen task'
  =^  t1  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-1] task]
      :~  :-  ~[//unix]
          [%give %send [%& ~bud] (etch-shot:ames request)]
          [~[/ames] %pass fine-behn-wire %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  ~?  >  dbug  'poke requester %ames with a second %keen task'
  :-  t1  |.  :-  %|
  =^  t2  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-2] task]
      ~
    ==
  ::
  :-  t2  |.  :-  %|
  =/  peer=peer-state:ames
    (ames-scry-peer:v ames.nec [~1111.1.8 0xbeef.dead *roof] [~nec ~bud])
  =/  listeners=(jug duct ints:ames)
    ?~  keen=(~(get by keens.peer) scry-path)
      ~
    listeners:u.keen
  ~?  >  dbug  'checks two listeners for the requested scry path'
  =/  t3=tang
    %+  expect-eq
      !>((sy ~[~[/keen-duct-1] ~[/keen-duct-2]]))
    !>(~(key by listeners))
  ::
  :-  t3  |.  :-  %|
  ~?  >  dbug  'gives a remote scry response to listeners'
  =/  [sig=@ux meows=(list @ux)]
    %:  ames-scry-hunk:v  ames.bud
      [~1111.1.2 0xbeef.dead kelvin-roof]
      ~bud
      [1 16.384 (weld /~bud/1/1 scry-path)]
    ==
  =/  response=shot:ames
    :*  [sndr=~bud rcvr=~nec]
        req=|  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        ::  we know that for /sys/kelvin its contents fit
        ::  in one packet -- TODO multipacket response
        content=?>(?=([@ *] meows) i.meows)
    ==
  ::
  =/  =sage:mess:ames  [~bud^scry-path hoon/kelvin]
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames response)
      ==
      :~  [~[//fine] %pass /qos %d %flog %text "; ~bud is your neighbor"]
          [~[/keen-duct-2] %give %sage sage]
          :^  ~[/keen-duct-2]  %pass  /prune-tip
          [%a %deep %prun ~bud scry-path ~[/keen-duct-2] scry-path]
          [~[/keen-duct-1] %give %sage sage]
          :^  ~[/keen-duct-1]  %pass  /prune-tip
          [%a %deep %prun ~bud scry-path ~[/keen-duct-1] scry-path]
          [~[/ames] %pass fine-behn-wire %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t4  |.  :-  %|
  =/  request=shot:ames
    :*  [sndr=~nec rcvr=~bud]
        req=&  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=(etch-request-content ~nec (weld /~bud/1/1 future-path) 1)
    ==
  ~?  >  dbug  'poke requester %ames with a %keen task for a future case'
  =^  t5  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-3] %keen ~ ~bud future-path]
      :~  [~[//unix] [%give %send [%& ~bud] (etch-shot:ames request)]]
          [~[/ames] %pass future-behn %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t5  |.  :-  %|
  ~?  >  dbug  'cancel %keen task, from requester'
  =^  t6  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-3] %yawn ~bud future-path]
      :~  :^  ~[/keen-duct-3]  %pass  /prune-tip
          [%a %deep %prun ~bud future-path [i=/keen-duct-3 t=~] future-path]
          [~[/ames] %pass future-behn %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t6  |.  :-  %|
  ~?  >  dbug  'poke requester %ames with a new %keen task for a future case'
  =^  t7  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-4] %keen ~ ~bud future-path]
      :~  [~[//unix] [%give %send [%& ~bud] (etch-shot:ames request)]]
          [~[/ames] %pass future-behn %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t7  |.  :-  %|
  ~?  >  dbug  'poke requester %ames with a second %keen task for a future case'
  =^  t8  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-5] %keen ~ ~bud future-path]
      ~
    ==
  :-  t8  |.  :-  %|
  ~?  >  dbug  'cancel scry for all listeners (%wham)'
  =^  t9  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/wham-duct] %wham ~bud future-path]
      :~  [~[/keen-duct-4] [%give %sage [~bud future-path] ~]]
          :^  ~[/keen-duct-4]  %pass  /prune-tip
          [%a %deep %prun ~bud future-path ~[/keen-duct-4] future-path]
          [~[/keen-duct-5] [%give %sage [~bud future-path] ~]]
          :^  ~[/keen-duct-5]  %pass  /prune-tip
          [%a %deep %prun ~bud future-path ~[/keen-duct-5] future-path]
          [~[/ames] %pass future-behn %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  :-  t9  |.  :-  %&
  =/  peer=peer-state:ames
    (ames-scry-peer:v ames.nec [~1111.1.8 0xbeef.dead *roof] [~nec ~bud])
  =/  listeners=(jug duct ints:ames)
    ?~  keen=(~(get by keens.peer) scry-path)
      ~
    listeners:u.keen
  ~?  >  dbug  'checks no more listeners'
  (expect-eq !>(~) !>(~(key by listeners)))
::
++  test-fine-misordered
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud-zod:v life=[nec=1 bud=1 zod=1] rift=[nec=1 bud=1 zod=1])
  ::  uncomment to turn on verbose debug output
  ::=^  *  ames.nec
  ::  (ames-call:v ames.nec ~[/none] [%spew ~[%msg %snd %rcv %odd]] *roof)
  ::=^  *  ames.bud
  ::  (ames-call:v ames.bud ~[/none] [%spew ~[%msg %snd %rcv %odd]] *roof)
  =/  scry-path=path       /g/x/0/dap//some/data/atom
  =/  fine-behn-wire=wire  (weld /fine/behn/wake/~bud scry-path)
  =/  =task:ames           [%keen ~ ~bud scry-path]
  ::
  =/  requests=(list shot:ames)
    %+  turn  (gulf 1 3)
    |=  frag=@ud
    ^-  shot:ames
    :*  [sndr=~nec rcvr=~bud]
        req=&  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=(etch-request-content ~nec (weld /~bud/1/1 scry-path) frag)
    ==
  =+  ^=  [req1 req2 req3]
    ?>  ?=([^ ^ ^ *] requests)
    [i i.t i.t.t]:requests
  ~?  >  dbug  'poke requester %ames with a %keen task'
  =^  t1  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-1] task]
      :~  [~[//unix] [%give %send [%& ~bud] (etch-shot:ames req1)]]
          [~[/ames] %pass fine-behn-wire %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  =/  [sig=@ux meows=(list @ux)]
    %:  ames-scry-hunk:v  ames.bud
      [~1111.1.1 0xbeef.dead bex-roof]
      ~bud
      [1 16.384 (weld /~bud/1/1 scry-path)]
    ==
  =/  responses=(list shot:ames)
    %+  turn  meows
    |=  m=@ux
    ^-  shot:ames
    :*  [sndr=~bud rcvr=~nec]
        req=|  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=m
    ==
  =+  ^=  [resp1 resp2 resp3]
    ?>  ?=([^ ^ ^ *] responses)
    [i i.t i.t.t]:responses
  ::
  =/  =sage:mess:ames  [~bud^scry-path atom/(bex (bex 14))]
  ::
  :-  t1  |.  :-  %|
  ~?  >  dbug  'hear first response fragment'
  =^  t2  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames resp1)
      ==
      :~  [~[//fine] %pass /qos %d %flog %text "; ~bud is your neighbor"]
          [~[//unix] [%give %send [%& ~bud] (etch-shot:ames req2)]]
          [~[//unix] [%give %send [%& ~bud] (etch-shot:ames req3)]]
          [~[/ames] %pass fine-behn-wire %b %rest ~1111.1.1..00.00.01]
          [~[/ames] %pass fine-behn-wire %b %wait ~1111.1.2..00.02.00]
      ==
    ==
  ::
  :-  t2  |.  :-  %|
  ~?  >  dbug  'hear third response fragment'
  =^  t3  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames resp3)
      ==
    ::
      ~
    ==
  :-  t3  |.  :-  %&
  ~?  >  dbug  'hear second response fragment'
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.3 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames resp2)
      ==
      :~  [~[/keen-duct-1] %give %sage sage]
          :^  ~[/keen-duct-1]  %pass  /prune-tip
          [%a %deep %prun ~bud scry-path ~[/keen-duct-1] scry-path]
          [~[/ames] %pass fine-behn-wire %b %rest ~1111.1.2..00.02.00]
      ==
    ==
  ::
  t4
::
::  remote scry of our own ship: answered locally, no packets, no peer
::
++  test-fine-self
  %-  run-chain
  |.  :-  %|
  =+  (nec-bud-zod:v life=[nec=1 bud=1 zod=1] rift=[nec=1 bud=1 zod=1])
  =/  scry-path=path  /c/x/1/kids/sys/kelvin
  =/  =sage:mess:ames  [~nec^scry-path hoon/kelvin]
  ::
  ~?  >  dbug  'public %keen to ourselves'
  =^  t1  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef kelvin-roof]
      [~[/keen-duct-1] [%keen ~ ~nec scry-path]]
      [~[/keen-duct-1] %give %sage sage]~
    ==
  :-  t1  |.  :-  %|
  ~?  >  dbug  '%chum to ourselves'
  =^  t2  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef kelvin-roof]
      [~[/keen-duct-2] [%chum ~nec scry-path]]
      [~[/keen-duct-2] %give %sage sage]~
    ==
  :-  t2  |.  :-  %|
  ~?  >  dbug  'group-key %keen to ourselves ignores the key'
  =^  t3  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef kelvin-roof]
      [~[/keen-duct-3] [%keen `[1 0xdead] ~nec scry-path]]
      [~[/keen-duct-3] %give %sage sage]~
    ==
  :-  t3  |.  :-  %|
  ~?  >  dbug  'large payload is given whole'
  =/  big-path=path  /g/x/0/dap//some/data/atom
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef bex-roof]
      [~[/keen-duct-4] [%keen ~ ~nec big-path]]
      [~[/keen-duct-4] %give %sage ~nec^big-path atom/(bex (bex 14))]~
    ==
  :-  t4  |.  :-  %|
  ~?  >  dbug  'blocked scry gives empty %sage at once'
  =^  t5  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef block-roof]
      [~[/keen-duct-5] [%keen ~ ~nec scry-path]]
      [~[/keen-duct-5] %give %sage ~nec^scry-path ~]~
    ==
  :-  t5  |.  :-  %|
  ~?  >  dbug  'malformed path gives empty %sage'
  =^  t6  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef kelvin-roof]
      [~[/keen-duct-6] [%keen ~ ~nec /foo]]
      [~[/keen-duct-6] %give %sage ~nec^/foo ~]~
    ==
  :-  t6  |.  :-  %|
  ~?  >  dbug  '%yawn and %wham to ourselves are silent'
  =^  t7  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-1] [%yawn ~nec scry-path]]
      ~
    ==
  :-  t7  |.  :-  %|
  =^  t8  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/wham-duct] [%wham ~nec scry-path]]
      ~
    ==
  :-  t8  |.  :-  %&
  ~?  >  dbug  'no peer state was created for ourselves'
  =/  peers=(map ship ?(%alien %known))
    !<  (map ship ?(%alien %known))
    =<  q
    %-  need  %-  need
    %-  (ames-scry-gate:v [~1111.1.1 0xdead.beef *roof] ames.nec)
    [[~ ~] / %x [[~nec %$ da+~1111.1.1] /peers]]
  (expect-eq !>(|) !>((~(has by peers) ~nec)))
::
::  gall skips the group-key fetch for a secret %keen to ourselves
::
++  test-gall-keen-self
  =+  (nec-bud-zod:v life=[nec=1 bud=1 zod=1] rift=[nec=1 bud=1 zod=1])
  =.  gall.nec  (load-agent:v ~nec gall.nec %keen keen-agent)
  =/  scry-path=path  /g/x/0/keen//1/foo
  =/  =task:gall  [%deal [~nec ~nec /] %keen %poke noun+!>(~nec)]
  =^  moves  gall.nec  (gall-call:v gall.nec ~[/keen] task *roof)
  =/  passes=(list move:gall-bunt:v)
    %+  skim  moves
    |=  =move:gall-bunt:v
    ?=([* %pass * %a *] move)
  ;:  weld
    ~?  >  dbug  'one %keen to ames, no key request'
    (expect-eq !>(1) !>((lent passes)))
  ::
    ?>  ?=([[* %pass * %a *] ~] passes)
    %+  expect-eq
      !>(`*`[%keen ~ ~nec scry-path])
    !>(`*`+>+.move.i.passes)
  ==
--
