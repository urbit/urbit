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
          [~[//unix] %pass fine-behn-wire %b %wait ~1111.1.1..00.00.01]
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
          [~[//unix] %pass fine-behn-wire %b %rest ~1111.1.1..00.00.01]
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
          [~[//unix] %pass future-behn %b %wait ~1111.1.1..00.00.01]
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
          [~[//unix] %pass future-behn %b %rest ~1111.1.1..00.00.01]
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
          [~[//unix] %pass future-behn %b %wait ~1111.1.1..00.00.01]
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
          [~[//unix] %pass future-behn %b %rest ~1111.1.1..00.00.01]
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
          [~[//unix] %pass fine-behn-wire %b %wait ~1111.1.1..00.00.01]
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
          [~[//unix] %pass fine-behn-wire %b %rest ~1111.1.1..00.00.01]
          [~[//unix] %pass fine-behn-wire %b %wait ~1111.1.2..00.02.00]
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
          [~[//unix] %pass fine-behn-wire %b %rest ~1111.1.2..00.02.00]
      ==
    ==
  ::
  t4
--
