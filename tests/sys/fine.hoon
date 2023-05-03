::  test fine, remote-scry request and response
::
/+  *test, v=test-ames-gall
/*  kelvin  %hoon  /sys/kelvin
=>  |%
    ++  crypto-core
      |%  ++  nec  (pit:nu:crub:crypto 512 (shaz 'nec'))
          ++  bud  (pit:nu:crub:crypto 512 (shaz 'bud'))
          ++  sign
            |=  [=ship data=@ux]
            %.  data
            ?:(=(ship ~nec) sigh:as:nec sigh:as:bud)
      --
    ::
    ++  n-frags
      |=  n=@
      ^-  @ux
      ::  6 chosen randomly to get some trailing zeros
      ::
      %+  rsh  10
      %+  rep  13
      %+  turn  (gulf 1 n)
      |=(x=@ (fil 3 1.024 (dis 0xff x)))
    ::
    ++  custom-roof
      ^-  roof
      ::
      |=  [lyc=gang vis=view bem=beam]
      ^-  (unit (unit cage))
      ?+  vis  ~
          %cp
        =/  black=dict:clay
          %*(. *dict:clay mod.rul %black)
        ``noun+!>([black black])
      ::
          %cz
        ?+  -.r.bem  !!
          %ud  ``noun+!>((n-frags p.r.bem))
        ==
      ::
          %cx  ``hoon+!>(kelvin)
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
  =+  (nec-bud:v life=[nec=1 bud=1] rift=[nec=1 bud=1])
  ::  uncomment to turn on verbose debug output
  ::=^  *  ames.nec
  ::  (ames-call:v ames.nec ~[/none] [%spew ~[%msg %snd %rcv %odd]] *roof)
  ::=^  *  ames.bud
  ::  (ames-call:v ames.bud ~[/none] [%spew ~[%msg %snd %rcv %odd]] *roof)
  =/  scry-path=path       /c/x/1/kids/sys/kelvin
  =/  fine-behn-wire=wire  (weld /fine/behn/wake/~bud scry-path)
  =/  future-path=path     /c/x/5/kids/sys/kelvin
  =/  future-behn=wire     (weld /fine/behn/wake/~bud future-path)
  =/  =task:ames           [%keen ~bud scry-path]
  ::
  =/  request=shot:ames
    :*  [sndr=~nec rcvr=~bud]
        req=&  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=(etch-request-content ~nec /~bud/1/1/c/x/1/kids/sys/kelvin 1)
    ==
  ~&  >  'poke requester %ames with a %keen task'
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
  ~&  >  'poke requester %ames with a second %keen task'
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
  =/  listeners=(set duct)
    ?~  keen=(~(get by keens.peer) scry-path)
      ~
    listeners:u.keen
  ~&  >  'checks two listeners for the requested scry path'
  =/  t3=tang
    %+  expect-eq
      !>((sy ~[~[/keen-duct-1] ~[/keen-duct-2]]))
    !>(listeners)
  ::
  :-  t3  |.  :-  %|
  ~&  >  'gives a remote scry response to listeners'
  =/  [sig=@ux meows=(list @ux)]
    %:  ames-scry-hunk:v  ames.bud
      [~1111.1.2 0xbeef.dead custom-roof]
      ~bud
      [1 16.384 /~bud/1/1/c/x/1/kids/sys/kelvin]
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
  =/  roar=(unit roar:ames)
    :+  ~  [/~bud/1/1/c/x/1/kids/sys/kelvin `hoon+kelvin]
    [[~bud [1 sig]] ~ ~]
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames response)
      ==
      :~  [~[//fine] %pass /qos %d %flog %text "; ~bud is your neighbor"]
          [~[/keen-duct-2] %give %tune [~bud scry-path] roar]
          [~[/keen-duct-1] %give %tune [~bud scry-path] roar]
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
        content=(etch-request-content ~nec /~bud/1/1/c/x/5/kids/sys/kelvin 1)
    ==
  ~&  >  'poke requester %ames with a %keen task for a future case'
  =^  t5  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-3] %keen ~bud future-path]
      :~  [~[//unix] [%give %send [%& ~bud] (etch-shot:ames request)]]
          [~[//unix] %pass future-behn %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t5  |.  :-  %|
  ~&  >  'cancel %keen task, from requester'
  =^  t6  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-3] %yawn ~bud future-path]
      [~[//unix] %pass future-behn %b %rest ~1111.1.1..00.00.01]~
    ==
  ::
  :-  t6  |.  :-  %|
  ~&  >  'poke requester %ames with a new %keen task for a future case'
  =^  t7  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-4] %keen ~bud future-path]
      :~  [~[//unix] [%give %send [%& ~bud] (etch-shot:ames request)]]
          [~[//unix] %pass future-behn %b %wait ~1111.1.1..00.00.01]
      ==
    ==
  ::
  :-  t7  |.  :-  %|
  ~&  >  'poke requester %ames with a second %keen task for a future case'
  =^  t8  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/keen-duct-5] %keen ~bud future-path]
      ~
    ==
  :-  t8  |.  :-  %|
  ~&  >  'cancel scry for all listeners (%wham)'
  =^  t9  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
      [~[/wham-duct] %wham ~bud future-path]
      :~  [~[/keen-duct-4] [%give %tune [~bud /c/x/5/kids/sys/kelvin] ~]]
          [~[/keen-duct-5] [%give %tune [~bud /c/x/5/kids/sys/kelvin] ~]]
          [~[//unix] %pass future-behn %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  :-  t9  |.  :-  %&
  =/  peer=peer-state:ames
    (ames-scry-peer:v ames.nec [~1111.1.8 0xbeef.dead *roof] [~nec ~bud])
  =/  listeners=(set duct)
    ?~  keen=(~(get by keens.peer) scry-path)
      ~
    listeners:u.keen
  ~&  >  'checks no more listeners'
  (expect-eq !>(~) !>(listeners))
--
