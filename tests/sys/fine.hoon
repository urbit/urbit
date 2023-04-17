::  test fine, remote-scry request and response
::
/+  *test, v=test-ames-gall
/*  kelvin  %hoon  /sys/kelvin
=>  |%
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
    --
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
  =/  =task:ames           [%keen ~bud scry-path]
  ::
  =/  content=@ux
    %:  ames-scry-hunk:v  ames.bud
      [~1111.1.2 0xbeef.dead custom-roof]
      ~bud
      [1 16.384 /~bud/1/1/c/x/1/kids/sys/kelvin]
    ==
  =/  =shot:ames
    :*  [sndr=~bud rcvr=~nec]
        req=|  sam=|
        sndr-tick=0b1
        rcvr-tick=0b1
        origin=~
        content=content
    ==
  ::
  ~&  >  'poke requester %ames with a %keen task'
  =^  t1  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.1 0xdead.beef *roof]
       [~[/keen-duct-1] task]
      :~  :-  ~[//unix]
          :*  %give  %send  [%& ~bud]
              0x6e69.766c.656b.2f73.7973.2f73.6469.6b2f.312f.782f.632f.312f.312f.
              6475.627e.2f00.1f00.0000.0107.7900.3220.8214.2705.fb35.0288.4b05.
              06f2.e713.a557.9049.745f.2f6d.8871.afd8.ceeb.cebf.1db5.8e23.619e.
              1dd2.c92e.b7f8.a142.1746.b5f0.d7f1.5155.2d30.9093.7ee4.ce00.0200.
              0111.e898.b008
          ==
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
  ~&  >  'two listeners for the requested path'
  =/  t3=tang
    %+  expect-eq
      !>((sy ~[~[/keen-duct-1] ~[/keen-duct-2]]))
    !>(listeners)
  ::
  :-  t3  |.  :-  %&
  =/  sig=@
     0x1c8.d78c.e1f2.ed72.a80a.1f3e.7c36.f488.9322.df15.91f3.ecb5.
     8e3b.0dcb.eeec.a273.b993.c70b.08f2.abfd.1bb7.2cc8.e3d5.c844.
     773b.02fc.43e9.e763.5c9e.399a.6c09.9aef
  ~&  >  'hears a remote scry response'
  =^  t4  ames.nec
    %:  ames-check-call:v  ames.nec
      [~1111.1.2 0xbeef.dead *roof]
      :-  ~[//fine]
      :*  %hear  [%& ~bud]
          (etch-shot:ames shot)
      ==
      :~  [~[//fine] [%pass /qos %d %flog %text "; ~bud is your neighbor"]]
          :-  ~[/keen-duct-2]
          :*  %give  %tune  /~bud/1/1/c/x/1/kids/sys/kelvin
              sig
              `hoon+kelvin
          ==
          :-  ~[/keen-duct-1]
          :*  %give  %tune  /~bud/1/1/c/x/1/kids/sys/kelvin
              sig
              `hoon+kelvin
          ==
          [~[//unix] %pass fine-behn-wire %b %rest ~1111.1.1..00.00.01]
      ==
    ==
  ::
  t4
--
