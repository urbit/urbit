/+  *test
/=  gall-raw  /sys/vane/gall
::
=/  test-pit=vase  !>(..zuse)
=/  gall-gate  (gall-raw test-pit)
::
|%
::  +test-init: test %init
::
++  test-init
  ^-  tang
  ::
  =/  time  ~1111.1.1
  ::
  =/  call-args
    =/  =duct  ~[/init]
    =/  =type  -:!>(*task:able:gall)
    =/  =task:able:gall  [%init ~nec]
    [duct type task]
  ::
  =/  expected-moves  ~
  ::
  =^  results  gall-gate
    (gall-call gall-gate time *sley call-args expected-moves)
  ::
  results
::  +test-conf: test %conf: TODO: fix
::
++  test-conf
  ^-  tang
  ::
  =/  =duct  ~[/init]
  =/  time  (add ~1111.1.1 ~s1)
  =/  dap=term  %my-agent
  =/  ship  ~nec
  ::
  =/  call-args
    =/  =type  -:!>(*task:able:gall)
    =/  =task:able:gall  [%conf dap]
    [duct type task]
  ::
  =/  =move:gall-gate
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/home/(scot %da time)
    =/  =note-arvo
      [%c %warp ship %home ~ %sing %a da+time /app/[dap]/hoon]
    [duct %pass wire note-arvo]
  ::
  =/  expected-moves=(list move:gall-gate)  ~[move]
  ::
  =^  results  gall-gate
    (gall-call gall-gate time *sley call-args expected-moves)
  ::
  results
::  +gall-call: have %gall run a +task and assert it produces expected-moves
::
++  gall-call
  |=  $:  gall-gate=_gall-gate
          now=@da
          scry=sley
          call-args=[=duct =type wrapped-task=(hobo task:able:gall)]
          expected-moves=(list move:gall-gate)
      ==
  ^-  [tang _gall-gate]
  ::
  =/  gall-core  (gall-gate our=~nec now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  gall-gate  (call:gall-core [duct ~ type wrapped-task]:call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output gall-gate]
--
