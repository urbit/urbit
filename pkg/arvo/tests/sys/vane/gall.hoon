/+  *test
/=  gall-raw  /sys/vane/gall
::
=/  gall-gate  (gall-raw ~nec)
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
    =/  =task:gall  [%init ~]
    [duct task]
  ::
  =/  expected-moves=(list move:gall-gate)  ~
  ::
  =/  res
    (gall-call gall-gate time *roof call-args expected-moves)
  ::
  -.res
::  +test-conf: test %conf; TODO: test clay response
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
    =/  =task:gall  [%conf dap %home]
    [duct task]
  ::
  =/  =move:gall-gate
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/home/(scot %da time)
    =/  =note-arvo
      [%c %warp ship %home ~ %sing %a da+time /app/[dap]/hoon]
    [duct %pass wire note-arvo]
  ::
  =/  expected-moves=(list move:gall-gate)  ~[move]
  ::
  =/  res
    (gall-call gall-gate time *roof call-args expected-moves)
  ::
  -.res
::  +gall-call: have %gall run a +task and assert it produces expected-moves
::
++  gall-call
  |=  $:  gall-gate=_gall-gate
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:gall)]
          expected-moves=(list move:gall-gate)
      ==
  =/  gall-core  (gall-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =/  res
    =/  =type  -:!>(*task:gall)
    (call:gall-core duct.call-args dud=~ wrapped-task.call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  -.res
  ::
  [output +.res]
--
