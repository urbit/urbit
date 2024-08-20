/+  *test
/=  gall-raw  /sys/vane/gall
::
=/  nec  (gall-raw ~nec)
::
|%
++  time  ~1111.1.1
::  +test-init: test %init
::
++  test-init
  ^-  tang
  ::
  ::
  =/  call-args
    =/  =duct  ~[/init]
    =/  =task:gall  [%init ~]
    [duct task]
  ::
  ::
  =^  moves  nec
    (gall-call nec time *roof call-args)
  (expect-eq !>(moves) !>(*(list move:nec)))
::  +gall-call: have %gall run a +task and assert it produces expected-moves
::
++  gall-call
  |=  $:  nec=_nec
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:gall)]
      ==
  =/  gall-core  (nec now=now eny=`@`0xdead.beef scry=scry)
  (call:gall-core duct.call-args dud=~ wrapped-task.call-args)
--
