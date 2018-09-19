/+  *test
::
/=  light-raw  /:  /===/sys/vane/light  /!noun/
::
!:
::
=/  test-pit=vase  !>(..zuse)
=/  light-gate  (light-raw test-pit)
::
|%
++  test-init
  =^  results1  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::
  results1
::
++  test-duplicate-bindings
  ::
  =^  results1  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.2
      scry=*sley
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app2 tries to bind to the same path and fails
  ::
  =^  results3  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.3
      scry=*sley
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.n [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
  ==
::
++  test-remove-binding
  ::
  =^  results1  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.2
      scry=*sley
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app1 unbinds
  ::
  =^  results3  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.3
      scry=*sley
      call-args=[duct=~[/app1] ~ [%disconnect [~ /]]]
      expected-moves=~
    ==
  ::  app2 binds successfully
  ::
  =^  results4  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.4
      scry=*sley
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.y [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-cant-remove-other-ducts-binding
  ::
  =^  results1  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.2
      scry=*sley
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::  app2 tries to steal the binding by disconnecting the path
  ::
  =^  results3  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.3
      scry=*sley
      call-args=[duct=~[/app2] ~ [%disconnect [~ /]]]
      expected-moves=~
    ==
  ::  app2 doesn't bind successfully because it couldn't remove app1's binding
  ::
  =^  results4  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.4
      scry=*sley
      call-args=[duct=~[/app2] ~ [%connect [~ /] %app2]]
      expected-moves=[duct=~[/app2] %give %bound %.n [~ /]]~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::
++  test-basic-request
  ::
  =^  results1  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.1
      scry=*sley
      call-args=[duct=~[/init] ~ [%init ~nul]]
      expected-moves=~
    ==
  ::  app1 binds successfully
  ::
  =^  results2  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.2
      scry=*sley
      call-args=[duct=~[/app1] ~ [%connect [~ /] %app1]]
      expected-moves=[duct=~[/app1] %give %bound %.y [~ /]]~
    ==
  ::
  ::
  =^  results3  light-gate
    %-  light-call  :*
      light-gate
      now=~1111.1.3
      scry=*sley
      call-args=[duct=~[/http-blah] ~ [%inbound-request %.n [%ipv4 .192.168.1.1]]]
      expectec-moves=~
    ==
  ;:  weld
    results1
    results2
    results3
  ==
::
++  light-call
  |=  $:  light-gate=_light-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:light-gate)]
          expected-moves=(list move:light-gate)
      ==
  ^-  [tang _light-gate]
  ::
  =/  light-core  (light-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  light-gate  (call:light-core call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output light-gate]
--
