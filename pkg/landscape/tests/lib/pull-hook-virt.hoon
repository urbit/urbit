/+  pull-hook-virt, *test, resource
|%
++  bowl  *bowl:gall
::
++  virt  ~(. pull-hook-virt bowl)
::
++  test-mule-scry-bad-time
  %+  expect-eq  !>(~)
  !>  %+  mule-scry:virt  **
  /gx/(scot %p ~zod)/graph-store/(scot %da ~2010.1.1)/keys/noun
::
++  test-mule-scry-bad-ship
  %+  expect-eq  !>(~)
  !>  %+  mule-scry:virt  **
  /gx/(scot %p ~bus)/graph-store/(scot %da *time)/keys/noun
::
++  test-kick-mule
  =/  rid=resource
    [~zod %test]
  =/  pax=path
    /gx/(scot %p ~zod)/graph-store/(scot %da *time)/keys/noun
  =/  test-trp=(trap *)
    |.
    :-  ~
    .^(path pax)
  =/  harness-trp=(trap *)
    |.((kick-mule:virt rid test-trp))
  %+  expect-eq  !>(``/foo)
  !>  
  =/  res=toon
    %+  mock  [harness-trp %9 2 %0 1]
    |=  [ref=* raw=*]
    =/  pox=(unit path)
      ((soft path) raw)
    ?~  pox  ~
    ?:  =(u.pox pax)
      ``/foo
    ``.^(* u.pox)
  ?>  ?=(%0 -.res)
  ;;((unit (unit path)) p.res)
::
--
    
