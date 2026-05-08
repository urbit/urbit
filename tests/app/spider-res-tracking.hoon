/-  spider
/+  *test-agent, test, libstrand=strand, strandio
/=  agent  /app/spider
=,  strand=strand:libstrand
|%
+$  card           card:libstrand
+$  task-user-v1   task-user-v1:rand
+$  arvo-resource  arvo-resource:gall
+$  resource-deet  resource-deet:gall
+$  tid            tid:spider
+$  input          input:spider
+$  yarn           (list tid)
+$  thread-form    _*eval-form:eval:(strand ,vase)
+$  trying         ?(%build %none)
+$  clean-state
  $:  %8
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk from=desk])
      scrying=(jug tid [wire ship path])
      resources=(jug tid arvo-resource)
      resource-deets=(map tid (map arvo-resource resource-deet))
  ==
--
|%
++  scries
  |=  =path
  ^-  (unit vase)
  =+  bond=*bond:ward:clay
  ?+    path  ~
    [%cx @ %$ @ %bond @ ~]  `!>(bond(peg (sy :~([%super ~]))))
  ::
    [%cy @ @ @ %ted *]      `!>([`0v0 ~])
  ==
::
++  tid  (scot %uv 0v0)
++  our  ~dev
++  now  ~2000.1.1
++  wir  /thread/[tid]/blah
::
++  thread
  |=  task=task-user-v1
  !>  !>
  ^-  thread:spider
  |=  arg=vase
  =/  m  (strand:rand ,vase)
  ;<  ~  bind:m  (send-raw-card:strandio [%pass /blah [%arvo task]])
  ;<  *  bind:m  take-gift-user:strandio
  (pure:m !>('done'))
::
++  run-thread
  |=  $:  task=task-user-v1
          gift=(unit gift-user-v1:gall)
          ex-res=(set arvo-resource)
          ex-scry=(set [wire ship path])
          ex=(list $-(card:agent:gall tang))
      ==
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *      bind:m  (do-init %spider agent)
  ;<  *      bind:m
    %-  jab-bowl
    |=(b=bowl b(our our, src our, sap /gall/foo, byk [our %foo da+now]))
  ;<  =bowl  bind:m  get-bowl
  ;<  *      bind:m  (set-scry-gate scries)
  ;<  *      bind:m
    %+  do-poke  %spider-start
    !>([~ `tid byk.bowl %thread !>([~ ~])])
  ::
  =/  rant               *rant:clay
  ;<  caz2=(list card:agent:gall)  bind:m  (do-arvo /build/[tid] [%clay %read ~ `rant(r [%vase (thread task)])])
  ;<  vas=vase          bind:m  get-save
  =+  !<(state2=clean-state vas)
  =/  res=(set arvo-resource)
    (~(gut by resources.state2) tid ~)
  ;<  ~                 bind:m  (ex-equal !>(res) !>(ex-res))
  ::
  =/  scry=(set [wire ship path])  (~(gut by scrying.state2) tid ~)
  ;<  ~                 bind:m  (ex-equal !>(scry) !>(ex-scry))
  ::
  ;<  ~                 bind:m
    (ex-cards caz2 [(ex-user-task tid task) ~])
  ::
  ;<  caz3=(list card:agent:gall)  bind:m
    ?:  ?=(^ gift)  (do-arvo wir u.gift)
    (do-poke [%spider-stop !>([tid &])])
  ;<  ~  bind:m
    %+  ex-cards  caz3
    %-  welp  :_  ex
    :~  ex-thread-done
        ex-kick
    ==
  ;<  vas2=vase         bind:m  get-save
  =+  !<(state3=clean-state vas2)
  =/  res2=(set arvo-resource)
    (~(gut by resources.state3) tid ~)
  (ex-equal !>(res2) !>(~))
::
::
++  ex-user-task
  |=  [tid=@ta task=task-user-v1:gall]
  (ex-card %pass wir %arvo task)
::
++  ex-thread-done
  |=  =card:agent:gall
  ?:  ?=([%give %fact * %thread-done *] card)  ~
  :~  'expected %thread-done'
  ==
::
++  ex-kick
  |=  =card:agent:gall
  ?:  ?=([%give %kick *] card)  ~
  :~  'expected %kick'
  ==
::
::
++  test-rt-clay-read-sing
  %:  run-thread
      [%clay %read 123 our %foo [%sing %x da+now /foo/hoon]]
      `[%clay %read 123 ~]
      (sy :~([wir [%clay %warp 123]]))
      ~
      ~
  ==
::
++  test-rt-clay-read-many
::  TODO: check resource-deets.state as well as resources
  =/  =rant:clay  [[%x ud+1 %foo] /foo/hoon [%$ !>(0)]]
  %:  run-thread
      [%clay %read 123 our %foo [%many & ud+1 ud+3 /foo/hoon]]
      `[%clay %read 123 `rant]
      (sy :~([wir [%clay %warp 123]]))
      ~
      [(ex-user-task tid [%clay %rest 123]) ~]
  ==
::
++  test-rt-iris-request-cancel-ted
  %:  run-thread
      [%iris %request *request:http *outbound-config:iris]
      ~
      (sy :~([wir [%iris %request]]))
      ~
      [(ex-user-task tid [%iris %cancel-request ~]) ~]
  ==
::
++  test-rt-iris-request-got-gift
  %:  run-thread
      [%iris %request *request:http *outbound-config:iris]
      `[%iris %http-response *client-response:iris]
      (sy :~([wir [%iris %request]]))
      ~
      ~
  ==
::
::
++  test-remote-scry-task-user-keen
  =/  =spar:ames  [~rus /foo/bar]
  %:  run-thread
      [%ames %keen & spar]
      `[%syscall 123 [%tune *roar:ames]]
      ~
      (sy :~([wir -.spar +.spar]))
      ~
  ==
++  test-remote-scry-syscall-keen
  =/  =spar:ames  [~rus /foo/bar]
  %:  run-thread
      [%syscall %a %keen `[1 2] spar]
      `[%syscall 123 [%tune *roar:ames]]
      ~
      (sy :~([wir -.spar +.spar]))
      ~
  ==
++  test-remote-scry-syscall-chum
  =/  =spar:ames  [~rus /foo/bar]
  %:  run-thread
      [%syscall %a %chum spar]
      `[%syscall 123 [%tune *roar:ames]]
      ~
      (sy :~([wir -.spar +.spar]))
      ~
  ==
--