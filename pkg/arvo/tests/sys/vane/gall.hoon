/+  dbug, default-agent, strandio, *test
/=  gall-raw  /sys/vane/gall
::
=/  nec-gall-pupal   (gall-raw ~nec)
=/  dep-gall-pupal   (gall-raw ~dep)
=.  now.dep-gall-pupal        ~1111.1.1
=.  eny.dep-gall-pupal        `@uvJ`0xdead.beef
=.  rof.dep-gall-pupal        |=(* ``[%noun !>(*(list turf))])
::
=/  test-desk=desk  %lab
::
::  metamorphose
=/  dep-gall  +:(call:(dep-gall-pupal) ~[/init] ~ %init ~)
::
=>  |%
    +|  %gall-structures
    +$  move  [=duct move=(wind note-arvo gift-arvo)]
    ::
    +|  %test-dummy-structures
    +$  card  card:agent:gall
    +$  poke
      $%  [%run-test p=$-(* ?)]
          [%get-perms ~]
          [%ames-test p=task:ames]
      ==
    ::
    ::    makes a test-dummy which is basically a default-agent with bowl scry
    ++  test-dummy
      %-  agent:dbug
      ^-  agent:gall
      |_  =bowl:gall
      +*  this  .
          def  ~(. (default-agent this %.n) bowl)
      ::
      ++  on-init   on-init:def
      ++  on-save   on-save:def
      ++  on-load   on-load:def
      ++  on-poke
        |=  [=mark =vase]
        ^-  (quip card _this)
        ?.  ?=(%noun mark)
          `this
        =/  action  !<(poke vase)
        ?-    action
            [%run-test *]
          ~&  (p.action bowl)
          `this
        ::
            [%get-perms ~]
          ~&  per.bowl
          :_  this
          [%give %fact ~ %noun !>(per.bowl)]~
        ::
            [%ames-test *]
          :_  this
          [%pass /test %arvo %a p.action]~
        ==
      ++  on-watch  on-watch:def
      ++  on-leave  on-leave:def
      ++  on-peek
       |=  =path
       ^-  (unit (unit cage))
       ?.  =(path [%x %bowl ~])
         (on-peek:def path)
       ``noun+!>(bowl)
      ++  on-agent  on-agent:def
      ++  on-arvo
        |=  [=wire =sign-arvo]
        ^-  (quip card _this)
        ?+    wire  (on-arvo:def wire sign-arvo)
        ::
            [%~.~ ~]
          ?+    sign-arvo  (on-arvo:def wire sign-arvo)
          ::
              [%gall %perm *]
            ~&  >>  ['permissions change' +>.sign-arvo]
            `this
          ==
        ==
      ++  on-fail   on-fail:def
      --
    --
::
|%
+|  %tests
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
  =/  expected-moves=(list move)  ~
  ::
  =/  res
    (gall-call nec-gall-pupal time *roof call-args expected-moves)
  ::
  -.res
::
::  +test-jolt: test %jolt; TODO: test clay response
::
++  test-jolt
  ^-  tang
  ::
  =/  =duct  ~[/init]
  =/  time  (add ~1111.1.1 ~s1)
  =/  dap=term  %my-agent
  =/  ship  ~nec
  ::
  =/  call-args
    =/  =task:gall  [%jolt %base dap]
    [duct task]
  ::
  =/  =move
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/base/(scot %da time)
    =/  =note-arvo
      [%c %warp ship %base ~ %sing %a da+time /app/[dap]/hoon]
    [duct %pass wire note-arvo]
  ::
  =/  expected-moves=(list _move)  ~[move]
  ::
  =/  res
    (gall-call nec-gall-pupal time *roof call-args expected-moves)
  ::
  -.res
::
++  test-dummy-empty-bowl
  ^-  tang
  ::
  =/  =duct  ~[/init]
  =/  gat
    |=  =bowl:gall
    =(~ per.bowl)
  ::
  =/  =task:agent:gall
    :+  %poke  %noun
    !>  [%run-test gat]
  ::
  =^  moves  dep-gall
    (inject-agent dep-gall test-dummy test-desk %buster ~[/perm])
  =^  moves  dep-gall
    (task-test-dummy dep-gall task)
  *tang
  ::
::
++  test-free-test-dummy
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  per  (~(gas in *(set perm:gall)) [%ames %debug]~)
  =/  =task:gall  [%free test-desk per]
  ::
  =^  moves  dep-gall
    (inject-agent dep-gall test-dummy test-desk %buster ~[/perm])
  =^  moves  dep-gall
    (call dep-gall duct task)
  =/  =bowl:gall  (scry-test-dummy-bowl dep-gall)
  ::
  %+  expect-eq
    !>  per
  ::
    !>  per.bowl
::
++  test-lock-test-dummy
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  per  %-    ~(gas in *(set perm:gall))
           ~[[%ames %debug] [%behn %timer]]
  =/  ner   (~(del in per) [%behn %timer])
  =/  task-1=task:gall    [%free test-desk per]
  =/  task-2=task:gall    [%lock test-desk ner]
  ::
  =^  moves  dep-gall
    (inject-agent dep-gall test-dummy test-desk %buster duct)
  =^  moves  dep-gall
    (call dep-gall duct task-1)
  =^  moves  dep-gall
    (call dep-gall duct task-2)
  =/  =bowl:gall  (scry-test-dummy-bowl dep-gall)
  ::
  %+  expect-eq
    !>  (~(dif in per) ner)
  ::
    !>  per.bowl
::
++  test-read-permissions-dummy
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  per=(set perm:gall)
    (~(gas in *(set perm:gall)) [%ames %debug]~)
  =/  task-1=task:gall
    [%free test-desk per]
  =/  poke-1=task:agent:gall
    [%poke %noun !>(`poke`[%get-perms ~])]
  ::
  =/  expected-moves=(list move)
    =/  move-1=move
      [duct %give %unto %poke-ack ~]
    =/  move-2=move
      =/  =sign:agent:gall  [%fact %noun !>(per)]
      [duct %give %unto sign]
    [move-1 move-2 ~]
  ::
  =^  moves  dep-gall
    (inject-agent dep-gall test-dummy test-desk %buster duct)
  =^  moves  dep-gall
    (call dep-gall duct task-1)
  =^  moves  dep-gall
    (task-test-dummy dep-gall poke-1)
  ::
  %+  expect-eq
    !>  expected-moves
  ::
    !>  moves
::
++  test-scry-desk-perms
  ^-  tang
  ::  TODO: should be able to scry permissions for a desk from gall
  *tang
::
::  +test-ward-wink-notifications: can get a permission change notification
::  after sending a %ward task. after sending a %wink, notifications cease.
::  these tests are grouped since if %ward doesn't work, %wink would be a
::  no-op
++  test-ward-wink-notifications
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  per  %-  ~(gas in *(set perm:gall))
           ~[[%ames %debug]]
  ::
  =/  task-1=task:gall  [%ward ~]
  =/  task-2=task:gall  [%free test-desk per]
  =/  task-3=task:gall  [%wink ~]
  ::
  =/  expected-moves-ward=(list move)
    [duct %give %perm %free test-desk per]~
  =|  expected-moves-wink=(list move)
  ::
  =^  moves  dep-gall
    (call dep-gall duct task-1)
  =^  moves-ward  dep-gall
    (call dep-gall duct task-2)
  =^  moves-wink  dep-gall
    (call dep-gall duct task-3)
  ::
  ;:  weld
    %+  expect-eq
      !>  expected-moves-ward
    ::
      !>  moves-ward
  ::
    %+  expect-eq
      !>  expected-moves-wink
    ::
      !>  moves-wink
  ==
::
::  +test-base-perms: %base agents should have all permissions by default
::  TODO: if the desk is not %base, gall does emit a %would-drop printf but
::  the move still shows up. do i need to try to pass it along in order for
::  it to be dropped?
  ++  test-base-perms
    ^-  tang
    ::
    =/  =duct  ~[/perm]
    ::
    =/  task-1=task:ames  [%sift *(list ship)]
    ::
    =/  poke-1=task:agent:gall
      [%poke %noun !>(`poke`[%ames-test task-1])]
    ::
    =/  expected-moves=(list move)
      =/  move-1=move
        [duct %give %unto %poke-ack ~]
      =/  move-2=move
        [~[/init] %pass /use/buster/0w1.d6Isf/~dep/test %a task-1]
      ~[move-1 move-2]
    ::
    =^  moves  dep-gall
      (inject-agent dep-gall test-dummy %base %buster duct)
    =^  moves  dep-gall
      (task-test-dummy dep-gall poke-1)
    ::
    %+  expect-eq
      !>  expected-moves
    ::
      !>  moves
::
+|  %utilities
::
++  call
  |=  [vane=_dep-gall =duct =task:gall]
  ^-  [moves=(list move) _dep-gall]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (call:vane-core duct ~ task)
::
++  scry
  |=  [vane=_dep-gall care=term bem=beam]
  =/  res  (scry:(vane) ~ care bem)
  res
::
++  take
  |=  [vane=_dep-gall =wire =duct =sign-arvo]
  ^-  [moves=(list move) _dep-gall]
  ::
  =/  vane-core  (vane(now `@da`(add ~s1 now.vane)))
  ::
  (take:vane-core wire duct ~ sign-arvo)
::
::  +inject-agent: creates a .agent named .dude in .desk at .duct
++  inject-agent
  |=  [vane=_dep-gall =agent:gall =desk =dude:gall =duct]
  ^-  [moves=(list move) _dep-gall]
  =/  =wire  /sys/cor/[dude]/~dep/[desk]/foo
  =/  =sign-arvo
    =;  =gift:clay
      [%clay gift]
    :-  %writ
    %-  some
    :+  [*care:clay *case:clay desk]
      *path
    [%vase !>(!>(agent))]
  ::
  =^  moves  dep-gall  (take dep-gall wire duct sign-arvo)
  [moves dep-gall]
::
++  task-test-dummy
  |=  [vane=_dep-gall taz=task:agent:gall]
  ^-  [moves=(list move) _dep-gall]
  ::
  =;  =task:gall  (call vane ~[/perm] task)
  =/  =sock  [~dep ~dep]
  =/  =term  %buster
  [%deal sock term taz]
::
++  scry-test-dummy-bowl
  |=  vane=_dep-gall
  ^-  bowl:gall
  ::
  =;  res  !<(bowl:gall q:(need (need res)))
  %-  scry
  :+  vane  %x
  [[p=~dep q=%buster r=[%da now.dep-gall]] s=/bowl/noun]
::
::  +gall-call: have %gall run a +task and assert it produces expected-moves
::
++  gall-call
  |=  $:  gall-gate=_nec-gall-pupal
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:gall)]
          expected-moves=(list move)
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
