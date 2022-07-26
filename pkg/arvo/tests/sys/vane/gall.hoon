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
        ~&  'on-poke'
        ^-  (quip card _this)
        ?.  ?=(%noun mark)
          `this
        =/  action  !<(poke vase)
        ~&  action
        ?-  action
          [%run-test *]
            ~&  (p.action bowl)
            `this
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
    :-  %poke
    :-  %noun
    !>  [%run-test gat]
  ::
  =^  moves  dep-gall  (make-test-dummy dep-gall test-desk %buster)
  =^  moves  dep-gall  (task-test-dummy dep-gall task)
  *tang
  ::
::
++  test-free-test-dummy
  :: it seems that the perms make it into the state
  :: of gall - in perms=(map desk (set perm)). but its not
  :: ending up in the bowl for some reason
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  per  (~(gas in *(set perm:gall)) [%ames %debug]~)
  =/  =task:gall  [%free test-desk per]
  ::
  =^  moves  dep-gall  (make-test-dummy dep-gall test-desk %buster)
  =^  moves  dep-gall   (call dep-gall duct task)
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
  =^  moves  dep-gall  (make-test-dummy dep-gall test-desk %buster)
  =^  moves  dep-gall  (call dep-gall duct task-1)
  =^  moves  dep-gall  (call dep-gall duct task-2)
  =/  =bowl:gall  (scry-test-dummy-bowl dep-gall)
  ::
  %+  expect-eq
    !>  (~(dif in per) ner)
  ::
    !>  per.bowl
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
::  +make-test-dummy: creates a +test-dummy named .dude in .desk
++  make-test-dummy
  |=  [vane=_dep-gall =desk =dude:gall]
  ^-  [moves=(list move) _dep-gall]
  =/  =wire  /sys/cor/[dude]/~dep/[desk]/foo
  =/  =duct  ~[/perm]
  =/  =sign-arvo
    =;  =gift:clay
      [%clay gift]
    :-  %writ
    %-  some
    :+  [*care:clay *case:clay desk]
      *path
    [%vase !>(!>(test-dummy))]
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
