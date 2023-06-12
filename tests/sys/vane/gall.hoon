/+  dbug, default-agent, strandio, *test
/=  gall-raw  /sys/vane/gall
/=  clay-raw  /sys/vane/clay
::
::  gall boilerplate
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
::  clay boilerplate
=/  clay-gate  (clay-raw ~nec)
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
          ~&  pes.bowl
          :_  this
          [%give %fact ~ %noun !>(`*`pes.bowl)]~
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
            :_  this
            [%give %fact ~ %noun !>(+>.sign-arvo)]~
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
++  test-load-dudes
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  expected-moves=(list move)
    [duct %pass /sys/say %d %text "gall: booted %buster"]~
  ::
  =^  moves  dep-gall
    (load-one dep-gall duct test-desk ~ `[%buster test-dummy])
  ::
  %+  expect-eq
    !>  expected-moves
  ::
    !>  moves
::
++  test-load-perms
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  =/  perms
    (~(gas in *(set perm:gall)) [%ames %debug ~]~)
  =/  expected-jug
    (~(gas ju *(jug desk perm:gall)) [test-desk [%ames %debug ~]]~)
  ::
  =^  moves  dep-gall
    (load-one dep-gall duct test-desk perms ~)
  ::
  %+  expect-eq
    !>  perms.state.dep-gall
  ::
    !>  expected-jug
::
++  test-dummy-empty-bowl
  ^-  tang
  ::
  =/  =duct  ~[/init]
  =/  gat
    |=  =bowl:gall
    =(~ pes.bowl)
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
++  test-read-bowl-permissions
  ^-  tang
  =/  =duct  ~[/perm]
  =/  pes  (~(gas in *(set perm:gall)) [%ames %debug ~]~)
  ::
  =/  poke-1=task:agent:gall
    [%poke %noun !>(`poke`[%get-perms ~])]
  ::
  =/  expected-moves=(list move)
    =/  move-1=move
      [duct %give %unto %poke-ack ~]
    =/  move-2=move
      =/  =sign:agent:gall  [%fact %noun !>(`*`pes)]
      [duct %give %unto sign]
    [move-1 move-2 ~]
  ::
  =^  moves  dep-gall
    (load-one dep-gall duct test-desk pes `[%buster test-dummy])
  =^  moves  dep-gall
    (task-test-dummy dep-gall poke-1)
  ::TODO: there's something weird about comparing these. the values are
  ::equal, but the types are different, even though they're both
  ::(set perm:gall)...????
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
++  test-ward-wink-notifications
  ^-  tang
  ::
  =/  duct-1=duct  ~[/perm]
  =/  duct-2=duct  ~[/init]
  =/  pes  (~(gas in *(set perm:gall)) [%ames %debug ~]~)
  ::
  =/  task-1=task:gall  [%ward ~]
  =/  task-2=task:gall  [%wink ~]
  ::
  =/  expected-moves-ward=(list move)
    [duct-1 %give %perm test-desk free=pes lock=*(set perm:gall)]~
  =|  expected-moves-wink=(list move)
  ::
  =^  moves  dep-gall
    (call dep-gall duct-1 task-1)
  =^  moves-ward  dep-gall
    (load-one dep-gall duct-2 test-desk pes ~)
  =^  moves  dep-gall
    (call dep-gall duct-1 task-2)
  =^  moves-wink  dep-gall
    (load-one dep-gall duct-2 test-desk *(set perm:gall) ~)
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
::  +test-base-perms: %base agents should have all permissions by default
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
    (load-one dep-gall duct %base ~ `[%buster test-dummy])
  =^  moves  dep-gall
    (task-test-dummy dep-gall poke-1)
  ::
  %+  expect-eq
    !>  expected-moves
  ::
    !>  moves
::
::  +test-non-base-perms: make sure moves get dropped outside of base w/o perms
::TODO: fails, gall printfs %would-drop but the move still appears in the list
++  test-non-base-perms
  ^-  tang
  ::
  =/  =duct  ~[/perm]
  ::
  =/  task-1=task:ames  [%sift *(list ship)]
  ::
  =/  poke-1=task:agent:gall
    [%poke %noun !>(`poke`[%ames-test task-1])]
  ::
  =^  moves  dep-gall
    (load-one dep-gall duct test-desk ~ `[%buster test-dummy])
  =^  moves  dep-gall
    (task-test-dummy dep-gall poke-1)
  ::
  %-  expect  !>
  ::  avoid having to account for tank printing nonsense
  ::TODO PERM  replace the last * with ~ in below when we start enforcing
  ?=([[* %give %unto %poke-ack ~] *] moves)
::
+|  %gall-utilities
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
  =/  res  (scry:(vane) ~ / care bem)
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
::  +load-one: passes a %load task for an agent and perms on one desk
++  load-one
  |=  $:  vane=_dep-gall
          =duct
          =desk
          pes=(set perm:gall)
          dud=(unit [=dude:gall =agent:gall])
      ==
  ^-  [(list move) _dep-gall]
  ::
  =/  task-1=task:gall
    =/  perms=(list [_desk (set perm:gall)])
      [desk pes]~
    :+  %load  perms
    ?~  dud  ~
    =/  =beak  [~dep desk [%da now.dep-gall]]
    [dude.u.dud beak agent.u.dud]~
  ::
  =^  moves  dep-gall  (call dep-gall duct task-1)
  [moves dep-gall]
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
    :+  [*care:clay *case desk]
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
  =/  =sack  [~dep ~dep /]
  =/  =term  %buster
  [%deal sack term taz]
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
::
+|  %clay-utilities
::
++  clay-call
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          call-args=[=duct wrapped-task=(hobo task:clay)]
          expected-moves=(list move:clay-gate)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (call:clay-core [duct ~ wrapped-task]:call-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output clay-gate]
::
++  clay-take
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=roof
          take-args=[=wire =duct =sign:clay-gate]
          expected-moves=(list move:clay-gate)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (take:clay-core [wire duct ~ sign]:take-args)
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output clay-gate]
--
