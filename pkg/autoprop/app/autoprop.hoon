::  autoprop: make pills & props when desk contents change
::
::    link at the dojo command line:
::      |link %autoprop
::
::    auto-build a new .urb/put/latest-solid.pill:
::      +latest-solid solid %kids %garden
::
::    stop auto-building latest-solid:
::      -latest-solid
::
::    auto-build a new .urb/put/somedesk.jam:
::      +some-desk desk %somedesk
::
::    see currently configured build tasks:
::      ?
::
::    run task right now:
::      !latest-solid
::
/+  libpill=pill, shoe, verb, dbug, default-agent
::
=,  clay
::
|%
+$  state-0
  $:  %0
      make=(map @ta [next=(unit @da) =task])            ::  things to make
      hear=(set desk)                                   ::  observed desks
      vers=path                                         ::  runtime version
      sole=sole-id                                      ::  the way out
  ==
::
+$  sole-id  sole-id:sole:shoe
::
+$  task
  $%  pill
      prop
  ==
::
+$  pill
  $%  [%ivory base=desk]
      [%solid base=desk etc=(set desk)]
      [%brass base=desk etc=(set desk)]
  ==
::
+$  prop
  $%  [%desk =desk]
  ==
::
+$  command
  $%  [%put name=@ta =task]  ::  configure pill build
      [%del name=@ta]        ::  remove pill build
      [%see ~]               ::  help & current config
      [%run name=@ta]        ::  force build
  ==
::
+$  card  card:shoe
::
++  delay  ~d5
--
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
%-  (agent:shoe command)
^-  (shoe:shoe command)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    des   ~(. (default:shoe this command) bowl)
    rev   rev:.^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
::
++  on-init
  ^-  (quip card _this)
  :_  this(vers rev)
  [%pass /vers %arvo %b %wait (add now.bowl ~m5)]~
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  command-parser
  |=  =sole-id
  ^+  |~(nail *(like [? command]))
  %+  pick
    (cold [%see ~] wut)
  |^  ;~  pose
        (stag %put ;~(plug (ifix [lus ace] sym) ;~(pose pil pro)))
        ;~(plug (cold %del hep) sym)
        ;~(plug (cold %run zap) sym)
      ==
  ::
  ++  pil
    ;~  pose
      ;~(plug (perk %ivory ~) ;~(pfix ace des))
      ;~(plug (perk %solid ~) ;~(pfix ace dez))
      ;~(plug (perk %brass ~) ;~(pfix ace dez))
    ==
  ::
  ++  pro
    ;~(plug (perk %desk ~) ;~(pfix ace des))
  ::
  ++  des
    ;~(pfix cen sym)
  ::
  ++  dez
    ;~  plug
      des
      ;~  pose
        ;~(pfix ace (cook ~(gas in *(set desk)) (more ace des)))
        (easy ~)
      ==
    ==
  --
::
++  on-command
  |=  [=sole-id =command]
  ^-  (quip card _this)
  ?-  -.command
      %put
    =*  task  task.command
    =/  targ=(set desk)
      ?-  -.task
        %ivory            [base.task ~ ~]
        ?(%solid %brass)  (~(put in etc.task) base.task)
        %desk             [desk.task ~ ~]
      ==
    =/  news  (~(dif in targ) hear)
    =.  hear  (~(uni in hear) targ)
    =.  make  (~(put by make) name.command `now.bowl task.command)
    :_  this
    %+  turn  ~(tap in news)
    |=  =desk
    :+  %pass  /desk/[desk]
    [%arvo %c %warp our.bowl desk `[%next %z da+now.bowl /]]
  ::
      %del
    ::NOTE  deletion from hear, if necessary, handled in +on-arvo
    [~ this(make (~(del by make) name.command))]
  ::
      %see
    :_  this
    ^-  (list card)
    =-  [%shoe [sole-id]~ %sole %mor -]~
    ^-  (list sole-effect:shoe)
    :-  [%txt "to add:  +task-name type %args"]
    :-  [%txt "         where type is one of: solid, brass, ivory, desk"]
    :-  [%txt "to del:  -task-name"]
    :-  [%txt "to run:  !task-name"]
    ?:  =(~ make)
      [%txt "no builds configured"]~
    :-  [%txt "--"]
    :-  [%txt "builds:"]
    %+  turn  ~(tap by make)
    |=  [name=@ta next=(unit @da) =task]
    :-  %txt
    %-  trip
    %+  rap  3
    :*  name  ' ('
        ?~(next 'up to date' (scot %da u.next))
        ') : +'  -.task  ' %'
      ::
        ?-  -.task
          %ivory            [base.task ~]
          ?(%solid %brass)  :-  base.task
                            ?:  =(~ etc.task)  ~
                            [' %' (join ' %' ~(tap in etc.task))]
          %desk             [desk.task ~]
    ==  ==
  ::
      %run
    =*  name  name.command
    =+  (~(got by make) name)
    :_  =-  this(make (~(jab by make) name -))
        |=([next=(unit @da) =^task] [~ task])
    ::TODO  just poke hood instead?
    =;  sag=sole-effect:shoe
      :_  ~
      :+  %shoe  [sole]~
      :+  %sole  %mor
      :~  [%txt "{(trip dap.bowl)} built {(trip name)}"]
          sag
      ==
    |^  =*  base  base.task
        :+  %sag
          =/  ver=@ta
            =+  .^(vere %$ /(scot %p our.bowl)//(scot %da now.bowl)/zen/ver)
            ?+  rev  (rap 3 (join '-' rev))
              [%vere @ ~]    (fall (slaw %ta i.t.rev) i.t.rev)
              [%vere @ @ ~]  (fall (slaw %ta i.t.t.rev) i.t.t.rev)
            ==
          ?-  -.task
            ?(%solid %brass %ivory)  /[dap.bowl]/[ver]/[name]/pill
            %desk                    /[dap.bowl]/[ver]/[name]/jam
          ==
        ?-  -.task
          %ivory  (ivory:libpill (sys base))
        ::
            ?(%solid %brass)
          =/  dez=(list [desk path])
            %+  turn  ~(tap in etc.task)
            |=(d=desk [d (bek d)])
          ?-  -.task
            %solid  (solid:libpill (sys base) dez | now.bowl & ~)
            %brass  (brass:libpill (sys base) dez & ~)
          ==
        ::
            %desk
          =*  desk  desk.task
          (install:events:libpill desk (byk desk) &)
        ==
    ::
    ++  sys
      |=  d=desk
      `path`(snoc (bek d) %sys)
    ::
    ++  byk
      |=  d=desk
      `beak`[our.bowl d da+now.bowl]
    ::
    ++  bek
      |=  d=desk
      `path`(en-beam (byk d) /)
    --
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?:  ?=([%build ~] wire)
    ::  on-wake, build all tasks whose time has come
    ::
    ?>  ?=(%wake +<.sign)
    =/  tasks=(list @ta)
      %+  murn  ~(tap by make)
      |=  [name=@ta next=(unit @da) task]
      ?~  next  ~
      ?:((lte u.next now.bowl) (some name) ~)
    ?^  error.sign
      ((slog 'on-wake build failed' >tasks< u.error.sign) ~ this)
    ::
    =|  cards=(list card)
    |-
    ?~  tasks  [cards this]
    =^  caz  this  (on-command sole %run i.tasks)
    $(tasks t.tasks, cards (weld cards caz))
  ::
  ?:  ?=([%vers ~] wire)
    ::  on-wake, republish props if we're on a new runtime
    ::
    ?>  ?=(%wake +<.sign)
    =/  next=card
      [%pass /vers %arvo %b %wait (add now.bowl ~m5)]
    ?^  error.sign
      %-  (slog 'on-wake vers failed' u.error.sign)
      [[next]~ this]
    ?:  =(rev vers)  [[next]~ this]
    =.  vers  rev
    =/  tasks=(list @ta)  ~(tap in ~(key by make))
    =|  cards=(list card)
    |-
    ?~  tasks  [[next cards] this]
    =^  caz  this  (on-command sole %run i.tasks)
    $(tasks t.tasks, cards (weld cards caz))
  ::
  ?>  ?=([%desk @ ~] wire)
  =*  desk  i.t.wire
  ?.  ?=(%writ +<.sign)
    ~&  [dap.bowl %unexpected-sign +<.sign]
    [~ this]
  ::  on-writ, bump build timers for all affected tasks
  ::
  =/  tasks=(list @ta)
    %+  murn  ~(tap by make)
    |=  [name=@ta (unit @da) =task]
    =-  ?:(- (some name) ~)
    ?-  -.task
      %ivory            =(desk base.task)
      ?(%solid %brass)  |(=(desk base.task) (~(has in etc.task) desk))
      %desk             =(desk desk.task)
    ==
  ?:  =(~ tasks)
    [~ this(hear (~(del in hear) desk))]
  ::
  =/  next=@da  (add now.bowl delay)
  :_  ::  delay next build for affected tasks
      ::
      |-  ?~  tasks  this
      =.  make
        %+  ~(jab by make)  i.tasks
        |=([(unit @da) =task] [`next task])
      $(tasks t.tasks)
  :~  ::  watch for the next change on this desk
      ::
      :+  %pass  /desk/[desk]
      [%arvo %c %warp our.bowl desk `[%next %z da+now.bowl /]]
    ::
      ::  set a timer for building affected tasks
      ::
      [%pass /build %arvo %b %wait next]
  ==
::
++  on-connect
  |=  =sole-id
  ^-  (quip card _this)
  ::TODO  actually should just poke drum to write
  [~ this(sole sole-id)]
::
++  can-connect     |=(* =(src our):bowl)
++  on-disconnect   on-disconnect:des
++  tab-list        tab-list:des
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
