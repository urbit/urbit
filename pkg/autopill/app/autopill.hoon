::  autopill: make pills when desk contents change
::
::    auto-build a new .urb/put/latest-solid.pill:
::      latest-solid +solid %kids %garden
::
::    stop auto-building latest-solid:
::      -latest-solid
::
::    see currently configured pill builds:
::      ?
::
/+  libpill=pill, shoe, verb, dbug, default-agent
|%
+$  state-0
  $:  %0
      make=(map @ta pill)  ::  pills to make
      hear=(set desk)      ::  desks we are observing
      sole=@ta             ::  the way out
  ==
::
+$  pill
  $%  [%ivory base=desk]
      [%solid base=desk etc=(set desk)]
      [%brass base=desk etc=(set desk)]
  ==
::
+$  command
  $%  [%put name=@ta =pill]  ::  configure pill build
      [%del name=@ta]        ::  remove pill build
      [%see ~]               ::  help & current config
  ==
::
+$  card  card:shoe
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
::
++  on-init
  ^-  (quip card _this)
  [~ this]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  command-parser
  |=  sole-id=@ta
  ^+  |~(nail *(like [? command]))
  %+  pick
    (cold [%see ~] wut)
  |^  ;~  pose
        (stag %put ;~(plug sym ;~(pfix ace lus pil)))
        ;~(plug (cold %del hep) sym)
      ==
  ::
  ++  pil
    ;~  pose
      ;~(plug (perk %ivory ~) ;~(pfix ace des))
      ;~(plug (perk %solid ~) ;~(pfix ace dez))
      ;~(plug (perk %brass ~) ;~(pfix ace dez))
    ==
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
  |=  [sole-id=@ta =command]
  ^-  (quip card _this)
  ?-  -.command
      %put
    =*  pill  pill.command
    =/  target-desks=(set desk)
      ?-  -.pill
        %ivory            [base.pill ~ ~]
        ?(%solid %brass)  (~(put in etc.pill) base.pill)
      ==
    =/  new-desks=(set desk)
      (~(dif in target-desks) hear)
    ::
    =.  make  (~(put by make) +.command)
    =.  hear  (~(uni in hear) target-desks)
    :_  this
    %+  turn  ~(tap in new-desks)
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
    :-  [%txt "to add:  pill-name +type %args"]
    :-  [%txt "to del:  -pill-name"]
    ?:  =(~ make)
      [%txt "no builds configured"]~
    :-  [%txt "--"]
    :-  [%txt "builds:"]
    %+  turn  ~(tap by make)
    |=  [name=@ta =pill]
    :-  %txt
    %-  trip
    %+  rap  3
    =-  [name ' : +' -.pill ' %' -]
    ?-  -.pill
      %ivory            [base.pill ~]
      ?(%solid %brass)  [base.pill ' %' (join ' %' ~(tap in etc.pill))]
    ==
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?>  ?=([%desk @ ~] wire)
  =*  desk  i.t.wire
  ?.  ?=(%writ +<.sign)
    ~&  [dap.bowl %unexpected-sign +<.sign]
    [~ this]
  ::
  =/  pills=(list [name=@ta =pill])
    %+  skim  ~(tap by make)
    |=  [@ta =pill]
    ?-  -.pill
      %ivory            =(desk base.pill)
      ?(%solid %brass)  |(=(desk base.pill) (~(has in etc.pill) desk))
    ==
  ?:  =(~ pills)
    [~ this(hear (~(del in hear) desk))]
  ::
  =|  cards=(list card)
  |-
  ?~  pills
    =-  [[- cards] this]
    :+  %pass  /desk/[desk]
    [%arvo %c %warp our.bowl desk `[%next %z da+now.bowl /]]
  =;  =card
    $(pills t.pills, cards [card cards])
  ::
  =*  name  name.i.pills
  =*  pill  pill.i.pills
  =-  [%shoe [sole]~ %sole %mor -]
  =;  sag=sole-effect:shoe
    [[%txt "built {(trip name)}.pill"] sag ~]
  |^  =*  base  base.pill
      :+  %sag  /pills/[name]/pill
      ?-  -.pill
        %ivory  (ivory:libpill (sys base))
      ::
          ?(%solid %brass)
        =/  dez=(list [^desk path])
          %+  turn  ~(tap in etc.pill)
          |=(d=^desk [d (bek d)])
        ?-  -.pill
          %solid  (solid:libpill (sys base) dez | now.bowl)
          %brass  (brass:libpill (sys base) dez)
        ==
      ==
  ::
  ++  sys
    |=  d=^desk
    (snoc (bek d) %sys)
  ::
  ++  bek
    |=  d=^desk
    ^-  path
    /(scot %p our.bowl)/[d]/(scot %da now.bowl)
  --
::
++  on-connect
  |=  sole-id=@ta
  ^-  (quip card _this)
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
