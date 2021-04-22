::  group-timer: automatically remove old members from groups
::
::    to activate, with a timeout of 7 days:
::      :group-timer [%enable [~host %groupname] ~d7]
::
::    after activation for a group, *new* members will be automatically
::    removed from the group after the specified timeout.
::    ships on the whitelist will not be kicked. after being kicked once,
::    ships are automatically added to the whitelist and so will not be
::    kicked a second time if they re-join.
::
::    to manually add ships to the whitelist:
::      :group-timer [%whitelist [~host %groupname] ~[~bud ~nec]]
::
::    to disable for a group:
::      :group-timer [%disable ~host %groupname]
::
/-  gr=group-store
/+  re=resource, verb, dbug, default-agent
::
|%
+$  state
  $:  %0
      active=(map resource:re config)
  ==
::
+$  config
  $:  kick-time=@dr         ::  time to wait post join
      will-kick=(set ship)  ::  timers outstanding for
      whitelist=(set ship)  ::  exempt from timer kick
  ==
::
+$  action
  $%  [%enable group=resource:re timeout=@dr]
      [%disable group=resource:re]
      [%whitelist group=resource:re ships=(list ship)]
  ==
::
+$  card  card:agent:gall
--
::
=|  state
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
      do   ~(. +> bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [%pass / %agent [our.bowl %group-store] %watch /groups]~
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    [~ this(state !<(^state vase))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
        %noun
      =^  cards  state  (on-action:do !<(action vase))
      [cards this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} failed to watch group-store!"
      ((slog tank u.p.sign) [~ this])
    ::
        %kick
      :_  this
      [%pass / %agent [our.bowl %group-store] %watch /groups]~
    ::
        %fact
      ?:  ?=(%group-initial p.cage.sign)  [~ this]
      ?>  ?=(%group-update-0 p.cage.sign)
      =+  !<(=update:gr q.cage.sign)
      ?.  ?=(%add-members -.update)     [~ this]
      =|  cards=(list card)
      =/  ships=(list ship)  ~(tap in ships.update)
      |-
      ?~  ships  [cards this]
      =^  caz  state  (on-join:do resource.update i.ships)
      $(ships t.ships, cards (weld cards caz))
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%behn %wake *]
      =^  cards  state  (on-timer:do wire)
      [cards this]
    ==
  ::
  ++  on-watch  on-watch:def
  ++  on-peek   on-peek:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  en-wire
  |=  [group=resource:re =ship]
  ^-  wire
  [(scot %p ship) (en-path:re group)]
::
++  de-wire
  |=  =wire
  ^-  [group=resource:re =ship]
  ~|  wire=wire
  ?>  ?=([@ *] wire)
  [(de-path:re t.wire) (slav %p i.wire)]
::
++  set-timer
  |=  [group=resource:re =ship timeout=@dr]
  ^-  card
  [%pass (en-wire group ship) %arvo %b %wait (add now.bowl timeout)]
::
++  kick-from-group
  |=  [group=resource:re =ship]
  ^-  card
  :+  %pass   (en-wire group ship)
  :+  %agent  [our.bowl %group-push-hook]
  :+  %poke   %group-update
  !>  ^-  update:gr
  [%remove-members group (sy ship ~)]
::
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  =-  [~ state(active -)]
  ?-  -.action
      %enable
    =,  action
    ?.  (~(has by active) group)
      ~&  [%set-up-for group]
      (~(put by active) group [timeout ~ ~])
    ~&  %updating-timeout
    %+  ~(jab by active)  group
    |=(config +<(kick-time timeout))
  ::
      %disable
    (~(del by active) group.action)
  ::
      %whitelist
    =,  action
    ~|  [%not-active-for-group group]
    %+  ~(jab by active)  group
    |=(config +<(whitelist (~(gas in whitelist) ships)))
  ==
::
++  on-timer
  |=  =wire
  ^-  (quip card _state)
  =+  (de-wire wire)
  ?~  cof=(~(get by active) group)  [~ state]
  :-  ?:  (~(has in whitelist.u.cof) ship)  ~
      [(kick-from-group group ship)]~
  =.  will-kick.u.cof  (~(del in will-kick.u.cof) ship)
  =.  whitelist.u.cof  (~(put in whitelist.u.cof) ship)
  =.  active           (~(put by active) group u.cof)
  state
::
++  on-join
  |=  [group=resource:re =ship]
  ^-  (quip card _state)
  ?~  cof=(~(get by active) group)  [~ state]
  ?:  ?|  (~(has in whitelist.u.cof) ship)
          (~(has in will-kick.u.cof) ship)
      ==
    [~ state]
  :-  [(set-timer group ship kick-time.u.cof)]~
  =.  will-kick.u.cof  (~(put in will-kick.u.cof) ship)
  =.  active           (~(put by active) group u.cof)
  state
--
