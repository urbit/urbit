::  link-listen-hook: get your friends' bookmarks
::
::    on-init, subscribes to all groups on this ship.
::    for every ship in a group, we subscribe to their link's local-pages
::    at the group path (through link-proxy-hook),
::    and forwards all entries into our link as submissions.
::
/-  *link, group-store
/+  default-agent, verb, dbug
::
|%
+$  state-0
  $:  %0
      ~
      ::NOTE  this means we could get away with just producing cards everywhere,
      ::      never producing new state outside of the agent interface core.
      ::      we opt to keep ^-(quip card _state) in place for most logic arms
      ::      because it doesn't cost much, results in unsurprising code, and
      ::      makes adding any state in the future easier.
  ==
::
+$  target
  $:  what=%local-pages  ::TODO  %annotations
      who=ship
      where=path
  ==
++  wire-to-target
  |=  =wire
  ^-  target
  ?>  ?=([%local-pages @ ^] wire)  ::TODO  %annotations
  [i.wire (slav %p i.t.wire) t.t.wire]
++  target-to-wire
  |=  target
  ^-  wire
  [what (scot %p who) where]
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    [watch-groups:do]~
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?:  ?=([%groups ~] wire)
      =^  cards  state
        (take-groups-sign:do sign)
      [cards this]
    ?:  ?=([%links @ @ ^] wire)
      =^  cards  state
        (take-links-sign:do (wire-to-target t.wire) sign)
      [cards this]
    ?:  ?=([%forward ^] wire)
      =^  cards  state
        (take-forward-sign:do t.wire sign)
      [cards this]
    ~|  [dap.bowl %weird-wire wire]
    !!
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%g %done *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    ?~  error.sign-arvo  [~ this]
    =/  =tank  leaf+"{(trip dap.bowl)}'s message went wrong!"
    %-  (slog tank tang.u.error.sign-arvo)
    [~ this]
  ::
  ++  on-poke   on-poke:def
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
::
|_  =bowl:gall
::
::  groups subscription
::
++  watch-groups
  ^-  card
  [%pass /groups %agent [our.bowl %group-store] %watch /all]
::
++  take-groups-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /groups] !!)
    %kick       [[watch-groups]~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bowl)} failed subscribe to groups. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ~&  [dap.bowl %fact mark]
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
      %group-initial  (handle-group-initial !<(groups:group-store vase))
      %group-update   (handle-group-update !<(group-update:group-store vase))
    ==
  ==
::
++  handle-group-initial
  |=  =groups:group-store
  ^-  (quip card _state)
  =|  cards=(list card)
  =/  groups=(list [=path =group:group-store])
    ~(tap by groups)
  |-
  ?~  groups  [cards state]
  =^  caz  state
    %-  handle-group-update
    [%add [group path]:i.groups]
  $(cards (weld cards caz), groups t.groups)
::
++  handle-group-update
  |=  upd=group-update:group-store
  ^-  (quip card _state)
  :_  state
  ?+  -.upd  ~
      ?(%path %add %remove)
    =/  whos=(list ship)  ~(tap in members.upd)
    |-  ^-  (list card)
    ?~  whos  ~
    ::  no need to subscribe to ourselves
    ::
    ?:  =(our.bowl i.whos)
      $(whos t.whos)
    :_  $(whos t.whos)
    %.  [%local-pages i.whos pax.upd]  ::TODO  %annotations
    ?:  ?=(%remove -.upd)
      end-link-subscription
    start-link-subscription
  ==
::
::  link subscriptions
::
++  start-link-subscription
  |=  =target
  ^-  card
  :*  %pass
      [%links (target-to-wire target)]
      %agent
      [who.target %link-proxy-hook]
      %watch
      [what where]:target
  ==
::
++  end-link-subscription
  |=  =target
  ^-  card
  :*  %pass
      [%links (target-to-wire target)]
      %agent
      [who.target %link-proxy-hook]
      %leave
      ~
  ==
::
++  take-links-sign
  |=  [=target =sign:agent:gall]
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /links target] !!)
    %kick       [[(start-link-subscription target)]~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    ::  our subscription request got rejected for whatever reason,
    ::  (most likely difference in group membership,)
    ::  so we don't try again.
    ::TODO  but now the only way to retry is to remove from group and re-add...
    ::      this is a problem because our and their group may not update
    ::      simultaneously...
    [~ state]
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
        %link-update
      %-  handle-link-update
      [who.target where.target !<(update vase)]
    ==
  ==
::
++  handle-link-update
  |=  [who=ship where=path =update]
  ^-  (quip card _state)
  ?>  ?=(%local-pages -.update)  ::TODO  %annotations
  ?>  =(src.bowl who)
  :_  state
  %+  turn  pages.update
  |=  =page
  ^-  card
  :*  %pass
      [%forward -.update (scot %p who) where]
      %agent
      [our.bowl %link-store]
      %poke
      %link-action
      !>([%hear where src.bowl page])
  ==
::
++  take-forward-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  ~|  [%unexpected-sign on=[%forward wire] -.sign]
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign  [~ state]
  =/  =tank
    :-  %leaf
    ;:  weld
      (trip dap.bowl)
      " failed to save submission from "
      (spud wire)
    ==
  %-  (slog tank u.p.sign)
  [~ state]
--
