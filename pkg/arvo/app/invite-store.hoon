::  invite-store [landscape]
/-  store=invite-store
/+  res=resource, default-agent, dbug, *migrate
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
  ==
::
+$  invitatory-0  (map serial:store invite-0)
+$  invite-0
  $:  =ship           ::  ship to subscribe to upon accepting invite
      app=@tas        ::  app to subscribe to upon accepting invite
      =path           ::  path to subscribe to upon accepting invite
      recipient=ship  ::  recipient to receive invite
      text=cord       ::  text to describe the invite
  ==
::
+$  state-0  [%0 invites=(map path invitatory-0)]
+$  state-1  [%1 =invites:store]
--
::
=|  state-1
=*  state  -
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :-  ~
  %_  this
      invites.state
    %-  ~(gas by *invites:store)
    [%graph *invitatory:store]~
  ==
::
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  =/  old  !<(versioned-state old-vase)
  ?:  ?=(%1 -.old)
   `this(state old)
  :-  =-  [%pass / %agent [our.bowl %invite-store] %poke %invite-action -]~
      !>  ^-  action:store
      [%create %graph]
  %=  this
      state
    :-  %1
    %-  ~(gas by *invites:store)
    %+  murn  ~(tap by invites.old)
    |=  [=path =invitatory-0]
    ^-  (unit [term invitatory:store])
    ?.  ?=([@ ~] path)  ~
    :-  ~
    :-  i.path
    %-  ~(gas by *invitatory:store)
    %+  murn  ~(tap by invitatory-0)
    |=  [=serial:store =invite-0]
    ^-  (unit [serial:store invite:store])
    =/  resource=(unit resource:res)  (de-path-soft:res path.invite-0)
    ?~  resource  ~
    :-  ~
    :-  serial
    ^-  invite:store
    :*  ship.invite-0
        app.invite-0
        u.resource
        recipient.invite-0
        text.invite-0
    ==
  ==
::
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+    path  (on-watch:def path)
        [%all ~]      [%give %fact ~ %invite-update !>([%initial invites])]~
        [%updates ~]  ~
        [%invitatory @ ~]
      =/  inv=invitatory:store  (~(got by invites) i.t.path)
      [%give %fact ~ %invite-update !>([%invitatory inv])]~
    ==
  [cards this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
      %invite-action  (poke-invite-action !<(action:store vase))
      %import         (poke-import q.vase)
    ==
  [cards this]
  ::
  ++  poke-import
    |=  arc=*
    ^-  (quip card _state)
    =/  sty=state-1
      :-  %1
      %-  remake-map-of-map
      ;;((tree [term (tree [serial:store invite:store])]) +.arc)
    [~ sty]
  ::
  ++  poke-invite-action
    |=  =action:store
    ^-  (quip card _state)
    ?-  -.action
      %create   (handle-create +.action)
      %delete   (handle-delete +.action)
      %invite   (handle-invite +.action)
      %accept   (handle-accept +.action)
      %decline  (handle-decline +.action)
      %invites  ~|('only send this to %invite-hook' !!)
    ==
  ::
  ++  handle-create
    |=  =term
    ^-  (quip card _state)
    ?:  (~(has by invites) term)
      [~ state]
    :-  (send-diff term [%create term])
    state(invites (~(put by invites) term *invitatory:store))
  ::
  ++  handle-delete
    |=  =term
    ^-  (quip card _state)
    ?.  (~(has by invites) term)
      [~ state]
    :-  (send-diff term [%delete term])
    state(invites (~(del by invites) term))
  ::
  ++  handle-invite
    |=  [=term =serial:store =invite:store]
    ^-  (quip card _state)
    ?.  (~(has by invites) term)
      [~ state]
    =/  container  (~(got by invites) term)
    =.  serial  (sham eny.bowl)
    =.  container  (~(put by container) serial invite)
    :-  (send-diff term [%invite term serial invite])
    state(invites (~(put by invites) term container))
  ::
  ++  handle-accept
    |=  [=term =serial:store]
    ^-  (quip card _state)
    ?.  (~(has by invites) term)
      [~ state]
    =/  container  (~(got by invites) term)
    =/  invite  (~(get by container) serial)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) serial)
    :-  (send-diff term [%accepted term serial u.invite])
    state(invites (~(put by invites) term container))
  ::
  ++  handle-decline
    |=  [=term =serial:store]
    ^-  (quip card _state)
    ?.  (~(has by invites) term)
      [~ state]
    =/  container  (~(got by invites) term)
    =/  invite  (~(get by container) serial)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) serial)
    :-  (send-diff term [%decline term serial])
    state(invites (~(put by invites) term container))
  ::
  ++  update-subscribers
    |=  [=path =update:store]
    ^-  card
    [%give %fact ~[path] %invite-update !>(update)]
  ::
  ++  send-diff
    |=  [=term =update:store]
    ^-  (list card)
    :~  (update-subscribers /all update)
        (update-subscribers /updates update)
        (update-subscribers /invitatory/[term] update)
    ==
  --
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
      [%x %all ~]
    ``noun+!>(invites)
  ::
      [%x %invitatory @ ~]
    :^  ~  ~  %noun
    !>  ^-  (unit invitatory:store)
    (~(get by invites) i.t.t.path)
  ::
      [%x %invite @ @ ~]
    =*  term  i.t.t.path
    =/  =serial:store  (slav %uv i.t.t.t.path)
    ?.  (~(has by invites) term)
      ~
    =/  =invitatory:store  (~(got by invites) term)
    :^  ~  ~  %noun
    !>  ^-  (unit invite:store)
    (~(get by invitatory) serial)
  ::
      [%x %export ~]      ``noun+!>(state)
  ==
--
