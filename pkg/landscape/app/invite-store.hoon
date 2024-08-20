::  invite-store [landscape]
/-  store=invite-store
/+  res=resource, default-agent, dbug, *migrate
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
      state-2
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
+$  state-2  [%2 =invites:store]
--
::
=|  state-2
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
    :~  [%graph *invitatory:store]
        [%groups *invitatory:store]
    ==
  ==
::
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  =/  old  !<(versioned-state old-vase)
  =|  cards=(list card)
  |-
  ?:  ?=(%2 -.old)
    [cards this(state old)]
  ?:  ?=(%1 -.old)
    =.  cards
      :~  =-  [%pass / %agent [our.bowl %invite-store] %poke %invite-action -]
          !>  ^-  action:store
          [%create %groups]
        ::
          =-  [%pass / %agent [our.bowl %invite-store] %poke %invite-action -]
          !>  ^-  action:store
          [%delete %contacts]
      ==
    $(-.old %2)
  $(old [%1 (~(gas by *invites:store) [%graph *invitatory:store]~)])
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
    =/  sty=state-2
      :-  %2
      %-  remake-map-of-map
      ;;((tree [term (tree [serial:store invite:store])]) +.arc)
    :_  sty
    :~  =-  [%pass / %agent [our.bowl %invite-store] %poke %invite-action -]
        !>  ^-  action:store
        [%create %groups]
      ::
        =-  [%pass / %agent [our.bowl %invite-store] %poke %invite-action -]
        !>  ^-  action:store
        [%delete %contacts]
    ==
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
    ``invite-update+!>([%initial invites])
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
