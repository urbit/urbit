::  invite-store [landscape]
/-  *invite-store
/+  res=resource, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
  ==
::
+$  invitatory-0  (map serial invite-0)
+$  invite-0
  $:  =ship           ::  ship to subscribe to upon accepting invite
      app=@tas        ::  app to subscribe to upon accepting invite
      =path           ::  path to subscribe to upon accepting invite
      recipient=ship  ::  recipient to receive invite
      text=cord       ::  text to describe the invite
  ==
::
+$  state-0  [%0 invites=(map path invitatory-0)]
+$  state-1  [%1 =invites]
--
::
=|  state-1
=*  state  -
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this      .
    def       ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old-vase=vase
  =/  old  !<(versioned-state old-vase)
  ?:  ?=(%1 -.old)
   `this(state old)
  :-  ~
  %=  this
      state
    :-  %1
    %-  ~(gas by *^invites)
    %+  turn  ~(tap by invites.old)
    |=  [=path =invitatory-0]
    ^-  [resource invitatory]
    ?>  ?=([@ @ ~] path)
    :-  [(slav %p i.path) i.t.path]
    %-  ~(gas by *invitatory)
    %+  turn  ~(tap by invitatory-0)
    |=  [uid=serial =invite-0]
    ^-  [serial invite]
    :-  uid
    :*  ship.invite-0
        app.invite-0
        (de-path:res path.invite-0)
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
        [%invitatory *]
      =/  inv=invitatory  (~(got by invites) (de-path:res t.path))
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
      %invite-action  (poke-invite-action !<(invite-action vase))
    ==
  [cards this]
  ::
  ++  poke-invite-action
    |=  action=invite-action
    ^-  (quip card _state)
    ?-  -.action
        %create   (handle-create +.action)
        %delete   (handle-delete +.action)
        %invite   (handle-invite +.action)
        %accept   (handle-accept +.action)
        %decline  (handle-decline +.action)
    ==
  ::
  ++  handle-create
    |=  =resource
    ^-  (quip card _state)
    ?:  (~(has by invites) resource)
      [~ state]
    :-  (send-diff resource [%create resource])
    state(invites (~(put by invites) resource *invitatory))
  ::
  ++  handle-delete
    |=  =resource
    ^-  (quip card _state)
    ?.  (~(has by invites) resource)
      [~ state]
    :-  (send-diff resource [%delete resource])
    state(invites (~(del by invites) resource))
  ::
  ++  handle-invite
    |=  [=resource uid=serial =invite]
    ^-  (quip card _state)
    ?.  (~(has by invites) resource)
      [~ state]
    =/  container  (~(got by invites) resource)
    =.  uid  (sham eny.bowl)
    =.  container  (~(put by container) uid invite)
    :-  (send-diff resource [%invite resource uid invite])
    state(invites (~(put by invites) resource container))
  ::
  ++  handle-accept
    |=  [=resource uid=serial]
    ^-  (quip card _state)
    ?.  (~(has by invites) resource)
      [~ state]
    =/  container  (~(got by invites) resource)
    =/  invite  (~(get by container) uid)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) uid)
    :-  (send-diff resource [%accepted resource uid u.invite])
    state(invites (~(put by invites) resource container))
  ::
  ++  handle-decline
    |=  [=resource uid=serial]
    ^-  (quip card _state)
    ?.  (~(has by invites) resource)
      [~ state]
    =/  container  (~(got by invites) resource)
    =/  invite  (~(get by container) uid)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) uid)
    :-  (send-diff resource [%decline resource uid])
    state(invites (~(put by invites) resource container))
  ::
  ++  update-subscribers
    |=  [=path upd=invite-update]
    ^-  card
    [%give %fact ~[path] %invite-update !>(upd)]
  ::
  ++  send-diff
    |=  [=resource upd=invite-update]
    ^-  (list card)
    :~  (update-subscribers /all upd)
        (update-subscribers /updates upd)
        (update-subscribers [%invitatory (en-path:res resource)] upd)
    ==
  --
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  |^
  ?+  path  (on-peek:def path)
    [%x %all ~]         [~ ~ %noun !>(invites)]
    [%x %invitatory *]  (peek-x-invitatory t.t.path)
    [%x %invite *]      (peek-x-invite t.t.path)
  ==
  ::
  ++  peek-x-invitatory
    |=  pax=^path
    ^-  (unit (unit cage))
    ?~  pax
      ~
    =/  invitatory=(unit invitatory)
      (~(get by invites) (de-path:res pax))
    [~ ~ %noun !>(invitatory)]
  ::
  ++  peek-x-invite
    |=  pax=^path
    ^-  (unit (unit cage))
    ::  /:path/:uid
    =/  pas  (flop pax)
    ?~  pas
      ~
    =/  uid=serial  (slav %uv i.pas)
    =.  pax  (scag (dec (lent pax)) `(list @ta)`pax)
    =/  invitatory=(unit invitatory)
      (~(get by invites) (de-path:res pax))
    ?~  invitatory
      ~
    =/  invite=(unit invite)  (~(get by u.invitatory) uid)
    [~ ~ %noun !>(invite)]
  --
--
