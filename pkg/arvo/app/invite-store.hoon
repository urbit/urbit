::  invite-store [landscape]
/-  *invite-store
/+  default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0  [%0 =invites]
--
::
=|  state-0
=*  state  -
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this      .
    def       ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load
  |=  old=vase
  `this(state !<(state-0 old))
::
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =/  cards=(list card)
    ?+    path  (on-watch:def path)
        [%all ~]      [%give %fact ~ %invite-update !>([%initial invites])]~
        [%updates ~]  ~
        [%invitatory *]
      =/  inv=invitatory  (~(got by invites) t.path)
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
        %create   (handle-create action)
        %delete   (handle-delete action)
        %invite   (handle-invite action)
        %accept   (handle-accept action)
        %decline  (handle-decline action)
    ==
  ::
  ++  handle-create
    |=  act=invite-action
    ^-  (quip card _state)
    ?>  ?=(%create -.act)
    ?:  (~(has by invites) path.act)
      [~ state]
    :-  (send-diff path.act act)
    state(invites (~(put by invites) path.act *invitatory))
  ::
  ++  handle-delete
    |=  act=invite-action
    ^-  (quip card _state)
    ?>  ?=(%delete -.act)
    ?.  (~(has by invites) path.act)
      [~ state]
    :-  (send-diff path.act act)
    state(invites (~(del by invites) path.act))
  ::
  ++  handle-invite
    |=  act=invite-action
    ^-  (quip card _state)
    ?>  ?=(%invite -.act)
    ?.  (~(has by invites) path.act)
      [~ state]
    =/  container  (~(got by invites) path.act)
    =.  uid.act  (sham eny.bowl)
    =.  container  (~(put by container) uid.act invite.act)
    :-  (send-diff path.act act)
    state(invites (~(put by invites) path.act container))
  ::
  ++  handle-accept
    |=  act=invite-action
    ^-  (quip card _state)
    ?>  ?=(%accept -.act)
    ?.  (~(has by invites) path.act)
      [~ state]
    =/  container  (~(got by invites) path.act)
    =/  invite  (~(get by container) uid.act)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) uid.act)
    :-  (send-diff path.act [%accepted path.act uid.act u.invite])
    state(invites (~(put by invites) path.act container))
  ::
  ++  handle-decline
    |=  act=invite-action
    ^-  (quip card _state)
    ?>  ?=(%decline -.act)
    ?.  (~(has by invites) path.act)
      [~ state]
    =/  container  (~(got by invites) path.act)
    =/  invite  (~(get by container) uid.act)
    ?~  invite
      [~ state]
    =.  container  (~(del by container) uid.act)
    :-  (send-diff path.act act)
    state(invites (~(put by invites) path.act container))
  ::
  ++  update-subscribers
    |=  [pax=path upd=invite-update]
    ^-  card
    [%give %fact ~[pax] %invite-update !>(upd)]
  ::
  ++  send-diff
    |=  [pax=path upd=invite-update]
    ^-  (list card)
    :~  (update-subscribers /all upd)
        (update-subscribers /updates upd)
        (update-subscribers [%invitatory pax] upd)
    ==
  --
::
++  on-leave  on-leave:def
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
    =/  invitatory=(unit invitatory)  (~(get by invites) pax)
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
    =/  invitatory=(unit invitatory)  (~(get by invites) pax)
    ?~  invitatory
      ~
    =/  invite=(unit invite)  (~(get by u.invitatory) uid)
    [~ ~ %noun !>(invite)]
  --
--
