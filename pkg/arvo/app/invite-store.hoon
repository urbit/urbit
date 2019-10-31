/+  *invite-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff diff]
      [%quit ~]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  =invites
  ==
::
+$  diff
  $%  [%invite-initial invites]
      [%invite-update invite-update]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  peek-x-all
  |=  pax=path
  ^-  (unit (unit [%noun (map path invitatory)]))
  [~ ~ %noun invites]
::
++  peek-x-invitatory
  |=  pax=path
  ^-  (unit (unit [%noun (unit invitatory)]))
  ?~  pax
    ~
  =/  invitatory=(unit invitatory)  (~(get by invites) pax)
  [~ ~ %noun invitatory]
::
++  peek-x-invite
  |=  pax=path
  ^-  (unit (unit [%noun (unit invite)]))
  ?~  pax
    ~
  =/  invitatory=(unit invitatory)  (~(get by invites) t.pax)
  ?~  invitatory
    ~
  =/  uid=serial  (slav %uv i.pax)
  =/  invite=(unit invite)  (~(get by u.invitatory) uid)
  [~ ~ %noun invite]
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  send all updates from now on
  :_  this
  [ost.bol %diff %invite-initial invites]~
::
++  peer-updates
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  send all updates from now on
  [~ this]
::
++  peer-invitatory
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  =/  inv=(unit invitatory)  (~(get by invites) pax)
  ?~  inv  !!
  :_  this
  [ost.bol %diff %invite-update [%invitatory u.inv]]~
::
++  poke-json
  |=  =json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  (poke-invite-action (json-to-action json))
::
++  poke-invite-action
  |=  action=invite-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %create
    (handle-create action)
  ::
      %delete
    (handle-delete action)
  ::
      %invite
    (handle-invite action)
  ::
      %accept
    (handle-accept action)
  ::
      %decline
    (handle-decline action)
  ::
  ==
::
++  handle-create
  |=  act=invite-action
  ^-  (quip move _this)
  ?>  ?=(%create -.act)
  ?:  (~(has by invites) path.act)
    [~ this]
  :-  (send-diff path.act act)
  this(invites (~(put by invites) path.act *invitatory))
::
++  handle-delete
  |=  act=invite-action
  ^-  (quip move _this)
  ?>  ?=(%delete -.act)
  ?.  (~(has by invites) path.act)
    [~ this]
  :-  (send-diff path.act act)
  this(invites (~(del by invites) path.act))
::
++  handle-invite
  |=  act=invite-action
  ^-  (quip move _this)
  ?>  ?=(%invite -.act)
  ?.  (~(has by invites) path.act)
    [~ this]
  =/  container  (~(got by invites) path.act)
  =.  uid.act  (sham eny.bol)
  =.  container  (~(put by container) uid.act invite.act)
  :-  (send-diff path.act act)
  this(invites (~(put by invites) path.act container))
::
++  handle-accept
  |=  act=invite-action
  ^-  (quip move _this)
  ?>  ?=(%accept -.act)
  ?.  (~(has by invites) path.act)
    [~ this]
  =/  container  (~(got by invites) path.act)
  =/  invite  (~(get by container) uid.act)
  ?~  invite
    [~ this]
  =.  container  (~(del by container) uid.act)
  :-  (send-diff path.act [%accepted path.act uid.act u.invite])
  this(invites (~(put by invites) path.act container))
::
++  handle-decline
  |=  act=invite-action
  ^-  (quip move _this)
  ?>  ?=(%decline -.act)
  ?.  (~(has by invites) path.act)
    [~ this]
  =/  container  (~(got by invites) path.act)
  =/  invite  (~(get by container) uid.act)
  ?~  invite
    [~ this]
  =.  container  (~(del by container) uid.act)
  :-  (send-diff path.act act)
  this(invites (~(put by invites) path.act container))
::
++  update-subscribers
  |=  [pax=path upd=invite-update]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %invite-update upd]
::
++  send-diff
  |=  [pax=path upd=invite-update]
  ^-  (list move)
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%invitatory pax] upd)
  ==
::
--
