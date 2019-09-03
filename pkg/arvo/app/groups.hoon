:: service/groups.hoon
/-  *groups
|%
+$  move  [bone card]
+$  card
  $%  [%diff diff]
      [%quit ~]
  ==
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  groups=(map path group)
  ==
::
+$  diff
  $%  [%group-update group-update]
      [%group-initial group-initial]
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
++  peek-x
  |=  pax=path
  ^-  (unit (unit [%noun (unit group)]))
  ?~  pax
    [~ ~ %noun ~]
  =/  grp=(unit group)  (~(get by groups) pax)
  [~ ~ %noun grp]
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we now proxy all events to this path
  :_  this
  [ost.bol %diff %group-initial groups]~
::
++  peer-keys
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %group-update [%keys ~(key by groups)]]~
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  =/  grp=(unit group)  (~(get by groups) pax)
  ?~  grp
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %group-update [%path u.grp pax]]~
::
++  poke-noun
  |=  cmd=cord
  ^-  (quip move _this)
  ~&  groups
  [~ this]
::
++  poke-group-action
  |=  action=group-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?-  -.action
      %add
    (handle-add action)
  ::
      %remove
    (handle-remove action)
  ::
      %bundle
    (handle-bundle action)
  ::
      %unbundle
    (handle-unbundle action)
  ::
  ==
::
++  handle-add
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%add -.act)
  ?~  pax.act
    [~ this]
  :-  (send-diff pax.act act)
  ?:  (~(has by groups) pax.act)
    =/  members=group  (~(got by groups) pax.act)
    =.  members  (~(uni in members) members.act)
    this(groups (~(put by groups) pax.act members))
  this(groups (~(put by groups) pax.act members.act))
::
++  handle-remove
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%remove -.act)
  ?~  pax.act
    [~ this]
  ?.  (~(has by groups) pax.act)
    [~ this]
  =/  members  (~(got by groups) pax.act)
  =.  members  (~(dif in members) members.act)
  :-  (send-diff pax.act act)
  this(groups (~(put by groups) pax.act members))
::
++  handle-bundle
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%bundle -.act)
  ?~  pax.act
    [~ this]
  ?:  (~(has by groups) pax.act)
    [~ this]
  :-  (send-diff pax.act act)
  this(groups (~(put by groups) pax.act *group))
::
++  handle-unbundle
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%unbundle -.act)
  ?~  pax.act
    [~ this]
  ?.  (~(has by groups) pax.act)
    [~ this]
  :-  (send-diff pax.act act)
  this(groups (~(del by groups) pax.act))
::
++  send-diff
  |=  [pax=path action=group-action]
  ^-  (list move)
  ;:  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %group-update action]
  ::
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%group pax] bol)
    |=  [=bone *]
    [bone %diff %group-update action]
  ::
    ^-  (list move)
    ?.  |(=(%bundle -.action) =(%unbundle -.action))
      ~
    %+  turn  (prey:pubsub:userlib /keys bol)
    |=  [=bone *]
    [bone %diff %group-update action]
  ==
::
--

