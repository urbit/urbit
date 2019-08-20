:: service/groups.hoon
/-  *groups
|%
+$  move  [bone card]
+$  card
  $%  [%diff [%noun group-diff]]
      [%quit ~]
      ::[%pull wire dock ~]
  ==
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  groups=(map path group)
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
  =/  group-path=(list @tas)  (limo pax)
  =.  group-path  ?^(group-path t.group-path ~)
  =/  grp=(unit group)  (~(get by groups) group-path)
  [~ ~ %noun grp]
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ::  we now proxy all events to this path
  [~ this]
::
++  peer-keys
  |=  pax=path
  ^-  (quip move _this)
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %noun [%keys ~(key by groups)]]~
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  =/  grp=(unit group)  (~(get by groups) pax)
  ?~  grp
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %noun [%path u.grp pax]]~
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
  :-  (send-diff pax.act act)
  ?~  pax.act
    this
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
  :-  (send-diff pax.act act)
  ?~  pax.act
    this
  ?.  (~(has by groups) pax.act)
    this
  ~&  act
  =/  members  (~(got by groups) pax.act)
  =.  members  (~(dif in members) members.act)
  this(groups (~(put by groups) pax.act members))
::
++  handle-bundle
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%bundle -.act)
  :-  (send-diff pax.act act)
  ?~  pax.act
    this
  ?:  (~(has by groups) pax.act)
    this
  this(groups (~(put by groups) pax.act *group))
::
++  handle-unbundle
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%unbundle -.act)
  :-  (send-diff pax.act act)
  ?~  pax.act
    this
  ?.  (~(has by groups) pax.act)
    this
  this(groups (~(del by groups) pax.act))
::
++  send-diff
  |=  [pax=path action=group-action]
  ^-  (list move)
  ::  TODO: if bundle or unbundle, send an update to keys
  %+  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %noun action]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib [%groups pax] bol)
  |=  [=bone *]
  [bone %diff %noun action]
::
--

