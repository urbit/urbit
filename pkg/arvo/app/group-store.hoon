::  group-store: data store for groups of ships 
::
/-  *group-store
|%
+$  move  [bone [%diff diff]]
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  =groups
  ==
::
+$  diff
  $%  [%group-update group-update]
      [%group-initial groups]
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
  ?>  (team:title our.bol src.bol)
  ::  we now proxy all events to this path
  :_  this
  [ost.bol %diff %group-initial groups]~
::
++  peer-keys
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %group-update [%keys ~(key by groups)]]~
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  =/  grp=(unit group)  (~(get by groups) pax)
  ?~  grp  !!
  :_  this
  [ost.bol %diff %group-update [%path u.grp pax]]~
::
++  poke-group-action
  |=  action=group-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %add       (handle-add action)
      %remove    (handle-remove action)
      %bundle    (handle-bundle action)
      %unbundle  (handle-unbundle action)
  ==
::
++  handle-add
  |=  act=group-action
  ^-  (quip move _this)
  ?>  ?=(%add -.act)
  ?~  pax.act
    [~ this]
  ?.  (~(has by groups) pax.act)
    [~ this]
  =/  members=group  (~(got by groups) pax.act)
  =.  members  (~(uni in members) members.act)
  ?:  =(members (~(got by groups) pax.act))
    [~ this]
  :-  (send-diff pax.act act)
  this(groups (~(put by groups) pax.act members))
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
  ?:  =(members (~(got by groups) pax.act))
    [~ this]
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
++  update-subscribers
  |=  [pax=path act=group-action]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %group-update act]
::
++  send-diff
  |=  [pax=path act=group-action]
  ^-  (list move)
  %-  zing
  :~  (update-subscribers /all act)
      (update-subscribers [%group pax] act)
      ?.  |(=(%bundle -.act) =(%unbundle -.act))
        ~
      (update-subscribers /keys act)
  ==
::
--

