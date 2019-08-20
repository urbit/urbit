:: service/groups.hoon
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%noun group-diff]]
      [%peer wire dock path]
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
::
++  group  (set ship)
::
+$  group-action
  $%  [%add members=group pax=path]
      [%remove members=group pax=path]
      [%bundle pax=path]
      [%unbundle pax=path]
  ==
::
+$  group-diff
  $%  [%keys keys=(set path)]
      [%path members=groups pax=path]
      [%add members=groups pax=path]
      [%remove members=groups pax=path]
      [%bundle pax=path]
      [%unbundle pax=path]
  ==
::
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
::
::
::  +peek
::  /groups/:pax  (pax is optional, if ~ receive set of keys)
::  /member/:ship
::  
::
::++  peek  .
::
++  peer
  |=  pax=path
  ^-  (quip move _this)
  ::  no-op
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ::  we now proxy all events to this path
  ?:  =(/all pax)
    [~ this]  
  ::  we send the list of keys then quit the subscription
  ?:  =(/keys pax)
    :_  this
    :~  [ost.bol %diff %noun [%keys ~(key by groups)]]
        [ost.bol %quit ~]
    ==
  :: we now send all diffs that affect this path to this subscriber
  ?:  =(%groups &1:pax)
    =/  group-path  (limo pax)
    =.  group-path  ?^(group-path t.group-path ~)
    =/  grp=(unit group)  (~(get by groups) group-path)
    :_  this
    [ost.bol %diff %noun [%path grp]]~
  :: otherwise we quit the subscription
  [[ost.bol %quit ~]~ this]
::
++  poke-noun
  |=  action=group-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    this
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
    =/  members  (~(got by groups) pax.act)
    =.  members  (~(put in members) members.act)
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
  =/  members  (~(got by groups) pax.act)
  =.  members  (~(del in members) members.act)
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
  %+  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %noun action]
    list-move
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%groups pax] bol)
    |=  [=bone *]
    [bone %diff %noun action]
  ==
::
--

