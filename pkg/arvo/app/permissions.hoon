/-  *permissions
::  service/permissions.hoon
::
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff diff]
      [%quit ~]
  ==
+$  diff
  $%  [%permission-initial =permission-map]
      [%permission-update =permission-update]
  ==
::
+$  state
  $:  permissions=permission-map
  ==
--
::
|_  [bol=bowl:gall %v0 state]
::
++  this  .
::
::  gall interface
::
++  peer-all
  |=  =path
  ^-  (quip move _this)
  ?~  path
    [[ost.bol %quit ~]~ this]
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we now proxy all events to this path
  :_  this
  [ost.bol %diff %permission-initial permissions]~
::
++  peer-permission
  |=  =path
  ^-  (quip move _this)
  ?~  path
    [[ost.bol %quit ~]~ this]
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ?.  (~(has by permissions) path)
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %permission-update [%create path (~(got by permissions) path)]]~
::
++  peek-x-keys
  |=  pax=path
  ^-  (unit (unit (set path)))
  ``~(key by permissions)
::
++  peek-x-permissions
  |=  =path
  ^-  (unit (unit permission))
  `(~(get by permissions) path)
::
++  peek-x-permitted
  |=  =path
  ^-  (unit (unit ?))
  ?~  path
    ~
  =/  pem  (~(get by permissions) t.path)
  ?~  pem
    ~
  =/  who  (slav %p i.path)
  =/  has  (~(has in who.u.pem) who)
  :+  ~  ~
  ?-(kind.u.pem %black !has, %white has)
::
++  poke-permission-action
  |=  action=permission-action
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
      %create
    (handle-create action)
  ::
      %delete
    (handle-delete action)
  ::
      %allow
    (handle-allow action)
  ::
      %deny
    (handle-deny action)
  ::
  ==
::
++  handle-add
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%add -.act)
  ?~  path.act
    [~ this]
  ::  TODO: calculate diff
  ::  =+  new=(~(dif in who.what.action) who.u.pem)
  ::  ?~(new ~ `what.action(who new))
  ?.  (~(has by permissions) path.act)
    [~ this]
  :-  (send-diff path.act act)
  =/  perm  (~(got by permissions) path.act)
  =.  who.perm  (~(uni in who.perm) who.act)
  this(permissions (~(put by permissions) path.act perm))
::
++  handle-remove
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%remove -.act)
  ?~  path.act
    [~ this]
  ?.  (~(has by permissions) path.act)
    [~ this]
  =/  perm  (~(got by permissions) path.act)
  =.  who.perm  (~(dif in who.perm) who.act)
  ::  TODO: calculate diff
  ::  =+  new=(~(int in who.what.action) who.u.pem)
  ::  ?~(new ~ `what.action(who new))
  :-  (send-diff path.act act)
  this(permissions (~(put by permissions) path.act perm))
::
++  handle-create
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%create -.act)
  ?~  path.act
    [~ this]
  ?:  (~(has by permissions) path.act)
    [~ this]
  :: TODO: calculate diff
  :-  (send-diff path.act act)
  this(permissions (~(put by permissions) path.act permission.act))
::
++  handle-delete
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%delete -.act)
  ?~  path.act
    [~ this]
  ?.  (~(has by permissions) path.act)
    [~ this]
  :-  (send-diff path.act act)
  this(permissions (~(del by permissions) path.act))
::
++  handle-allow
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%allow -.act)
  ?~  path.act
    [~ this]
  =/  perm  (~(get by permissions) path.act)
  ?~  perm
    [~ this]
  ?:  =(kind.u.perm %white)
    (handle-add [%add +.act])
  (handle-remove [%remove +.act])
::
++  handle-deny
  |=  act=permission-action
  ^-  (quip move _this)
  ?>  ?=(%deny -.act)
  ?~  path.act
    [~ this]
  =/  perm  (~(get by permissions) path.act)
  ?~  perm
    [~ this]
  ?:  =(kind.u.perm %black)
    (handle-add [%add +.act])
  (handle-remove [%remove +.act])
::
++  send-diff
  |=  [pax=path update=permission-update]
  ^-  (list move)
  ;:  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %permission-update update]
  ::
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%permission pax] bol)
    |=  [=bone *]
    [bone %diff %permission-update update]
  ::
  ==
::
--
