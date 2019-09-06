/-  *permissions
::  service/permissions.hoon
::
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff %permissions-diff diff]
  ==
::
+$  diff
  ::TODO  should path and ship not be included in outgoing diffs? known info...
  $%  [%permission =path what=permission-diff]
      [%affiliation who=ship what=affiliation-diff]
  ==
::
+$  state
  $:  permissions=permission-map
      ::TODO  do we want to track these for whitelists only? probably no?
      affiliation=affiliation-map
  ==
--
::
|_  [=bowl:gall %v0 state]
::
++  this  .
::
::  primitive state operations
::
++  create-permission
  |=  [=path =permission]
  ^+  permissions
  ::NOTE  if we ever want to allow overwriting by creation,
  ::      we'll need to smarter affiliation diff calculation.
  ~|  [%permission-already-exists path]
  ?<  (~(has by permissions) path)
  (~(put by permissions) path permission)
::
++  delete-permission
  |=  =path
  ^+  permissions
  (~(del by permissions) path)
::
++  add-to-permission
  |=  [=path who=(set ship)]
  ^+  permissions
  %+  ~(put by permissions)  path
  =/  =permission
    ~|  [%no-such-permission path]
    (~(got by permissions) path)
  permission(who (~(uni in who.permission) who))
::
++  remove-from-permission
  |=  [=path who=(set ship)]
  ^+  permissions
  %+  ~(put by permissions)  path
  =/  =permission
    ~|  [%no-such-permission path]
    (~(got by permissions) path)
  permission(who (~(dif in who.permission) who))
::
++  add-affiliation
  |=  [who=ship where=(set path)]
  ^+  affiliation
  %-  ~(gas ju affiliation)
  (turn ~(tap in where) (tack who))
::
++  remove-affiliation
  |=  [who=ship where=(set path)]
  ^+  affiliation
  ?~  where  affiliation
  =.  affiliation  (~(del ju affiliation) who n.where)
  =.  affiliation  $(where l.where)
  $(where r.where)
::
::  diff application
::
++  apply-diffs
  |=  diffs=(list diff)
  ^+  this
  ?~  diffs  this
  =-  $(this -, diffs t.diffs)
  ?-  -.i.diffs
    %permission  (apply-permission-diff +.i.diffs)
    %affiliation  (apply-affiliation-diff +.i.diffs)
  ==
::
++  apply-permission-diff
  |=  [=path diff=permission-diff]
  ^+  this
  =-  this(permissions -)
  ?-  -.diff
    %create  (create-permission path +.diff)
    %delete  (delete-permission path)
    %add     (add-to-permission path +.diff)
    %remove  (remove-from-permission path +.diff)
  ==
::
++  apply-affiliation-diff
  |=  [who=ship diff=affiliation-diff]
  ^+  this
  =-  this(affiliation -)
  ?-  -.diff
    %add  (add-affiliation who +.diff)
    %remove  (remove-affiliation who +.diff)
  ==
::
::  diff calculation
::
++  calculate-diffs
  |=  action=permission-action
  ^-  (list diff)
  =+  dif=(calculate-permission-diff action)
  ?~  dif  ~
  :-  [%permission path.action u.dif]
  %+  turn
    (calculate-affiliation-diffs path.action u.dif)
  (tack %affiliation)
::
++  calculate-permission-diff
  |=  action=permission-action
  ^-  (unit permission-diff)
  =/  pem=(unit permission)
    (~(get by permissions) path.action)
  ?:  ?=(%create -.what.action)  `what.action
  ?~  pem  ~
  |-  ^-  (unit permission-diff)
  ?-  -.what.action
      %delete  `what.action
  ::
      %allow  ::TODO  smaller?
    :-  ~
    ?:  ?=(%black kind.u.pem)
      [%add who.what.action]
    [%remove who.what.action]
  ::
      %deny
    :-  ~
    ?:  ?=(%black kind.u.pem)
      [%add who.what.action]
    [%remove who.what.action]
  ::
      %add
    =+  new=(~(dif in who.what.action) who.u.pem)
    ?~(new ~ `what.action(who new))
  ::
      %remove
    =+  new=(~(int in who.what.action) who.u.pem)
    ?~(new ~ `what.action(who new))
  ==
::
++  calculate-affiliation-diffs
  |=  [=path diff=permission-diff]
  ^-  (list [ship affiliation-diff])
  %+  murn
    ^-  (list ship)
    %~  tap  in
    ?-  -.diff
      %create           who.permission.diff
      %delete           who:(~(got by permissions) path)
      ?(%add %remove)   who.diff
    ==
  |=  who=ship
  ^-  (unit [ship affiliation-diff])
  ?-  -.diff
      ?(%create %add)
    ?:  (~(has ju affiliation) who path)  ~
    `[who %add [path ~ ~]]
  ::
      ?(%remove %delete)
    ?.  (~(has ju affiliation) who path)  ~
    `[who %remove [path ~ ~]]
  ==
::
::  diff communication
::
++  diff-move
  |=  [=bone =diff]
  ^-  move
  [bone %diff %permissions-diff diff]
::
++  send-diffs
  |=  diffs=(list diff)
  ^-  (list move)
  (zing (turn diffs send-diff))
::
++  send-diff
  |=  =diff
  ^-  (list move)
  %+  murn  ~(tap by sup.bowl)
  |=  [=bone =ship =path]
  ^-  (unit move)
  ?.  ?-  -.diff
        %permission   =(path [-.diff path.diff])
        %affiliation  =(path /(scot %p who.diff))
      ==
    ~
  `(diff-move bone diff)
::
::  gall interface
::
++  poke-permission-action
  |=  action=permission-action
  ^-  (quip move _this)
  =/  diffs=(list diff)  (calculate-diffs action)
  ?~  diffs  [~ this]
  :-  (send-diffs diffs)
  (apply-diffs diffs)
::
++  peer
  |=  =path
  ^-  (quip move _this)
  :_  this
  ~|  [%invalid-subscription-path path]
  ?+  path  !!
      [%permissions ^]
    ?.  (~(has by permissions) t.path)  ~
    =-  [(diff-move ost.bowl -) ~]
    [%permission t.path %create (~(got by permissions) t.path)]
  ::
      [%affiliation @ ~]
    =+  who=(slav %p i.t.path)
    ?.  (~(has by affiliation) who)  ~
    =-  [(diff-move ost.bowl -) ~]
    [%affiliation who %add (~(get ju affiliation) who)]
  ==
::
++  peek-x-keys
  |=  pax=path
  ^-  (unit (unit (set path)))
  ``~(key by permissions)
::
++  peek-x-permissions
  |=  =path
  ^-  (unit (unit permission))
  =/  pem  (~(get by permissions) path)
  `pem
::
++  peek-x-affiliation
  |=  pax=path
  ^-  (unit (unit (set path)))
  ?~  pax
    ~
  =/  who=ship  (slav %p i.pax)
  =/  paths=(unit (set path))  (~(get by affiliation) who)
  `paths
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
::
::TODO  stdlib additions
::
++  tack
  |*  a=*
  |*  b=*
  [a b]
--
