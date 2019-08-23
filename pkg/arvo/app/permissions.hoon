::  service/permissions.hoon
::
|%
+$  permission
  [kind=?(%black %white) who=(set ship)]
::
+$  state
  $:  permissions=(map path permission)
      ::TODO  do we want to track these for whitelists only? probably no?
      affiliation=(map ship (set path))  ::  jug
  ==
::
+$  action
  $%  [%create pax=path =permission]
      [%delete pax=path]
      [%add pax=path ships=(set ship)]
      [%remove pax=path ships=(set ship)]
      [%allow pax=path ships=(set ship)]
      [%deny pax=path ships=(set ship)]
      [%debug ~]
  ==
::
::
+$  move  [bone card]
+$  card
  $%  [%diff *]
  ==
--
::
|_  [=bowl:gall %v0 state]
::
++  this  .
::
::  simple state operations
::
++  affiliate
  |=  [=path who=(set ship)]
  ^+  affiliation
  ?~  who  affiliation
  =.  affiliation  (~(put ju affiliation) n.who path)
  =.  affiliation  $(who l.who)
  $(who r.who)
::
++  unaffiliate
  |=  [=path who=(set ship)]
  ^+  affiliation
  ?~  who  affiliation
  =.  affiliation  (~(del ju affiliation) n.who path)
  =.  affiliation  $(who l.who)
  $(who r.who)
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
++  modify-permission
  |=  [add=? =path who=(set ship)]
  %_  this
      permissions
    %.  [path who]
    ?:(add add-to-permission remove-from-permission)
  ::
      affiliation
    %.  [path who]
    ?:(add affiliate unaffiliate)
  ==
::
::  actions-on-state
::
++  create
  |=  [=path =permission]
  %_  this
    permissions  (~(put by permissions) path permission)
    affiliation  (affiliate path who.permission)
  ==
::
++  delete
  |=  =path
  %_  this
    permissions  (~(del by permissions) path)
  ::
      affiliation
    ~|  [%no-such-permission path]
    (unaffiliate path who:(~(got by permissions) path))
  ==
::
++  add
  (cury modify-permission &)
::
++  remove
  (cury modify-permission |)
::
++  allow
  |=  [=path who=(set ship)]
  ^+  this
  =-  (modify-permission - path who)
  =/  =permission  (~(got by permissions) path)
  ?=(%white kind.permission)
::
++  deny
  |=  [=path who=(set ship)]
  ^+  this
  =-  (modify-permission - path who)
  =/  =permission  (~(got by permissions) path)
  ?=(%black kind.permission)
::
::  gall interface
::
++  poke-noun
  |=  =action
  ^-  (quip move _this)
  :-  ~  ::TODO  send action as diff if it did anything?
  ?-  -.action
    %create  (create +.action)
    %delete  (delete +.action)
    %add     (add +.action)
    %remove  (remove +.action)
    %allow   (allow +.action)
    %deny    (deny +.action)
  ::
      %debug
    ~&  `(list [path permission])`~(tap by permissions)
    ~&  `(list [ship (set path)])`~(tap by affiliation)
    this
  ==
::
--