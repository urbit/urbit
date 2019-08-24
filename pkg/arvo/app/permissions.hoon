::  service/permissions.hoon
::
|%
+$  permission
  [kind=?(%black %white) who=(set ship)]
::
+$  permission-diff
  $%  [%create =permission]
      [%delete ~]
      [%add who=(set ship)]
      [%remove who=(set ship)]
  ==
::
+$  action
  $:  =path
    ::
      $=  what
      $%  permission-diff
          [%allow who=(set ship)]
          [%deny who=(set ship)]
      ==
  ==
::
+$  diff
  $:  =path
      what=permission-diff
  ==
::
::
+$  state
  $:  permissions=(map path permission)
      ::TODO  do we want to track these for whitelists only? probably no?
      affiliation=(map ship (set path))  ::  jug
  ==
::
+$  move  [bone card]
+$  card
  $%  [%diff %permissions-diff diff]
  ==
--
::
|_  [=bowl:gall %v0 state]
::
++  this  .
::
::  primitive state operations
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
::  actions on state
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
  ?.  (~(has by permissions) path)  this
  %_  this
    permissions  (~(del by permissions) path)
    affiliation  (unaffiliate path who:(~(got by permissions) path))
  ==
::
++  add
  (cury modify-permission &)
::
++  remove
  (cury modify-permission |)
::
::  diff handling
::
++  calculate-diff
  |=  =action
  ^-  (unit diff)
  =/  pem=(unit permission)
    (~(get by permissions) path.action)
  ?:  ?=(%create -.what.action)  `action
  ?~  pem  ~
  |-  ^-  (unit diff)
  ?-  -.what.action
      %delete  `action
    ::
        %allow  ::TODO  smaller?
      =-  $(what.action -)
      ?:  ?=(%black kind.u.pem)
        [%add who.what.action]
      [%remove who.what.action]
  ::
        %deny
      =-  $(what.action -)
      ?:  ?=(%black kind.u.pem)
        [%add who.what.action]
      [%remove who.what.action]
  ::
      %add
    =+  new=(~(dif in who.what.action) who.u.pem)
    ?~(new ~ `action(who.what new))
  ::
      %remove
    =+  new=(~(int in who.what.action) who.u.pem)
    ?~(new ~ `action(who.what new))
  ==
::
++  apply-diff
  |=  =diff
  ^+  this
  ?-  -.what.diff
    %create  (create [path +.what]:diff)
    %delete  (delete path.diff)
    %add     (add [path +.what]:diff)
    %remove  (remove [path +.what]:diff)
  ==
::
++  send-diff
  |=  =diff
  ^-  (list move)
  %+  murn  ~(tap by sup.bowl)
  |=  [=bone =ship =path]
  ^-  (unit move)
  ?.  =(path path.diff)  ~
  `(diff-move bone diff)
::
++  diff-move
  |=  [=bone =diff]
  ^-  move
  [bone %diff %permissions-diff diff]
::
::  gall interface
::
++  poke-noun
  |=  =action
  ^-  (quip move _this)
  =/  diff=(unit diff)  (calculate-diff action)
  ?~  diff  [~ this]
  :-  (send-diff u.diff)
  (apply-diff u.diff)
::
++  peer
  |=  =path
  ^-  (quip move _this)
  :_  this
  ~|  [%invalid-subscription-path path]
  ?+  path  !!
      [%permissions ^]
    ?.  (~(has by permissions) path)  ~
    [(diff-move ost.bowl path %create (~(got by permissions) path)) ~]
  ::
      [%affiliation @ ~]
    =+  who=(slav %p i.t.path)
    ?.  (~(has by affiliation) who)  ~
    ::TODO  ah crud we're gonna need multiple diffs per poke aren't we?
    ~|  %affiliation-subs-unimplemented
    !!
  ==
--