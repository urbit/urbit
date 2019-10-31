::  invite-view: provide a json interface to invite-store
::
/+  *invite-json
::
|%
+$  move  [bone card]
::
+$  card
  $%  [%peer wire dock path]
      [%diff %json json]
  ==
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
++  prep
  |=  old=*
  ^-  (quip move _this)
  :_  this
  [ost.bol %peer / [our.bol %invite-store] /updates]~
::
++  peer-primary
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  [ost.bol %diff %json (invites-to-json invites-scry)]~
::
++  diff-invite-update
  |=  [wir=wire upd=invite-update]
  ^-  (quip move _this)
  =/  updates-json  (update-to-json upd)
  :_  this
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [=bone *]
  [bone %diff %json updates-json]
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %peer / [our.bol %invite-store] /updates]~
::
++  invites-scry
  ^-  invites
  .^(invites %gx /=invite-store/(scot %da now.bol)/all/noun)
--
