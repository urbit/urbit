/+  *contact-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%contact-update contact-update]]
      [%quit ~]
  ==
::
+$  state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =rolodex
  ==
--
::
|_  [bol=bowl:gall state-zero]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  :-  ~
  ?~  old  this
  ?-  -.old
      %0  this(+<+ u.old)
  ==
::
++  peek-x-all
  |=  pax=path
  ^-  (unit (unit [%noun (map path contacts)]))
  [~ ~ %noun rolodex]
::
++  peek-x-contacts
  |=  pax=path
  ^-  (unit (unit [%noun (unit contacts)]))
  ?~  pax
    ~
  =/  contacts=(unit contacts)  (~(get by rolodex) pax)
  [~ ~ %noun contacts]
::
++  peek-x-contact
  |=  pax=path
  ^-  (unit (unit [%noun (unit contact)]))
  ::  /:path/:ship
  =/  pas  (flop pax)
  ?~  pas
    ~
  =/  =ship  (slav %p i.pas)
  =.  pax  (scag (dec (lent pax)) `(list @ta)`pax)
  =/  contacts=(unit contacts)  (~(get by rolodex) pax)
  ?~  contacts
    ~
  =/  contact=(unit contact)  (~(get by u.contacts) ship)
  [~ ~ %noun contact]
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  send all updates from now on
  :_  this
  [ost.bol %diff %contact-update [%rolodex rolodex]]~
::
++  peer-updates
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  send all updates from now on
  [~ this]
::
++  peer-contacts
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  [ost.bol %diff %contact-update [%contacts pax (~(got by rolodex) pax)]]~
::
::++  poke-json
::  |=  =json
::  ^-  (quip move _this)
::  ?>  (team:title our.bol src.bol)
::  (poke-contact-action (json-to-action json))
::
++  poke-contact-action
  |=  action=contact-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ?-  -.action
      %create   (handle-create action)
      %delete   (handle-delete action)
      %add      (handle-add action)
      %remove   (handle-remove action)
      %edit     (handle-edit action)
  ==
::
++  handle-create
  |=  act=contact-action
  ^-  (quip move _this)
  ?>  ?=(%create -.act)
  ?:  (~(has by rolodex) path.act)
    [~ this]
  :-  (send-diff path.act act)
  this(rolodex (~(put by rolodex) path.act *contacts))
::
++  handle-delete
  |=  act=contact-action
  ^-  (quip move _this)
  ?>  ?=(%delete -.act)
  ?.  (~(has by rolodex) path.act)
    [~ this]
  :-  (send-diff path.act act)
  this(rolodex (~(del by rolodex) path.act))
::
++  handle-add
  |=  act=contact-action
  ^-  (quip move _this)
  ?>  ?=(%add -.act)
  =/  contacts  (~(got by rolodex) path.act)
  ?>  (~(has by contacts) ship.act)
  =.  contacts  (~(put by contacts) ship.act contact.act)
  :-  (send-diff path.act act)
  this(rolodex (~(put by rolodex) path.act contacts))
::
++  handle-remove
  |=  act=contact-action
  ^-  (quip move _this)
  ?>  ?=(%remove -.act)
  =/  contacts  (~(got by rolodex) path.act)
  ?<  (~(has by contacts) ship.act)
  =.  contacts  (~(del by contacts) ship.act)
  :-  (send-diff path.act act)
  this(rolodex (~(put by rolodex) path.act contacts))
::
++  handle-edit
  |=  act=contact-action
  ^-  (quip move _this)
  ?>  ?=(%edit -.act)
  =/  contacts  (~(got by rolodex) path.act)
  =/  contact  (~(got by contacts) ship.act)
  =.  contact  (edit-contact contact edit-field.act)
  =.  contacts  (~(put by contacts) ship.act contact)
  :-  (send-diff path.act act)
  this(rolodex (~(put by rolodex) path.act contacts))
::
++  edit-contact
  |=  [con=contact edit=edit-field]
  ^-  contact
  ?-  -.edit
      %nickname  con(nickname nickname.edit)
      %email     con(email email.edit)
      %phone     con(phone phone.edit)
      %website   con(website website.edit)
      %notes     con(notes notes.edit)
      %color     con(color color.edit)
      %avatar    con(avatar avatar.edit)
  ==
::
++  update-subscribers
  |=  [pax=path upd=contact-update]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %contact-update upd]
::
++  send-diff
  |=  [pax=path upd=contact-update]
  ^-  (list move)
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%contacts pax] upd)
  ==
::
--
