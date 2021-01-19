/-  store=contact-store
/+  group
|_  =bowl:gall
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %contact-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
::
++  get-contact
  |=  =ship
  ^-  (unit contact:store)
  =/  upd  (scry-for (unit update:store) /contact/(scot %p ship))
  ?~  upd  ~
  ?>  ?=(%add -.u.upd)
  `contact.u.upd
::
++  is-allowed
  |=  =ship
  ^-  ?
  =/  shp  (scry-for ? /allowed-ship/(scot %p ship))
  ?:  shp  %.y
  =/  allowed-groups  ~(tap in (scry-for (set resource) /allowed-groups))
  =/  grp  ~(. group bowl)
  |-
  ?~  allowed-groups  %.n
  ?:  ~(has in (members:grp i.allowed-groups) ship)
    %.y
  $(allowed-groups t.allowed-groups)
--
