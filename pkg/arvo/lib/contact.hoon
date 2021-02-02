/-  store=contact-store, *resource
/+  group, grpl=group
|_  =bowl:gall
+*  grp  ~(. grpl bowl)
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
++  resource-for-update
  |=  =vase
  ^-  (list resource)
  |^
  =/  =update:store  !<(update:store vase)
  ?-  -.update
    %initial     ~
    %add         (rids-for-ship ship.update)
    %remove      (rids-for-ship ship.update)
    %edit        (rids-for-ship ship.update)
    %allow       ~
    %disallow    ~
    %set-public  ~
  ==
  ::
  ++  rids-for-ship
    |=  s=ship
    ^-  (list resource)
    ::  if the ship is in any group that I am pushing updates for, push
    ::  it out to that resource.
    ::
    =/  rids
      %+  skim  ~(tap in scry-sharing)
      |=  r=resource
      (is-member:grp s r)
    ?.  =(s our.bowl)
      rids
    (snoc rids [our.bowl %''])
  --
++  scry-sharing
  .^  (set resource)
    %gx
    (scot %p our.bowl)
    %contact-push-hook
    (scot %da now.bowl)
    /sharing/noun
  ==
::
++  get-contact
  |=  =ship
  ^-  (unit contact:store)
  =/  =rolodex:store
    (scry-for rolodex:store /all)
  (~(get by rolodex) ship)
::
++  is-allowed
  |=  [rid=resource =ship]
  ^-  ?
  =/  grp  ~(. group bowl)
  =/  shp  (scry-for ? /allowed-ship/(scot %p ship))
  ?:  shp  %.y
  ?:  ?&  (~(has in scry-sharing) rid)
          (~(has in (members:grp rid)) ship)
      ==
    %.y
  =/  allowed-groups  ~(tap in (scry-for (set resource) /allowed-groups))
  |-
  ?~  allowed-groups  %.n
  ?:  (~(has in (members:grp i.allowed-groups)) ship)
    %.y
  $(allowed-groups t.allowed-groups)
--
