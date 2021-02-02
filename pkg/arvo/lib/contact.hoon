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
++  scry-is-public
  .^  ?
    %gx
    (scot %p our.bowl)
    %contact-store
    (scot %da now.bowl)
    /is-public/noun
  ==
::
++  is-allowed
  |=  [rid=resource =ship]
  ^-  ?
  =/  grp  ~(. group bowl)
  ::  if they are requesting our personal profile, check if we are
  ::  either public, or if they are on the allowed-ships list.
  ::  this is used for direct messages and leap searches
  ::
  ?:  ?&  =(rid [our.bowl %''])
      ?|  scry-is-public
          (scry-for ? /allowed-ship/(scot %p ship))
      ==
    %.y
  ::  if they are requesting our profile within a group, make sure we
  ::  are the host of that group and that they are a member of the group
  ::
  =/  allowed-groups  (scry-for (set resource) /allowed-groups)
  ?&  (~(has in allowed-groups) rid)
      (~(has in scry-sharing) rid)
      (~(has in (members:grp rid)) ship)
  ==
--
