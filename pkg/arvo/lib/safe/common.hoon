/-  common=safe-common
::  Shared implementation between the client and the server in safe
::
|%
::  +bunt-a-vase: Given a vase which contains a mold, get a vase with the bunt
::
::    All hail joe for this.
::
++  bunt-a-vase
  |=  v=vase
  ^-  vase
  (slap v [%kttr [%like [[%& 1] ~] ~]])
::  +apply-event-log-item-to-state: applies the event
::
++  apply-event-log-item-to-state
  |=  [app-map=(map @t vase) item=event-log-item:common =snapshot:common]
  ^-  snapshot:common
  ::
  ?-    -.item
  ::
      %toplevel-init
    ::
    =/  app-vase=vase       (~(got by app-map) app-type.item)
    =/  snapshot-type=vase       (slap app-vase [%limb %snapshot])
    ::
    %_    snapshot
        app-type    app-type.item
        top-state   `[initial-invited community-name original-host]:item
        signature-type  signature-type.item
        snapshot    (bunt-a-vase snapshot-type)
        children    ~
    ==
  ::
      %toplevel-invite
    ?>  ?=(^ top-state.snapshot)
    %_  snapshot
      invited.u.top-state  (~(put in invited.u.top-state.snapshot) ship.item)
    ==
  ::
      %init
    ::
    =/  app-vase=vase       (~(got by app-map) app-type.item)
    =/  snapshot-type=vase       (slap app-vase [%limb %snapshot])
    ::
    %_    snapshot
        app-type  app-type.item
        top-state  ~
    ::  todo: remove route from init item? Add route to snapshot instead?
        signature-type  signature-type.item
        snapshot  (bunt-a-vase snapshot-type)
        children    ~
    ==
  ::
      %log
    ::  when we receive a %log event, we commit this to the event log
    ::
    =/  app-vase=vase       (~(got by app-map) app-type.snapshot)
    =/  apply-event-to-snapshot=vase
      (slap app-vase [%limb %apply-event-to-snapshot])
    =/  args  :(slop user-event.item private-event.item snapshot.snapshot)
    =.  snapshot.snapshot  (slam apply-event-to-snapshot args)
    ::
    snapshot
  ::
      %create
    snapshot(children (~(put in children.snapshot) sub-id.item))
  ==
--
