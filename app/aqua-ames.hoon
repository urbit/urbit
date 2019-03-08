::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium
=,  aquarium
=>  $~  |%
    +$  move  (pair bone card)
    +$  card
      $%  [%poke wire dock %aqua-events (list aqua-event)]
          [%peer wire dock path]
          [%pull wire dock ~]
      ==
    ::
    +$  state
      $:  %0
          subscribed=_|
      ==
    --
=,  gall
=|  moves=(list move)
|_  $:  bowl
        state
    ==
++  this  .
++  apex  %_(this moves ~)
++  abet  [(flop moves) this]
++  emit-moves
  |=  ms=(list move)
  %_(this moves (weld ms moves))
::
++  emit-aqua-events
  |=  aes=(list aqua-event)
  %-  emit-moves
  [ost %poke /aqua-events [our %aqua] %aqua-events aes]~
::
++  poke-aqua-vane-control
  |=  command=?(%subscribe %unsubscribe)
  :_  this(subscribed =(command %subscribe))
  (aqua-vane-control-handler our ost subscribed command)
::
++  diff-aqua-effects
  |=  [way=wire afs=aqua-effects]
  ^-  (quip move _this)
  =.  this  apex  =<  abet
  |-  ^+  this
  ?~  ufs.afs
    this
  =.  this
    ?+  -.q.i.ufs.afs  this
      %restore  (handle-restore who.afs)
      %send     (handle-send i.ufs.afs)
    ==
  $(ufs.afs t.ufs.afs)
::
++  handle-restore
  |=  who=@p
  %-  emit-aqua-events
  [%event who [//newt/0v1n.2m9vh %barn ~]]~
::
++  handle-send
  |=  [way=wire %send lan=lane:ames pac=@]
  ^+  this
  =/  hear  [//newt/0v1n.2m9vh %hear lan pac]
  %-  emit-aqua-events
  %+  turn
    .^((list ship) %gx /(scot %p our)/aqua/(scot %da now)/ships/noun)
  |=  who=ship
  [%event who hear]
--
