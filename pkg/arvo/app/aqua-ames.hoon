::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium
=,  aquarium
=>  |%
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
=|  aqua-event-list=(list aqua-event)
=|  ships=(list ship)
|_  $:  bowl
        state
    ==
++  this  .
++  apex  %_(this moves ~, aqua-event-list ~, ships ~)
++  abet
  =?  this  !=(~ aqua-event-list)
    %-  emit-moves
    [ost %poke /aqua-events [our %aqua] %aqua-events aqua-event-list]~
  ::  ~?  !?=(~ moves)  [%aqua-ames-moves (lent moves)]
  [moves this]
::
++  emit-moves
  |=  ms=(list move)
  %_(this moves (weld moves ms))
::
++  emit-aqua-events
  |=  aes=(list aqua-event)
  %_(this aqua-event-list (weld aqua-event-list aes))
::
++  poke-aqua-vane-control
  |=  command=?(%subscribe %unsubscribe)
  :_  this(subscribed =(command %subscribe))
  (aqua-vane-control-handler our ost subscribed command)
::
::  Handle effects from ships.  We only react to %send effects.
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
      %send     (handle-send who.afs i.ufs.afs)
    ==
  $(ufs.afs t.ufs.afs)
::
++  handle-restore
  |=  who=@p
  %-  emit-aqua-events
  [%event who [//newt/0v1n.2m9vh %born ~]]~
::
++  handle-send
  |=  [sndr=ship way=wire %send lan=lane:ames pac=@]
  ^+  this
  =/  rcvr=ship  (lane-to-ship lan)
  =/  hear-lane  (ship-to-lane sndr)
  ::~>  %slog.0^leaf/"aqua: {(scow %p sndr)} -> {(scow %p rcvr)}"
  %-  emit-aqua-events
  [%event rcvr //newt/0v1n.2m9vh %hear hear-lane pac]~
::  +lane-to-ship: decode a ship from an aqua lane
::
++  lane-to-ship
  |=  =lane:ames
  ^-  ship
  ::
  ?-  -.lane
    %&  p.lane
    %|  `ship``@`p.lane
  ==
::  +ship-to-lane: encode a lane to look like it came from .ship
::
::    Never shows up as a galaxy, because Vere wouldn't know that either.
::
++  ship-to-lane
  |=  =ship
  ^-  lane:ames
  ::
  [%| `address:ames``@`ship]
--
