::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium, spider
/+  aqua-vane-thread
=,  aquarium
=|  ships=(list ship)
|%
++  emit-aqua-events
  |=  [our=ship aes=(list aqua-event)]
  ^-  (list card:agent:gall)
  [%pass /aqua-events %agent [our %aqua] %poke %aqua-events !>(aes)]~
::
++  handle-restore
  |=  [our=ship who=@p]
  ^-  (quip card:agent:gall _ships)
  :_  ships
  %+  emit-aqua-events  our
  [%event who [//newt/0v1n.2m9vh %barn ~]]~
::
++  handle-send
  |=  [our=ship now=@da way=wire %send lan=lane:ames pac=@]
  ^-  (quip card:agent:gall _ships)
  =/  hear  [//newt/0v1n.2m9vh %hear lan pac]
  =?  ships  &  ::  =(~ ships)
    .^((list ship) %gx /(scot %p our)/aqua/(scot %da now)/ships/noun)
  :_  ships
  %+  emit-aqua-events  our
  %+  turn  ships
  |=  who=ship
  [%event who hear]
--
::
%-  aqua-vane-thread
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =^  cards  ships
    ?+  -.q.ue  `ships
      %restore  (handle-restore our.bowl who)
      %send     (handle-send our.bowl now.bowl ue)
    ==
  [cards this]
::
++  handle-arvo-response  _!!
--
