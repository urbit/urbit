::
::  Traditionally, ovo refers an event or card, and ova refers to a list
::  of them.  We have several versions of each of these depending on
::  context, so we do away with that naming scheme and use the following
::  naming scheme.
::
::  Every card is either a an `event` or an `effect`.  Prepended to this
::  is `unix` if it has no ship associated with it, or `aqua` if it
::  does.  `timed` is added if it includes the time of the event.
::
::  Short names are simply the first letter of each word plus `s` if
::  it's a list.
::
|%
+$  aqua-event
  $%  [%init-ship who=ship keys=(unit dawn-event)]
      [%pause-events who=ship]
      [%snap-ships lab=term hers=(list ship)]
      [%restore-snap lab=term]
      [%event who=ship ue=unix-event]
  ==
::
+$  aqua-effects
  [who=ship ufs=(list unix-effect)]
::
+$  aqua-events
  [who=ship utes=(list unix-timed-event)]
::
+$  aqua-boths
  [who=ship ub=(list unix-both)]
::
+$  unix-both
  $%  [%event unix-timed-event]
      [%effect unix-effect]
  ==
::
+$  unix-timed-event  [tym=@da ue=unix-event]
::
+$  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%whom p=ship]
      [%live p=@ud q=(unit @ud)]
      [%barn ~]
      [%boot $%([%fake p=ship] [%dawn p=dawn-event])]
      unix-task
  ==
::
+$  unix-effect
  %+  pair  wire
  $%  [%blit p=(list blit:dill)]
      [%send p=lane:ames q=@]
      [%doze p=(unit @da)]
      [%thus p=@ud q=(unit hiss:eyre)]
      [%ergo p=@tas q=mode:clay]
      [%sleep ~]
      [%restore ~]
  ==
+$  pill
  [boot-ova=* kernel-ova=(list unix-event) userspace-ova=(list unix-event)]
::
+$  dawn-event
  $:  =seed:able:jael
      spon=ship
      czar=(map ship [=life =pass])
      turf=(list turf)
      bloq=@ud
      node=(unit purl:eyre)
      snap=(unit snapshot:jael)
  ==
::
+$  vane-card
  $%  [%peer wire dock path]
      [%pull wire dock ~]
  ==
::
++  aqua-vane-control-handler
  |=  subscribed=?
  |=  command=?(%subscribe %unsubscribe)
  ^-  (list vane-cards)
  ?-    command
      %subscribe
    %+  weld
      ^-  (list vane-card)
      ?.  subscribed
        ~
      [%pull /aqua [our %ph]]~
    ^-  (list vane-card)
    [%peer /aqua [our %ph] /effects]~
  ::
      %unsubscribe
    ?.  subscribed
      ~
    [%pull /aqua [our %ph]]~
  --
--
