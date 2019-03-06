|%
+$  aqua-event
  $%  [%init-ship who=ship keys=(unit dawn-event)]
      [%pause-events who=ship]
      [%snap-ships lab=term hers=(list ship)]
      [%restore-snap lab=term]
      [%event who=ship ovo=unix-event]
  ==
::
+$  aqua-effects
  [who=ship ovo=(list unix-effect)]
::
+$  aqua-events
  [who=ship ovo=(list unix-timed-event)]
::
+$  aqua-boths
  [who=ship ovo=(list unix-both)]
::
+$  unix-both
  $%  [%event unix-timed-event]
      [%effect unix-effect]
  ==
::
+$  unix-timed-event  [tym=@da ovo=unix-event]
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
--
