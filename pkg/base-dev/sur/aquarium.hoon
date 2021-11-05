::  Traditionally, ovo refers to an ovum -- (pair wire card) -- and ova
::  refers to a list of them.  We have several versions of each of these
::  depending on context, so we do away with that naming scheme and use
::  the following naming scheme.
::
::  Every card is either an `event` or an `effect`.  Prepended to this
::  is `unix` if it has no ship associated with it, or `aqua` if it
::  does.  `timed` is added if it includes the time of the event.
::
::  Short names are simply the first letter of each word plus `s` if
::  it's a list.
::
/+  pill
=,  pill-lib=pill
|%
+$  az-log  [topics=(lest @) data=@t]
+$  az-state
  $:  logs=(list az-log)
      lives=(map ship [lyfe=life rut=rift])
      tym=@da
  ==
++  ph-event
  $%  [%test-done p=?]
      aqua-event
  ==
::
+$  unix-event  unix-event:pill-lib
+$  pill        pill:pill-lib
::
+$  aqua-event
  $%  [%init-ship who=ship fake=?]
      [%pause-events who=ship]
      [%snap-ships lab=term hers=(list ship)]
      [%restore-snap lab=term]
      [%event who=ship ue=unix-event]
  ==
::
+$  azimuth-action
  $%  [%init-azimuth ~]
      [%spawn who=ship]
      [%breach who=ship]
  ==
::
+$  aqua-effects
  [who=ship ufs=(list unix-effect)]
::
+$  aqua-effect
  [who=ship ufs=unix-effect]
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
+$  unix-effect
  %+  pair  wire
  $%  [%blit p=(list blit:dill)]
      [%send p=lane:ames q=@]
      [%doze p=(unit @da)]
      [%thus p=@ud q=(unit hiss:eyre)]
      [%ergo p=@tas q=mode:clay]
      [%sleep ~]
      [%restore ~]
      [%kill ~]
      [%init ~]
      [%request id=@ud request=request:http]
  ==
--
