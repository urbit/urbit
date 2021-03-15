/-  aquarium
=,  aquarium
:-  %say
|=  [* [her=ship command=tape ~] ~]
:-  %aqua-events
%+  turn
  ^-  (list unix-event)
  :~  [//term/1 %belt %key %ctl `@c`%e]
      [//term/1 %belt %key %ctl `@c`%u]
      [//term/1 %belt %txt ((list @c) command)]
      [//term/1 %belt %ret ~]
  ==
|=  ue=unix-event
[%event her ue]
