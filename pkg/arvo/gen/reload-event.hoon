::  Produce a raw event to reload a vane
::
::  Try: .event/ovo +reload-event %c, then restart urbit with
::  -I pier/.urb/put/event.ovo
::
:-  %say
|=  $:  [now=@da eny=@uvJ bek=beak]
        [[tam=term ~] ~]
    ==
:-  %ovo
=/  top  `path`/(scot %p p.bek)/[q.bek]/(scot r.bek)
=/  nam
  =/  van=(list [term ~])
    :-  zus=[%zuse ~]
    ~(tap by dir:.^(arch %cy (welp top /sys/vane)))
  ?.  =(1 (met 3 tam))
    tam
  =/  zaz=(list [p=knot ~])
      (skim van |=([a=term ~] =(tam (end 3 1 a))))
  ?>  ?=([[@ ~] ~] zaz)
  `term`p.i.zaz
=/  tip  (end 3 1 nam)
=/  bip  ?:(=('z' tip) %$ tip)
=/  way  ?:(=('z' tip) (welp top /sys/[nam]) (welp top /sys/vane/[nam]))
=/  fil  .^(@ %cx (welp way /hoon))
[//arvo %veer bip way fil]
