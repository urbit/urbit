::  Tree view recursive directory contents
::  
::::  /hoon#tree#gen
  ::
!:
::::
  ::
:-  %say
|=  {^ {pax/path fla/$@($~ {$full $~})} $~}
=+  len=(lent pax)
=+  rend=?^(fla smyt |=(a/path (smyt (slag len a))))
:-  %tang  %-  flop
|-  ^-  tang
=+  ark=;;(arch .^(cy#pax))
=-  ?~  fil.ark  -
    [(rend pax) -]
%-  zing
%+  turn
  (sort (~(tap by dir.ark)) aor)
|=  {a/@t $~} 
^$(pax (welp pax /[a]))
