::  Tree view recursive directory contents
::  
::::  /hoon/tree/gen
  ::
!:
::::
  ::
:-  %say
|=  [^ [pax=path fla=$|(~ [%full ~])] ~]
=+  len=(lent pax)
=+  rend=?^(fla dank:ut |=(a=path (dank:ut (slag len a))))
:-  %tang  %-  flop
|-  ^-  tang
=+  ark=;;(arch .^(cy/pax))
=-  ?~  fil.ark  -
    [(rend pax) -]
%-  zing
%+  turn
  (sort (~(tap by dir.ark)) aor)
|=  [a=@t ~] 
^$(pax (welp pax /[a]))
