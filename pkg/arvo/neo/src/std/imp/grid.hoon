/@  grid-diff
::
^-  firm:neo
|%
++  state  %grid
++  poke  (sy %grid-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%ud |/%ud |]
      [%grid-cell %sig]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  old=(unit vase)
    `(need old)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%grid-diff stud)
    =/  poke  !<(grid-diff vax)
    ?>  =(our ship.src):bowl
    ?-    -.poke
        %new
      :_  sta
      :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
          [%make %grid-cell `!>(value.poke) ~]
      ==
    ==
  --
--
