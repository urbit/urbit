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
    :_  sta
    :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
        ::  if user input a path, create a mirror.
        ::  else, treat as plaintext.
        =/  upith  (mole |.((pave:neo (stab value.poke))))
        ?~  upith
          [%make %grid-cell `!>(value.poke) ~]
        =/  conf  (malt ~[[%ref (welp here.bowl (need upith))]])
        [%make %grid-cell `!>(~) conf]
    ==
  --
--
