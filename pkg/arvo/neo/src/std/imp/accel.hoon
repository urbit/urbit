/@  accel-diff
::
^-  firm:neo
|%
++  state  %accel
++  poke  (sy %accel-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%ud |/%ud |]
      [%accel-cell %sig]
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
    ?>  =(%accel-diff stud)
    =/  poke  !<(accel-diff vax)
    ?>  =(our ship.src):bowl
    :_  sta
    :~  :-  (welp here.bowl ~[[ud/row.poke] [ud/column.poke]])
        ?~  ref.poke
          [%make %accel-cell `!>(text.poke) ~]
        ::  only handles local piths so far
        =/  conf  (malt ~[[%ref (welp here.bowl (need ref.poke))]])
        [%make %accel-cell `!>(text.poke) conf]
    ==
  --
--
