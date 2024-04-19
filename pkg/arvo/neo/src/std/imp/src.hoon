=>
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %counter
++  poke  (sy %gift ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  &
      [%hoon %sig]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%gift stud)
    =+  !<(=gift:neo vax)
    %-  (slog leaf/"gift" (sell vax) ~)
    :-  ~
    =-  !>(-)
    +(!<(@ud state-vase))
  ::
  ++  init
    |=  old=(unit vase)  `!>(0)
  --
--
