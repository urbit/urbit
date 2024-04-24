/@  grid-cell
::
^-  firm:neo
|%
++  state  %grid-cell
++  poke  ~
++  kids  *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  [%ref | [%grid-cell %sig] ~]
  ==
::
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    [~ state-vase]
  ++  init
    |=  old=(unit vase)
    `(need old)
  --
--
