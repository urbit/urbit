/@  grid-cell
::
^-  firm:neo
|%
++  state  %grid-cell
++  poke  ~
++  kids  *kids:neo
++  deps  *deps:neo
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
