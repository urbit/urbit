/@  planner-entry
^-  firm:neo
|%
++  state  %planner-entry
++  poke  ~
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  new=(unit vase)
    ^-  (quip card:neo vase)
    :-  ~
    ?~  new
      !>(*planner-entry)
    u.new
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    !!
  --
--
