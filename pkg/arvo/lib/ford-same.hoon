/-  neo
=> 
|%
++  card  card:neo
+$  state  [cache=(unit vase) ~]
+$  poke   [%dep ~]
--
^-  firm:neo
|%
++  poke    (sy %ford-in %rely ~)
++  state   %ford-out
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  src/dep:ford:neo
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<([cache=(unit vase) ~] state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  ?=(?(%ford-in %rely) stud)
    =/  sta  sta
    =.  cache.sta  (get-output:ford:neo bowl %src)
    :: ~&  ford-same/[were.bowl !=(~ cache.sta)]
    `!>(sta)
  ++  init
    |=  vax=(unit vase)
    ^-  (quip card:neo vase)
    =|  sta=[cache=(unit vase) ~]
    =.  cache.sta  (get-output:ford:neo bowl %src)
    `!>(sta)
  --
--
