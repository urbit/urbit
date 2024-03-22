/-  neo
=> 
|%
++  card  card:neo
++  build
  |=  =bowl:neo
  ^-  (unit vase)
  ?~  a=(get-output:ford:neo bowl %a)
    ~&  missing-a/were.bowl
    ~
  ?~  b=(get-output:ford:neo bowl %b)
    ~&  missing-b/were.bowl
    ~
  `(slop u.a u.b)
+$  state  [cache=(unit vase) ~]
--
^-  firm:neo
|%
++  state  %ford-out
++  poke   (sy %rely %ford-in ~)
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  a/dep:ford:neo
      b/dep:ford:neo
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(^state state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  ?=(?(%rely %ford-in) stud)
    =/  sta  sta
    =.  cache.sta  (build bowl)
    `!>(sta)
  ++  init
    |=  vax=(unit vase)
    =/  sta  *^state
    =.  cache.sta  (build bowl)
    `!>(sta)
  --
--
