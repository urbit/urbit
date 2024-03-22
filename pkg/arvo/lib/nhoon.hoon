/-  neo
=> 
|%
++  card  card:neo
++  build
  |=  [=bowl:neo sta=state]
  ^-  state
  ?~  sut=(get-output:ford:neo bowl %sut)
    sta(cache ~)
  =/  res=(each vase tang)
    (mule |.((slap u.sut hoon.sta)))
  ?:  ?=(%| -.res)
    %-  (slog p.res)
    ^-  state
    !!
  %-  (slog leaf/"built" (sell p.res) ~)
  sta(cache `p.res)
+$  state  [cache=(unit vase) =hoon]
--
^-  firm:neo
|%
++  poke   (sy %hoon %rely ~)
++  state   %nhoon
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  sut/dep:ford:neo
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(^state state-vase)
  ++  poke
    |=  [=stud:neo =vase]
    =/  sta  sta
    |^  ^-  (quip card:neo ^vase)
    ?+  stud  !!
      %hoon              hone
      ?(%rely %ford-in)  rely    
    ==
    ++  hone
      =+  !<(=hoon vase)
      =.  hoon.sta  hoon
      `!>((build bowl sta))
    ++  rely  `!>((build bowl sta))
    --
  ++  init
    |=  vax=(unit vase)
    ^-  (quip card:neo vase)
    ?~  vax  
      `!>(*^state)
    =+  !<(sta=^state u.vax)
    `!>((build bowl sta))
  --
--
