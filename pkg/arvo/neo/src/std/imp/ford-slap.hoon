=> 
|%
++  card  card:neo
++  build
  |=  =bowl:neo
  ^-  (unit vase)
  ?~  sut=(get-output:ford:neo bowl %sut)
    ~&  missing-sut/were.bowl
    ~
  ?~  hon=(get-hoon bowl)
    ~&  missing-hoon/were.bowl
    ~
  `(slap u.sut u.hon)
++  get-hoon
  |=  =bowl:neo
  ^-  (unit hoon)
  =;  res=(each hoon tang)
    ?:  ?=(%& -.res)
      `p.res
    %-  (slog leaf/"Parsing failed" p.res)
    ~
  %-  mule
  |.  ^-  hoon
  =/  [pax=pith:neo =cane:neo]  (~(got by deps.bowl) %hoon)
  =/  =name:neo  (de-pith:name:neo pax)
  =+  !<(src=@t q.pail.cane) 
  =/  =file:ford:neo
    (scan (trip src) (rein:ford:neo name))
  hoon.file

+$  state  [cache=(unit vase) ~]
--
^-  firm:neo
|%
++  state  %ford-out
++  poke   (sy %rely %ford-in ~)
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  sut/dep:ford:neo
      hoon/[& [%hoon %sig] ~]
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
    ?:  =(~ cache.sta)
      ~&  no-build/bowl
      !!
    `!>(sta)
  --
--
