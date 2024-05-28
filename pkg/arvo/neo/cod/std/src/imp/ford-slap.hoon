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
  =/  [pax=pith:neo =lore:neo]  (~(got by deps.bowl) %hoon)
  =/  =name:neo  (de-pith:name:neo pax)
  =+  !<(src=@t q.pail:~(rot of:neo lore))
  =/  =file:ford:neo
    (scan (trip src) (rein:ford:neo name))
  hoon.file

+$  state  [cache=(unit vase) ~]
--
^-  kook:neo
|%
++  state  [%or pro/%vase pro/%tang ~]
++  poke   (sy %rely %ford-in ~)
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  sut/dep:ford:neo
      hoon/[& [pro/%hoon ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `vase/(need (build bowl))
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `vase/(need (build bowl))
  --
--
