=> 
|%
++  card  card:neo
++  get-face
  |=  =bowl:neo
  ^-  @tas
  =/  =lore:neo  q:(~(got by deps.bowl) %face)
  =/  =idea:neo  ~(rot of:neo lore)
  !<(@tas q.pail.idea)
::
++  build
  |=  =bowl:neo
  ^-  (unit vase)
  ?~  sut=(get-output:ford:neo bowl %sut)
    ~&  missing-sut/were.bowl
    ~
  =/  face=@tas  (get-face bowl)
  :-  ~
  u.sut(p [%face (get-face bowl) p.u.sut])
+$  state  [cache=(unit vase) ~]
--
^-  kook:neo
|%
++  poke    (sy %rely %ford-in ~)
++  state   pro/%vase
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  sut/dep:ford:neo
      face/[& [pro/%term ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `vase/(need (build bowl))
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `vase/(need (build bowl))
  --
--
