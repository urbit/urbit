=> 
|%
++  card  card:neo
++  build
  |=  =bowl:neo
  ^-  vase
  =/  =lore:neo  q:(~(got by deps.bowl) %txt)
  =/  =idea:neo  (~(got of:neo lore) /)
  ?>  =(p.pail.idea %txt)
  q.pail.idea
--
^-  kook:neo
|%
++  poke    (sy %rely %ford-in ~)
++  state   pro/%vase
++  kids    ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  txt/[& [pro/%txt ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `vase/(build bowl)
  ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `vase/(build bowl)
  --
--
