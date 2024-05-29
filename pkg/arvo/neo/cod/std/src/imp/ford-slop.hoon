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
--
^-  kook:neo
|%
++  state  pro/%vase
++  poke   (sy %rely %ford-in ~)
++  kids  ~
++  deps
  %-  ~(gas by *deps:neo)
  :~  a/dep:ford:neo
      b/dep:ford:neo
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
