/@  tree
^-  kook:neo
|%
++  state  pro/%tree
++  poke  (sy %tree ~)
++  kids  *kids:neo
++  deps  *deps:neo
++  form  
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
  |=  pal=(unit pail:neo)
  ^-  (quip card:neo pail:neo)
  :_  tree/!>(~)
  ~
  ++  poke
  |=  [=stud:neo =vase]
  ^-  (quip card:neo pail:neo)
  =/  this  !<(=tree q.pail)
  ?+  stud  !!
  %tree  [~ tree/vase]
  ==
  --
--