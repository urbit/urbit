/@  root
^-  kook:neo
|%
++  state  pro/%root
++  poke  *(set stud:neo)
++  kids  *kids:neo
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    root/!>(~)
  ++  poke
    |=  =pail:neo
    ^-  (quip card:neo pail:neo)
    !!
  --
--
