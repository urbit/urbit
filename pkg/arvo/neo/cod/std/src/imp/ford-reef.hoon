=> 
|%
++  card  card:neo
--
^-  kook:neo
|%
++  state  pro/%vase
++  poke   *(set stud:neo)
++  kids  ~
++  deps  ~
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  +*  sta  !<([cache=(unit vase) ~] state-vase)
  ++  poke
    |=  pail:neo  
    ^-  (quip card:neo pail:neo)
    `pail
  ::
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need old)
  --
--
