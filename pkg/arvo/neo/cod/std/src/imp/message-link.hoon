/@  message
/@  chat
/@  chat-link
/@  sig
=> 
|%
++  card  card:neo
++  get-msg
  |=  =bowl:neo
  ^-  pail:neo
  pail:(need fil.q:(~(got by deps.bowl) %src))
::
--
^-  kook:neo
|%
++  state  pro/%message
++  poke   (sy %rely ~)
++  kids   *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  src/[req=& [pro/%message ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =saga:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card pail:neo)
    `(get-msg bowl)
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card pail:neo)
    `(get-msg bowl)
  --
--
