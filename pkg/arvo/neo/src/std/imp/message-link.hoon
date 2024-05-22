/@  message
/@  chat
/@  chat-link
/@  sig
=> 
|%
++  card  card:neo
++  get-msg
  |=  =bowl:neo
  ^-  vase
  q.q:(need fil.q:(~(got by deps.bowl) %src))
::
--
^-  kook:neo
|%
++  state  %message
++  poke   (sy %rely ~)
++  kids   *kids:neo
++  deps
  %-  ~(gas by *deps:neo)
  :~  src/[req=& [%message %sig] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card vase)
    ?>  =(%rely stud)
    `(get-msg bowl)
  ++  init
    |=  old=(unit vase)
    `(get-msg bowl)
  --
--
