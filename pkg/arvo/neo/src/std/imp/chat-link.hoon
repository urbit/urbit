/@  message
/@  chat-link
/@  sig
=> 
|%
++  card  card:neo
--
^-  firm:neo
|%
++  state  %chat-link
++  poke   (sy %rely ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  ~[&/%messages |/%da]
      [%message %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~
  ::
    :-  %src
    ::
    :^  req=&  %z  [%chat %sig]
    %-  ~(gas by *kids:neo)
    :~  :-  ~[&/%messages |/%da]
        [%message %sig]
  ::
    ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(chat-link state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ?>  =(%rely stud)
    =+  !<(=rely:neo vax)
    ~&  rely/rely
    `state-vase
  ++  init
    |=  old=(unit vase)
    `(need old)
  --
--
