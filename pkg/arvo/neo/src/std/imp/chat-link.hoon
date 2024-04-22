/@  message
/@  chat
/@  chat-link
/@  sig
=> 
|%
++  card  card:neo
++  get-chat
  |=  =bowl:neo
  ^-  chat
  =/  dep=cane:neo  q:(~(got by deps.bowl) %src)
  !<(chat q.pail.dep)
++  get-message
  |=  [=bowl:neo id=@da]
  ^-  message
  =/  pit=pith:neo  ~[%messages da/id]
  =/  dep=cane:neo  q:(~(got by deps.bowl) %src)
  =/  msg=pail:neo  pail:(~(got by kids.dep) pit)
  !<(message q.msg)
--
^-  firm:neo
|%
++  state  %chat-link
++  poke   (sy %rely ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [&/%messages |/%da |]
      [%message %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~
  ::
    :-  %src
    ::
    :+  req=&  [%chat %sig]
    :+  ~  %y
    %-  ~(gas by *kids:neo)
    :~  :-  [&/%messages |/%da |]
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
    ^-  (quip card vase)
    ?>  =(%rely stud)
    =+  !<([=term =stem:neo] vax)
    ~&  chat-link/[were.bowl stem]
    =/  sta  sta
    =.  chat.sta  (get-chat bowl)
    ?>  ?=(%y -.q.stem)
    =.  unread.sta  
      %+  add  unread.sta 
      (lent (skim ~(val by kids.q.stem) |=(* =(%add +<))))
    :_  !>(sta)
    %+  murn  ~(tap by kids.q.stem)
    |=  [=(pole iota) [=ever:neo =mode:neo *]]
    ^-  (unit card:neo)
    ?.  ?=([%messages [%da p=@] *] pole)
      ~
    ?.  ?=(%add mode)
      ~
    :+  ~  (welp were.bowl pole)
    [%make %message `!>((get-message bowl p.pole)) ~]
  ++  init
    |=  old=(unit vase)
    =/  sta
      ?~  old  *chat-link
      !<(chat-link u.old)
    =.  chat.sta  (get-chat bowl)
    =/  =cane:neo  q:(~(got by deps.bowl) %src)
    :_  !>(sta)
    ~&  init/kids.cane
    %+  murn  ~(tap by kids.cane)
    |=  [=pith [=ever:neo =pail:neo]]
    ^-  (unit card)
    ~
  --
--
