/@  message
/@  chat
/@  chat-link
/@  sig
=> 
|%
++  card  card:neo
++  get-chat-ax
  |=  =bowl:neo
  ^-  lore:neo
  q:(~(got by deps.bowl) %src)
++  get-chat
  |=  =bowl:neo
  !<(chat q.pail:(need fil:(get-chat-ax bowl)))
++  get-message
  |=  [=bowl:neo id=@da]
  ^-  message
  =/  pit=pith:neo  ~[%messages da/id]
  =/  dep=lore:neo  q:(~(got by deps.bowl) %src)
  !<(message q.pail:(~(got of:neo dep) pit))
--
^-  kook:neo
|%
++  state  pro/%chat-link
++  poke   (sy %rely ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [&/%messages |/%da |]
      [pro/%message ~]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~
  ::
    :-  %src
    ::
    :+  req=&  [pro/%chat ~]
    :+  ~  %y
    %-  ~(gas by *lads:neo)
    :~  :-  [&/%messages |/%da |]
        [pro/%message ~]
  ::
    ==
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(chat-link state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card pail:neo)
    ?>  =(%rely stud)
    =+  !<([=term =stem:neo] vax)
    ~&  chat-link/[were.bowl stem]
    =/  sta  sta
    =.  chat.sta  (get-chat bowl)
    ?>  ?=(%y -.q.stem)
    =.  unread.sta  
      %+  add  unread.sta 
      (lent (skim ~(val by kids.q.stem) |=(* =(%add +<))))
    :_  chat-link/!>(sta)
    %+  murn  ~(tap by kids.q.stem)
    |=  [=(pole iota) [=ever:neo =mode:neo *]]
    ^-  (unit card:neo)
    ?.  ?=([%messages [%da p=@] *] pole)
      ~
    ?.  ?=(%add mode)
      ~
    :+  ~  (welp were.bowl pole)
    [%make %message `message/!>((get-message bowl p.pole)) ~]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card pail:neo)
    =/  s=chat-link
      ?~  old  *chat-link
      !<(chat-link q.u.old)
    =.  chat.s  (get-chat bowl)
    :_  chat-link/!>(s)
    =/  kids  ~(tap of:neo (get-chat-ax bowl))
    %+  murn  kids
    |=  [=pith =idea:neo]
    ^-  (unit card)
    ~
  --
--
