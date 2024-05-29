/@  message
/@  chat-diff
/@  chat
/@  sig
=>
|%
++  state  chat
++  poke  chat-diff
++  card  card:neo
::
++  add-message
  |=  [=bowl:neo msg=message]
  ^-  card:neo
  (add-msg bowl %message !>(msg))
::
++  add-msg
  |=  [=bowl:neo =stud:neo vax=vase]
  ^-  card:neo
  :-  (welp were.bowl ~[%messages da/now.bowl])
  ^-  note:neo
  [%make stud `[%message vax] ~]
--
^-  kook:neo
|%
++  state  pro/%chat
++  poke  (sy %chat-diff ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [&/%messages |/%da |]
      [pro/%message ~]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  open/[required=| [pro/%bool ~] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  +*  sta  !<(chat state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%chat-diff stud)
    =+  !<(poke=chat-diff vax)
    =/  sta  sta
    ~&  testing-rebuild/2
    :: ?.  ;;(? +:(~(gut by deps.bowl) %open [*pith &]))
     :: ~&(dropping-poke/poke !>(sta))
    ?>  |(=(our ship.src):bowl (~(has in who.sta) ship.src.bowl))
    =^  cards=(list card)  sta
      ?-  -.poke
        %title  `sta(title title.poke)
        %add    `sta(who (~(put in who.sta) ship.poke))
        %del    `sta(who (~(del in who.sta) ship.poke))
        %dbug   `sta
        %msg    :_(sta (add-message bowl message.poke)^~)
        %custom  :_(sta (add-msg bowl [stud vase]:poke)^~)
      ==
    ::  %-  (slog leaf/"debug" (turn ~(val by ~(tar of:neo kids.bowl)) |=(=saga:neo (sell q.q.saga))))
    [cards chat/!>(sta)]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `chat/!>(*chat)
  --
--
