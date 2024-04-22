/@  message
/@  chat-diff
/@  chat
/@  sig
/-  _/thing
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
  [%make stud `vax ~]

--
^-  firm:neo
|%
++  state  %chat
++  poke  (sy %chat-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [&/%messages |/%da |]
      [%message %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  open/[required=| [%bool %sig] ~]
  ==
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  +*  sta  !<(chat state-vase)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%chat-diff stud)
    =+  !<(poke=chat-diff vax)
    =/  sta  sta
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
    %-  (slog leaf/"debug" (turn ~(val by kids.bowl) |=(p=pail:neo (sell q.p))))
    [cards !>(sta)]
  ++  init
    |=  old=(unit vase)
    `!>(*^state)
  --
--
