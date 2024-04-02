/@  message :: message:/~zod/desk/1 <- [p=stud q=*]
/@  chat-diff
/@  chat
/@  sig
=> 
|%
++  state  chat
++  poke  chat-diff
++  card  card:neo
++  add-msg
  |=  [=bowl:neo msg=message]
  ^-  card:neo
  :-  (welp were.bowl ~[da/now.bowl])
  ^-  note:neo
  [%make %message `!>(msg) ~]

--
^-  firm:neo
|%
++  state  %chat
++  poke  (sy %chat-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  ~[&/%messages |/%da]
      [%message %sig]
  ==
++  deps
  %-  ~(gas by *deps:neo)
  :~  open/[required=| %x %bool %sig]
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
        %msg    :_(sta (add-msg bowl msg.poke)^~)
      ==
    ~&  dbug/"testing changes foo bar baz"
    [cards !>(sta)]
  ++  init
    |=  old=(unit vase)
    `!>(*^state)
  --
--
