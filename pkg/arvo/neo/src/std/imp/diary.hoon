/@  txt
/@  diary
/@  diary-diff
::
^-  firm:neo
|%
++  state  %diary
++  poke  (sy %diary-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%da |]
      [%txt %sig]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?>  =(%diary-diff stud)
    =/  poke  (diary-diff !<(diary-diff vax))
    =/  sta  (diary !<(diary state-vase))
    ?>  =(our ship.src):bowl
    =^  cards=(list card:neo)  sta
      ?-  -.poke
        %put-entry
          :_  sta
          :~
            :-  (welp here.bowl ~[da/id.poke])
            ^-  note:neo
            [%make %txt `!>(txt.poke) ~]
          ==
        %del-entry  `sta
      ==
    [cards !>(sta)]
  ++  init
    |=  old=(unit vase)
    `(need old)
  --
--
