/@  txt         ::  @t
/@  diary       ::  name=@t
/@  diary-diff  ::  ?([%del-entry id=@da] [%put-entry id=@da =txt])
::
::  XX outer core defines what
^-  kook:neo
|%
::
::  XX state, what is pro
++  state
  ^-  curb:neo
  [%pro %diary]
::
::  takes pokes with stud %diary-diff
++  poke
  ^-  (set stud:neo)
  (sy %diary-diff ~)
::
::  XX define kids
++  kids
  ^-  kids:neo
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%txt ~]
  ==
::
::  XX document deps
++  deps
  ^-  deps:neo
  *deps:neo
::
++  form
  ::
  ::  inner core, business logic
  ^-  form:neo
  ::  XX use face pail and assert stud of state?
  |_  [=bowl:neo =aeon:neo state=pail:neo]
  ++  init
    |=  old=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ::  branch on whether unit is empty or not
    ?^  old
      u.old
    [%diary !>(*diary)]
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?>  =(%diary p.state)
    ?>  =(%diary-diff stud)
    =/  sta  !<(diary q.state)
    =/  poke  !<(diary-diff vax)
    ::  XX note new bowl type?
    ?>  =(our ship.src):bowl
    ::
    ::  XX document
    =^  cards=(list card:neo)  sta
      ?-  -.poke
        %put-entry
          :_  sta
          :~
            :-  (welp here.bowl ~[da/id.poke])
            ^-  note:neo
            [%make %txt `txt/!>(txt.poke) ~]
          ==
        %del-entry  `sta
      ==
    [cards [%diary !>(sta)]]
  --
--
