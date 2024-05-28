/@  planner
/@  planner-diff
/@  planner-entry
^-  firm:neo
|%
++  state  %planner
++  poke  (sy %planner-diff ~)
++  kids
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%da |]
      [%planner-entry %sig]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo sta=vase *]
  ++  init
    |=  new=(unit vase)
    ^-  (quip card:neo vase)
    :-  ~
    ?~  new
      !>([now.bowl (add now.bowl ~d30)])
    u.new
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    ?+    stud  !!
        %planner-diff
      =/  poke  !<(planner-diff vax)
      =/  this  !<(planner sta)
      ?-    -.poke
          %make
        :_  !>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da now.bowl])
          [%make %planner-entry `!>(entry.poke) ~]
        ==
      ::
          %edit
        :_  !>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da id.poke])
          [%make %planner-entry `!>(entry.poke) ~]
        ==
      ::
          %move
        :-  ~
        !>  this(start start.poke, end end.poke)
      ::
          %tomb
        :_  !>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da id.poke])
          =/  entry  [*@da %event *@dr '' &]
          [%make %planner-entry `!>(entry) ~]
        ==
      ==
    ==
  --
--
