/@  planner
/@  planner-diff
/@  planner-entry
^-  kook:neo
|%
++  state  pro/%planner
++  poke  (sy %planner-diff ~)
++  kids
  :-  ~
  :-  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%da |]
      [pro/%planner-entry (sy %sig ~)]
  ==
++  deps  *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  init
    |=  new=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ?~  new
      planner/!>([now.bowl (add now.bowl ~d30)])
    u.new
  ::
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  !!
        %planner-diff
      =/  poke  !<(planner-diff vax)
      =/  this  !<(planner q.pail)
      ?-    -.poke
          %make
        :_  planner/!>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da now.bowl])
          [%make %planner-entry `planner-entry/!>(entry.poke) ~]
        ==
      ::
          %edit
        :_  planner/!>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da id.poke])
          [%make %planner-entry `planner-entry/!>(entry.poke) ~]
        ==
      ::
          %move
        :-  ~
        planner/!>(this(start start.poke, end end.poke))
      ::
          %tomb
        :_  planner/!>(this)
        ^-  (list card:neo)
        :~
          :-  (snoc here.bowl [%da id.poke])
          =/  entry  [*@da %event *@dr '' &]
          [%make %planner-entry `planner-entry/!>(entry) ~]
        ==
      ==
    ==
  --
--
