/@  task
/@  task-diff
|%
++  state  pro/%task
++  poke   (sy %task-diff ~)
++  kids   
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%tas |]
    [pro/%task (sy %task-diff ~)]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  this  !<(task state-vase)
    =/  diff  !<(task-diff vax)
    ?-  -.diff
        %become
      ::  XX  if you try to become your own ancestor, then infinite loop
      =/  there  (welp [p/our.bowl]~ pith.diff)
      :_  task/!>(this)
      ^-  (list card:neo)
      :~
        [there %poke %task-diff !>([%prayer +.here.bowl])]
      ==
    ::
        %prayer
      :_  task/!>(this)
      %-  flop
      ^-  (list card:neo)
      :-
        [(welp [p/our.bowl]~ pith.diff) %make %task `task/!>(this) ~]
      %-  zing
      %+  turn  ~(tap by ~(tar of:neo kids.bowl))
      |=  [=pith =idea:neo]
      ^-  (list card:neo)
      :~
        [:(welp [p/our.bowl]~ pith.diff pith) %make %task `pail.idea ~]
        [(welp here.bowl pith) %poke %task-diff !>([%prayer (welp pith.diff pith)])]
      ==
    ::
        %nest
      =.  order.this  `(list pith)`(snoc order.this `pith`[`@tas`name.diff ~])
      :_  task/!>(this)
      :_  ~
      :*  (snoc here.bowl name.diff)
          %make  %task  `task/!>(task.diff)  ~
      ==
    ::
        %prep
      =.  order.this  `(list pith)`[[name.diff ~] order.this]
      :_  task/!>(this)
      :_  ~
      :*  (snoc here.bowl name.diff)
          %make  %task  `task/!>(task.diff)  ~
      ==
    ::
        %edit
      :-  ~
      :-  %task
      !>  this(text text.diff, done done.diff)
    ::
        %done
      :-  ~
      :-  %task
      !>  this(done !done.this)
    ::
        %kid-done
      :_  task/!>(this)
      :_  ~
      :*  (welp here.bowl pith.diff)
          %poke  %task-diff  !>([%done ~])
      ==
    ::
        %oust
      =/  i  (find [pith.diff ~] order.this)
      ?~  i  `task/!>(this)
      :_  task/!>(this(order (oust [(need i) 1] order.this)))
      ~
      :::_  ~
      :::*  (welp here.bowl pith.diff)
      ::    %tomb
      ::==
    ::
        %reorder
      `task/!>(this(order order.diff))
    ==
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
  --
--
