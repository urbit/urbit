/@  task
/@  task-diff
|%
++  state  %task
++  poke   (sy %task-diff ~)
++  kids   
  ^-  kids:neo
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%tas |]
    [%task %task-diff]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =ever:neo state-vase=vase *]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo vase)
    =/  this  (task !<(task state-vase))
    =/  diff  (task-diff !<(task-diff vax))
    ?-  -.diff
        %become
      ::  XX  if you try to become your own ancestor, then infinite loop
      =/  there  (welp [p/our.bowl]~ pith.diff)
      :_  state-vase
      ^-  (list card:neo)
      :~
        [there %poke %task-diff !>([%prayer +.here.bowl])]
      ==
    ::
        %prayer
      :_  state-vase
      %-  flop
      ^-  (list card:neo)
      :-
        [(welp [p/our.bowl]~ pith.diff) %make %task `!>(this) ~]
      %-  zing
      %+  turn  ~(tap by kids.bowl)
      |=  [=pith =pail:neo]
      :~
        [:(welp [p/our.bowl]~ pith.diff pith) %make %task `q.pail ~]
        [(welp here.bowl pith) %poke %task-diff !>([%prayer (welp pith.diff pith)])]
      ==
    ::
        %nest
      :_  !>  this(order `(list pith)`(snoc order.this `pith`[`@tas`name.diff ~]))
      :_  ~
      :*  (snoc here.bowl name.diff)
          %make  %task  `!>(task.diff)  ~
      ==
    ::
        %prep
      :_  !>  this(order `(list pith)`[[name.diff ~] order.this])
      :_  ~
      :*  (snoc here.bowl name.diff)
          %make  %task  `!>(task.diff)  ~
      ==
    ::
        %edit
      :-  ~
      !>  this(text text.diff, done done.diff)
    ::
        %done
      :-  ~
      !>  this(done !done.this)
    ::
        %kid-done
      :_  !>  this
      :_  ~
      :*  (welp here.bowl pith.diff)
          %poke  %task-diff  !>([%done ~])
      ==
    ::
        %oust
      =/  i  (find [pith.diff ~] order.this)
      ?~  i  `!>(this)
      :_  !>  this(order (oust [(need i) 1] order.this))
      ~
      :::_  ~
      :::*  (welp here.bowl pith.diff)
      ::    %tomb
      ::==
    ::
        %reorder
      :-  ~
      !>  this(order order.diff)
    ==
  ++  init
    |=  vas=(unit vase)
    `(need vas)
  --
--
