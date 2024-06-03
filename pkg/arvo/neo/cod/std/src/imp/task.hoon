/@  task
/@  task-diff
=> 
|%
++  assign-name
  |=  =bowl:neo
  ^-  @ud
  ?:  =([~ ~] kids.bowl)  1
  =/  sorted-names=(list @ud)
    %-  sort  :_  lth
    %+  turn  ~(tap by ~(tar of:neo kids.bowl))
      |=  [=pith =idea:neo]
      +:(,[%ud @ud] (rear pith))
  =/  last-name=@ud  (rear sorted-names)
  =/  name-missing=(list @ud)
    %+  skim  (gulf 1 last-name)
      |=  n=@ud
        =(~ (find ~[n] sorted-names))
  ?~  name-missing  +(last-name)
  (rear name-missing)
--
^-  kook:neo
|%
++  state  pro/%task
++  poke   (sy %task-diff ~)
++  kids   
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |]
    [pro/%task (sy %task-diff ~)]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    `(need pal)
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
        %prepend
      =/  name=@ud  (assign-name bowl)
      =.  order.this  `(list pith)`[~[ud/name] order.this]
      :_  task/!>(this)
      :~  :-  (welp here.bowl ~[ud/name])
              [%make %task `task/!>(task.diff) ~]
      ==
    ::
        %append
      =/  name=@ud  (assign-name bowl)
      =.  order.this  `(list pith)`(snoc order.this `pith`[ud/name ~])
      :_  task/!>(this)
      :~  :-  (welp here.bowl ~[ud/name])
              [%make %task `task/!>(task.diff) ~]
      ==
    ::
        %edit
      :_  :-  %task
          !>  this(text text.diff, done done.diff)
      ?:  =(done.this done.diff)  ~
      ::checks if task has task parent shrub
      ?.  ?=([%ud @ud] (rear (snip here.bowl)))  
        ~
      ::  sends card to parent shrub to check on rest of the kids done.task
      :_  ~
      :*  (snip here.bowl)
          %poke  %task-diff  !>([%check-kids done.diff])
      ==
    ::
        %check-kids 
      ?:  done.diff
        ::check if all kid tasks are done
        ?:  %+  levy  order.this
            |=  =pith
              =/  =task  !<(task q.pail:(need (~(get by ~(tar of:neo kids.bowl)) pith)))
              done.task
          :_  :-  %task  !>  this(done done.diff)  ~
      ::   ::  just one task out of many done do nothing
        :_  task/!>(this)  ~
      :: ::  check if one kid undone 
      ?:  =(done.diff done.this)  
        ::  alredy like diff
        :_  task/!>(this)  ~
      ::  kid task undone
      :_  :-  %task  !>  this(done done.diff)  ~
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
  --
--
