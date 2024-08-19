/@  task
/@  task-diff
=>
|%
++  check-kids
  |=  =bowl:neo
  ^-  ?
  ?:  =([~ ~] kids.bowl)
    %.y
  =/  piths  ~(tap in ~(key by ~(tar of:neo kids.bowl)))
  %+  levy  piths
  |=  =pith
  =/  =task
    !<  task
    q.pail:(need (~(get by ~(tar of:neo kids.bowl)) pith))
  done.task
::
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
++  poke   (sy %task-diff %gift ~)
++  kids
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |]
    [pro/%task (sy %task-diff %gift ~)]
  ==
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo stud:neo state-vase=vase]
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    ?^  pal  u.pal
    task/!>(*task)
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    =/  this  !<(task state-vase)
    ?+    stud  !!
        %gift
      ::check if %del case from tree view
      =/  =gift:neo  !<(gift:neo vax)
      =/  gift-card=[=pith:neo =loot:neo]
        %+  snag  0
         ~(tap of:neo gift)
      ?:  =(mode.loot.gift-card %del)  
        =/  i  (find [pith.gift-card ~] order.this)
        ?~  i  [~ task/!>(this)]
        [~ task/!>(this(order (oust [(need i) 1] order.this)))]
      ::check if all kid tasks are done
      =/  dun  (check-kids bowl)
      [~ task/!>(this(done dun, kids-done dun))]
    ::
        %task-diff
      =/  diff  !<(task-diff vax)
      ?-    -.diff
          %new
        =/  name=@ud  (assign-name bowl)
        =.  order.this
          ?:  prepend.diff
            [~[ud/name] order.this]
          (snoc order.this `pith`[ud/name ~])
        =.  done.this  |
        =.  kids-done.this  |
        :_  task/!>(this)
        :~  :-  (welp here.bowl ~[ud/name])
            [%make %task `task/!>(task.diff) ~]
        ==
      ::
          %edit
        :-  ~
        :-  %task
        !>
        %=  this
          text  text.diff
          done  ?:  kids-done.this
                  done.diff
                %.n
        ==
      ::
          %oust
        =/  i  (find [pith.diff ~] order.this)
        ?~  i  `task/!>(this)
        :_  task/!>(this(order (oust [(need i) 1] order.this)))
        :~  [(welp here.bowl pith.diff) [%cull ~]]
            [(welp here.bowl pith.diff) [%tomb ~]]
        ==
      ::
          %reorder
        `task/!>(this(order order.diff))
      ==
    ==
  --
--
