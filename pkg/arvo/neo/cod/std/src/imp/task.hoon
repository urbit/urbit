/@  task
/@  task-diff
=> 
|%
++  check-kids
  |=  =bowl:neo
  ^-  ?
  ?:  =(kids.bowl ~)
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
++  poke   (sy %task-diff %rely ~)
++  kids   
  :+  ~  %y
  %-  ~(gas by *lads:neo)
  :~  :-  [|/%ud |]
    [pro/%task (sy %task-diff %rely ~)]
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
    ?+    stud  !!
        %rely
      ::check if all kid tasks are done
      ?:  (check-kids bowl)
        [~ task/!>(this(done %.y))]
      [~ task/!>(this(done %.n))]
    ::  
        %task-diff
      =/  diff  !<(task-diff vax)
      ?-    -.diff
          %new
        =/  name=@ud  (assign-name bowl)
        =.  order.this  
          ?:  prepend.diff
            `(list pith)`[~[ud/name] order.this]
          `(list pith)`(snoc order.this `pith`[ud/name ~])
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
          done  ?:  (check-kids bowl)
                  done.diff
                %n
        ==
      ::
          %oust
        =/  i  (find [pith.diff ~] order.this)
        ?~  i  `task/!>(this)
        :_  task/!>(this(order (oust [(need i) 1] order.this)))
        :~  [(welp here.bowl pith.diff) [%tomb ~]]
        ==
      ::
          %reorder
        `task/!>(this(order order.diff))
      ==
    ==
  --
--
