/@  task
/@  task-diff
=> 
|%
++  assign-name
  |=  =bowl:neo
  ^-  @ud
  ?~  ~(key by kids.bowl)  1
  =/  piths-ud=(set @ud)
      %-  %~  run  in 
              ~(key by kids.bowl)
          |=  =pith 
            +:(,[%ud @ud] (rear pith))
  =/  last-name=@ud  (rear (sort ~(tap in piths-ud) lth))
  =/  name-missing=(list @ud)
    %~  tap  in
    %-  %~  dif  in
        %-  silt 
            %+  gulf  1 
                last-name
        piths-ud
  ?~  name-missing  +(last-name)
  (rear name-missing)
--
^-  firm:neo
|%
++  state  %task
++  poke   (sy %task-diff ~)
++  kids   
  ^-  kids:neo
  %-  ~(gas by *kids:neo)
  :~  :-  [|/%ud |]
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
      =/  name=@ud  (assign-name bowl)
      =/  new-order
        `(list pith)`(snoc order.this `pith`[ud/name ~])
      :_  !>  this(order new-order)
      :~  :-  (welp here.bowl ~[ud/name])
              [%make %task `!>(task.diff) ~]
      ==
    ::
        %prep
      =/  name=@ud  (assign-name bowl)
      :_  !>  this(order `(list pith)`[~[ud/name] order.this])
      :_  ~
      :*  (welp here.bowl ~[ud/name])
          %make  %task  `!>(task.diff)  ~
      ==
    ::
        %edit
      ::  change to edit cards with text.state and updated done
      :_  !>  this(text text.diff, done done.diff)
      ::  checks if done.task changed
      ?:  =(done.this done.diff)  ~
      ::checks if task has task parent shrub
      ?.  ?=([%ud @ud] (rear (snip here.bowl)))  
        ~
      ::  sends card to parent shrub to check on rest of the kids done.task
      :_  ~
      :*  (snip here.bowl)
        %poke  %task-diff  !>([%check-kids done.diff])
      ==
        %check-kids 
      ?:  done.diff
        ::check if all kid tasks are done
        ?:  %+  levy  order.this
            |=  =pith
              =/  =task  !<(task q:(need (~(get by kids.bowl) pith)))
              done.task
          :_  !>  this(done done.diff)  ~
        ::  just one task out of many done do nothing
        :_  !>  this  ~
      ::  check if one kid undone 
      ?:  =(done.diff done.this)  
        ::  alredy like diff
        :_  !>  this  ~
      ::  kid task undone
      :_  !>  this(done done.diff)  ~
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
