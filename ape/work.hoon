::
::::
  ::
/?  314
/-  *work
/+  talk
!:
::::
  ::
|%
++  move  (pair bone card)                              ::  all actions
++  card                                                ::  general card
  $%  [%diff %work-report client]                       ::
      [%peer wire dock path]                            ::
      [%poke wire dock pear]                            ::
  ==                                                    ::
++  pear                                                ::  poke fruit
  $%  [%talk-command command:talk]                      ::
  ==                                                    ::
--
!:
::::
  ::
|_  [bowl client connected=_|]
++  at
  |=  [task audience=(set station:talk)]
  =*  tax  +<-
  =|  moves=(list move)
  |%
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$(tasks (~(put by tasks) id tax audience))]
  ++  send
    |=  action=duty:work-stuff:talk
    ^+  +>
    %_    +>.$
        eny    (sham eny action)
        moves
      :_  ~
      ^-  move
      :*  ost  %poke
          /sending/(scot %uw id)/(scot %ud version)
          [our %talk]
          %talk-command
          ^-  command:talk
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            [[[%& our %porch] [*envelope %pending]] ~ ~]
          [now *bouquet [%tax action]]
      ==
    ==
  ++  create            (send `duty:work-stuff:talk`[%create `task`tax])
  ++  send-update       |*(* (send %update id +(version) +<))
  ++  announce          (send-update %announce ~)
  ++  release           (cury send-update %announce)
  ++  accept            (send-update %accept ~)
  ++  delete            (send-update %delete ~)
  ++  set-date-due      (cury send-update %set-date-due)
  ++  set-tags          (cury send-update %set-tags)
  ++  set-title         (cury send-update %set-title)
  ++  set-description   (cury send-update %set-description)
  ++  set-done          (cury send-update %set-done)
  ++  add-comment       (cury send-update %add-comment)
  ++  set-audience      ~|(%not-implemented !!)
  ++  claim             ~|(%not-implemented !!)
  ++  process-update
    |=  up=update
    ^+  +>
    ?-    -.up
        %add  ?>(?=(%comment +<.up) (add-comment +>.up))
        %own
      ?-  +<.up
        %announce  announce
        %claim     claim
      ==
        %set
      ?-  +<.up
        %date-due     (set-date-due +>.up)
        %title        (set-title +>.up)
        %description  (set-description +>.up)
        %tags         (set-tags +>.up)
        %done         (set-done +>.up)
        %audience     ~|(%not-implemented !!) ::(set-audience +>.up)
      ==
    ==
  --
::
++  initialize
  ^-  [(list move) _.]
  :_  .  :_  ~
  [ost %peer /peering [our %talk] /f/porch/0]
::
++  process-duty
  |=  [when=@da her=ship from=(set station:talk) action=duty:work-stuff:talk]
  ^-  [(list move) _+>.$]
  =<  mirror-to-web
  ?-    -.action
      %create
    :-  ~
    =+  existing-task=(~(get by tasks) id.p.action)
    ~?  ?&  ?=(^ existing-task)
            !=(p.action u.existing-task)
        ==
      :*  %new-task-with-old-id
          her=her
          from=from
          new-task=p.action
          existing-task=u.existing-task
      ==
    =.  tasks
      %^  ~(put by tasks)  id.p.action  p.action
      ?~  existing-task  from
      (~(uni in audience.u.existing-task) from)
    =.  sort  ?~(existing-task sort [id.p.action sort])
    +>.$
  ::
      %update
    =+  tax=(~(get by tasks) id.action)
    ?~  tax
      ~&  :*  %update-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?.  =(version.action +(version.task.u.tax))
      ~&  :*  %update-bad-version
              her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    =.  tasks
      %^  ~(put by tasks)  id.action
        ?:  ?&  ?=(?(%announce %release %accept) -.meat.action)
                !=(her owner.task.u.tax)
            ==
          ~&  :*  %not-owner
                  her=her
                  from=from
                  action=action
                  tax=tax
              ==
          task.u.tax
        ?-    -.meat.action
          %announce         task.u.tax(status %announced)
          %release          task.u.tax(owner p.meat.action, status %released)
          %accept           task.u.tax(status %accepted)
          %delete           !!
          %set-date-due     task.u.tax(date-due p.meat.action)
          %set-tags         task.u.tax(tags p.meat.action)
          %set-title        task.u.tax(title p.meat.action)
          %set-description  task.u.tax(description p.meat.action)
          %set-done         task.u.tax(done `when)
          %add-comment
            %=  task.u.tax
              discussion  [[when her p.meat.action] discussion.task.u.tax]
            ==
        ==
      (~(uni in audience.u.tax) from)
    +>.$
  ==
::
++  mirror-to-web
  ^-  [(list move) _.]
  :_  .
  %+  turn  (~(tap by sup))
  |=  [ust=bone *]
  [ust %diff %work-report tasks sort]
::
++  reap-peering
  |=  [way=wire saw=(unit tang)]
  ^-  [(list move) _+>.$]
  ?>  ?=([~ ~] +<)
  [~ +>.$]
::
++  poke-work-command
  |=  cod=command
  =^  mos  +>.$
    ?:  connected
      [~ +>.$]
    initialize
  =^  mof  +>.$
    ?-  -.cod
      %new    abet:create:(at +.cod)
      %old    abet:(process-update:(at (~(got by tasks) id.cod)) dif.cod)
      %sort   !!
    ==
  [(welp mos mof) +>.$]
::
::  XX  maybe need to check that we haven't received this message before
::      by keeping a counter of last message received
++  diff-peering
  |=  [way=wire %talk-report rep=report]
  ^-  [(list move) _+>.$]
  ?>  ?=(%grams -.rep)
  |-  ^-  [(list move) _+>.^$]
  ?~  q.rep  [~ +>.^$]
  =*  her   p.i.q.rep
  =*  when  p.r.q.i.q.rep
  =*  said  r.r.q.i.q.rep
  =+  ^-  from=(set station:talk)
      %-  sa  ^-  (list station:talk)
      %+  murn  (~(tap by q.q.i.q.rep))
      |=  [par=partner *]
      `(unit station:talk)`?.(?=(%& -.par) ~ `p.par)
  ?.  ?=(%tax -.said)
    $(p.rep +(p.rep), q.rep t.q.rep)
  =^  mos  +>.^$  (process-duty when her from +.said)
  =^  mof  +>.^$  $(p.rep +(p.rep), q.rep t.q.rep)
  [(weld mos mof) +>.^$]
--
