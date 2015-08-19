::
::::
  ::
/?  314
/-  *talk, *work
/+  talk
!:
::::
  ::
|%
++  move  (pair bone card)                              ::  all actions
++  card                                                ::  general card
  $%  [%diff lime]                                      ::
      [%peer wire dock path]                            ::
      [%poke wire dock pear]                            ::
  ==                                                    ::
++  pear                                                ::  poke fruit
  $%  [%talk-command command]                           ::
  ==                                                    ::
--
!:
::::
  ::
|_  [hid=bowl client]
++  at
  |=  [task audience=(set station)]
  =|  moves=(list move)
  |%
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$(tasks (~(put by tasks) tax))]
  ++  send
    |=  action=duty
    ^+  +>
    %_    +>.$
        eny    (sham eny action)
        moves
      :*  ost  %poke
          /sending/(scot %uw id)/(scot %ud version)
          [our %talk]
          %talk-command
          ^-  command
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            [[[%& our %tasks] [*envelope %pending]] ~ ~]
          [now *bouquet [%tax action]]
      ==
    ==
  ++  update            |*(* (send %update id +(version) +<))
  ++  announce          (update %announce ~)
  ++  release           (cury update %announce)
  ++  accept            (update %accept ~)
  ++  delete            (update %delete ~)
  ++  set-due-date      (cury update %set-due-date)
  ++  set-tags          (cury update %set-tags)
  ++  set-title         (cury update %set-title)
  ++  set-description   (cury update %set-description)
  ++  set-complete      (update %set-complete ~)
  ++  add-comment       (cury update %add-comment)
  --
::
++  initialize
  ^-  [(list move) _.]
  :_  .  :_  ~
  [ost %peer /peering [our %talk] /f/tasks/0]
::
++  process-duty
  |=  [when=@da her=ship from=(set station) action=duty]
  ^-  [(list move) _+>.$]
  =<  mirror-to-web
  ?-    -.action
      %proclaim
    :-  ~
    =+  existing-task=(~(get by tasks) id.p.action)
    ~?  !=(p.action u.existing-task)
      $:  %new-task-with-old-id
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
      ~&  $:  %update-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?.  =(version.action +(version.u.tax))
      ~&  $:  %update-bad-version
              her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    =.  tasks
      %^  ~(put by tasks)  id.action
        ?:  $&  ?=(?(%announce %release %accept) -.meat.action)
                !=(her owner.task.u.tax))
            ==
          ~&  $:  %not-owner
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
          %set-due-date     task.u.tax(date-due p.meat.action)
          %set-tags         task.u.tax(tags p.meat.action)
          %set-title        task.u.tax(title p.meat.action)
          %set-description  task.u.tax(description p.meat.action)
          %set-complete     task.u.tax(complete `when)
          %add-comment
            task.u.tax(discussion [[when her p.meat.action] discussion)
        ==
      (~(uni in audience.u.tax) from)
    +>.$
  ==
::
++  mirror-to-web
  ^-  [(list move) _.]
  :_  .
  %+  turn  (~(tap by sup.hid))
  |=  [ust=bone *]
  [ust %diff %work-report tasks sort]
::
++  reap-peering
  |=  [way=wire saw=(unit tang)]
  ^-  [(list move) _+>.$]
  ?>  ?=([~ ~] +<)
  [~ +>.$]
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
  =+  ^-  from=(set station)
      %-  sa
      %+  murn  (~(tap by q.q.i.q.rep))
      |=  par=partner
      `(unit station)`?.(?=(%& -.par) ~ `par)
  ?.  ?=(%tax -.said)
    $(p.rep +(p.rep), q.rep t.q.rep)
  =^  mos  +>.^$  (process-duty her when from said)
  =^  mof  +>.^$  $(p.rep +(p.rep), q.rep t.q.rep)
  [(weld mos mof) +>.^$]
--
