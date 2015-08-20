::  XX  need to deal with versions and date modified
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
|_  [bowl client connected=_| claiming=?]
++  at
  |=  [task audience=(set station:talk)]
  =*  tax  +<-
  =|  moves=(list move)
  |%
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$(tasks (~(put by tasks) id tax audience claiming))]
  ++  send
    |=  action=duty:work-stuff:talk
    ^+  +>
    %_    +>.$
        eny    (sham eny action)
        moves
      :_  ~
      ^-  move
      :*  ost  %poke
          /sending/(scot %uv id)/(scot %ud version)
          [our %talk]
          %talk-command
          ^-  command:talk
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            %-  mo  ^-  (list ,[partner envelope delivery]:talk)
            %+  turn  (~(tap in audience))
            |=(sat=station:talk [[%& sat] [*envelope %pending]])
          [now *bouquet [%tax action]]
      ==
    ==
  ++  claim
    %_    .
        eny  (sham eny %direct)
        moves
      :_  ~
      ^-  move
      :*  ost  %poke
          /claiming/(scot %uv id)
          [our %talk]
          %talk-command
          ^-  command:talk
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            [[[%& owner (main owner)] [*envelope %pending]] ~ ~]
          [now *bouquet [%tax %claim id]]
      ==
    ==
  ++  create            (send `duty:work-stuff:talk`[%create `task`tax])
  ++  send-update       |*(* (send %update id +(version) +<))
  ++  announce          (send-update %announce ~)
  ++  release           (cury send-update %release)
  ++  accept            (send-update %accept ~)
  ++  delete            (send-update %delete ~)
  ++  set-date-due      (cury send-update %set-date-due)
  ++  set-tags          (cury send-update %set-tags)
  ++  set-title         (cury send-update %set-title)
  ++  set-description   (cury send-update %set-description)
  ++  set-done          (cury send-update %set-done)
  ++  add-comment       (cury send-update %add-comment)
  ++  set-audience      ~|(%not-implemented !!)
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
  :_  .(connected %&)  :_  ~
  [ost %peer /peering [our %talk] /f/(main our)/0]
::
++  process-duty
  |=  [when=@da her=ship from=(set station:talk) action=duty:work-stuff:talk]
  ^-  [(list move) _+>.$]
  =-  =^  mof  con  mirror-to-web:con
      [(welp mos mof) con]
  ^-  [mos=(list move) con=_+>.$]
  ?-    -.action
      %create                         ::  XX  should verify ownership
    =+  existing-task=(~(get by tasks) id.p.action)
    ~?  ?&  ?=(^ existing-task)
            !=(p.action task.u.existing-task)
        ==
      :*  %new-task-with-old-id
          her=her
          from=from
          new-task=p.action
          existing-task=u.existing-task
      ==
    =.  tasks
      %^  ~(put by tasks)  id.tax.action  tax.action
      :_  |
      ?~  existing-task  from
      (~(uni in audience.u.existing-task) from)
    =.  sort  ?~(existing-task sort [id.p.action sort])
    [~ +>.$]
  ::
      %claim
    abet:(release:(at (~(got by tasks) id.action)) her)
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
          %release          task.u.tax(owner her.meat.action, status %released)
          %accept           task.u.tax(status %accepted)
          %delete           ~|(%not-implemented !!)
          %set-date-due     task.u.tax(date-due wen.meat.action)
          %set-tags         task.u.tax(tags tag.meat.action)
          %set-title        task.u.tax(title til.meat.action)
          %set-description  task.u.tax(description des.meat.action)
          %set-done         task.u.tax(done ?.(don.meat.action ~ `when))
          %add-comment
            %=  task.u.tax
              discussion  [[when her com.meat.action] discussion.task.u.tax]
            ==
        ==
      :-  (~(uni in audience.u.tax) from)
      claiming.u.tax
    ?:  =([%release our] meat.action)
      abet:accept:(at (~(got by tasks) id.action))
    [~ +>.$]
  ==
::
++  mirror-to-web
  ^-  [(list move) _.]
  ~&  [%mirroring tasks=tasks sort=sort]
  :_  .
  %+  murn  (~(tap by sup))
  |=  [ust=bone her=ship pax=path]
  ^-  (unit move)
  ?:  ?=([%sole *] pax)
    ~
  `[ust %diff %work-report tasks sort]
::
++  coup
  |=  [way=wire saw=(unit tang)]
  ^-  [(list move) _+>.$]
  ?>  ?=(~ saw)
  [~ +>.$]
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
      %sort   ~|(%not-implemented !!)
    ==
  [(welp mos mof) +>.$]
::
::  XX  maybe need to check that we haven't received this message before
::      by keeping a counter of last message received
++  diff-talk-report
  |=  [way=wire rep=report:talk]
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
