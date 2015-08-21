::  not implemented:  set audience
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
  |=  [claiming=? audience=(set station:talk) task]
  =*  tax  +<+>
  =|  moves=(list move)
  |%
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$(tasks (~(put by tasks) id +<.$))]
  ::
  ++  abut
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$]
  ::
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
  ::
  ++  claim
    %_    .
        eny       (sham eny %direct)
        claiming  &
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
  ::
  ++  create            %+  send  %create
                        tax(date-created now, version 0, date-modified now)
  ++  send-update       |*(* (send %update id +<))
  ++  release           |=([vers=@u her=@p] (send-update vers %release her))
  ++  accept            |=(vers=@u (send-update vers %accept ~))
  ++  process-update
    |=  [vers=@u up=update]
    ^+  +>
    ?-    -.up
        %add  ?>(?=(%comment +<.up) (send-update vers %add-comment +>.up))
        %own
      ?-  +<.up
        %announce  (send-update vers %announce ~)
        %claim     claim
      ==
        %set
      ?-  +<.up
        %date-due     (send-update vers %set-date-due +>.up)
        %title        (send-update vers %set-title +>.up)
        %description  (send-update vers %set-description +>.up)
        %tags         (send-update vers %set-tags +>.up)
        %done         (send-update vers %set-done +>.up)
        %audience     ~|(%not-implemented !!)
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
    =+  existing-task=(~(get by tasks) id.tax.action)
    ~?  ?&  ?=(^ existing-task)
            !=(tax.action task.u.existing-task)
        ==
      :*  %new-task-with-old-id
          her=her
          from=from
          new-task=tax.action
          existing-task=u.existing-task
      ==
    ?.  |(=(her owner.tax.action) =(%released status.tax.action))
      ~&  :*  %created-with-bad-owner
              her=her
              from=from
              new-task=tax.action
              existing-task=existing-task
          ==
      [~ +>.$]
    ?.  =(0 version.tax.action)
      ~&  :*  %new-task-version-not-zero
              her=her
              from=from
              new-task=tax.action
              existing-task=existing-task
          ==
      [~ +>.$]
    =.  tasks
      %^  ~(put by tasks)  id.tax.action  |  
      :_  tax.action
      ?~  existing-task  from
      (~(uni in audience.u.existing-task) from)
    =.  sort  ?^(existing-task sort [id.tax.action sort])
    [~ +>.$]
  ::
      %claim
    =+  tax=(~(got by tasks) id.action)
    ?.  &(=(our owner.task.tax) =(%announced status.task.tax))
      ~&  :*  %bad-claim
              her=her
              from=from
              task=tax
          ==
      [~ +>.$]
    abet:(release:(at (~(got by tasks) id.action)) +(version.task.tax) her)
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
        ?:  ?=(%release -.meat.action)
          |
        claiming.u.tax
      :-  (~(uni in audience.u.tax) from)
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
      =.  version.task.u.tax        version.action
      =.  date-modified.task.u.tax  when
      ?-    -.meat.action
        %announce         task.u.tax(status %announced)
        %release          task.u.tax(owner her.meat.action, status %released)
        %accept           task.u.tax(status %accepted)
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
    ?:  ?&  =([%release our] meat.action)
            claiming.u.tax
        ==
      abet:(accept:(at (~(got by tasks) id.action)) +(+(version.task.u.tax)))
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
  ?:(?=([%sole *] pax) ~ `[ust full-report])
::
++  full-report  [%diff %work-report tasks sort]
++  peer-repo  |=(path [[ost full-report]~ +>.$])
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
  ?.  =(our src)
    ~|([%wrong-user our=our src=src] !!)
  =^  mos  +>.$
    ?:  connected
      [~ +>.$]
    initialize
  =^  mof  +>.$
    ?-  -.cod
      %new    abut:create:(at [| - +]:+.cod)
      %old    =+  (at (~(got by tasks) id.cod))
              abet:(process-update:- version.cod dif.cod)
      %sort   mirror-to-web(sort p.cod)
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
