::  also check if talk can add stations to something other than porch
::  maybe look into storing a "following" set
::  make most updates not rely on knowing about task (all but claim?)
::  should let non-owners suggest that owner cross-post to another
::    station
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
  |=  client-task
  =|  moves=(list move)
  |%
  ++  abet
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$(tasks (~(put by tasks) id.tax +<.$))]
  ::
  ++  abut
    ^-  [(list move) _+>.$]
    [(flop moves) +>.$]
  ::
  ++  send-audience
    |=  [to=(set station:talk) action=duty:work-stuff:talk]
    ^+  +>
    %_    +>.$
        eny    (sham eny action)
        moves
      :_  moves
      ^-  move
      :*  ost  %poke
          /sending/(scot %uv id.tax)/(scot %ud version.tax)
          [our %talk]
          %talk-command
          ^-  command:talk
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            %-  mo  ^-  (list ,[partner envelope delivery]:talk)
            %+  turn  (~(tap in to))
            |=(sat=station:talk [[%& sat] [*envelope %pending]])
          [now *bouquet [%tax action]]
      ==
    ==
  ::
  ++  send
    |=  action=duty:work-stuff:talk
    (send-audience audience action)
  ::
  ++  claim
    (send-audience(claiming &) [[owner.tax (main owner.tax)] ~ ~] %claim id.tax)
  ::
  ++  send-archive
    |=  to=(set station:talk)
    (send-audience to %archive id.tax)
  ::
  ++  send-create       (send %create tax)
  ++  send-update       |*(* (send %update id.tax +<))
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
      ==
    ==
  ++  process-audience
    |=  to=(set station:talk)
    ^+  +>
    =.  +>.$  (send-archive (~(dif in audience) to))
    =.  +>.$  (send-audience (~(dif in to) audience) %create tax)
    +>.$(audience to)
  --
::
++  prep
  |=  [old=(unit (pair client ,_|))]
  ^-  [(list move) _+>.$]
  initialize(+<+ ?~(old +<+.+>.$ u.old))
::
++  initialize
  ^-  [(list move) _.]
  ?:  connected
    [~ .]
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
      %create
    =+  existing-task=(~(get by tasks) id.tax.action)
    ?:  ?&  ?=(^ existing-task)
            !=(tax.action tax.u.existing-task)
            !archived.u.existing-task
        ==
      ~&  :*  %new-task-with-old-id
              her=her
              from=from
              new-task=tax.action
              existing-task=u.existing-task
          ==
      [~ +>.$]
    ?.  |(=(her owner.tax.action) =(%released status.tax.action))
      ~&  :*  %created-with-bad-owner
              her=her
              from=from
              new-task=tax.action
              existing-task=existing-task
          ==
      [~ +>.$]
    =.  tasks
      %^  ~(put by tasks)  id.tax.action  |
      :-  |  :_  tax.action
      ?~  existing-task  from
      (~(uni in audience.u.existing-task) from)
    =.  sort  ?^(existing-task sort [id.tax.action sort])
    [~ +>.$]
  ::
      %claim
    =+  tax=(~(got by tasks) id.action)
    ?.  &(=(our owner.tax.tax) =(%announced status.tax.tax))
      ~&  :*  %bad-claim
              her=her
              from=from
              task=tax
          ==
      [~ +>.$]
    abet:(release:(at (~(got by tasks) id.action)) +(version.tax.tax) her)
  ::
      %archive
    =+  tax=(~(get by tasks) id.action)
    ?~  tax
      ~&  :*  %archive-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?:  !=(her owner.tax.u.tax)
      ~&  :*  %archiver-not-owner
              her=her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    =.  tasks
      %+  ~(put by tasks)  id.action
      :*  claiming.u.tax
          =(~ (~(dif in audience.u.tax) from))
          (~(dif in audience.u.tax) from)
          tax.u.tax
      ==
    [~ +>.$]
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
    ?.  =(version.action +(version.tax.u.tax))
      ~&  :*  %update-bad-version
              her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    ?:  ?&  ?=(?(%announce %release %accept) -.meat.action)
            !=(her owner.tax.u.tax)
        ==
      ~&  :*  %not-owner
              her=her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    =.  tasks
      %+  ~(put by tasks)  id.action
      :*  ?:  ?=(%release -.meat.action)
            |
          claiming.u.tax
      ::
          archived.u.tax
      ::
          (~(uni in audience.u.tax) from)
      ::
          =.  version.tax.u.tax        version.action
          =.  date-modified.tax.u.tax  when
          ?-    -.meat.action
            %announce         tax.u.tax(status %announced)
            %release          tax.u.tax(owner her.meat.action, status %released)
            %accept           tax.u.tax(status %accepted)
            %set-date-due     tax.u.tax(date-due wen.meat.action)
            %set-tags         tax.u.tax(tags tag.meat.action)
            %set-title        tax.u.tax(title til.meat.action)
            %set-description  tax.u.tax(description des.meat.action)
            %set-done         tax.u.tax(done ?.(don.meat.action ~ `when))
            %add-comment
              %=  tax.u.tax
                discussion  [[when her com.meat.action] discussion.tax.u.tax]
              ==
          ==
      ==
    ?:  ?&  =([%release our] meat.action)
            claiming.u.tax
        ==
      abet:(accept:(at (~(got by tasks) id.action)) +(+(version.tax.u.tax)))
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
  ?-  -.cod
    %sort       mirror-to-web(sort p.cod)
    %audience
      =^  mow  +>.$
        abet:(process-audience:(at (~(got by tasks) id.cod)) to.cod)
      =^  mov  +>.$  mirror-to-web
      [(welp mow mov) +>.$]
    %old
      =+  (at (~(got by tasks) id.cod))
      abet:(process-update:- version.cod dif.cod)
    %new
      =.  +>.cod  +>.cod(date-created now, version 0, date-modified now)
      abut:send-create:(at | | +.cod)
  ==
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
