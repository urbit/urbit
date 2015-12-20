::
::::
  ::
/?  314
/-  work
/+  talk
!:
::::
  ::
[. work]
|%
++  move  (pair bone card)                              ::  all actions
++  card                                                ::  general card
  $%  {$diff $work-report client}                       ::
      {$peer wire dock path}                            ::
      {$poke wire dock pear}                            ::
  ==                                                    ::
++  pear                                                ::  poke fruit
  $%  {$talk-command command:talk}                      ::
  ==                                                    ::
--
!:
::::
  ::
|_  $:  bowl
        client
        connected/?                                     ::  subscribed to talk
        count/@ud                                       ::  # messages from talk
        unordered/(map {@uvH @u} (pair ship flesh:work-stuff:talk))
    ==
++  at
  |=  client-task
  =|  moves/(list move)
  |%
  ++  abet
    ^-  {(list move) _+>.$}
    [(flop moves) +>.$(tasks (~(put by tasks) id.tax +<.$))]
  ::
  ++  abut
    ^-  {(list move) _+>.$}
    [(flop moves) +>.$]
  ::
  ++  send-audience
    |=  {to/(set station:talk) action/duty:work-stuff:talk}
    %_    +>.$
        eny    (sham eny action)
        moves
      :_  moves
      ^-  move
      :*  ost  %poke
          /sending+(scot %uv id.tax)/(scot %ud version.tax)
          [our %talk]
          %talk-command
          =>  [. talk]  ^-  command:talk
          :-  %publish
          |-  ^-  (list thought)
          :_  ~
          :+  (shaf %task eny)
            %-  mo  ^-  (list {partner envelope delivery})
            %+  turn  (~(tap in to))
            |=(sat/station [[%& sat] [*envelope %pending]])
          [now *bouquet [%tax action]]
      ==
    ==
  ::
  ++  send
    |=  action/duty:work-stuff:talk
    (send-audience audience action)
  ::
  ++  send-archive
    |=  to/(set station:talk)
    (send-audience to %archive id.tax)
  ::
  ++  send-create       (send %create tax)
  ++  send-change       |*  *
                        ?:  =(our creator.tax)
                          (send-update +(version.tax) our +<)
                        %+  send-audience
                          [[creator.tax (main:talk creator.tax)] ~ ~]
                        [%change id.tax +<]
  ++  send-update       |*(* (send %update id.tax +<))
  ++  process-update
    |=  up/update
    ^+  +>
    ?-    -.up
        $add  ?>(?=($comment +<.up) (send-change %add-comment our +>.up))
        $doer
      ?-  +<.up
        $release  (send-change %set-doer ~)
        $claim    (send-change %set-doer `our)
      ==
    ::
        $set
      ?-  +<.up
        $audience     (process-audience to.up)
        $date-due     (send-change %set-date-due +>.up)
        $title        (send-change %set-title +>.up)
        $description  (send-change %set-description +>.up)
        $tags         (send-change %set-tags +>.up)
        $done         (send-change %set-done +>.up)
      ==
    ==
  ++  process-audience
    |=  to/(set station:talk)
    ^+  +>
    =.  +>.$  (send-archive (~(dif in audience) to))
    =.  +>.$  (send-audience (~(dif in to) audience) %create tax)
    +>.$(audience to)
  --
::
++  prep
  |=  $=  old
      $_
      =<  $
      %-  unit
      $:  client
          ?
          @ud
          (map {@uvH @u} (pair ship flesh:work-stuff:talk))
      ==
  ^-  {(list move) _+>.$}
  initialize(+<+ ?~(old +<+.+>.$ u.old))
::
++  initialize
  ^-  {(list move) _.}
  ?:  connected
    [~ .]
  :_  .(connected %&)  :_  ~
  [ost %peer /peering [our %talk] /f+(main:talk our)/(scot %ud count)]
::
++  process-duty
  |=  {when/@da her/ship from/(set station:talk) action/duty:work-stuff:talk}
  ^-  {(list move) _+>.$}
  =-  =^  mof  con  mirror-to-web:con
      [(welp mof mos) con]
  ^-  {mos/(list move) con/_+>.$}
  ?-    -.action
      $create
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
    ?.  =(her creator.tax.action)
      ~&  :*  %created-with-bad-creator
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
      $archive
    =+  tax=(~(get by tasks) id.action)
    ?~  tax
      ~&  :*  %archive-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?:  !=(her creator.tax.u.tax)
      ~&  :*  %archiver-not-creator
              her=her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    =.  tasks
      %+  ~(put by tasks)  id.action
      :*  =(~ (~(dif in audience.u.tax) from))
          (~(dif in audience.u.tax) from)
          tax.u.tax
      ==
    [~ +>.$]
  ::
      $change
    =+  tax=(~(get by tasks) id.action)
    ?~  tax
      ~&  :*  %change-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?:  !=(our creator.tax.u.tax)
      ~&  :*  %me-not-creator
              her=her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    abet:(send-update:(at u.tax) +(version.tax.u.tax) her meat.action)
  ::
      $update
    =+  tax=(~(get by tasks) id.action)
    ?~  tax
      ~&  :*  %update-for-nonexistent-task
              her=her
              from=from
              action=action
          ==
      [~ +>.$]
    ?:  !=(her creator.tax.u.tax)
      ~&  :*  %her-not-creator
              her=her
              from=from
              action=action
              tax=tax
          ==
      [~ +>.$]
    ?.  =(version.action +(version.tax.u.tax))
      ~&  :*  %update-bad-version
              her
              from=from
              action=action
              tax=tax
          ==
      ?:  (lte version.action version.tax.u.tax)
        ~&  %really-bad-version
        [~ +>.$]
      :-  ~
      %_    +>.$
          unordered
        %+  ~(put by unordered)
          [id.action version.action]
        [her.action meat.action]
      ==
    |-
    =.  tasks
      %+  ~(put by tasks)  id.action
      :+  archived.u.tax
        (~(uni in audience.u.tax) from)
      =.  version.tax.u.tax        version.action
      =.  date-modified.tax.u.tax  when
      ?-    -.meat.action
        $set-doer         tax.u.tax(doer her.meat.action)
        $set-date-due     tax.u.tax(date-due wen.meat.action)
        $set-tags         tax.u.tax(tags tag.meat.action)
        $set-title        tax.u.tax(title til.meat.action)
        $set-description  tax.u.tax(description des.meat.action)
        $set-done         tax.u.tax(done ?.(don.meat.action ~ `when))
        $add-comment
          %=  tax.u.tax
            discussion  [[when [who com]:meat.action] discussion.tax.u.tax]
          ==
      ==
    =+  ooo=(~(get by unordered) id.action +(version.action))
    ?~  ooo
      [~ +>.^$]
    %=  $
      version.action  +(version.action)
      her.action      p.u.ooo
      meat.action     q.u.ooo
    ==
  ==
::
++  mirror-to-web
  ^-  {(list move) _.}
  ~&  [%mirroring sort=(turn sort |=(a/@uv `@uv`(rsh 2 25 a)))]
  :_  .
  %+  murn  (~(tap by sup))
  |=  {ust/bone her/ship pax/path}
  ^-  (unit move)
  ?:(?=({$sole *} pax) ~ `[ust full-report])
::
++  full-report  [%diff %work-report tasks sort]
++  peer-repo  |=(path [[ost full-report]~ +>.$])
++  coup
  |=  {way/wire saw/(unit tang)}
  ^-  {(list move) _+>.$}
  ?>  ?=($~ saw)
  [~ +>.$]
::
++  quit-peering  |=(way/wire ?>(?=($~ way) initialize(connected |)))
++  reap-peering
  |=  {way/wire saw/(unit tang)}
  ^-  {(list move) _+>.$}
  ?>  ?=({$~ $~} +<)
  [~ +>.$]
::
++  poke-work-command
  |=  cod/command
  ?.  =(our src)
    ~|([%wrong-user our=our src=src] !!)
  ?-  -.cod
    $sort       mirror-to-web(sort p.cod)
    $old
      =^  mow  +>.$
        =+  (at (~(got by tasks) id.cod))
        abet:(process-update:- dif.cod)
      =^  mov  +>.$  mirror-to-web
      [(welp mov mow) +>.$]
    $new
      =.  +>.cod  +>.cod(date-created now, version 0, date-modified now)
      abut:send-create:(at | +.cod)
  ==
::
::  XX  test the disconnection case
++  diff-talk-report
  |=  {way/wire rep/report:talk}
  ^-  {(list move) _+>.$}
  ?>  ?=($grams -.rep)
  |-  ^-  {(list move) _+>.^$}
  ?~  q.rep  [~ +>.^$]
  =.  count  +(count)
  =*  her   p.i.q.rep
  =*  when  p.r.q.i.q.rep
  =*  said  r.r.q.i.q.rep
  ?.  ?=($tax -.said)
    $(p.rep +(p.rep), q.rep t.q.rep)
  =+  ^-  from/(set station:talk)
      %-  sa  ^-  (list station:talk)
      %+  murn  (~(tap by q.q.i.q.rep))
      =>  talk
      |=  {par/partner *}
      `(unit station)`?.(?=($& -.par) ~ `p.par)
  =^  mos  +>.^$  (process-duty when her from +.said)
  =^  mof  +>.^$  $(p.rep +(p.rep), q.rep t.q.rep)
  [(weld mos mof) +>.^$]
--
