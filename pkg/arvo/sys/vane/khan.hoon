::  %behn, just a timer
!:
!?  164
::
=,  khan
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $%  $:  %k                                        ::   to self
                $>  $?  %crud                           ::
                        %avow                           ::
                        %fyrd                           ::
                ==                                      ::
             task                                       ::
          ==                                            ::
          $:  %d                                        ::    to %dill
              $>(%flog task:dill)                       ::  log output
      ==  ==                                            ::
    +$  sign
      $%  [%khan $>(%command gift)]
      ==
    ::
    +$  khan-state
      $:  %0
          unix-duct=duct
      ==
    ::
    --
::
=>
~%  %khan  ..part  ~
|%
++  per-event
  =|  moves=(list move)
  |=  [[now=@da =duct] state=khan-state]
  ::
  |%
  ::  %entry-points
  ::
  ::  +born: urbit restarted; refresh?
  ::
  ++  born  !!
  ::  +crud: handle failure of previous arvo event
  ::
  ++  crud
    |=  [tag=@tas error=tang]
    ^+  [moves state]
    !!
  ::  +avow: give back
  ::
  ++  avow
    |=  syn=sign-arvo
    =<  [moves state]
    event-core(moves [duct %give %avow syn]~)
  ::  +fyrd: commands
  ::
  ++  fyrd  %command
  ::  +trim: in response to memory pressue
  ::
  ++  trim  [moves state]
  ::  +vega: learn of a kernel upgrade
  ::
  ++  vega  [moves state]
  ::  %utilities
  ::
  ::+|
  ::
  ++  event-core  .
--
::
=|  khan-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  khan-gate  .
^?
|%
::  +call: handle a +task:khan request
::
++  call
  ~%  %khan-call  ..part  ~
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _khan-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  =/  event-core  (per-event [now hen] state)
  ::
  =^  moves  state
    ::
    ::  handle error notifications
    ::
    ?^  dud
      (crud:event-core -.task tang.u.dud)
    ::
    ?-  -.task
      %born  born:event-core
      %trim  trim:event-core
      %vega  vega:event-core
      %fyrd  fyrd:event-core
    ==
  [moves khan-gate]
::  +load: migrate an old state to a new khan version
::
++  load
  |=  old=khan-state
  ^+  khan-gate
  khan-gate(state old)
::  +scry: view state
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem

  ?:  &(=(ren %$) =(tyl /whey))
    =/  maz=(list mass)
      :~  timers+&+state
      ==
    ``mass+!>(maz)
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ~
  ?.  ?=(%x ren)  ~
  ?+  tyl  [~ ~]
      [%debug %state ~]
    [~ ~]
  ==
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _khan-gate]
  ?^  dud
    ~|(%khan-take-dud (mean tang.u.dud))
  ::
  ?>  ?=([%drip @ ~] tea)
  =/  event-core  (per-event [now hen] state)
  =^  moves  state
    (take-drip:event-core (slav %ud i.t.tea) error.hin)
  [moves khan-gate]
--
