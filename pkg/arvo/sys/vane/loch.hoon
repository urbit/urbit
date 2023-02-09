::  %loch
!:
!?  164
::
=,  loch
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                          ::  out request $->
      $%
        $:  %j                                        :: to %jael
              $>(%step task:jael)
          ==
         $:  %d                                        ::    to %dill
              $>(%flog task:dill)                       ::  log output
      ==  ==
      ::
    +$  sign
      $%  [%loch $>(%read gift)]                        :: read
      ==
    ::
    +$  loch-state
      $:  %0
          unix-duct=duct
      ==
    ::
    --
::
=>
~%  %loch  ..part  ~
|%
++  per-event
  =|  moves=(list move)
  |=  [[now=@da =duct] state=loch-state]
  ::
  |%
  ++  this  .
  ::  %entry-points
  ::
  ::  +crud: handle failure of previous arvo event
  ::
  ++  crud
    |=  [tag=@tas error=tang]
    ^+  [moves state]
    [[duct %slip %d %flog %crud tag error]~ state]
  ::  +read: give back
  ::
  ++  read  [moves state]
  ::  +fyrd: commands
  ::
  ::++  fyrd
    ::|=  com=^fyrd
    ::^+  [moves state]
    ::=<  [moves state]
    ::~&  >  fyrd+com
    ::~!  -.com
    ::?-  -.com
      ::%mas  ~&  todo+com  event-core                    :: |mass
      ::%cod
        ::::=/  cov
          ::[duct %pass %j %step ~]~
        ::::~!  move+cov  event-core(moves cov)
    ::==
  ::  +born: in response to memory pressue
  ::
  ++  born  [moves state]
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
--
::
=|  loch-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  loch-gate  .
^?
|%
::  +call: handle a +task:loch request
::
++  call
  ~%  %loch-call  ..part  ~
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _loch-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  =/  event-core  (per-event [now hen] state)
  ::
  =^  moves  state
    ::
    ::  handle error notifications
    ::
    ::
    ?^  dud
      (crud:event-core -.task tang.u.dud)
    ::
    ~!  -.task
    ?-  -.task  
      %born  born:event-core
      %trim  trim:event-core
      %vega  vega:event-core                            :: vega
      ::%read  read:event-core
    ==
  [moves loch-gate]
::  +load: migrate an old state to a new loch version
::
++  load
  |=  old=loch-state
  ^+  loch-gate
  loch-gate(state old)
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
      :~  state+&+state
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
    ``state+!>([~ state])
  ==
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _loch-gate]
  ?^  dud
    ~|(%loch-take-dud (mean tang.u.dud))
  ::
  [~ loch-gate]
--
