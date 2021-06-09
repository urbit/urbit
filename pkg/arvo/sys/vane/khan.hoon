::  %khan
!:
!?  164
::
=,  khan
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $%  $:  %k                                        ::   to self
                $>  $?
                        %fyrd
                        %born
                ==                                      ::
             task                                       ::
          ==                                            ::
          $:  %d                                        ::    to %dill
              $>(%flog task:dill)                       ::  log output
      ==  ==
      ::
    +$  sign
      $%  [%khan $>(%avow gift)]
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
  ++  born  [~ state]
  ::  +crud: handle failure of previous arvo event
  ::
  ++  crud
    |=  [tag=@tas error=tang]
    ^+  [moves state]
    [[duct %slip %d %flog %crud tag error]~ state]
  ::  +avow: give back
  ::
  ++  avow
    |=  syn=sign-arvo
    =<  [moves state]
    event-core(moves [duct %give %avow syn]~)
  ::  +fyrd: commands
  ::
  ++  fyrd
    |=  com=^fyrd
    =<  [moves state]
    ~&  >  fyrd+com
    ~!  -.com
    ?-  -.com
      %mas  ~&  todo+com  event-core                    :: |mass
      %cod  event-core(moves [[%pass / %arvo %j %step ~] moves]) :: code reset
    ==
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
    ::
    ?^  dud
      (crud:event-core -.task tang.u.dud)
    ::
    ~!  -.task
    ?-  -.task
      %born  born:event-core
      %trim  trim:event-core
      %vega  vega:event-core                            :: vega
      %fyrd  (fyrd:event-core p.task)
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
  ^-  [(list move) _khan-gate]
  ?^  dud
    ~|(%khan-take-dud (mean tang.u.dud))
  ::
  ?>  ?=([%drip @ ~] tea)
  [~ khan-gate]
--
