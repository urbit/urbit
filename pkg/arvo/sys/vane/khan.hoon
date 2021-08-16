::  %khan
!:
!?  164
::
=,  khan
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                          ::  out request $->
      $%
        $:  %j                                        :: to %jael
              $>(%step task:jael)
          ==
        $:  %k                                        ::   to self
                $>  $?
                        %fyrd
                ==                                      ::
             task                                       ::
          ==                                            ::
          $:  %d                                        ::    to %dill
              $>(%flog task:dill)                       ::  log output
      ==  ==
      ::
    +$  sign
      $%  [%khan $>(%response gift)]
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
  |=  [[now=@da =duct rof=roof] state=khan-state]
  ::
  |%
  ::  %entry-points
  ::
  ::  +crud: handle failure of previous arvo event
  ::
  ++  crud
    |=  [tag=@tas error=tang]
    ^+  [moves state]
    [[duct %slip %d %flog %crud tag error]~ state]
  ::
  ::  +born: alive, set up config
  ++  born
    ^+  [moves state]
    =<  [moves state]
    event-core(moves [duct %give %set-config & ~]~)   :: live event?
  ::  +avow: give back gifts
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
    ^+  event-core
    ~&  >  fyrd+com
    ?-  -.com
      %mas  ~&  todo+com  event-core                    :: |mass
      %cod
        =/  cov
          ?.    +.com
            =/  sey=(unit (unit cage))
              ~&  sey+"here"
              (rof ~ %j [our %code da+now] /(scot %p our))
            =/  res=(unit @p)
              ?~  sey  ~
              ?~  u.sey  ~
              `!<(@p q.u.u.sey)
            ~&  res+res
            [duct %give [%response %socket-done res]]~ :: give code

          [duct %pass / %j %step ~]~
        event-core(moves cov)
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
  ~&  task+task
  =/  event-core  (per-event [now hen rof] state)
  ::
  =^  moves  state
    ::
    ::  handle error notifications
    ::
    ?^  dud
      (crud:event-core -.task tang.u.dud)
    ?-  -.task
      %trim  trim:event-core
      %vega  vega:event-core                            :: vega
      %born  born:event-core
      %fyrd  (fyrd:event-core p.task)                   :: fyrd
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
  ~&  hin+hin
  ?^  dud
    ~|(%khan-take-dud (mean tang.u.dud))
  ::
  [~ khan-gate]
--
