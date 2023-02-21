::  %loch
!:
!?  164
::
=,  loch
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                          ::  out request $->
      $%  $:  %g                                        ::    to %gall
              $>(%deal task:gall)                       ::  full transmission
          ==                                            ::
      ==                                            ::
      ::
    +$  sign
      $%  [%loch $>(%red gift)]                        :: read
          [%loch $>(%read gift)]
      ==
    ::
    +$  loch-state
      $:  %0
          unix-duct=duct
          devices=(map @tas device)
          commands=(list cmd)
          pathing=(map dev duct)
      ==
    ::
    +$  cmd  [cmd=@tas =wut =dev =duct]
    ::
    +$  device  [name=@tas status=@ reg=(unit @tas)] 
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
  ++  crud  [moves state]
  ::  +read: give back
  ::
  ++  read  [moves state]
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
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _loch-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ~&  >  ["1 loch call task:" task]
  ~&  >>  ["2 loch hen:" hen]
  ::~&  >>  :*  "loch" 
            ::["unix duct" unix-duct:loch-gate]
            ::["devices" devices:loch-gate]
            ::["commands" commands:loch-gate]
            ::["pathing" pathing:loch-gate]
          ::==
  ::?^  dud
    ::~|(%loch-call-dud (mean tang.u.dud))
  ?+   -.task  [~ loch-gate]
      %born     :: When born you need to wipe your current state
    :-  ~  
      loch-gate(unix-duct hen, commands [~], devices ~, pathing ~) 
    ::
      %read     :: When you read you need to save the command and the wire to return the results
    =/  =param  param:task 
    ::~&  >>  ['param' param]
    :-  ~[[unix-duct.state %give [%read param]]]
      %_  loch-gate
        pathing  (~(put by pathing) dev.param hen)
      ==
    ::
      %devs  
    =/  dev  +.task 
    :-  ~
      %_  loch-gate
        devices  (~(put by devices) name.dev [name.dev stat.dev ~])
      ==
    ::
      %turn
    =/  duct  (~(get by pathing) dev.task)
    :-  ~[[+.duct %give %red dev.task dat.task]]
      loch-gate
  ==
::  +load: migrate an old state to a new loch version
::
++  load
  |=  old=loch-state
  ^+  loch-gate
  ~&  >>  "loch load:"
  loch-gate(state old)
::  +scry: view state
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ~&  >>  "loch scry:" 
  ~
::
++  stay  
  ~&  >>  "loch stay:"
   state 
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _loch-gate]
  ?^  dud
    ~|(%loch-take-dud (mean tang.u.dud))
  ::
  ~&  >>  "loch take:"
  [~ loch-gate]
--
