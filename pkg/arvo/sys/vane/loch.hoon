::  %loch
!:
!?  164
::
=,  loch
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note  ~                                        ::  out request $->
      ::$%  $:  %g                                        ::    to %gall
              ::$>(%deal task:gall)                       ::  full transmission
          ::==                                            ::
      ::==                                            ::
      ::
    +$  sign  ~
      ::$%  [%loch $>(%red gift)]                        :: read
      ::    [%loch $>(%read gift)]
      ::==
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
    +$  device  [name=@tas dat=@ tus=@] 
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
  ~&  >  ["loch call task:" task]
  ~&  >>  ["loch hen:" hen]
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
    :-  ~[[unix-duct.state %give [%read dev.task wut.task cmd.task cnt.task]]]
      %_  loch-gate
        pathing  (~(put by pathing) dev.task hen)
      ==
    ::
      %rite     :: When you read you need to save the command and the wire to return the results
    :-  ~[[unix-duct.state %give [%rite dev.task wut.task cmd.task dat.task cnt.task]]]
      %_  loch-gate
        pathing  (~(put by pathing) dev.task hen)
      ==
    ::
      %devs  
    =/  dev  +.task 
    :-  ~
      %_  loch-gate
        devices  (~(put by devices) name.dev [name.dev ~ stat.dev])
      ==
    ::
      %seen
    =/  duct  (~(get by pathing) dev.task)
    =/  device-save  [dev.task dat.task tus.task]
    ~&  >>  ["devicesave" device-save]
    :-  ~[[+.duct %give %seen dev.task dat.task tus.task]]
      %_  loch-gate
        devices  (~(put by devices) dev.task device-save)
      ==
    ::
      %rote
    =/  duct  (~(get by pathing) dev.task)
    =/  device-save  [dev.task 0 tus.task]
    :-  ~[[+.duct %give %rote dev.task tus.task]]
      %_  loch-gate
        devices  (~(put by devices) dev.task device-save)
      ==
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
::  %a  scry out a list of devices
::  %d  get a device's  dat and tus
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ~&  >>  "loch scry:" 
  ~&  >>  ["lyc" lyc]
  ~&  >>  ["car" car]
  ~&  >>  ["bem" bem]
  |^
  ::  only respond for the local identity, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
      ==
    ~
  ?+  car  ~
    %a  (read-a lyc bem)
    %d  (read-d lyc bem)
  ==
  ::  +read-a: scry our list of devices
  ::
  ++  read-a
    |=  [lyc=gang bem=beam]
    ^-  (unit (unit cage))
    =/  devs  ~(tap in ~(key by devices))
    ``[%noun !>(devs)]
  ::  +read d: get devices dat and tus
  ::
  ++  read-d    
    |=  [lyc=gang bem=beam]
    ^-  (unit (unit cage))
    ~&  >>>  ["lyc" lyc]
    ~&  >>>  ["bem" bem]
    =*  tyl  -.s.bem
    =/  devs  (~(got by devices) tyl)
    ``[%noun !>(devs)]
  --    
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
