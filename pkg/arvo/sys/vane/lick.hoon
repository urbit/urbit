::  %lick
!:
!?  164
::
=,  lick
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
      ::$%  [%lick $>(%red gift)]                        :: read
      ::    [%lick $>(%read gift)]
      ::==
    ::
    +$  lick-state
      $:  %0
          unix-duct=duct
          agents=(map name.agent agent)
      ==
    ::
    +$  agent  [=name =ver] 
    +$  name   @tas
    +$  ver    @tas
    --
::
=>
~%  %lick  ..part  ~
|%
++  per-event
  =|  moves=(list move)
  |=  [[now=@da =duct] state=lick-state]
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
=|  lick-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  lick-gate  .
^?
|%
::  +register: Create a move to register an agent with vere
::
++  register
  |=  =agent
  ^-  move
  [unix-duct.state %give [%book name.agent ver.agent]]
::  +call: handle a +task:lick request
::
++  call 
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _lick-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ?+   -.task  [~ lick-gate]
      %born     :: When born you need to register all devices
    =/  m=(list move)  (turn ~(val by agents.state) register)
    ~&  >>>  m
    :-  (turn ~(val by agents.state) register)
      lick-gate(unix-duct hen) 
    ::
      %book     :: A gall agent wants to book a communication line
    =/  =agent  [nam.task ver.task]
    :-  ~[(register agent)]
      lick-gate(agents (~(put by agents) nam.task agent))
      
  ==
::  +load: migrate an old state to a new lick version
::
++  load
  |=  old=lick-state
  ^+  lick-gate
  ~&  >>  "lick load:"
  lick-gate(state old)
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
  ~&  >>  "lick scry:" 
  ~&  >>  ["lyc" lyc]
  ~&  >>  ["car" car]
  ~&  >>  ["bem" bem]
  ~
::
++  stay  
  ~&  >>  "lick stay:"
   state 
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _lick-gate]
  ?^  dud
    ~|(%lick-take-dud (mean tang.u.dud))
  ::
  ~&  >>  "lick take:"
  [~ lick-gate]
--
