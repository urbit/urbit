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
  ~&  >  ["loch call hen:" hen]
  ::~&  >>  ["loch call dud:" dud]
  ~&  >>>  ["loch call wrapped-task:" wrapped-task]
  ::
  =/  =task  ((harden task) wrapped-task)
  ~&  >>  ["loch call task:" task]
  ::?^  dud
    ::~|(%loch-call-dud (mean tang.u.dud))
  ?+    -.task  
        ~&  >>  ["loch task:" -.task]
        [~ loch-gate]
      %born     [~ loch-gate]
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
