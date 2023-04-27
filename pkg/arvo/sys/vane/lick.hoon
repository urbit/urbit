::  %lick
!:
!?  164
::
=,  lick
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note  ~                                         ::  out request $->
      ::$%  $:  %g                                        ::    to %gall
      ::        $>(%deal task:gall)                       ::  full transmission
      ::    ==                                            ::
      ::==                                            ::
      ::
    +$  sign  ~
    ::
    +$  lick-state
      $:  %0
          unix-duct=duct
          agents=(map name duct)
      ==
    ::
    +$  name   @tas
    --
::
~%  %lick  ..part  ~
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
  |=  =name
  ^-  move
  [unix-duct.state %give [%spin name]]
::  +disconnect: Create Move to send a disconnect soak to am agent
::
++  disconnect
  |=  =name
  ^-  move
  =/  =duct  (~(get by agents) name)
  [+.duct %give [%soak name %disconnect ~]]
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
      %born     :: need to register devices with vere and send disconnect soak
    :-  %+  weld 
        (turn ~(tap in ~(key by agents.state)) register) 
        (turn ~(tap in ~(key by agents.state)) disconnect)
    lick-gate(unix-duct hen) 
    ::
      %spin     :: A gall agent wants to spin a communication line
    :-  ~[(register name.task)]
    lick-gate(agents (~(put by agents) name.task hen))
    ::
      %shut     :: shut down a communication line
    :-  ~[[unix-duct.state %give [%shut name.task]]]
    lick-gate(agents (~(del by agents) name.task))
    ::
      %soak     :: push a soak to the ipc's owner
    =/  ner=duct  (~(get by agents.state) name.task)
    :_  lick-gate
    ~[[+.ner %give [%soak name.task mark.task noun.task]]]
    ::
      %spit     :: push a spit to ipc
    :_  lick-gate
    ~[[unix-duct.state %give [%spit name.task mark.task noun.task]]]
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
