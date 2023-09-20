::  %lick
!:
!?  164
::
=,  lick
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note  ~                                         ::  out request $->
    +$  sign  ~
    ::
    +$  lick-state
      $:  %0
          unix-duct=_`duct`[//lick ~]
          owners=(map name duct)
      ==
    ::
    +$  name   path
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
  =/  =duct  (~(get by owners) name)
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
          (turn ~(tap in ~(key by owners.state)) register) 
        (turn ~(tap in ~(key by owners.state)) disconnect)
    lick-gate(unix-duct hen) 
    ::
      %spin     :: A gall agent wants to spin a communication line
    :-  ~[(register name.task)]
    lick-gate(owners (~(put by owners) name.task hen))
    ::
      %shut     :: shut down a communication line
    :-  [unix-duct.state %give [%shut name.task]]~
    lick-gate(owners (~(del by owners) name.task))
    ::
      %soak     :: push a soak to the ipc's owner
    =/  ner=duct  (~(get by owners.state) name.task)
    :_  lick-gate
    [+.ner %give [%soak name.task mark.task noun.task]]~
    ::
      %spit     :: push a spit to ipc
    :_  lick-gate
    [unix-duct.state %give [%spit name.task mark.task noun.task]]~
  ==
::  +load: migrate an old state to a new lick version
::
++  load
  |=  old=lick-state
  ^+  lick-gate
  lick-gate(state old)
::  +scry: view state
::
::  %a  scry out a list of all ipc ports
::  %d  get the owner of an ipc port
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  |^
  ::  only respond for the local identity, current timestamp
  ::
  ?.  ?&  =(our p.bem)
          =(%$ q.bem) 
          =([%da now] r.bem)
      ==
    ~
  ?+  car  ~
    %a  read-a 
    %d  read-d
    %u  read-u
  ==
  ::  +read-a: scry our list of ports
  ::
  ++  read-a
    ^-  (unit (unit cage))
    =/  ports=(list name)  ~(tap in ~(key by owners))
    ``[%noun !>(ports)]
  ::  +read d: get ports owner 
  ::
  ++  read-d    
    ^-  (unit (unit cage))
    =/  devs=(unit duct)  (~(get by owners) s.bem)
    ?~  devs  [~ ~]
    ``[%noun !>(devs)]
  ::  +read u: does a port exist
  ::
  ++  read-u
    ^-  (unit (unit cage))
    ``[%noun !>((~(has by owners) s.bem))]
  :: 
  --    
::
++  stay  
   state 
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _lick-gate]
  ?^  dud
    ~|(%lick-take-dud (mean tang.u.dud))
  ::
  [~ lick-gate]
--
