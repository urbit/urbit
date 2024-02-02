::  %faye
!:
!?  164
::
=,  faye
|=  our=ship
=>  |%
    :: +$  move  [p=duct q=(wite note gift)]
    :: +$  move  [=duct move=(wind note-arvo gift-arvo)]
    +$  note  ~                                         ::  out request $->
    +$  sign  ~
    ::
    ::  $state: overall faye state
    ::
    ::    system-duct: outbound moves to other vanes
    ::    outstanding: outstanding request queue
    ::    cave: running scamps
    +$  faye-state
      $:  %0
          :: system-duct=duct
          :: outstanding=(map [wire duct] (qeu remote-request))
          cave=(map path lair)
      ==
    ::
    ::  $lair: scamp runner state
    ::
    ::    control-duct: scamp-level control duct
    ::    scamp: scamp core
    ::    live: addressable (running) or suspended
    +$  lair
      $:  control-duct=duct
          =scamp
          live=?
      ==
    --
::
~%  %faye  ..part  ~
::
=|  faye-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  faye-gate  .
^?
|%
::  +call: handle a +task:faye request
::
++  call 
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _faye-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ?+   -.task  [~ faye-gate]
      %load
    =/  =lair  :+  *duct
                   scamp.task
                   %.y
    `faye-gate(cave (~(put by cave) path.task lair))
    ::
      %togl
    =/  have=(unit lair)  (~(get by cave) path.task)
    ?~  have  `faye-gate
    =/  =lair  :+  control-duct.u.have
                   scamp.u.have
                   !live.u.have
    `faye-gate(cave (~(put by cave) path.task lair))
    ::
      %nuke
    `faye-gate(cave (~(del by cave) path.task))
    ::
      %write
    |^
    =/  have=(unit lair)  (~(get by cave) path.task)
    ?~  have  `faye-gate
    =/  =scamp  scamp.u.have
    ~&  >  |.((on-write:~(. scamp [our eny now]) cage.task))
    =/  temp=_^?(|.(*[(list move) form:^scamp]))
    :: |.((~(on-write scamp [our eny now]) cage.task))
    |.((on-write:~(. scamp [our eny now]) cage.task))
    =/  result  (burro temp)
    =/  maybe-tang=(unit tang)
      ?:  ?=(%& -.result)
        ~
      `p.result
    ?:  ?=(%| -.result)
      `faye-gate
    =/  =lair  :+  control-duct.u.have
                   +>.result
                   live.u.have
    `faye-gate(cave (~(put by cave) path.task lair))
    ::  a little +mule
    ++  burro
      |=  run=_^?(|.(*[(list move) form:scamp]))
      ^-  (each [(list move) form:scamp] tang)
      =/  res  (mock [run %9 2 %0 1] (look rof ~ [%faye path.task]))
      ?-  -.res
        %0  [%& !<([(list move) form:scamp] [-:!>(*[(list move) form:scamp]) p.res])]
        %1  [%| (smyt ;;(path p.res)) ~]
        %2  [%| p.res]
      ==
    --
  ==
::  +load: migrate an old state to a new faye version
::
++  load
  |=  old=faye-state
  ^+  faye-gate
  faye-gate(state old)
::  +scry: view state
::
::  %u  existence of scamp
::  %v  state of scamp (existence + live?)
::  %x  scry out data at path in scamp
::
::  suspended = accepts reads but not writes
::  delete me if you don't want reads
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  ~&  >>>  cave
  |^
  ::  only respond for the local identity, current timestamp
  ::
  ?.  ?&  =(our p.bem)
          :: =(%$ q.bem) 
          =([%da now] r.bem)
      ==
    ~
  ?+  car  ~
    %u  read-u
    %v  read-v
    %x  read-x 
  ==
  ::  +read-u: check existence of a scamp
  ::
  ++  read-u
    ^-  (unit (unit cage))
    =/  have=?  (~(has by cave) s.bem)
    ``[%noun !>(have)]
  ::  +read-v: check state of a scamp
  ::
  ++  read-v  
    ^-  (unit (unit cage))
    =/  have=(unit lair)  (~(get by cave) pov)
    ?~  have  ~
    ``[%noun !>(live.u.have)]
  ::  +read-x: scry out data at path in scamp
  ::  .^(* %fx /===/path/to/scamp//path/to/value/mark)
  ::
  ++  read-x
    ^-  (unit (unit cage))
    =/  offset  (find ~[%$] s.bem)
    ?~  offset  ~
    =/  pts=path  (scag u.offset s.bem)
    =/  ptv=path  (slag +(u.offset) s.bem)
    ::  chop off the tail
    =/  =mark     (rear ptv)
    =.  ptv       (scag (dec (lent ptv)) ptv)
    =/  lair=(unit lair)  (~(get by cave) pts)
    ?~  lair  ~
    (~(on-read scamp.u.lair [our eny now]) ptv)
  :: 
  --    
::
++  stay  state
::
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _faye-gate]
  ?^  dud
    ~|(%faye-take-dud (mean tang.u.dud))
  ::
  [~ faye-gate]
--
