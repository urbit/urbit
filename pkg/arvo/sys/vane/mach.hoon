::  %mach
!:
!?  164
::
=,  mach
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note  ~                                         ::  out request $->
    +$  sign  ~                                         ::  in result $<-
    +$  mach-state
      $:  %0
          next=@ud
          unix-duct=_`duct`[//mach ~]
          names=(map name [=duct id=@ud])
          ids=(map @ud name)
      ==
    --
::
~%  %mach  ..part  ~
::
=|  mach-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  mach-gate  .
^?
|%
::  +call: handle a +task:mach request
::
++  call
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _mach-gate]
  =/  =task  ((harden task) wrapped-task)
  ?-    -.task
      %born
    :-  %+  turn
          ~(tap by names)
        |=  [=name =duct id=@ud]
        ^-  move
        [duct %give %closed name %local %reborn]
    %=  mach-gate
      unix-duct  hen
      names      ~
      ids        ~
    ==
  ::
      %trim
    ::  tell each owner, and have the driver close its socket
    ::
    :-  %-  zing
        %+  turn
          ~(tap by names)
        |=  [=name =duct id=@ud]
        ^-  (list move)
        :~  [duct %give %closed name %local %trimmed]
            [unix-duct %give %close id]
        ==
    %=  mach-gate
      names  ~
      ids    ~
    ==
  ::
      %vega
    [~ mach-gate]
  ::
      %connect
    %-  (slog leaf+"mach: %connect {<name.task>} {<addr.task>}" ~)
    ?:  (~(has by names) name.task)
      ::  existing connection stays open
      %-  (slog leaf+"mach: {<name.task>} taken" ~)
      [[hen %give %closed name.task %connect %taken]~ mach-gate]
    =.  names  (~(put by names) name.task [hen next])
    =.  ids    (~(put by ids) next name.task)
    :-  [unix-duct %give %connect next addr.task]~
    mach-gate(next +(next))
  ::
      ?(%send %close)
    =/  nam=name
      ?-  -.task
        %send   name.task
        %close  name.task
      ==
    =/  con  (~(get by names) nam)
    ?~  con
      %-  (slog leaf+"mach: {<-.task>} on {<nam>}, no connection" ~)
      [~ mach-gate]
    ?.  =(hen duct.u.con)
      %-  (slog leaf+"mach: {<-.task>} on {<nam>}, not from owner" ~)
      [~ mach-gate]
    :_  mach-gate
    ?-  -.task
      %send   [unix-duct %give %send id.u.con octs.task]~
      %close  [unix-duct %give %close id.u.con]~
    ==
  ::
      ?(%closed %opened %received)
    ?.  =(hen unix-duct)
      ::  XX crash here? would reject bad events from log
      %-  (slog leaf+"mach: {<-.task>} on bad duct {<hen>}" ~)
      [~ mach-gate]
    =/  id=@ud
      ?-  -.task
        %opened    id.task
        %received  id.task
        %closed    id.task
      ==
    =/  nam=(unit name)  (~(get by ids) id)
    ?~  nam
      %-  (slog leaf+"mach: {<-.task>} on old connection, ignoring" ~)
      [~ mach-gate]
    =*  owner  duct:(~(got by names) u.nam)
    ?-   -.task
        %opened
      [[owner %give %connected u.nam]~ mach-gate]
    ::
        %received
      [[owner %give %received u.nam octs.task]~ mach-gate]
    ::
        %closed
      :-  [owner %give %closed u.nam p.task]~
      mach-gate(names (~(del by names) u.nam), ids (~(del by ids) id))
    ==
  ==
::  +load: migrate an old state to a new mach version
::
++  load
  |=  old=mach-state
  ^+  mach-gate
  mach-gate(state old)
::  +scry: view state
::
::    %a  /connections     list open connections by name
::    %d  /owner/<name>    get the duct that opened a connection
::    %u  /<name>          does a connection exist
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  |^
  ::  only respond on our, now, root
  ::
  ?.  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%da now] r.bem)
          =([~ ~] lyc)
      ==
    ~
  ?+  car  ~
    %a  read-a
    %d  read-d
    %u  read-u
  ==
  ::  +read-a: list open connections
  ::
  ++  read-a
    ^-  (unit (unit cage))
    ?.  ?=([%connections ~] s.bem)
      [~ ~]
    ``[%noun !>(`(list name)`~(tap in ~(key by names)))]
  ::  +read-d: get the duct that opened a connection
  ::
  ++  read-d
    ^-  (unit (unit cage))
    ?.  ?=([%owner *] s.bem)
      [~ ~]
    =/  con  (~(get by names) t.s.bem)
    ?~  con
      [~ ~]
    ``[%noun !>(duct.u.con)]
  ::  +read-u: does a connection exist
  ::
  ++  read-u
    ^-  (unit (unit cage))
    ``[%noun !>((~(has by names) s.bem))]
  --
::
++  stay  state
::  +take: handle a response from another vane
::
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _mach-gate]
  [~ mach-gate]
--
