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
          ids=(map @ud name)
          names=(map name [=duct id=@ud])
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
      %vega
    [~ mach-gate]
  ::
      %born
    :-  %+  turn
          ~(tap by names)
        |=  [=name =duct id=@ud]
        ^-  move
        [duct %give %closed name %local %reborn]
    %=  mach-gate
      ids        ~
      names      ~
      unix-duct  hen
    ==
  ::
      %trim
    ::  tell each owner, and have the driver close its socket
    ::
    :_  %=  mach-gate
            ids    ~
            names  ~
        ==
    %-  zing
    %+  turn
      ~(tap by names)
    |=  [=name =duct id=@ud]
    ^-  (list move)
    :~  [unix-duct %give %close id]
        [duct %give %closed name %local %trimmed]
    ==
  ::
      %connect
    %-  (slog leaf+"mach: %connect {<name.task>} {<addr.task>}" ~)
    ?:  (~(has by names) name.task)
      ::  existing connection stays open
      %-  (slog leaf+"mach: bad %connect on {<name.task>}: already connected" ~)
      ::  prospective connection is closed
      [[hen %give %closed name.task %connect %taken]~ mach-gate]
    =.  ids    (~(put by ids) next name.task)
    =.  names  (~(put by names) name.task [hen next])
    :_  mach-gate(next +(next))
    [unix-duct %give %connect next addr.task]~
  ::
      ?(%send %close)
    =/  nym=name
      ?-  -.task
        %send   name.task
        %close  name.task
      ==
    =/  con  (~(get by names) nym)
    ?~  con
      %-  (slog leaf+"mach: bad {<-.task>} on {<nym>}: no connection" ~)
      [~ mach-gate]
    ?.  =(hen duct.u.con)
      %-  (slog leaf+"mach: bad {<-.task>} on {<nym>}: not from owner" ~)
      [~ mach-gate]
    :_  mach-gate
    ?-  -.task
      %close  [unix-duct %give %close id.u.con]~
      %send   [unix-duct %give %send id.u.con octs.task]~
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
    =/  nym=(unit name)  (~(get by ids) id)
    ?~  nym
      %-  (slog leaf+"mach: {<-.task>} on old connection, ignoring" ~)
      [~ mach-gate]
    =/  owner=duct  duct:(~(got by names) u.nym)
    ?-   -.task
        %opened
      [[owner %give %connected u.nym]~ mach-gate]
    ::
        %received
      [[owner %give %received u.nym octs.task]~ mach-gate]
    ::
        %closed
      :-  [owner %give %closed u.nym p.task]~
      %=  mach-gate
        ids    (~(del by ids) id)
        names  (~(del by names) u.nym)
      ==
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
::    %d  /owner/[name]    get the duct that opened a connection
::    %u  /[name]          does a connection exist
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  |^
  ::  only respond to our, now, root
  ::
  ?.  ?&  =(%$ q.bem)
          =([~ ~] lyc)
          =(our p.bem)
          =([%da now] r.bem)
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
