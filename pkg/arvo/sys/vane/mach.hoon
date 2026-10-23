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
    ::
    ::  XX reset to %0 before release; needs fresh dev ships
    +$  mach-state
      $:  %2
          ::  XX  randomize next on init?
          ::      per https://datatracker.ietf.org/doc/html/draft-ietf-tcpm-tcp-security#section-3.1
          ::      could also randomize on %born
          next=@ud
          unix-duct=_`duct`[//mach ~]
          owners=(map @ud duct)
          ducts=(map duct @ud)
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
::
::  +call: handle a +task:mach request
++  call
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _mach-gate]
  =/  =task  ((harden task) wrapped-task)
  ?-    -.task
      %born
    :-  %+  turn
          ~(tap by owners)
        |=  [* =duct]
        ^-  move
        [duct %give %closed %local %reborn]
    %=  mach-gate
      unix-duct  hen
      ducts      ~
      owners     ~
    ==
  ::
      %trim
    ::
    ::  tell each owner, and have the driver close its socket
    :-  %-  zing
        %+  turn
          ~(tap by owners)
        |=  [id=@ud =duct]
        ^-  (list move)
        :~  [duct %give %closed %local %trimmed]
            [unix-duct %give %close id]
        ==
    %=  mach-gate
      ducts   ~
      owners  ~
    ==
  ::
      %vega
    [~ mach-gate]
  ::
      %connect
    %-  (slog leaf+"mach: %connect {<secure.task>} {<fief.task>}" ~)
    ::
    ::  an agent connects on a duct only once its last connection there
    ::  has closed, so an id still held is stale: the event carrying
    ::  its %closed crashed. have the driver drop it, and forget it, so
    ::  a later %closed for it is ignored
    =/  old=(unit @ud)  (~(get by ducts) hen)
    =/  drop=(list move)
      ?~  old  ~
      %-  (slog leaf+"mach: %connect replaces connection {<u.old>}" ~)
      [unix-duct %give %close u.old]~
    =?  owners  ?=(^ old)  (~(del by owners) u.old)
    =.  ducts   (~(del by ducts) hen)
    ::
    ::  certificates name hosts, so tls needs a domain
    ?:  &(secure.task !?=(%turf -.fief.task))
      %-  (slog leaf+"mach: tls needs a %turf" ~)
      [(snoc drop [hen %give %closed %tls %invalid]) mach-gate]
    =*  id      next
    =.  next    +(next)
    =.  ducts   (~(put by ducts) [hen id])
    =.  owners  (~(put by owners) [id hen])
    :_  mach-gate
    (snoc drop [unix-duct %give %connect id secure.task fief.task])
  ::
      %send
    %-  (slog leaf+"mach: %send {<p.octs.task>} bytes" ~)
    ?.  (~(has by ducts) hen)
      ::  XX print duct
      %-  (slog leaf+"mach: no connection" ~)
      [~ mach-gate]
    =*  id  (~(got by ducts) hen)
    :_  mach-gate
    :~  [unix-duct %give %send id octs.task]
    ==
  ::
      %close
    %-  (slog leaf+"mach: %close" ~)
    ?.  (~(has by ducts) hen)
      ::  XX print duct
      %-  (slog leaf+"mach: no connection" ~)
      [~ mach-gate]
    =*  id  (~(got by ducts) hen)
    :_  mach-gate
    :~  [unix-duct %give %close id]
    ==
  ::
      ?(%closed %opened %received)
    ?.  =(hen unix-duct)
      ::  XX crash here? would reject bad events from log
      %-  (slog leaf+"mach: {<-.task>} on bad duct {<hen>}" ~)
      [~ mach-gate]
    =*  id  id.task
    =/  owner=(unit duct)  (~(get by owners) id)
    ?~  owner
      %-  (slog leaf+"mach: {<-.task>} on old connection, ignoring" ~)
      [~ mach-gate]
    ?-   -.task
        %opened
      :_  mach-gate
      :~  [u.owner %give %connected p.task]
      ==
    ::
        %received
      :_  mach-gate
      :~  [u.owner %give %received octs.task]
      ==
    ::
        %closed
      =.  ducts   (~(del by ducts) u.owner)
      =.  owners  (~(del by owners) id)
      :_  mach-gate
      :~  [u.owner %give %closed p.task]
      ==
    ==
  ==
::
::  +load: migrate an old state to a new mach version
++  load
  |=  old=mach-state
  ^+  mach-gate
  mach-gate(state old)
::
::  +scry: view state
::
::    %a  /connections   list open connections
::    %d  /owner/<id>    get the duct that opened a connection
::    %u  /<id>          does a connection exist
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  |^
  ::
  ::  only respond on our, now, root
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
  ::
  ::  +read-a: list open connections
  ++  read-a
    ^-  (unit (unit cage))
    ?.  ?=([%connections ~] s.bem)
      [~ ~]
    ``[%noun !>(`(list @ud)`~(tap in ~(key by owners)))]
  ::
  ::  +read-d: get the duct that opened a connection
  ++  read-d
    ^-  (unit (unit cage))
    ?.  ?=([%owner @ ~] s.bem)
      [~ ~]
    =/  id=(unit @ud)  (slaw %ud i.t.s.bem)
    ?~  id
      [~ ~]
    =/  hen=(unit duct)  (~(get by owners) u.id)
    ?~  hen
      [~ ~]
    ``[%noun !>(u.hen)]
  ::
  ::  +read-u: does a connection exist
  ++  read-u
    ^-  (unit (unit cage))
    ?.  ?=([@ ~] s.bem)
      [~ ~]
    =/  id=(unit @ud)  (slaw %ud i.s.bem)
    ?~  id
      [~ ~]
    ``[%noun !>((~(has by owners) u.id))]
  --
::
++  stay  state
::
::  +take: handle a response from another vane
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _mach-gate]
  [~ mach-gate]
--
