::  %khan, thread runner
!:
!?  164
::
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift:khan)]
    +$  note                                            ::  out request $->
      $~  [%g %deal *sock *term *deal:gall]
      $%  $:  %g                                        ::  to %gall
              $>(%deal task:gall)                       ::  full transmission
      ==  ==                                            ::
    +$  sign                                            ::  in response $<-
      $%  $:  %gall
              $>(%unto gift:gall)
          ==
          $:  %khan
              $>(%avow gift:khan)
      ==  ==
    +$  khan-state
      $:  %0
          unix-duct=duct
      ==
    --
=>
|%
++  start-spider
  |=  [our=@p =cage]
  ^-  note
  [%g %deal [our our] %spider %poke cage]
++  watch-spider
  |=  [our=@p =path]
  ^-  note
  [%g %deal [our our] %spider %watch path]
--
=|  khan-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  khan-gate  .
^?
|%
::  +call: handle a +task:khan request
::
++  call
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task:khan)
      ==
  ^-  [(list move) _khan-gate]
  ::
  =/  =task:khan  ((harden task:khan) wrapped-task)
  ?+    -.task  [~ khan-gate]
      %born
    [~ khan-gate(unix-duct hen)]
      %fyrd
    ::  start the thread on %spider.
    ::
    =/  wir=wire  (head hen)
    =/  rid=@ta   (rear wir)
    ::  XX  what if the client sends a duplicate rid?
    ::  is this fine? should we inject some randomness?
    ::
    =/  tid=@ta
      (cat 3 'khan-fyrd--' rid)
    =/  args
      :*  ~  `tid  [our desk.task %da now]  name.task
          ::  XX  can't do this:
          ::
          ::    !>(data.task)
          ::
          ::  special case for -hi:
          ::
          !>(;;([~ arg=$@(who=ship [who=ship mez=tape])] data.task))
      ==
    =/  start-moves=(list move)
      %+  turn
        :~  (watch-spider our /thread-result/[tid])
            (start-spider our %spider-start !>(args))
        ==
      |=(=note ^-(move [hen %pass //g note]))
    [start-moves khan-gate]
  ==
::  +load: migrate an old state to a new khan version
::
++  load
  |=  old=khan-state
  ^+  khan-gate
  khan-gate(state old)
::  +scry: nothing to see as yet
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ~
++  stay  state
::  +take: handle responses.
::
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _khan-gate]
  ?^  dud
    ~|(%khan-take-dud (mean tang.u.dud))
  ~&  >  [tea+tea hen+hen hin+hin]
  ?-    -.hin
      %gall
    ?+    -.p.hin  [~ khan-gate]
        ?(%poke-ack %watch-ack)
      ?~  p.p.hin  [~ khan-gate]
      %-  (slog u.p.p.hin)
      :_  khan-gate
      [hen %give %avow %| -.p.hin u.p.p.hin]~
    ::
        %fact
      =*  cag  cage.p.hin
      ?+    p.cag  ~&(bad-fact+p.cag !!)
          %thread-fail
        =/  =tang  !<(tang q.cag)
        %-  (slog tang)
        :_  khan-gate
        [hen %give %avow %| p.cag tang]~
      ::
          %thread-done
        :_  khan-gate
        ::  XX  mark conversion
        ::
        =/  res=*  !<(* q.cag)
        [hen %give %avow %& %noun res]~
      ==
    ==
  ::
      %khan
    ::  XX  unreachable?
    ::
    !!
  ==
--
