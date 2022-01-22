::  %khan, thread runner
!:
!?  164
::
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift:khan)]
    +$  note                                            ::  out request $->
      $%  $:  %g                                        ::  to %gall
              $>(%deal task:gall)                       ::  full transmission
      ==  ==                                            ::
    +$  sign                                            ::  in response $<-
      !!
    +$  khan-state
      $:  %0
          unix-duct=duct
      ==
    --
=>
|%
++  start-spider
  |=  *
  !!
++  watch-spider
  |=  *
  !!
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
  ?-    -.task
      %vega
    [~ khan-gate]
      %trim
    [~ khan-gate]
      %done
    [~ khan-gate]
      %born
    [~ khan-gate(unix-duct hen)]
      %fyrd
    =/  rid=@ta  (rear (head hen))
    =/  tid=@ta
      (cat 3 'khan-fyrd--' rid)
    =/  start-moves=(list move)
      :~  (watch-spider beak.task name.task /thread-result/[tid])
          (start-spider beak.task name.task !>(data.task))
      ==
    ::  send to gall, sub for updates
    [start-moves khan-gate]
  ==
::  +load: migrate an old state to a new khan version
::
++  load
  |=  old=khan-state
  ^+  khan-gate
  khan-gate(state old)
::  +scry: view khan state
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ~
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _khan-gate]
  ?^  dud
    ~|(%khan-take-dud (mean tang.u.dud))
  ::  switch on type of thing you get back, update state
  [~ khan-gate]
--
