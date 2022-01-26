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
    =/  args
      :*  ~  `tid  [our q.beak.task %da now]  name.task
          !>([~ ~zod])
      ==
    =/  start-moves=(list move)
      %+  turn
        :~  (watch-spider p.beak.task /thread-result/[tid])
            (start-spider p.beak.task %spider-start !>(args))
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
  ~&  >  [tea+tea hen+hen hin+hin]
  ?-    -.hin
      %gall
    ?+    -.p.hin  [~ khan-gate]
        ?(%poke-ack %watch-ack)
      ?~  p.p.hin  [~ khan-gate]
      %-  (slog u.p.p.hin)
      [~ khan-gate]
    ::
        %fact
      =*  cag  cage.p.hin
      ?+    p.cag  !!
          %thread-fail
        %-  (slog !<(tang q.cag))
        [~ khan-gate]
      ::
          %thread-done
        :_  khan-gate
        =/  mov=move
          [hen %give %avow %& %tape !>(~)]
        ~[mov]
      ==
    ==
  ::
      %khan
    !!
  ==
--
