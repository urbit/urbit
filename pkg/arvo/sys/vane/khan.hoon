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
          hey=duct                                      ::  current unix duct
          run=(map duct thread-state)                   ::  running threads
      ==
    ::  TODO  move to lull
    ::
    +$  thread-state  [tid=@ta mak=mark bek=beak]
    --
=>
|%
++  get-dais
  |=  [=beak =mark rof=roof]
  ^-  dais:clay
  ?~  ret=(rof ~ %cb beak /[mark])
    ~|(%mark-unknown !!)
  ?~  u.ret
    ~|(%mark-invalid !!)
  ?>  =(%dais p.u.u.ret)
  !<(dais:clay q.u.u.ret)
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
    :-  ~
    %=  khan-gate
      hey  hen
      run  *(map duct thread-state)
    ==
  ::
      %fyrd
    =/  wir=wire  (head hen)
    =/  rid=@ta   (rear wir)
    =*  fyd  p.task
    ?:  (~(has by run) hen)
      ~|(%fyrd-duplicate-rid !!)
    =/  tid=@ta
      (cat 3 'khan-fyrd--' rid)
    =/  =beak
      ?@(bear.fyd [our bear.fyd %da now] [our bear.fyd])
    =/  =dais:clay  (get-dais beak p.data.fyd rof)
    =/  args
      :*  ~  `tid  beak  name.fyd
          (vale.dais q.data.fyd)
      ==
    =/  start-moves=(list move)
      %+  turn
        :~  (watch-spider our /thread-result/[tid])
            (start-spider our %spider-start !>(args))
        ==
      |=(=note ^-(move [hen %pass //g note]))
    =.  run  (~(put by run) hen tid mark.fyd beak)
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
    =.  run  (~(del by run) hen)
    ~|(%khan-take-dud (mean tang.u.dud))
  ?.  ?=(%gall -.hin)
    [~ khan-gate]
  ?+    -.p.hin  [~ khan-gate]
      ?(%poke-ack %watch-ack)
    ?~  p.p.hin  [~ khan-gate]
    =.  run  (~(del by run) hen)
    %-  (slog 'khan-ack' u.p.p.hin)
    :_  khan-gate
    [hen %give %avow %| -.p.hin u.p.p.hin]~
  ::
      %fact
    =*  cag  cage.p.hin
    ?+    p.cag  ~&(bad-fact+p.cag !!)
        %thread-fail
      =/  =tang  !<(tang q.cag)
      %-  (slog 'khan-fact' tang)
      :_  khan-gate
      [hen %give %avow %| p.cag tang]~
    ::
        %thread-done
      :_  khan-gate
      =/  tad=thread-state  (~(got by run) hen)
      =/  =dais:clay  (get-dais bek.tad mak.tad rof)
      =/  =vase  (vale:dais q.q.cag)
      [hen %give %avow %& mak.tad q.vase]~
    ==
      %kick
    [~ khan-gate(run (~(del by run) hen))]
  ==
--
