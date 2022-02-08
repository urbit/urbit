::  %khan, thread runner
::
::  this vane presents a command/response interface for running
::  threads. two modes are supported: %fard for intra-arvo
::  requests (i.e. within the same kernel space) and %fyrd for
::  external requests (e.g. from the unix control plane.)
::
::  both modes take a thread start request consisting of a
::  namespace, thread name, and input data; they respond over the
::  same duct with either success or failure. %fard takes its
::  input arguments as a $vase and produces $arow, which contains
::  a $vase on success (or $tang on failure). %fyrd takes an
::  output mark and input `(cask)` (short for `(cask *)`); it
::  produces $avow, which contains a `(cask)` on success.
::
::  %fard passes its arguments and produces its result
::  unmodified. %fyrd does mark conversion on both ends, and
::  additionally lifts its input into a $unit. this second step
::  is done because threads conventionally take their input as a
::  $unit, with ~ for the case of "no arguments".
::
::  n.b. the current convention for threads is to use !< to
::  unpack their input into a well-defined type. !< imposes the
::  requirement that the input type nests within the specified
::  type. this limits %fyrd to threads with inputs for which a
::  named mark exists; it is impossible to use %noun in general
::  since it does not nest. to support using the %noun mark for
::  inputs, it would be sufficient to convert threads to use ;;
::  rather than !< on their inputs, at the cost of losing type
::  validation.
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
              $>  ?(%arow %avow)
              gift:khan
      ==  ==
    +$  khan-state
      $:  %1
          hey=duct                                      ::  current unix duct
          run=(map duct thread-state)                   ::  running threads
      ==
    +$  thread-state
      [tid=@ta p=(unit [mak=mark bek=beak])]
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
  |=  [our=@p =vase]
  ^-  note
  [%g %deal [our our] %spider %poke %spider-start vase]
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
      %fard
    =*  fad  p.task
    =/  tid=@ta
      (cat 3 'khan-fard--' (scot %uv (sham eny)))
    =/  =beak
      ?@(bear.fad [our bear.fad %da now] bear.fad)
    =/  args
      [~ `tid beak name.fad data.fad]
    =/  start-moves=(list move)
      %+  turn
        :~  (watch-spider our /thread-result/[tid])
            (start-spider our !>(args))
        ==
      |=(=note ^-(move [hen %pass //g note]))
    =.  run  (~(put by run) hen tid ~)
    [start-moves khan-gate]
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
      ?@(bear.fyd [our bear.fyd %da now] bear.fyd)
    =/  =dais:clay  (get-dais beak p.data.fyd rof)
    =/  args
      :*  ~  `tid  beak  name.fyd
          (slam !>(some) (vale.dais q.data.fyd))
      ==
    =/  start-moves=(list move)
      %+  turn
        :~  (watch-spider our /thread-result/[tid])
            (start-spider our !>(args))
        ==
      |=(=note ^-(move [hen %pass //g note]))
    =.  run  (~(put by run) hen tid `[out-mark.fyd beak])
    [start-moves khan-gate]
  ==
::  +load: migrate an old state to a new khan version
::
++  load
  |=  $=  old
      $%  $:  %0
              hey=duct
              run=(map duct [tid=@ta mak=mark bek=beak])
          ==
          khan-state
      ==
  ^+  khan-gate
  =/  new=khan-state
    ?:  ?=(%0 -.old)
      :+  %1
        hey.old
      %-  ~(urn by run.old)
      |=  [a=duct b=[tid=@ta mak=mark bek=beak]]
      [tid.b `[mak.b bek.b]]
    old
  khan-gate(state new)
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
    =/  tad  (~(got by run) hen)
    ?~  p.tad
      [hen %give %arow %| -.p.hin u.p.p.hin]~
    [hen %give %avow %| -.p.hin u.p.p.hin]~
  ::
      %fact
    =*  cag  cage.p.hin
    ?+    p.cag  ~&(bad-fact+p.cag !!)
        %thread-fail
      =/  =tang  !<(tang q.cag)
      %-  (slog 'khan-fact' tang)
      :_  khan-gate
      =/  tad  (~(got by run) hen)
      ?~  p.tad
        [hen %give %arow %| p.cag tang]~
      [hen %give %avow %| p.cag tang]~
    ::
        %thread-done
      :_  khan-gate
      =/  tad  (~(got by run) hen)
      ?~  p.tad
        [hen %give %arow %& q.cag]~
      =/  mak  mak.u.p.tad
      =/  bek  bek.u.p.tad
      =/  =dais:clay  (get-dais bek mak rof)
      =/  =vase  (vale:dais q.q.cag)
      [hen %give %avow %& mak q.vase]~
    ==
      %kick
    [~ khan-gate(run (~(del by run) hen))]
  ==
--
