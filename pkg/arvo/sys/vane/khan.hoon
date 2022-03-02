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
::  input arguments as a vase and produces %arow, which contains
::  a vase on success (or tang on failure). %fyrd takes an output
::  mark and input crag; it produces %avow, which contains a crag
::  on success.
::
::  %fard passes its arguments and produces its result
::  unmodified. %fyrd does mark conversion on both ends, and
::  additionally lifts its input into a $unit. this second step
::  is done because threads conventionally take their input as a
::  unit, with ~ for the case of "no arguments".
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
=,  khan
|=  our=ship
=>  |%                                                  ::  %khan types
    +$  move  [p=duct q=(wite note gift)]               ::
    +$  note                                            ::    out request $->
      $~  [%g %deal *sock *term *deal:gall]             ::
      $%  $:  %g                                        ::    to %gall
              $>(%deal task:gall)                       ::  full transmission
          ==                                            ::
          $:  %k                                        ::    to self
              $>(%fard task)                            ::  internal thread
      ==  ==                                            ::
    +$  sign                                            ::    in response $<-
      $%  $:  %gall                                     ::    from %gall
              $>(%unto gift:gall)                       ::  update
          ==                                            ::
          $:  %khan                                     ::    from self
              $>  ?(%arow %avow)  gift                  ::  thread result
      ==  ==                                            ::
    +$  khan-state                                      ::
      [%2 hey=duct]                                     ::  current unix duct
    --                                                  ::
=>
|%
++  get-beak
  |=  [=bear now=@da]
  ?@(bear [our bear %da now] bear)
++  get-dais
  |=  [=beak =mark rof=roof]
  ^-  dais:clay
  ?~  ret=(rof ~ %cb beak /[mark])
    ~|(%mark-unknown !!)
  ?~  u.ret
    ~|(%mark-invalid !!)
  ?>  =(%dais p.u.u.ret)
  !<(dais:clay q.u.u.ret)
++  make-wire
  |=  [=beak =mark]
  ^-  wire
  :~  %fyrd
      (scot %p p.beak)
      q.beak
      -.r.beak
      (scot r.beak)
      mark
  ==
++  read-case
  |=  a=[p=@ @]
  ^-  case
  ::  XX  should be:
  ::    ?>  ?=(?(%da %tas %ud) p.a)
  ::    [p.a (slav a)]
  ::
  ?+  p.a   !!
    %da   [p.a (slav a)]
    %tas  [p.a (slav a)]
    %ud   [p.a (slav a)]
  ==
++  read-wire
  |=  =wire
  ^-  (pair beak mark)
  ~|  khan-read-wire+wire
  ?>  ?=([%fyrd @ @ @ @ @ ~] wire)
  :_  i.t.t.t.t.t.wire
  :+  (slav %p i.t.wire)
    i.t.t.wire
  (read-case [i i.t]:t.t.t.wire)
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
::  +call: handle a +task request
::
++  call
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _khan-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ?^  dud
    ~|(%khan-call-dud (mean tang.u.dud))
  ?+    -.task  [~ khan-gate]
      %born
    [~ khan-gate(hey hen)]
  ::
      %fard
    =/  tid=@ta
      (cat 3 'khan-fyrd--' (scot %uv (sham eny)))
    =*  fyd     p.task
    =/  =beak   (get-beak bear.fyd now)
    =/  args    [~ `tid beak name.fyd args.fyd]
    :_  khan-gate
    %+  turn
      :~  (watch-spider our /thread-result/[tid])
          (start-spider our !>(args))
      ==
    |=(=note ^-(move [hen %pass //g note]))
  ::
      %fyrd
    =*  fyd     p.task
    =/  =beak   (get-beak bear.fyd now)
    =/  =wire   (make-wire beak p.args.fyd)
    =/  =dais:clay
      (get-dais beak p.q.args.fyd rof)
    =/  =vase
      (slam !>(some) (vale.dais q.q.args.fyd))
    :_  khan-gate
    [hen %pass wire %k %fard bear.fyd name.fyd vase]~
  ==
::  +load: migrate an old state to a new khan version
::
++  load
  |=  $=  old
      $%  [?(%0 %1) hey=duct *]
          khan-state
      ==
  ^+  khan-gate
  =/  new=khan-state
    ?:  ?=(%2 -.old)
      old
    [%2 hey.old]
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
    ~|(%khan-take-dud (mean tang.u.dud))
  :_  khan-gate
  ?-    -.hin
      %gall
    ?+    -.p.hin  ~
        ?(%poke-ack %watch-ack)
      ?~  p.p.hin  ~
      %-  (slog 'khan-ack' u.p.p.hin)
      [hen %give %arow %| -.p.hin u.p.p.hin]~
    ::
        %fact
      =*  cag  cage.p.hin
      ?+    p.cag  ~&(bad-fact+p.cag !!)
          %thread-fail
        =/  =tang  !<(tang q.cag)
        %-  (slog 'khan-fact' tang)
        [hen %give %arow %| p.cag tang]~
      ::
          %thread-done
        [hen %give %arow %& q.cag]~
      ==
    ==
  ::
      %khan
    ?.  ?=(%arow +<.hin)  ~
    ?~  tea  ~
    ?.  ?=(%fyrd -.tea)  ~
    =*  row  p.hin
    ?.  ?=(%& -.row)
      [hen %give %avow row]~
    =/  [=beak =mark]
      (read-wire tea)
    =/  =dais:clay  (get-dais beak mark rof)
    =/  =vase       (vale:dais q.p.row)
    [hen %give %avow %& mark q.vase]~
  ==
--
