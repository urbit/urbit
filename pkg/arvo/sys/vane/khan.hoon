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
::  input arguments as a cage and produces %arow, which contains
::  a cage on success (or tang on failure). %fyrd takes an output
::  mark and input page; it produces %avow, which contains a page
::  on success.
::
::  threads currently expect input and produce output as vase,
::  not cage. %fard/%arow use cage instead since this is the
::  eventual desired thread API; however, the input mark is
::  currently ignored, and the output mark is always %noun. (for
::  forward compatibility, it is safe to specify %noun as the
::  input mark.)
::
::  %fyrd does mark conversion on both ends, and additionally
::  lifts its input into a $unit. this second step is done
::  because threads conventionally take their input as a unit,
::  with ~ for the case of "no arguments".
::
::  n.b. the current convention for threads is to use !< to
::  unpack their input vase. !< imposes the requirement that the
::  input type nests within the specified type. this limits %fyrd
::  to threads with inputs for which a named mark exists; it is
::  impossible to use %noun in general since it does not nest.
::  threads written against the current vase-based API could use
::  ;; instead of !< to unpack their input, thus allowing the
::  use of %fyrd with %noun. however the eventual solution is
::  probably to make threads consume and produce cages, and do
::  mark conversion where appropriate.
!:
!?  164
::
=,  khan
|=  our=ship
=>  |%                                                  ::  %khan types
    +$  move  [p=duct q=(wite note gift)]               ::
    +$  note                                            ::    out request $->
      $~  [%g %deal *sack *term *deal:gall]             ::
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
              $>(?(%arow %avow) gift)                   ::  thread result
      ==  ==                                            ::
    +$  khan-state                                      ::
      $:  %0                                            ::    state v0
          hey=duct                                      ::  unix duct
          tic=@ud                                       ::  tid counter
      ==                                                ::
    --                                                  ::
=>
|%
++  get-beak
  |=  [=bear now=@da]
  ?@(bear [our bear %da now] bear)
::
++  get-dais
  |=  [=beak =mark rof=roof]
  ^-  dais:clay
  ?~  ret=(rof ~ /khan %cb beak /[mark])
    ~|(mark-unknown+mark !!)
  ?~  u.ret
    ~|(mark-invalid+mark !!)
  ?>  =(%dais p.u.u.ret)
  !<(dais:clay q.u.u.ret)
::
++  get-tube
  |=  [=beak =mark =out=mark rof=roof]
  ^-  tube:clay
  ?~  ret=(rof ~ /khan %cc beak /[mark]/[out-mark])
    ~|(tube-unknown+[mark out-mark] !!)
  ?~  u.ret
    ~|(tube-invalid+[mark out-mark] !!)
  ?>  =(%tube p.u.u.ret)
  !<(tube:clay q.u.u.ret)
::
++  make-wire
  |=  [=beak =mark]
  ^-  wire
  [%fyrd (en-beam beak mark ~)]
::
++  read-wire
  |=  =wire
  ^-  (pair beak mark)
  ~|  khan-read-wire+wire
  ?>  ?=([%fyrd ^] wire)
  =/  =beam  (need (de-beam t.wire))
  ?>(?=([@ ~] s.beam) beam(s i.s.beam))
::
++  poke-spider
  |=  [hen=duct =cage]
  ^-  move
  [hen %pass //g %g %deal [our our /khan] %spider %poke cage]
::
++  watch-spider
  |=  [hen=duct =path]
  ^-  move
  [hen %pass //g %g %deal [our our /khan] %spider %watch path]
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
    [~ khan-gate(hey hen, tic 0)]
  ::
      %fard  (bard hen 'khan-fyrd--' bear.p.task %| [name args]:p.task)
      %lard  (bard hen 'khan-lard--' bear.task %& shed.task)
      %fyrd
    =*  fyd         p.task
    =/  =beak       (get-beak bear.fyd now)
    =/  =wire       (make-wire beak p.args.fyd)
    =/  =dais:clay  (get-dais beak p.q.args.fyd rof)
    =/  =vase
      (slap (vale.dais q.q.args.fyd) !,(*hoon [~ u=.]))
    =-  [[hen %pass wire -]~ khan-gate]
    [%k %fard bear.fyd name.fyd p.q.args.fyd vase]
  ==
::
++  bard
  |=  [hen=duct prefix=@ta =bear payload=(each shed [name=term args=cage])]
  ^-  [(list move) _khan-gate]
  =/  =tid:rand  (cat 3 prefix (scot %uv (sham (mix tic eny))))
  =/  =beak      (get-beak bear now)
  =/  =cage
    ?-  -.payload
      %&  [%spider-inline !>([~ `tid beak p.payload])]
      %|  [%spider-start !>([~ `tid beak [name q.args]:p.payload])]
    ==
  =.  tic  +(tic)
  :_  khan-gate
  :~  (watch-spider hen /thread-result/[tid])
      (poke-spider hen cage)
  ==
::
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
  |=  [lyc=gang pov=path car=term bem=beam]
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
        ::  %-  (slog 'khan-fact' tang)
        [hen %give %arow %| p.cag tang]~
      ::
          %thread-done
        [hen %give %arow %& %noun q.cag]~
      ==
    ==
  ::
      %khan
    ?.  ?=(%arow +<.hin)    ~
    ?.  ?=([%fyrd *] tea)   ~
    =*  row  p.hin
    ?.  ?=(%& -.row)
      [hen %give %avow row]~
    =/  [=beak =mark]   (read-wire tea)
    =/  =tube:clay      (get-tube beak p.p.row mark rof)
    =/  =vase           (tube q.p.row)
    [hen %give %avow %& mark q.vase]~
  ==
--
