/-  spider
/+  libstrand=strand, default-agent, verb, server, dbug
=,  strand=strand:libstrand
~%  %spider-top  ..part  ~
|%
+$  card         card:agent:gall
+$  thread       thread:spider
+$  tid          tid:spider
+$  input        input:spider
+$  yarn         (list tid)
+$  thread-form  _*eval-form:eval:(strand ,vase)
+$  trying       ?(%build %none)
+$  state
  $:  starting=(map yarn [=trying =vase])
      running=(axal thread-form)
      tid=(map tid yarn)
      serving=(map tid [(unit @ta) =mark =desk])
  ==
::
+$  clean-slate-any
  $^  clean-slate-ket
  $%  clean-slate-sig
      clean-slate-1
      clean-slate-2
      clean-slate-3
      clean-slate
  ==
::
+$  clean-slate
  $:  %4
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit @ta) =mark =desk])
  ==
::
+$  clean-slate-3
  $:  %3
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [@ta =mark =desk])
  ==
::
+$  clean-slate-2
  $:  %2
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [@ta =mark])
  ==
::
+$  clean-slate-1
  $:  %1
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
  ==
::
+$  clean-slate-ket
  $:  starting=(map yarn [trying=?(%build %find %none) =vase])
      running=(list yarn)
      tid=(map tid yarn)
  ==
::
+$  clean-slate-sig
  $:  starting=~
      running=(list yarn)
      tid=(map tid yarn)
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  =state
=<
  %+  verb  |
  ~%  %spider-agent  ..bind-eyre  ~
  |_  =bowl:gall
  +*  this         .
      spider-core  +>
      sc           ~(. spider-core bowl)
      def          ~(. (default-agent this %|) bowl)
      bec          byk.bowl(r da+now.bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this
    ~[bind-eyre:sc]
  ++  on-save   clean-state:sc
  ++  on-load
    |^
    |=  old-state=vase
    =+  !<(any=clean-slate-any old-state)
    =?  any  ?=(^ -.any)  (old-to-1 any)
    =?  any  ?=(~ -.any)  (old-to-1 any)
    =^  upgrade-cards  any
      (old-to-2 any)
    =.  any  (old-to-3 any)
    =.  any  (old-to-4 any)
    ?>  ?=(%4 -.any)
    ::
    =.  tid.state  tid.any
    =/  yarns=(list yarn)
      %+  welp  running.any
      ~(tap in ~(key by starting.any))
    |-  ^-  (quip card _this)
    ?~  yarns
      [~[bind-eyre:sc] this]
    =^  cards-1  state
      (handle-stop-thread:sc (yarn-to-tid i.yarns) |)
    =^  cards-2  this
      $(yarns t.yarns)
    [:(weld upgrade-cards cards-1 cards-2) this]
    ::
    ++  old-to-1
      |=  old=clean-slate-ket
      ^-  clean-slate-1
      1+old(starting (~(run by starting.old) |=([* v=vase] none+v)))
    ::
    ++  old-to-2
      |=  old=clean-slate-any
      ^-  (quip card clean-slate-any)
      ?>  ?=(?(%1 %2 %3 %4) -.old)
      ?:  ?=(?(%2 %3 %4) -.old)
        `old
      :-  ~[bind-eyre:sc]
      :*  %2
        starting.old
        running.old
        tid.old
        ~
      ==
    ::
    ++  old-to-3
      |=  old=clean-slate-any
      ^-  clean-slate-any
      ?>  ?=(?(%2 %3 %4) -.old)
      ?:  ?=(?(%3 %4) -.old)
        old
      :*  %3
        starting.old
        running.old
        tid.old
        (~(run by serving.old) |=([id=@ta =mark] [id mark q.byk.bowl]))
      ==
    ++  old-to-4
      |=  old=clean-slate-any
      ^-  clean-slate
      ?>  ?=(?(%3 %4) -.old)
      ?:  ?=(%4 -.old)
        old
      :*  %4
        starting.old
        running.old
        tid.old
        (~(run by serving.old) |=([id=@ta =mark =desk] [`id mark q.byk.bowl]))
      ==
    --
  ::
  ++  on-poke
    ~/  %on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?:  ?=(%spider-kill mark)
      (on-load on-save)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %spider-input   (on-poke-input:sc !<(input vase))
          %spider-start   (handle-start-thread:sc !<(start-args:spider vase))
          %spider-inline  (handle-inline-thread:sc !<(inline-args:spider vase))
          %spider-stop    (handle-stop-thread:sc !<([tid ?] vase))
          %handle-http-request
        (handle-http-request:sc !<([@ta =inbound-request:eyre] vase))
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      ?+  path  (on-watch:def path)
        [%thread @ *]         (on-watch:sc t.path)
        [%thread-result @ ~]  (on-watch-result:sc i.t.path)
        [%http-response *]     `state
      ==
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    ~/  %on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+    path  (on-peek:def path)
        [%x %tree ~]
      ``noun+!>((turn ~(tap of running.state) head))
    ::
        [%x %starting @ ~]
      ``noun+!>((~(has of running.state) (~(got by tid.state) i.t.t.path)))
    ::
        [%x %saxo @ ~]
      ``noun+!>((~(got by tid.state) i.t.t.path))
    ==
  ::
  ++  on-agent
    ~/  %on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  !!
        [%thread @ *]  (on-agent:sc i.t.wire t.t.wire sign)
      ==
    [cards this]
  ::
  ++  on-arvo
    ~/  %on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (on-arvo:def wire sign-arvo)
        [%thread @ *]  (handle-sign:sc i.t.wire t.t.wire sign-arvo)
        [%build @ ~]   (handle-build:sc i.t.wire sign-arvo)
        [%bind ~]      `state
      ==
    [cards this]
  ::  On unexpected failure, kill all outstanding strands
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card _this)
    %-  (slog leaf+"spider crashed, killing all strands: {<term>}" tang)
    (on-load on-save)
  --
::
~%  %spider-helper  ..card  ~
|_  =bowl:gall
++  bec  `beak`byk.bowl(r da+now.bowl)
++  bind-eyre
  ^-  card
  [%pass /bind %arvo %e %connect [~ /spider] %spider]
::
++  new-thread-id
  |=  file=term
  :((cury cat 3) file '--' (scot %uv (sham eny.bowl)))
::
++  handle-http-request
  ~/  %handle-http-request
  |=  [eyre-id=@ta =inbound-request:eyre]
  ^-  (quip card _state)
  ::?>  authenticated.inbound-request
  =/  url
    (parse-request-line:server url.request.inbound-request)
  ?>  ?=([%spider @t @t @t @t ~] site.url)
  =*  desk         i.t.site.url
  =*  input-mark   i.t.t.site.url
  =*  thread       i.t.t.t.site.url
  =*  output-mark  i.t.t.t.t.site.url
  =/  =tid         (new-thread-id thread)
  =.  serving.state
    (~(put by serving.state) tid [`eyre-id output-mark desk])
  ::  TODO: speed this up somehow. we spend about 15ms in this arm alone
  ::
  =/  tube  (convert-tube %json input-mark desk bowl)
  ?>  ?=(^ body.request.inbound-request)
  =/  body=json  (need (de-json:html q.u.body.request.inbound-request))
  =/  input=vase  (slop !>(~) (tube !>(body)))
  =/  boc  bec
  =/  =start-args:spider  [~ `tid boc(q desk, r da+now.bowl) thread input]
  (handle-start-thread start-args)
::
++  on-poke-input
  |=  input
  =/  yarn  (~(got by tid.state) tid)
  (take-input yarn ~ %poke cage)
::
++  on-watch
  |=  [=tid =path]
  (take-input (~(got by tid.state) tid) ~ %watch path)
::
++  on-watch-result
  |=  =tid
  ^-  (quip card ^state)
  `state
::
++  handle-sign
  ~/  %handle-sign
  |=  [=tid =wire =sign-arvo]
  =/  yarn  (~(get by tid.state) tid)
  ?~  yarn
    %-  (slog leaf+"spider got sign for non-existent {<tid>}" ~)
    `state
  (take-input u.yarn ~ %sign wire sign-arvo)
::
++  on-agent
  |=  [=tid =wire =sign:agent:gall]
  =/  yarn  (~(get by tid.state) tid)
  ?~  yarn
    %-  (slog leaf+"spider got agent for non-existent {<tid>}" ~)
    `state
  (take-input u.yarn ~ %agent wire sign)
::
++  handle-start-thread
  ~/  %handle-start-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak file=term =vase]
  (prep-thread parent-tid use beak %| file vase)
::
++  handle-inline-thread
  ~/  %handle-inline-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak =shed:khan]
  (prep-thread parent-tid use beak %& shed)
::
++  prep-thread
  |=  $:  parent-tid=(unit tid)  use=(unit tid)  =beak
          source=(each shed:khan [file=term =vase])
      ==
  ^-  (quip card ^state)
  =/  parent-yarn=yarn
    ?~  parent-tid
      /
    (~(got by tid.state) u.parent-tid)
  =/  new-tid
    ?^  use
      u.use
    %-  new-thread-id
    ?-  -.source
      %&  (cat 3 'inline-' q.beak)
      %|  file.p.source
    ==
  ::
  =/  =yarn  (snoc parent-yarn new-tid)
  ::
  ?:  (~(has of running.state) yarn)
    ~|  [%already-started yarn]
    !!
  ?:  (~(has by starting.state) yarn)
    ~|  [%already-starting yarn]
    !!
  ::
  =?  serving.state  !(~(has by serving.state) new-tid)
    (~(put by serving.state) new-tid [~ %noun q.beak])
  ::
  =.  tid.state       (~(put by tid.state) new-tid yarn)
  ?-    -.source
      %&  (begin-shed yarn p.source)
      %|
    =.  starting.state  (~(put by starting.state) yarn [%build vase.p.source])
    =/  pax=path
      ~|  no-file-for-thread+file.p.source
      (need (get-fit:clay beak %ted file.p.source))
    :_  state
    :_  ~
    :+  %pass  /build/[new-tid]
    [%arvo %c %warp p.beak q.beak ~ %sing %a r.beak pax]
  ==
::
++  handle-build
  ~/  %handle-build
  |=  [=tid =sign-arvo]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%none vase]))
  ~|  sign+[- +<]:sign-arvo
  ?>  ?=([?(%behn %clay) %writ *] sign-arvo)
  =/  =riot:clay  p.sign-arvo
  ?~  riot
    (thread-fail-not-running tid %build-thread-error *tang)
  ?.  ?=(%vase p.r.u.riot)
    (thread-fail-not-running tid %build-thread-strange >[p q]:u.riot< ~)
  =/  maybe-thread  (mule |.(!<(thread !<(vase q.r.u.riot))))
  ?:  ?=(%| -.maybe-thread)
    (thread-fail-not-running tid %thread-not-thread ~)
  (slam-thread yarn p.maybe-thread)
::
++  slam-thread
  ~/  %slam-thread
  |=  [=yarn =thread]
  ^-  (quip card ^state)
  =/  =vase  vase:(~(got by starting.state) yarn)
  =/  res  (mule |.((thread vase)))
  ?:  ?=(%| -.res)
    (thread-fail-not-running (yarn-to-tid yarn) %false-start p.res)
  =.  starting.state  (~(del by starting.state) yarn)
  (begin-shed yarn p.res)
::
++  begin-shed
  |=  [=yarn =shed:khan]
  ?<  (~(has of running.state) yarn)
  =/  m  (strand ,vase)
  =/  =eval-form:eval:m  (from-form:eval:m shed)
  =.  running.state  (~(put of running.state) yarn eval-form)
  (take-input yarn ~)
::
++  handle-stop-thread
  |=  [=tid nice=?]
  ^-  (quip card ^state)
  =/  yarn=(unit yarn)  (~(get by tid.state) tid)
  ?~  yarn
    ~&  %stopping-nonexistent-thread
    [~ state]
  ?:  (~(has of running.state) u.yarn)
    ?:  nice
      (thread-done u.yarn *vase)
    (thread-fail u.yarn %cancelled ~)
  ?:  (~(has by starting.state) u.yarn)
    (thread-fail-not-running tid %stopped-before-started ~)
  ~&  [%thread-not-started u.yarn]
  ?:  nice
    (thread-done u.yarn *vase)
  (thread-fail u.yarn %cancelled ~)
::
++  take-input
  ~/  %take-input
  |=  [=yarn input=(unit input:strand)]
  ^-  (quip card ^state)
  =/  m  (strand ,vase)
  ?.  (~(has of running.state) yarn)
    %-  (slog leaf+"spider got input for non-existent {<yarn>}" ~)
    `state
  =/  =eval-form:eval:m
    (need fil:(~(dip of running.state) yarn))
  =|  cards=(list card)
  |-  ^-  (quip card ^state)
  =^  r=[cards=(list card) =eval-result:eval:m]  eval-form
    =/  out
      %-  mule  |.
      (take:eval:m eval-form (convert-bowl yarn bowl) input)
    ?-  -.out
      %&  p.out
      %|  [[~ [%fail %crash p.out]] eval-form]
    ==
  =.  running.state  (~(put of running.state) yarn eval-form)
  =/  =tid  (yarn-to-tid yarn)
  =.  cards.r
    %+  turn  cards.r
    |=  =card
    ^-  ^card
    ?+  card  card
        [%pass * *]  [%pass [%thread tid p.card] q.card]
        [%give ?(%fact %kick) *]
      =-  card(paths.p -)
      %+  turn  paths.p.card
      |=  =path
      ^-  ^path
      [%thread tid path]
    ==
  =.  cards  (weld cards cards.r)
  =^  final-cards=(list card)  state
    ?-  -.eval-result.r
      %next  `state
      %fail  (thread-fail yarn err.eval-result.r)
      %done  (thread-done yarn value.eval-result.r)
    ==
  [(weld cards final-cards) state]
::
++  thread-fail-not-running
  |=  [=tid =term =tang]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  :_  state(starting (~(del by starting.state) yarn))
  =/  moz  (thread-say-fail tid term tang)
  ?.  ?=([~ %build *] (~(get by starting.state) yarn))
    moz
  :_(moz [%pass /build/[tid] %arvo %c %warp our.bowl %base ~])
::
++  thread-say-fail
  |=  [=tid =term =tang]
  ^-  (list card)
  :~  [%give %fact ~[/thread-result/[tid]] %thread-fail !>([term tang])]
      [%give %kick ~[/thread-result/[tid]] ~]
  ==
++  thread-http-fail
  |=  [=tid =term =tang]
  ^-  (quip card ^state)
  =-  (fall - `state)
  %+  bind
    (~(get by serving.state) tid)
  |=  [eyre-id=(unit @ta) output=mark =desk]
  :_  state(serving (~(del by serving.state) tid))
  ?~  eyre-id
    ~
  %+  give-simple-payload:app:server  u.eyre-id
  ^-  simple-payload:http
  :_  ~  :_  ~
  ?.  ?=(http-error:spider term)
    ((slog tang) 500)
  ?-  term
    %bad-request  400
    %forbidden    403
    %nonexistent  404
    %offline      504
  ==
::
++  thread-fail
  |=  [=yarn =term =tang]
  ^-  (quip card ^state)
  ::%-  (slog leaf+"strand {<yarn>} failed" leaf+<term> tang)
  =/  =tid  (yarn-to-tid yarn)
  =/  fail-cards  (thread-say-fail tid term tang)
  =^  cards  state  (thread-clean yarn)
  =^  http-cards  state  (thread-http-fail tid term tang)
  [:(weld fail-cards cards http-cards) state]
::
++  thread-http-response
  |=  [=tid =vase]
  ^-  (quip card ^state)
  =-  (fall - `state)
  %+  bind
    (~(get by serving.state) tid)
  |=  [eyre-id=(unit @ta) output=mark =desk]
  ?~  eyre-id
    `state
  =/  tube  (convert-tube output %json desk bowl)
  :_  state(serving (~(del by serving.state) tid))
  %+  give-simple-payload:app:server  u.eyre-id
  (json-response:gen:server !<(json (tube vase)))
::
++  thread-done
  |=  [=yarn =vase]
  ^-  (quip card ^state)
  ::  %-  (slog leaf+"strand {<yarn>} finished" (sell vase) ~)
  =/  =tid  (yarn-to-tid yarn)
  =/  done-cards=(list card)
    :~  [%give %fact ~[/thread-result/[tid]] %thread-done vase]
        [%give %kick ~[/thread-result/[tid]] ~]
    ==
  =^  http-cards  state
    (thread-http-response tid vase)
  =^  cards  state  (thread-clean yarn)
  [:(weld done-cards cards http-cards) state]
::
++  thread-clean
  |=  =yarn
  ^-  (quip card ^state)
  =/  children=(list ^yarn)
    %+  turn
      ~(tap of (~(dip of running.state) yarn))
    |=  [child=^yarn *]
    (welp yarn child)
  |-  ^-  (quip card ^state)
  ?~  children
    `state
  =^  cards-children  state  $(children t.children)
  =^  cards-our  state
    =/  =^yarn  i.children
    =/  =tid  (yarn-to-tid yarn)
    =:  running.state  (~(lop of running.state) yarn)

        tid.state      (~(del by tid.state) tid)
        serving.state  (~(del by serving.state) (yarn-to-tid yarn))
      ==
    :_  state
    %+  murn  ~(tap by wex.bowl)
    |=  [[=wire =ship =term] [acked=? =path]]
    ^-  (unit card)
    ?.  ?&  ?=([%thread @ *] wire)
            =(tid i.t.wire)
        ==
      ~
    `[%pass wire %agent [ship term] %leave ~]
  [(welp cards-children cards-our) state]
::
++  convert-bowl
  |=  [=yarn =bowl:gall]
  ^-  bowl:spider
  :*  our.bowl
    src.bowl
      (yarn-to-tid yarn)
      (yarn-to-parent yarn)
      wex.bowl
      sup.bowl
      eny.bowl
      now.bowl
      (yarn-to-byk yarn bowl)
  ==
::
++  yarn-to-tid
  |=  =yarn
  ^-  tid
  =/  nary  (flop yarn)
  ?>  ?=([@ *] nary)
  i.nary
::
++  yarn-to-parent
  |=  =yarn
  ^-  (unit tid)
  =/  nary  (flop yarn)
  ?>  ?=([@ *] nary)
  ?~  t.nary
    ~
  `i.t.nary
::
++  yarn-to-byk
  |=  [=yarn =bowl:gall]
  =/  [* * =desk]
    ~|  "no desk associated with {<tid>}"
     %-  ~(got by serving.state)  (yarn-to-tid yarn)
  =/  boc  bec
  boc(q desk)
::
++  clean-state
  !>  ^-  clean-slate
  4+state(running (turn ~(tap of running.state) head))
::
++  convert-tube
  |=  [from=mark to=mark =desk =bowl:gall]
  .^
    tube:clay
    %cc
    /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/[from]/[to]
  ==
--
