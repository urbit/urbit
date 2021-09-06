/-  spider
/+  libstrand=strand, default-agent, verb, server
=,  strand=strand:libstrand
~%  %spider-top  ..part  ~
|%
+$  card         card:agent:gall
+$  thread       thread:spider
+$  tid          tid:spider
+$  input        input:spider
+$  yarn         (list tid)
+$  thread-form  _*eval-form:eval:(strand ,vase)
+$  trie
  $~  [*thread-form ~]
  [=thread-form kid=(map tid trie)]
::
+$  trying  ?(%build %none)
+$  state
  $:  starting=(map yarn [=trying =vase])
      running=trie
      tid=(map tid yarn)
      serving=(map tid [@ta =mark])
  ==
::
+$  clean-slate-any
  $^  clean-slate-ket
  $%  clean-slate-sig
      clean-slate-1
      clean-slate
  ==
::
+$  clean-slate
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
::
+$  start-args
  [parent=(unit tid) use=(unit tid) =beak file=term =vase]
--
::
::  Trie operations
::
~%  %spider  ..card  ~
|%
++  get-yarn
  |=  [=trie =yarn]
  ^-  (unit =thread-form)
  ?~  yarn
    `thread-form.trie
  =/  son  (~(get by kid.trie) i.yarn)
  ?~  son
    ~
  $(trie u.son, yarn t.yarn)
::
++  get-yarn-children
  |=  [=trie =yarn]
  ^-  (list ^yarn)
  ?~  yarn
    (turn (tap-yarn trie) head)
  =/  son  (~(get by kid.trie) i.yarn)
  ?~  son
    ~
  $(trie u.son, yarn t.yarn)
::
::
++  has-yarn
  |=  [=trie =yarn]
  !=(~ (get-yarn trie yarn))
::
++  put-yarn
  |=  [=trie =yarn =thread-form]
  ^+  trie
  ?~  yarn
    trie(thread-form thread-form)
  =/  son  (~(gut by kid.trie) i.yarn [*^thread-form ~])
  %=    trie
      kid
    %+  ~(put by kid.trie)  i.yarn
    $(trie son, yarn t.yarn)
  ==
::
++  del-yarn
  |=  [=trie =yarn]
  ^+  trie
  ?~  yarn
    trie
  |-
  ?~  t.yarn
    trie(kid (~(del by kid.trie) i.yarn))
  =/  son  (~(get by kid.trie) i.yarn)
  ?~  son
    trie
  %=    trie
      kid
    %+  ~(put by kid.trie)  i.yarn
    $(trie u.son, yarn t.yarn)
  ==
::
++  tap-yarn
  |=  =trie
  %-  flop  ::  preorder
  =|  =yarn
  |-  ^-  (list [=^yarn =thread-form])
  %+  welp
    ?~  yarn
      ~
    [(flop yarn) thread-form.trie]~
  =/  kids  ~(tap by kid.trie)
  |-  ^-  (list [=^yarn =thread-form])
  ?~  kids
    ~
  =/  next-1  ^$(yarn [p.i.kids yarn], trie q.i.kids)
  =/  next-2  $(kids t.kids)
  (welp next-1 next-2)
--
::
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
    ?>  ?=(%2 -.any)
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
      ^-  (quip card clean-slate)
      ?>  ?=(?(%1 %2) -.old)
      ?:  ?=(%2 -.old)
        `old
      :-  ~[bind-eyre:sc]
      :*  %2
        starting.old
        running.old
        tid.old
        ~
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
        %spider-input  (on-poke-input:sc !<(input vase))
        %spider-start  (handle-start-thread:sc !<(start-args vase))
        %spider-stop   (handle-stop-thread:sc !<([tid ?] vase))
      ::
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
      ``noun+!>((turn (tap-yarn running.state) head))
    ::
        [%x %starting @ ~]
      ``noun+!>((has-yarn running.state (~(got by tid.state) i.t.t.path)))
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
~%  %spider-helper  ..get-yarn  ~
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
    (~(put by serving.state) tid [eyre-id output-mark])
  ::  TODO: speed this up somehow. we spend about 15ms in this arm alone
  ::
  =+  .^
      =tube:clay
      %cc 
      /(scot %p our.bowl)/[desk]/(scot %da now.bowl)/json/[input-mark]
    ==
  ?>  ?=(^ body.request.inbound-request)
  =/  body=json  (need (de-json:html q.u.body.request.inbound-request))
  =/  input=vase  (slop !>(~) (tube !>(body)))
  =/  boc  bec
  =/  =start-args  [~ `tid boc(q desk) thread input]
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
  ^-  (quip card ^state)
  =/  parent-yarn=yarn
    ?~  parent-tid
      /
    (~(got by tid.state) u.parent-tid)
  =/  new-tid  (fall use (new-thread-id file))
  =/  =yarn  (snoc parent-yarn new-tid)
  ::
  ?:  (has-yarn running.state yarn)
    ~|  [%already-started yarn]
    !!
  ?:  (~(has by starting.state) yarn)
    ~|  [%already-starting yarn]
    !!
  ::
  =:  starting.state  (~(put by starting.state) yarn [%build vase])
      tid.state       (~(put by tid.state) new-tid yarn)
    ==
  =/  pax=path
    ~|  no-file-for-thread+file
    (need (get-fit:clay beak %ted file))
  :_  state
  :_  ~
  :+  %pass  /build/[new-tid]
  [%arvo %c %warp p.beak q.beak ~ %sing %a r.beak pax]
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
  (start-thread yarn p.maybe-thread)
::
++  start-thread
  ~/  %start-thread
  |=  [=yarn =thread]
  ^-  (quip card ^state)
  =/  =vase  vase:(~(got by starting.state) yarn)
  ?<  (has-yarn running.state yarn)
  =/  m  (strand ,^vase)
  =/  res  (mule |.((thread vase)))
  ?:  ?=(%| -.res)
    (thread-fail-not-running (yarn-to-tid yarn) %false-start p.res)
  =/  =eval-form:eval:m
    (from-form:eval:m p.res)
  =:  starting.state  (~(del by starting.state) yarn)
      running.state   (put-yarn running.state yarn eval-form)
    ==
  (take-input yarn ~)
::
++  handle-stop-thread
  |=  [=tid nice=?]
  ^-  (quip card ^state)
  =/  yarn=(unit yarn)  (~(get by tid.state) tid)
  ?~  yarn
    ~&  %stopping-nonexistent-thread
    [~ state]
  ?:  (has-yarn running.state u.yarn)
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
  ?.  (has-yarn running.state yarn)
    %-  (slog leaf+"spider got input for non-existent {<yarn>} 2" ~)
    `state
  =/  =eval-form:eval:m
    thread-form:(need (get-yarn running.state yarn))
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
  =.  running.state  (put-yarn running.state yarn eval-form)
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
  |=  [eyre-id=@ta output=mark]
  :_  state(serving (~(del by serving.state) tid))
  %+  give-simple-payload:app:server  eyre-id
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
  |=  [eyre-id=@ta output=mark]
  =+    .^
      =tube:clay
      %cc
      /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[output]/json
    ==
  :_  state(serving (~(del by serving.state) tid))
  %+  give-simple-payload:app:server  eyre-id
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
    [yarn (get-yarn-children running.state yarn)]
  |-  ^-  (quip card ^state)
  ?~  children
    `state
  =^  cards-children  state  $(children t.children)
  =^  cards-our  state
    =/  =^yarn  i.children
    =/  =tid  (yarn-to-tid yarn)
    =:  running.state  (del-yarn running.state yarn)
        tid.state      (~(del by tid.state) tid)
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
      byk.bowl
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
++  clean-state
  !>  ^-  clean-slate
  2+state(running (turn (tap-yarn running.state) head))
--
