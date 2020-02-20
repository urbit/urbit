/-  spider
/+  libstrand=strand, default-agent, verb
=,  strand=strand:libstrand
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
+$  trying  ?(%find %build %none)
+$  state
  $:  starting=(map yarn [=trying =vase])
      running=trie
      tid=(map tid yarn)
  ==
::
+$  clean-slate
  $:  starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
  ==
::
+$  start-args
  [parent=(unit tid) use=(unit tid) file=term =vase]
--
::
::  Trie operations
::
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
  =|  =yarn
  |=  =trie
  ^-  (list [=^yarn =thread-form])
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
  |_  =bowl:gall
  +*  this         .
      spider-core  +>
      sc           ~(. spider-core bowl)
      def          ~(. (default-agent this %|) bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   clean-state:sc
  ++  on-load
    |=  old-state=vase
    =+  !<(=clean-slate old-state)
    =.  tid.state  tid.clean-slate
    =/  yarns=(list yarn)
      %+  welp  running.clean-slate
      ~(tap in ~(key by starting.clean-slate))
    |-  ^-  (quip card _this)
    ?~  yarns
      `this
    =^  cards-1  state
      (handle-stop-thread:sc (yarn-to-tid i.yarns) |)
    =^  cards-2  this
      $(yarns t.yarns)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %spider-input  (on-poke-input:sc !<(input vase))
        %spider-start  (handle-start-thread:sc !<(start-args vase))
        %spider-stop   (handle-stop-thread:sc !<([tid ?] vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      ?+  path  (on-watch:def path)
        [%thread @ *]         (on-watch:sc t.path)
        [%thread-result @ ~]  (on-watch-result:sc i.t.path)
      ==
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
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
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  !!
        [%thread @ *]  (on-agent:sc i.t.wire t.t.wire sign)
      ==
    [cards this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (on-arvo:def wire sign-arvo)
        [%thread @ *]  (handle-sign:sc i.t.wire t.t.wire sign-arvo)
        [%find @ ~]    (handle-find:sc i.t.wire sign-arvo)
        [%build @ ~]   (handle-build:sc i.t.wire sign-arvo)
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
|_  =bowl:gall
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
  |=  [parent-tid=(unit tid) use=(unit tid) file=term =vase]
  ^-  (quip card ^state)
  =/  parent-yarn=yarn
    ?~  parent-tid
      /
    (~(got by tid.state) u.parent-tid)
  =/  new-tid  (fall use (scot %uv (sham eny.bowl)))
  =/  =yarn  (snoc parent-yarn new-tid)
  ::
  ?:  (has-yarn running.state yarn)
    ~|  [%already-started yarn]
    !!
  ?:  (~(has by starting.state) yarn)
    ~|  [%already-starting yarn]
    !!
  ::
  =:  starting.state  (~(put by starting.state) yarn [%find vase])
      tid.state       (~(put by tid.state) new-tid yarn)
    ==
  =/  =card
    =/  =schematic:ford  [%path [our.bowl %home] %ted file]
    [%pass /find/[new-tid] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-find
  |=  [=tid =sign-arvo]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%none vase]))
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    (thread-fail-not-running tid %find-thread-incomplete tang.result.sign-arvo)
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    (thread-fail-not-running tid %find-thread-error message.build-result)
  ?.  ?=([%path *] +.build-result)
    (thread-fail-not-running tid %find-thread-strange ~)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%build vase]))
  =/  =card
    =/  =schematic:ford  [%core rail.build-result]
    [%pass /build/[tid] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-build
  |=  [=tid =sign-arvo]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%none vase]))
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    (thread-fail-not-running tid %build-thread-incomplete tang.result.sign-arvo)
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    (thread-fail-not-running tid %build-thread-error message.build-result)
  =/  =cage  (result-to-cage:ford build-result)
  ?.  ?=(%noun p.cage)
    (thread-fail-not-running tid %build-thread-strange >p.cage< ~)
  =/  maybe-thread  (mule |.(!<(thread q.cage)))
  ?:  ?=(%| -.maybe-thread)
    (thread-fail-not-running tid %thread-not-thread ~)
  (start-thread yarn p.maybe-thread)
::
++  start-thread
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
  =/  =yarn  (~(got by tid.state) tid)
  ?:  (has-yarn running.state yarn)
    ?:  nice
      (thread-done yarn *vase)
    (thread-fail yarn %cancelled ~)
  ?:  (~(has by starting.state) yarn)
    (thread-fail-not-running tid %stopped-before-started ~)
  ~&  [%thread-not-started yarn]
  ?:  nice
    (thread-done yarn *vase)
  (thread-fail yarn %cancelled ~)
::
++  take-input
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
  =/  =yarn  (~(got by tid.state) tid)
  :_  state(starting (~(del by starting.state) yarn))
  %-  welp  :_  (thread-say-fail tid term tang)
  =/  =trying  trying:(~(got by starting.state) yarn)
  ?-  trying
    %find   [%pass /find/[tid] %arvo %f %kill ~]~
    %build  [%pass /build/[tid] %arvo %f %kill ~]~
    %none   ~
  ==
::
++  thread-say-fail
  |=  [=tid =term =tang]
  ^-  (list card)
  :~  [%give %fact ~[/thread-result/[tid]] %thread-fail !>([term tang])]
      [%give %kick ~[/thread-result/[tid]] ~]
  ==
::
++  thread-fail
  |=  [=yarn =term =tang]
  ^-  (quip card ^state)
  %-  (slog leaf+"strand {<yarn>} failed" leaf+<term> tang)
  =/  =tid  (yarn-to-tid yarn)
  =/  fail-cards  (thread-say-fail tid term tang)
  =^  cards  state  (thread-clean yarn)
  [(weld fail-cards cards) state]
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
  =^  cards  state  (thread-clean yarn)
  [(weld done-cards cards) state]
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
  state(running (turn (tap-yarn running.state) head))
--
