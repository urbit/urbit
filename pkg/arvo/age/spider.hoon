/-  spider
/+  libthread=thread, default-agent
=,  thread=thread:libthread
|%
+$  card      card:agent:mall
+$  imp       imp:spider
+$  imp-name  term
+$  imput     [=imp-name =cage]
+$  state
  $:  started=(map imp-name vase)
      running=(map imp-name _*eval-form:[~!(. eval)]:(thread ,vase))
  ==
--
^-  agent:mall
=|  =state
=<
  |_  =bowl:mall
  +*  this  .
      spider-core  +>
      sc    ~(. spider-core bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   on-save:def
  ++  on-load
    |=  old-state=vase
    =/  runs  ~(tap by running.state)
    |-  ^-  (quip card _this)
    ?~  runs
      `this
    =^  cards-1  state  (handle-stop-imp:sc p.i.runs |)
    =^  cards-2  this   $(runs t.runs)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %spider-imput  (on-poke-imput:sc !<(imput vase))
        %spider-start  (handle-start-imp:sc !<([imp-name term ^vase] vase))
        %spider-stop   (handle-stop-imp:sc !<([imp-name ?] vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    =^  cards  state
      ?+  path  (on-watch:def path)
        [%imp @ *]         (on-watch:sc t.path)
        [%imp-result @ ~]  (on-watch-result:sc i.t.path)
      ==
    [cards this]
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
      [%x %started @ ~]  ``noun+!>((~(has by running.state) i.t.t.path))
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:mall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  !!
        [%imp @ *]  (on-agent:sc i.t.wire t.t.wire sign)
      ==
    [cards this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (on-arvo:def wire sign-arvo)
        [%imp @ *]    (handle-sign:sc i.t.wire t.t.wire sign-arvo)
        [%find @ ~]   (handle-find:sc i.t.wire sign-arvo)
        [%build @ ~]  (handle-build:sc i.t.wire sign-arvo)
      ==
    [cards this]
  ::
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:mall
++  on-poke-imput
  |=  imput
  (take-input imp-name ~ %poke cage)
::
++  on-watch
  |=  [=imp-name =path]
  (take-input imp-name ~ %watch path)
::
++  on-watch-result
  |=  =imp-name
  ^-  (quip card ^state)
  ?>  (~(has by started.state) imp-name)
  `state
::
++  handle-sign
  |=  [=imp-name =wire =sign-arvo]
  (take-input imp-name ~ %sign wire sign-arvo)
::
++  on-agent
  |=  [=imp-name =wire =sign:agent:mall]
  (take-input imp-name ~ %agent wire sign)
::
++  handle-start-imp
  |=  [=imp-name =term =vase]
  ^-  (quip card ^state)
  ?:  (~(has by running.state) imp-name)
    ~|  [%already-started imp-name]
    !!
  ?:  (~(has by started.state) imp-name)
    ~|  [%already-starting imp-name]
    !!
  =.  started.state  (~(put by started.state) imp-name vase)
  =/  =card
    =/  =schematic:ford  [%path [our.bowl %home] %imp term]
    [%pass /find/[imp-name] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-find
  |=  [=imp-name =sign-arvo]
  ^-  (quip card ^state)
  ?>  (~(has by started.state) imp-name)
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    %-  (slog leaf+"{<imp-name>} find incomplete" tang.result.sign-arvo)
    `state
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    %-  (slog leaf+"{<imp-name>} find error" message.build-result)
    `state
  ?.  ?=([%path *] +.build-result)
    %-  (slog leaf+"{<imp-name>} find strange" ~)
    `state
  =/  =card
    =/  =schematic:ford  [%core rail.build-result]
    [%pass /build/[imp-name] %arvo %f %build live=%.y schematic]
  [[card ~] state]
::
++  handle-build
  |=  [=imp-name =sign-arvo]
  ^-  (quip card ^state)
  ?>  (~(has by started.state) imp-name)
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    %-  (slog leaf+"{<imp-name>} build incomplete" tang.result.sign-arvo)
    `state
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    %-  (slog leaf+"{<imp-name>} build error" message.build-result)
    `state
  =/  =cage  (result-to-cage:ford build-result)
  ?.  ?=(%noun p.cage)
    %-  (slog leaf+"{<imp-name>} build not noun, is {<p.cage>}" ~)
    `state
  =/  maybe-imp  (mule |.(!<(imp q.cage)))
  ?:  ?=(%| -.maybe-imp)
    %-  (slog leaf+"{<imp-name>} not valid imp" p.maybe-imp)
    `state
  (start-imp imp-name p.maybe-imp)
::
++  start-imp
  |=  [=imp-name =imp]
  ^-  (quip card ^state)
  =/  =vase  (~(got by started.state) imp-name)
  =^  cards-1  state
    ?.  (~(has by running.state) imp-name)
      `state
    (imp-fail imp-name %updated ~)
  =/  m  (thread ,^vase)
  =/  =eval-form:eval:m  (from-form:eval:m (imp bowl vase))
  =.  running.state  (~(put by running.state) imp-name eval-form)
  =^  cards-2  state
    (take-input imp-name ~)
  [(weld cards-1 cards-2) state]
::
++  handle-stop-imp
  |=  [=imp-name nice=?]
  ^-  (quip card ^state)
  ~?  !(~(has by running.state) imp-name)
    [%not-started imp-name]
  ?:  nice
    (imp-done imp-name *vase)
  (imp-fail imp-name %cancelled ~)
::
++  take-input
  |=  [=imp-name input=(unit input:thread)]
  ^-  (quip card ^state)
  =/  m  (thread ,vase)
  ?.  (~(has by running.state) imp-name)
    %-  (slog leaf+"spider got input for non-existent {<imp-name>}" ~)
    `state
  =/  =eval-form:eval:m
    (~(got by running.state) imp-name)
  =|  cards=(list card)
  |-  ^-  (quip card ^state)
  =^  r=[cards=(list card) =eval-result:eval:m]  eval-form
    =/  out
      %-  mule  |.
      (take:eval:m eval-form bowl input)
    ?-  -.out
      %&  p.out
      %|  [[~ [%fail %crash p.out]] eval-form]
    ==
  =.  running.state  (~(put by running.state) imp-name eval-form)
  =.  cards.r
    %+  turn  cards.r
    |=  =card
    ^-  ^card
    ?+  card  card
        [%pass * *]  [%pass [%imp imp-name p.card] q.card]
        [%give %fact *]
      ?~  path.p.card
        card
      card(path.p `[%imp imp-name u.path.p.card])
    ::
        [%give %kick *]
      ?~  path.p.card
        card
      card(path.p `[%imp imp-name u.path.p.card])
    ==
  =.  cards  (weld cards cards.r)
  =^  final-cards=(list card)  state
    ?-  -.eval-result.r
      %next  `state
      %fail  (imp-fail imp-name err.eval-result.r)
      %done  (imp-done imp-name value.eval-result.r)
    ==
  [(weld cards final-cards) state]
::
++  imp-fail
  |=  [=imp-name =term =tang]
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp-name>} failed" leaf+<term> tang)
  =/  fail-cards=(list card)
    :~  [%give %fact `/imp-result/[imp-name] %imp-fail !>([term tang])]
        [%give %kick `/imp-result/[imp-name] ~]
    ==
  =^  cards  state  (imp-clean imp-name)
  [(weld fail-cards cards) state]
::
++  imp-done
  |=  [=imp-name =vase]
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp-name>} finished" (sell vase) ~)
  =/  done-cards=(list card)
    :~  [%give %fact `/imp-result/[imp-name] %imp-done vase]
        [%give %kick `/imp-result/[imp-name] ~]
    ==
  =^  cards  state  (imp-clean imp-name)
  [(weld done-cards cards) state]
::
++  imp-clean
  |=  =imp-name
  ^-  (quip card ^state)
  =.  started.state  (~(del by started.state) imp-name)
  =.  running.state  (~(del by running.state) imp-name)
  :_  state
  :-  [%pass /build/[imp-name] %arvo %f %kill ~]
  %+  murn  ~(tap by wex.bowl)
  |=  [[=wire =ship =term] [acked=? =path]]
  ^-  (unit card)
  ?.  ?&  ?=([%imp @ *] wire)
          =(imp-name i.t.wire)
      ==
    ~
  `[%pass wire %agent [ship term] %leave ~]
--
