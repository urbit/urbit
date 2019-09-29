/-  spider
/+  libthread=thread, default-agent
=,  thread=thread:libthread
|%
+$  card      card:agent:mall
+$  state     (map imp-name _*eval-form:[~!(. eval)]:(thread ,~))
+$  imp       imp:spider
+$  imp-name  term
+$  imput     [=imp-name =cage]
--
^-  agent:mall
=;  spider-core
  =|  =state
  |_  =bowl:mall
  +*  this  .
      sc  ~(. spider-core bowl state)
      def  ~(. default-agent bowl this)
  ::
  ++  handle-init            handle-init:def
  ++  handle-extract-state   handle-extract-state:def
  ++  handle-upgrade-state   handle-upgrade-state:def
  ++  handle-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (handle-poke:def mark vase)
        %spider-imput  (handle-poke-imput:sc !<(imput vase))
        %spider-start  (handle-start-imp:sc !<([imp-name term] vase))
        %spider-stop   (handle-stop-imp:sc !<(imp-name vase))
      ==
    [cards this]
  ::
  ++  handle-subscribe       handle-subscribe:def
  ++  handle-unsubscribe     handle-unsubscribe:def
  ++  handle-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (handle-peek:def path)
      [%x %started @ ~]  ``noun+!>((~(has by state) i.t.t.path))
    ==
  ::
  ++  handle-agent-response
    |=  [=wire =gift:agent:mall]
    ^-  (quip card _this)
    =^  cards  state
      ?+    wire  !!
        [%imp @ *]  (handle-agent-response:sc i.t.wire t.t.wire gift)
      ==
    [cards this]
  ::
  ++  handle-arvo-response
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (handle-arvo-response:def wire sign-arvo)
        [%imp @ *]    (handle-sign:sc i.t.wire t.t.wire sign-arvo)
        [%find @ ~]   (handle-find:sc i.t.wire sign-arvo)
        [%build @ ~]  (handle-build:sc i.t.wire sign-arvo)
      ==
    [cards this]
  ::
  ++  handle-error           handle-error:def
  --
::
|_  [=bowl:mall =state]
++  handle-poke-imput
  |=  imput
  (take-input imp-name ~ %poke cage)
::
++  handle-sign
  |=  [=imp-name =wire =sign-arvo]
  (take-input imp-name ~ %sign wire sign-arvo)
::
++  handle-agent-response
  |=  [=imp-name =wire =gift:agent:mall]
  (take-input imp-name ~ %agent wire gift)
::
++  handle-start-imp
  |=  [=imp-name =term]
  ^-  (quip card ^state)
  ?:  (~(has by state) imp-name)
    ~|  [%already-started imp-name]
    !!
  =/  =card
    =/  =schematic:ford  [%path [our.bowl %home] %imp term]
    [%pass /find/[imp-name] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-find
  |=  [=imp-name =sign-arvo]
  ^-  (quip card ^state)
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
  =/  m  (thread ,~)
  =/  =eval-form:eval:m  (from-form:eval:m (imp bowl))
  =.  state  (~(put by state) imp-name eval-form)
  (take-input imp-name ~)
::
++  handle-stop-imp
  |=  =imp-name
  ^-  (quip card ^state)
  ?.  (~(has by state) imp-name)
    ~|  [%not-started imp-name]
    !!
  (imp-fail imp-name %cancelled ~)
::
++  take-input
  |=  [=imp-name input=(unit input:thread)]
  ^-  (quip card ^state)
  =/  m  (thread ,~)
  =/  =eval-form:eval:m
    ~|  [%no-imp imp-name]
    (~(got by state) imp-name)
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
  =.  state  (~(put by state) imp-name eval-form)
  =.  cards.r
    %+  turn  cards.r
    |=  =card
    ^-  ^card
    ?+  card  card
        [%pass * *]  [%pass [%imp imp-name p.card] q.card]
        [%give %subscription-update *]
      ?~  path.p.card
        card
      card(path.p `[%imp imp-name u.path.p.card])
    ::
        [%give %subscription-close *]
      ?~  path.p.card
        card
      card(path.p `[%imp imp-name u.path.p.card])
    ==
  =.  cards  (weld cards cards.r)
  =^  final-cards=(list card)  state
    ?-  -.eval-result.r
      %next  `state
      %fail  (imp-fail imp-name err.eval-result.r)
      %done  (imp-done imp-name)
    ==
  [(weld cards final-cards) state]
::
++  imp-fail
  |=  [=imp-name =term =tang]
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp-name>} failed" leaf+<term> tang)
  :-  [%pass /build/[imp-name] %arvo %f %kill ~]~
  (~(del by state) imp-name)
::
++  imp-done
  |=  =imp-name
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp-name>} finished" ~)
  :-  [%pass /build/[imp-name] %arvo %f %kill ~]~
  (~(del by state) imp-name)
--
