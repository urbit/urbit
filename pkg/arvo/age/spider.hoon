/-  spider
/+  libthread=thread, default-agent, verb
=,  thread=thread:libthread
|%
+$  card        card:agent:mall
+$  imp-thread  imp:spider
+$  iid         iid:spider
+$  imput       imput:spider
+$  imp         (list iid)
+$  imp-form    _*eval-form:eval:(thread ,vase)
+$  trie
  $~  [*imp-form ~]
  [=imp-form kid=(map iid trie)]
::
+$  trying  ?(%find %build %none)
+$  state
  $:  starting=(map imp [=trying =vase])
      running=trie
      iid=(map iid imp)
  ==
::
+$  clean-slate
  $:  starting=(map imp [=trying =vase])
      running=(list imp)
      iid=(map iid imp)
  ==
::
+$  start-args
  [parent=(unit iid) use=(unit iid) file=term =vase]
--
::
::  Trie operations
::
|%
++  get-imp
  |=  [=trie =imp]
  ^-  (unit =imp-form)
  ?~  imp
    `imp-form.trie
  =/  son  (~(get by kid.trie) i.imp)
  ?~  son
    ~
  $(trie u.son, imp t.imp)
::
++  get-imp-children
  |=  [=trie =imp]
  ^-  (list ^imp)
  ?~  imp
    (turn (tap-imp trie) head)
  =/  son  (~(get by kid.trie) i.imp)
  ?~  son
    ~
  $(trie u.son, imp t.imp)
::
::
++  has-imp
  |=  [=trie =imp]
  !=(~ (get-imp trie imp))
::
++  put-imp
  |=  [=trie =imp =imp-form]
  ^+  trie
  ?~  imp
    trie(imp-form imp-form)
  =/  son  (~(gut by kid.trie) i.imp [*^imp-form ~])
  %=    trie
      kid
    %+  ~(put by kid.trie)  i.imp
    $(trie son, imp t.imp)
  ==
::
++  del-imp
  |=  [=trie =imp]
  ^+  trie
  ?~  imp
    trie
  |-
  ?~  t.imp
    trie(kid (~(del by kid.trie) i.imp))
  =/  son  (~(get by kid.trie) i.imp)
  ?~  son
    trie
  %=    trie
      kid
    %+  ~(put by kid.trie)  i.imp
    $(trie u.son, imp t.imp)
  ==
::
++  tap-imp
  =|  =imp
  |=  =trie
  ^-  (list [=^imp =imp-form])
  %+  welp
    ?~  imp
      ~
    [(flop imp) imp-form.trie]~
  =/  kids  ~(tap by kid.trie)
  |-  ^-  (list [=^imp =imp-form])
  ?~  kids
    ~
  =/  next-1  ^$(imp [p.i.kids imp], trie q.i.kids)
  =/  next-2  $(kids t.kids)
  (welp next-1 next-2)
--
::
^-  agent:mall
=|  =state
=<
  %+  verb  |
  |_  =bowl:mall
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
    =.  iid.state  iid.clean-slate
    =/  imps=(list imp)
      %+  welp  running.clean-slate
      ~(tap in ~(key by starting.clean-slate))
    |-  ^-  (quip card _this)
    ?~  imps
      `this
    =^  cards-1  state
      (handle-stop-imp:sc (imp-to-iid i.imps) |)
    =^  cards-2  this
      $(imps t.imps)
    [(weld cards-1 cards-2) this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %spider-imput  (on-poke-imput:sc !<(imput vase))
        %spider-start  (handle-start-imp:sc !<(start-args vase))
        %spider-stop   (handle-stop-imp:sc !<([iid ?] vase))
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
    ?+    path  (on-peek:def path)
        [%x %tree ~]
      ``noun+!>((turn (tap-imp running.state) head))
    ::
        [%x %starting @ ~]
      ``noun+!>((has-imp running.state (~(got by iid.state) i.t.t.path)))
    ::
        [%x %saxo @ ~]
      ``noun+!>((~(got by iid.state) i.t.t.path))
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
  ::  On unexpected failure, kill all outstanding threads
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card _this)
    %-  (slog leaf+"spider crashed, killing all threads: {<term>}" tang)
    (on-load on-save)
  --
::
|_  =bowl:mall
++  on-poke-imput
  |=  imput
  =/  imp  (~(got by iid.state) iid)
  (take-input imp ~ %poke cage)
::
++  on-watch
  |=  [=iid =path]
  (take-input (~(got by iid.state) iid) ~ %watch path)
::
++  on-watch-result
  |=  =iid
  ^-  (quip card ^state)
  `state
::
++  handle-sign
  |=  [=iid =wire =sign-arvo]
  =/  imp  (~(get by iid.state) iid)
  ?~  imp
    %-  (slog leaf+"spider got sign for non-existent {<iid>}" ~)
    `state
  (take-input u.imp ~ %sign wire sign-arvo)
::
++  on-agent
  |=  [=iid =wire =sign:agent:mall]
  =/  imp  (~(get by iid.state) iid)
  ?~  imp
    %-  (slog leaf+"spider got agent for non-existent {<iid>}" ~)
    `state
  (take-input u.imp ~ %agent wire sign)
::
++  handle-start-imp
  |=  [parent-iid=(unit iid) use=(unit iid) file=term =vase]
  ^-  (quip card ^state)
  =/  parent-imp=imp
    ?~  parent-iid
      /
    (~(got by iid.state) u.parent-iid)
  =/  new-iid  (fall use (scot %uv (sham eny.bowl)))
  =/  =imp  (snoc parent-imp new-iid)
  ::
  ?:  (has-imp running.state imp)
    ~|  [%already-started imp]
    !!
  ?:  (~(has by starting.state) imp)
    ~|  [%already-starting imp]
    !!
  ::
  =:  starting.state  (~(put by starting.state) imp [%find vase])
      iid.state       (~(put by iid.state) new-iid imp)
    ==
  =/  =card
    =/  =schematic:ford  [%path [our.bowl %home] %imp file]
    [%pass /find/[new-iid] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-find
  |=  [=iid =sign-arvo]
  ^-  (quip card ^state)
  =/  =imp  (~(got by iid.state) iid)
  =.  starting.state
    (~(jab by starting.state) imp |=([=trying =vase] [%none vase]))
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    (imp-fail-not-running iid %find-imp-incomplete tang.result.sign-arvo)
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    (imp-fail-not-running iid %find-imp-error message.build-result)
  ?.  ?=([%path *] +.build-result)
    (imp-fail-not-running iid %find-imp-strange ~)
  =.  starting.state
    (~(jab by starting.state) imp |=([=trying =vase] [%build vase]))
  =/  =card
    =/  =schematic:ford  [%core rail.build-result]
    [%pass /build/[iid] %arvo %f %build live=%.n schematic]
  [[card ~] state]
::
++  handle-build
  |=  [=iid =sign-arvo]
  ^-  (quip card ^state)
  =/  =imp  (~(got by iid.state) iid)
  =.  starting.state
    (~(jab by starting.state) imp |=([=trying =vase] [%none vase]))
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    (imp-fail-not-running iid %build-imp-incomplete tang.result.sign-arvo)
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    (imp-fail-not-running iid %build-imp-error message.build-result)
  =/  =cage  (result-to-cage:ford build-result)
  ?.  ?=(%noun p.cage)
    (imp-fail-not-running iid %build-imp-strange >p.cage< ~)
  =/  maybe-imp  (mule |.(!<(imp-thread q.cage)))
  ?:  ?=(%| -.maybe-imp)
    (imp-fail-not-running iid %imp-not-imp ~)
  (start-imp imp p.maybe-imp)
::
++  start-imp
  |=  [=imp =imp-thread]
  ^-  (quip card ^state)
  =/  =vase  vase:(~(got by starting.state) imp)
  ?<  (has-imp running.state imp)
  =/  m  (thread ,^vase)
  =/  =bowl:spider  (convert-bowl imp bowl)
  =/  res  (mule |.((imp-thread bowl vase)))
  ?:  ?=(%| -.res)
    (imp-fail-not-running (imp-to-iid imp) %false-start p.res)
  =/  =eval-form:eval:m
    (from-form:eval:m p.res)
  =:  starting.state  (~(del by starting.state) imp)
      running.state   (put-imp running.state imp eval-form)
    ==
  (take-input imp ~)
::
++  handle-stop-imp
  |=  [=iid nice=?]
  ^-  (quip card ^state)
  =/  =imp  (~(got by iid.state) iid)
  ?:  (has-imp running.state imp)
    ?:  nice
      (imp-done imp *vase)
    (imp-fail imp %cancelled ~)
  ?:  (~(has by starting.state) imp)
    (imp-fail-not-running iid %stopped-before-started ~)
  ~&  [%imp-not-started imp]
  `state
::
++  take-input
  |=  [=imp input=(unit input:thread)]
  ^-  (quip card ^state)
  =/  m  (thread ,vase)
  ?.  (has-imp running.state imp)
    %-  (slog leaf+"spider got input for non-existent {<imp>} 2" ~)
    `state
  =/  =eval-form:eval:m
    imp-form:(need (get-imp running.state imp))
  =|  cards=(list card)
  |-  ^-  (quip card ^state)
  =^  r=[cards=(list card) =eval-result:eval:m]  eval-form
    =/  out
      %-  mule  |.
      (take:eval:m eval-form (convert-bowl imp bowl) input)
    ?-  -.out
      %&  p.out
      %|  [[~ [%fail %crash p.out]] eval-form]
    ==
  =.  running.state  (put-imp running.state imp eval-form)
  =/  =iid  (imp-to-iid imp)
  =.  cards.r
    %+  turn  cards.r
    |=  =card
    ^-  ^card
    ?+  card  card
        [%pass * *]  [%pass [%imp iid p.card] q.card]
        [%give %fact *]
      ?~  path.p.card
        card
      card(path.p `[%imp iid u.path.p.card])
    ::
        [%give %kick *]
      ?~  path.p.card
        card
      card(path.p `[%imp iid u.path.p.card])
    ==
  =.  cards  (weld cards cards.r)
  =^  final-cards=(list card)  state
    ?-  -.eval-result.r
      %next  `state
      %fail  (imp-fail imp err.eval-result.r)
      %done  (imp-done imp value.eval-result.r)
    ==
  [(weld cards final-cards) state]
::
++  imp-fail-not-running
  |=  [=iid =term =tang]
  =/  =imp  (~(got by iid.state) iid)
  :_  state(starting (~(del by starting.state) imp))
  %-  welp  :_  (imp-say-fail iid term tang)
  =/  =trying  trying:(~(got by starting.state) imp)
  ?-  trying
    %find   [%pass /find/[iid] %arvo %f %kill ~]~
    %build  [%pass /build/[iid] %arvo %f %kill ~]~
    %none   ~
  ==
::
++  imp-say-fail
  |=  [=iid =term =tang]
  ^-  (list card)
  :~  [%give %fact `/imp-result/[iid] %imp-fail !>([term tang])]
      [%give %kick `/imp-result/[iid] ~]
  ==
::
++  imp-fail
  |=  [=imp =term =tang]
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp>} failed" leaf+<term> tang)
  =/  =iid  (imp-to-iid imp)
  =/  fail-cards  (imp-say-fail iid term tang)
  =^  cards  state  (imp-clean imp)
  [(weld fail-cards cards) state]
::
++  imp-done
  |=  [=imp =vase]
  ^-  (quip card ^state)
  ::  %-  (slog leaf+"thread {<imp>} finished" (sell vase) ~)
  =/  =iid  (imp-to-iid imp)
  =/  done-cards=(list card)
    :~  [%give %fact `/imp-result/[iid] %imp-done vase]
        [%give %kick `/imp-result/[iid] ~]
    ==
  =^  cards  state  (imp-clean imp)
  [(weld done-cards cards) state]
::
++  imp-clean
  |=  =imp
  ^-  (quip card ^state)
  =/  children=(list ^imp)
    [imp (get-imp-children running.state imp)]
  |-  ^-  (quip card ^state)
  ?~  children
    `state
  =^  cards-children  state  $(children t.children)
  =^  cards-our  state
    =/  =^imp  i.children
    =/  =iid  (imp-to-iid imp)
    =:  running.state  (del-imp running.state imp)
        iid.state      (~(del by iid.state) iid)
      ==
    :_  state
    %+  murn  ~(tap by wex.bowl)
    |=  [[=wire =ship =term] [acked=? =path]]
    ^-  (unit card)
    ?.  ?&  ?=([%imp @ *] wire)
            =(iid i.t.wire)
        ==
      ~
    `[%pass wire %agent [ship term] %leave ~]
  [(welp cards-children cards-our) state]
::
++  convert-bowl
  |=  [=imp =bowl:mall]
  ^-  bowl:spider
  :*  our.bowl
      src.bowl
      (imp-to-iid imp)
      (imp-to-parent imp)
      wex.bowl
      sup.bowl
      eny.bowl
      now.bowl
      byk.bowl
  ==
::
++  imp-to-iid
  |=  =imp
  ^-  iid
  =/  fimp  (flop imp)
  ?>  ?=([@ *] fimp)
  i.fimp
::
++  imp-to-parent
  |=  =imp
  ^-  (unit iid)
  =/  fimp  (flop imp)
  ?>  ?=([@ *] fimp)
  ?~  t.fimp
    ~
  `i.t.fimp
::
++  clean-state
  !>  ^-  clean-slate
  state(running (turn (tap-imp running.state) head))
--
