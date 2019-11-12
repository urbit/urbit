/-  spider
/+  libthread=thread, default-agent, verb
=,  thread=thread:libthread
|%
+$  card        card:agent:mall
+$  imp-thread  imp:spider
+$  pid         @udpid
+$  iid         iid:spider
+$  imp         (list pid)
+$  imput       [=imp =cage]
+$  imp-form    _*eval-form:eval:(thread ,vase)
+$  trie
  $~  [*imp-form ~]
  [=imp-form kid=(map pid trie)]
::
+$  state
  $:  started=(map imp vase)
      running=trie
      iid=(map iid imp)
      count=pid
  ==
::
+$  clean-slate
  $:  started=(map imp vase)
      running=(list imp)
      iid=(map iid imp)
      count=pid
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
    [imp imp-form.trie]~
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
  %+  verb  &
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
    =.  count.state  count.clean-slate
    =.  iid.state  iid.clean-slate
    |-  ^-  (quip card _this)
    ?~  running.clean-slate
      `this
    =^  cards-1  state
      (handle-stop-imp:sc (imp-to-iid i.running.clean-slate) |)
    =^  cards-2  this
      $(running.clean-slate t.running.clean-slate)
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
        [%next-iid ~]      on-watch-next-iid
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
        [%x %started @ ~]
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
  ::
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:mall
++  on-poke-imput
  |=  imput
  (take-input imp ~ %poke cage)
::
++  on-watch
  |=  [=iid =path]
  (take-input (~(got by iid.state) iid) ~ %watch path)
::
++  on-watch-result
  |=  =iid
  ^-  (quip card ^state)
  ?>  (lth (slav %ud iid) count.state) ::  (~(has by started.state) (~(got by iid.state) iid))
  `state
::
++  on-watch-next-iid
  ^-  (quip card ^state)
  :_  state(count +(count.state))
  :~  [%give %fact ~ %iid !>((scot %ud count.state))]
      [%give %kick ~ ~]
  ==
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
  =^  new-iid  count.state
    ?~  use
      [(scot %ud count.state) +(count.state)]
    [u.use count.state]
  =/  =imp  (snoc parent-imp (slav %ud new-iid))
  ::
  ?:  (has-imp running.state imp)
    ~|  [%already-started imp]
    !!
  ?:  (~(has by started.state) imp)
    ~|  [%already-starting imp]
    !!
  ::
  =:  started.state  (~(put by started.state) imp vase)
      iid.state      (~(put by iid.state) new-iid imp)
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
  ?>  (~(has by started.state) imp)
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    %-  (slog leaf+"{<imp>} find incomplete" tang.result.sign-arvo)
    `state
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    %-  (slog leaf+"{<imp>} find error" message.build-result)
    `state
  ?.  ?=([%path *] +.build-result)
    %-  (slog leaf+"{<imp>} find strange" ~)
    `state
  =/  =card
    =/  =schematic:ford  [%core rail.build-result]
    [%pass /build/[iid] %arvo %f %build live=%.y schematic]
  [[card ~] state]
::
++  handle-build
  |=  [=iid =sign-arvo]
  ^-  (quip card ^state)
  =/  =imp  (~(got by iid.state) iid)
  ?>  (~(has by started.state) imp)
  ?>  ?=([%f %made *] sign-arvo)
  ?:  ?=(%incomplete -.result.sign-arvo)
    %-  (slog leaf+"{<imp>} build incomplete" tang.result.sign-arvo)
    `state
  =/  =build-result:ford  build-result.result.sign-arvo
  ?:  ?=(%error -.build-result)
    %-  (slog leaf+"{<imp>} build error" message.build-result)
    `state
  =/  =cage  (result-to-cage:ford build-result)
  ?.  ?=(%noun p.cage)
    %-  (slog leaf+"{<imp>} build not noun, is {<p.cage>}" ~)
    `state
  =/  maybe-imp  (mule |.(!<(imp-thread q.cage)))
  ?:  ?=(%| -.maybe-imp)
    %-  (slog leaf+"{<imp>} not valid imp" p.maybe-imp)
    `state
  (start-imp imp p.maybe-imp)
::
++  start-imp
  |=  [=imp =imp-thread]
  ^-  (quip card ^state)
  =/  =vase  (~(got by started.state) imp)
  =^  cards-1  state
    ?.  (has-imp running.state imp)
      `state
    (imp-fail imp %updated ~)
  =/  m  (thread ,^vase)
  =/  =bowl:spider  (convert-bowl imp bowl)
  =/  =eval-form:eval:m
    (from-form:eval:m (imp-thread bowl vase))
  =.  running.state  (put-imp running.state imp eval-form)
  =^  cards-2  state
    (take-input imp ~)
  [(weld cards-1 cards-2) state]
::
++  handle-stop-imp
  |=  [=iid nice=?]
  ^-  (quip card ^state)
  =/  =imp  (~(got by iid.state) iid)
  ~?  !(has-imp running.state imp)
    [%not-started imp]
  ?:  nice
    (imp-done imp *vase)
  (imp-fail imp %cancelled ~)
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
++  imp-fail
  |=  [=imp =term =tang]
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp>} failed" leaf+<term> tang)
  =/  =iid  (imp-to-iid imp)
  =/  fail-cards=(list card)
    :~  [%give %fact `/imp-result/[iid] %imp-fail !>([term tang])]
        [%give %kick `/imp-result/[iid] ~]
    ==
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
    =:  started.state  (~(del by started.state) imp)
        running.state  (del-imp running.state imp)
        iid.state      (~(del by iid.state) iid)
      ==
    :_  state
    :-  [%pass /build/[iid] %arvo %f %kill ~]
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
  (scot %ud i.fimp)
::
++  imp-to-parent
  |=  =imp
  ^-  (unit iid)
  =/  fimp  (flop imp)
  ?>  ?=([@ *] fimp)
  ?~  t.fimp
    ~
  `(scot %ud i.t.fimp)
::
++  clean-state
  !>  ^-  clean-slate
  state(running (turn (tap-imp running.state) head))
--
