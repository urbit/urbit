/+  libthread=thread, threadio, default-agent
=,  thread=thread:libthread
~!  thread
|%
+$  card      card:agent:mall
+$  state     (map imp-name _*eval-form:[~!(. eval)]:(thread ,~))
+$  imp       $-(bowl:mall _*form:(thread ,~))
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
        %spider-start  (handle-start-imp:sc !<(imp-name vase))
      ==
    [cards this]
  ::
  ++  handle-subscribe       handle-subscribe:def
  ++  handle-unsubscribe     handle-unsubscribe:def
  ++  handle-peek            handle-peek:def
  ++  handle-agent-response  handle-agent-response:def
  ++  handle-arvo-response
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  (handle-arvo-response:def wire sign-arvo)
        [%imp @ *]  (handle-sign:sc i.t.wire t.t.wire sign-arvo)
      ==
    [cards this]
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
++  handle-start-imp
  |=  =imp-name
  ^-  (quip card ^state)
  ?:  (~(has by state) imp-name)
    ~|  [%already-started imp-name]
    !!
  ?.  (~(has by imps) imp-name)
    ~|  [%imp-not-found imp-name]
    !!
  =/  m  (thread ,~)
  =/  =eval-form:eval:m
    %-  from-form:eval:m
    ((~(got by imps) imp-name) bowl)
  =.  state  (~(put by state) imp-name eval-form)
  (take-input imp-name ~)
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
  `(~(del by state) imp-name)
::
++  imp-done
  |=  =imp-name
  ^-  (quip card ^state)
  %-  (slog leaf+"thread {<imp-name>} finished" ~)
  `(~(del by state) imp-name)
::
++  imps
  %-  malt
  ^-  (list [imp-name imp])
  |^
  :~  [%first-imp first-imp]
  ==
  ::
  ++  first-imp
    ^-  imp
    |=  bowl:mall
    =/  m  (thread ,~)
    ^-  form:m
    ;<  ~  bind:m  echo:threadio
    (pure:m ~)
  --
--
