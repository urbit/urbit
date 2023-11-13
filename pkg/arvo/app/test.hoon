/+  default-agent
!:
|%
+$  card  card:agent:gall
+$  test  ?(%agents %marks %generators)
+$  state
  $:  app=(set path)
      app-ok=?
      mar=(set path)
      mar-ok=?
      gen=(set path)
      gen-ok=?
  ==
--
=,  format
^-  agent:gall
=|  =state
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ^-  [(list card) _this]
  ?>  (team:title [our src]:bowl)
  |^
  =+  !<(=test vase)
  ?-  test
    %marks   test-marks
    %agents  test-agents
    %generators  test-generators
  ==
  ::
  ++  test-marks
    =|  fex=(list card)
    ^+  [fex this]
    ?>  =(~ mar.state)
    =.  mar-ok.state  %.y
    =+  .^(paz=(list path) ct+(en-beam now-beak /mar))
    |-  ^+  [fex this]
    ?~  paz  [(flop fex) this]
    =/  xap=path  (flop i.paz)
    ?.  ?=([%hoon *] xap)
      $(paz t.paz)
    =/  mak=^mark
      %-  crip
      %+  turn  (tail (spud (tail (flop (tail xap)))))
      |=(c=@tD `@tD`?:(=('/' c) '-' c))
    =/  sing=card
      :+  %pass  /build/mar/[mak]
      [%arvo %c %warp our.bowl q.byk.bowl ~ %sing %b da+now.bowl /[mak]]
    %_  $
      paz        t.paz
      fex        [sing fex]
      mar.state  (~(put in mar.state) /mar/[mak])
    ==
  ::
  ++  test-agents
    =|  fex=(list card)
    ^+  [fex this]
    ?>  =(~ app.state)
    =.  app-ok.state  %.y
    =+  .^(app-arch=arch cy+(en-beam now-beak /app))
    =/  daz  (sort ~(tap in ~(key by dir.app-arch)) |=((pair) !(aor p q)))
    |-  ^+  [fex this]
    ?~  daz  [fex this]
    =/  dap-pax=path  /app/[i.daz]/hoon
    =/  dap-arch  .^(arch cy+(en-beam now-beak dap-pax))
    ?~  fil.dap-arch
      $(daz t.daz)
    =/  sing=card
      :+  %pass  /build/app/[i.daz]
      [%arvo %c %warp our.bowl q.byk.bowl ~ %sing %a da+now.bowl dap-pax]
    %_  $
      daz        t.daz
      fex        [sing fex]
      app.state  (~(put in app.state) /app/[i.daz])
    ==
  ::
  ++  test-generators
    =|  fex=(list card)
    ^+  [fex this]
    ?>  =(~ gen.state)
    =.  gen-ok.state  %.y
    =+  .^(paz=(list path) ct+(en-beam now-beak /gen))
    |-  ^+  [fex this]
    ?~  paz  [(flop fex) this]
    =/  xap=path  (flop i.paz)
    ?.  ?=([%hoon *] xap)
      $(paz t.paz)
    =/  sing=card
      :+  %pass  build+i.paz
      [%arvo %c %warp our.bowl q.byk.bowl ~ %sing %a da+now.bowl i.paz]
    %_  $
      paz        t.paz
      fex        [sing fex]
      gen.state  (~(put in gen.state) i.paz)
    ==
  ::
  ++  now-beak  %_(byk.bowl r [%da now.bowl])
  --
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  =>  |%
      ++  report
        |*  [=path ok=?]
        =/  =tank  leaf+"{?:(ok "built " "FAILED")}  {(spud path)}"
        ~>(%slog.[0 tank] same)
      --
  ::
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  ?.  ?&  ?=([%build *] wire)
          ?=([%clay %writ *] sign-arvo)
      ==
    (on-arvo:def wire sign-arvo)
  =/  =path  t.wire
  ?+    path  ~|(path+path !!)
      [%app *]
    =/  ok
      ?~  p.sign-arvo  |
      (~(nest ut -:!>(*agent:gall)) | -:!<(vase q.r.u.p.sign-arvo))
    %-  (report path ok)
    =?  app-ok.state  !ok  %.n
    =.  app.state  (~(del in app.state) path)
    ~?  =(~ app.state)
      ?:(app-ok.state %all-agents-built %some-agents-failed)
    [~ this]
  ::
      [%mar *]
    =/  ok  ?=(^ p.sign-arvo)
    %-  (report path ok)
    =?  mar-ok.state  !ok  %.n
    =.  mar.state  (~(del in mar.state) path)
    ~?  =(~ mar.state)
      ?:(mar-ok.state %all-marks-built %some-marks-failed)
    [~ this]
  ::
      [%gen *]
    =/  ok  ?=(^ p.sign-arvo)
    %-  (report path ok)
    =?  gen-ok.state  !ok  %.n
    =.  gen.state  (~(del in gen.state) path)
    ~?  =(~ gen.state)
      ?:(gen-ok.state %all-generators-built %some-generators-failed)
    [~ this]
  ==
++  on-fail   on-fail:def
--
