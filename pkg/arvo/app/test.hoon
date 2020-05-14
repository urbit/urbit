/+  default-agent
!:
|%
+$  card  card:agent:gall
+$  test  ?(%agents %marks)
+$  state
  $:  app=(set path)
      app-ok=?
      mar=(set path)
      mar-ok=?
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
  |^
  =+  !<(=test vase)
  ?-  test
    %marks   test-marks
    %agents  test-agents
  ==
  ::
  ++  test-marks
    =|  fex=(list card)
    ^+  [fex this]
    ?>  =(~ mar.state)
    =.  mar-ok.state  %.y
    =+  .^(paz=(list path) ct+(en-beam now-beak /mar))
    |-  ^+  [fex this]
    ?~  paz  [fex this]
    =/  xap=path  (flop i.paz)
    ?.  ?=([%hoon *] xap)
      $(paz t.paz)
    =/  mak=^mark
      %-  crip
      %+  turn  (tail (spud (tail (flop (tail xap)))))
      |=(c=@tD `@tD`?:(=('/' c) '-' c))
    =/  sing=card
      :+  %pass  /build/mar/[mak]
      [%arvo %c %warp our.bowl %home ~ %sing %b da+now.bowl /[mak]]
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
    =/  daz  ~(tap in ~(key by dir.app-arch))
    |-  ^+  [fex this]
    ?~  daz  [fex this]
    =/  dap-pax=path  /app/[i.daz]/hoon
    =/  dap-arch  .^(arch cy+(en-beam now-beak (flop dap-pax)))
    ?~  fil.dap-arch
      $(daz t.daz)
    =/  sing=card
      :+  %pass  /build/app/[i.daz]
      [%arvo %c %warp our.bowl %home ~ %sing %a da+now.bowl dap-pax]
    %_  $
      daz        t.daz
      fex        [sing fex]
      app.state  (~(put in app.state) /app/[i.daz])
    ==
  ::
  ++  now-beak  %_(byk.bowl r [%da now.bowl])
  --
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  [(list card) _this]
  ?.  ?=([%build *] wire)
    (on-arvo:def wire sign-arvo)
  ?.  ?=(%writ +<.sign-arvo)
    (on-arvo:def wire sign-arvo)
  =/  =path  t.wire
  ?+    path  ~|(path+path !!)
      [%app *]
    =/  ok  ?=(^ p.sign-arvo)
    ~&  ?:  ok
          agent-built+path
        agent-failed+path
    =?  app-ok.state  !ok  %.n
    =.  app.state  (~(del in app.state) path)
    ~?  =(~ app.state)
      ?:  app-ok.state
        %all-agents-built
      %some-agents-failed
    [~ this]
  ::
      [%mar *]
    =/  ok  ?=(^ p.sign-arvo)
    ~&  ?:  ok
          mark-built+path
        mark-failed+path
    =?  mar-ok.state  !ok  %.n
    =.  mar.state  (~(del in mar.state) path)
    ~?  =(~ mar.state)
      ?:  mar-ok.state
        %all-marks-built
      %some-marks-failed
    [~ this]
  ==
++  on-fail   on-fail:def
--
