::  /app/arkhe.hoon
::  Agente Gall que mantém um nó Arkhe e responde a handovers remotos.
::
/+  arkhe, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-1
  ==
+$  state-1
  $:  %1
      node=node:arkhe
      recursion-depth=@ud
      safe-mode=?
  ==
--
%-  agent:dbug
=|  state=state-1
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
++  on-init
  ^-  (quip card _this)
  =/  initial-node  (make-node:arkhe our.bowl 0)
  ::  Register a default increment capability for demonstration
  =/  inc-handler
    |=  [int=intent:arkhe state=*]
    ^-  [result=* state=*]
    =/  s  (,@ud state)
    [+(s) +(s)]
  =.  node.state  (register-capability:arkhe initial-node %increment inc-handler)
  =.  recursion-depth.state  0
  =.  safe-mode.state  %.n
  `this
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  ::  Simple upgrade from state-0 if it existed, otherwise use state-1
  =/  old  !<(versioned-state old-vase)
  ?-    -.old
      %1  `this(state old)
  ==
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?:  safe-mode.state
    ?.  =(mark %arkhe-reset-safe-mode)
      ((slog leaf+"CONSTITUTIONAL EMERGENCY: Safe mode active" ~) `this)
    =.  safe-mode.state  %.n
    =.  recursion-depth.state  0
    ((slog leaf+"Safe mode deactivated" ~) `this)
  ::
  ?+    mark  (on-poke:def mark vase)
      %arkhe-handover-request
    =/  data  !<(intent:arkhe vase)
    =/  ret   (handover-local:arkhe node.state data now.bowl)
    ?~  ret
      ((slog leaf+"Handover failed (check constraints or constitution)" ~) `this)
    =+  [result new-node]=u.ret
    =.  node.state  new-node
    :_  this
    [%give %fact ~[/out] %arkhe-handover-result !>(result)]~
  ::
      %arkhe-spawn-self-model
    ?:  (gte recursion-depth.state 3)
      =.  safe-mode.state  %.y
      ((slog leaf+"CONSTITUTIONAL EMERGENCY: recursion depth exceeded, safe mode activated" ~) `this)
    =.  recursion-depth.state  +(recursion-depth.state)
    ((slog (leaf "Self-model spawned. Depth: {<recursion-depth.state>}") ~) `this)
  ::
      %arkhe-reset-safe-mode
    =.  safe-mode.state  %.n
    =.  recursion-depth.state  0
    `this
  ==
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
      [%x %node ~]  ``noun+!>(node.state)
      [%x %depth ~]  ``noun+!>(recursion-depth.state)
      [%x %safe ~]  ``noun+!>(safe-mode.state)
  ==
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
