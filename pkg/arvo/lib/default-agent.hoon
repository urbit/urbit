/+  skeleton
|*  [agent=* help=*]
?:  ?=(%& help)
  ~|  %default-agent-helpfully-crashing
  skeleton
|_  =bowl:mall
++  on-init
  `agent
::
++  on-save
  ~&  "extracting empty state for {<dap.bowl>}"
  !>(~)
::
++  on-load
  |=  old-state=vase
  ~&  "updating agent {<dap.bowl>} by throwing away old state"
  `agent
::
++  on-poke
  |=  =cage
  ~|  "unexpected poke to {<dap.bowl>} with mark {<p.cage>}"
  !!
::
++  on-watch
  |=  =path
  ~|  "unexpected subscription to {<dap.bowl>} on path {<path>}"
  !!
::
++  on-leave
  |=  path
  `agent
::
++  on-peek
  |=  =path
  ~|  "unexpected scry into {<dap.bowl>} on path {<path>}"
  !!
::
++  on-agent
  |=  [=wire =gift:agent:mall]
  ^-  (quip card:agent:mall _agent)
  ?-    -.gift
      %poke-ack
    ?~  p.gift
      `agent
    %-  (slog leaf+"poke failed from {<dap.bowl>} on wire {<wire>}" u.p.gift)
    `agent
  ::
      %watch-ack
    ?~  p.gift
      `agent
    =/  =tank  leaf+"subscribe failed from {<dap.bowl>} on wire {<wire>}"
    %-  (slog tank u.p.gift)
    `agent
  ::
      %kick  `agent
      %fact
    ~|  "unexpected subscription update to {<dap.bowl>} on wire {<wire>}"
    ~|  "with mark {<p.cage.gift>}"
    !!
  ::
      %http-response
    ~|  "unexpected http-response to {<dap.bowl>} on wire {<wire>}"
    !!
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ~|  "unexpected system response {<-.sign-arvo>} to {<dap.bowl>} on wire {<wire>}"
  !!
::
++  on-fail
  |=  [=term =tang]
  %-  (slog leaf+"error in {<dap.bowl>}" >term< tang)
  `agent
--
