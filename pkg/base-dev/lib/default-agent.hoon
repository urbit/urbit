/+  skeleton
|*  [agent=* help=*]
?:  ?=(%& help)
  ~|  %default-agent-helpfully-crashing
  skeleton
|_  =bowl:gall
++  on-init
  `agent
::
++  on-save
  !>(~)
::
++  on-load
  |=  old-state=vase
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
  |=  [=wire =sign:agent:gall]
  ^-  (quip card:agent:gall _agent)
  ?-    -.sign
      %poke-ack
    ?~  p.sign
      `agent
    %-  (slog leaf+"poke failed from {<dap.bowl>} on wire {<wire>}" u.p.sign)
    `agent
  ::
      %watch-ack
    ?~  p.sign
      `agent
    =/  =tank  leaf+"subscribe failed from {<dap.bowl>} on wire {<wire>}"
    %-  (slog tank u.p.sign)
    `agent
  ::
      %kick  `agent
      %fact
    ~|  "unexpected subscription update to {<dap.bowl>} on wire {<wire>}"
    ~|  "with mark {<p.cage.sign>}"
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
