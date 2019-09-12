|_  [=bowl:mall =agent:mall]
++  handle-init
  `agent
::
++  handle-prep
  |=  old-state=vase
  ~&  "updating agent {<dap.bowl>} by throwing away old state"
  `agent
::
++  handle-poke
  |=  =cage
  ~&  "unexpected poke to {<dap.bowl>} with mark {<p.cage>}"
  ~|  "unexpected poke to {<dap.bowl>} with mark {<p.cage>}"
  !!
::
++  handle-peer
  |=  =path
  ~&  "unexpected subscription to {<dap.bowl>} on path {<path>}"
  ~|  "unexpected subscription to {<dap.bowl>} on path {<path>}"
  !!
::
++  handle-pull
  |=  path
  `agent
::
++  handle-peek
  |=  path
  ~|  "unexpected scry into {<dap.bowl>} on path {<path>}"
  !!
::
++  handle-mall
  |=  [=wire =internal-gift:mall]
  ?-    -.internal-gift
      %coup  `agent
      %reap  `agent
      %quit
    ~|  "unexpected subscription closure to {<dap.bowl>} on wire {<wire>}"
    !!
  ::
      %diff
    ~|  "unexpected subscription update to {<dap.bowl>} on wire {<wire>}"
    ~|  "with mark {<p.p.internal-gift>}"
    !!
  ::
      %http-response
    ~|  "unexpected http-response to {<dap.bowl>} on wire {<wire>}"
    !!
  ==
::
++  handle-take
  |=  [=wire =vase]
  ~|  "unexpected system response {<q.vase>} to {<dap.bowl>} on wire {<wire>}"
  !!
::
++  handle-lame
  |=  [=term =tang]
  %-  (slog leaf+"error in {<dap.bowl>}" >term< tang)
  `agent
::
++  handle-stay
  ~&  "extracting empty state for {<dap.bowl>}"
  !>(~)
--
