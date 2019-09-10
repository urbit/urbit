/+  default-agent
^-  agent:mall
=|  state=@
|_  =bowl:mall
+*  this  .
++  handle-init            ~(handle-init default-agent bowl this)
++  handle-extract-state   ~(handle-extract-state default-agent bowl this)
++  handle-upgrade-state   ~(handle-upgrade-state default-agent bowl this)
++  handle-poke
  |=  in-poke-data=cage
  ~&  >  state=state
  ~&  >  in-poke-data
  =.  state  +(state)
  `this
::
++  handle-subscribe       ~(handle-subscribe default-agent bowl this)
++  handle-unsubscribe     ~(handle-unsubscribe default-agent bowl this)
++  handle-peek            ~(handle-peek default-agent bowl this)
++  handle-agent-response  ~(handle-agent-response default-agent bowl this)
++  handle-arvo-response   ~(handle-arvo-response default-agent bowl this)
++  handle-error           ~(handle-error default-agent bowl this)
--
