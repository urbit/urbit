/-  spider
/+  *ph-io
=,  strand=strand:spider
=>
|%
++  start-agent
  |=  [=ship agent=term]
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ;<  ~  bind:m  (dojo ship "|start {<agent>}")
  ;<  ~  bind:m  ::(wait-for-agent-start ship agent)
    (wait-for-output ship "activated app home/{(trip agent)}")
  (pure:m ~)
::
++  wait-for-agent-start
  |=  [=ship agent=term]
  =/  m  (strand:spider ,~)
  ^-  form:m
  =*  loop  $
  ;<  [her=^ship =unix-effect]  bind:m  take-unix-effect
  ?:  (is-dojo-output:util ship her unix-effect "activated app home/{(trip agent)}")
    (pure:m ~)
  loop
::
++  start-agents
  |=  =ship
  =/  m  (strand:spider ,~)
  ~&  %starting-agents
  ^-  form:m
  ;<  ~  bind:m  (start-agent ship %group-store)
  ;<  ~  bind:m  (start-agent ship %group-pull-hook)
  ;<  ~  bind:m  (start-agent ship %group-push-hook)
  ;<  ~  bind:m  (start-agent ship %group-view)
  ;<  ~  bind:m  (start-agent ship %dm-hook)
  ::
  ;<  ~  bind:m  (start-agent ship %hark-store)
  ;<  ~  bind:m  (start-agent ship %hark-graph-hook)
  ;<  ~  bind:m  (start-agent ship %hark-group-hook)
  ::
  ;<  ~  bind:m  (start-agent ship %metadata-store)
  ;<  ~  bind:m  (start-agent ship %metadata-pull-hook)
  ;<  ~  bind:m  (start-agent ship %metadata-push-hook)
  ::
  ;<  ~  bind:m  (start-agent ship %invite-store)
  ;<  ~  bind:m  (start-agent ship %invite-hook)
  ::
  ::
  ;<  ~  bind:m  (start-agent ship %contact-store)
  ;<  ~  bind:m  (start-agent ship %contact-push-hook)
  ;<  ~  bind:m  (start-agent ship %contact-pull-hook)
  ::
  ;<  ~  bind:m  (start-agent ship %graph-store)
  ;<  ~  bind:m  (start-agent ship %graph-push-hook)
  ;<  ~  bind:m  (start-agent ship %graph-pull-hook)
  ::
  (pure:m ~)
::
--
^-  thread:spider
|=  arg=vase
=+  !<(who=?(~ [@p ~]) arg)
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
::
?~  who
  ;<  ~  bind:m  (dojo ~zod "|mount %")
  ;<  ~  bind:m  (dojo ~bus "|mount %")
  ;<  ~  bind:m  (dojo ~web "|mount %")
  ;<  ~  bind:m  (start-agents ~zod)
  ;<  ~  bind:m  (start-agents ~bus)
  ;<  ~  bind:m  (start-agents ~web)
  (pure:m *vase)
::
;<  ~  bind:m  (dojo -.who "|mount %")
;<  ~  bind:m  (start-agents -.who)
(pure:m *vase)
