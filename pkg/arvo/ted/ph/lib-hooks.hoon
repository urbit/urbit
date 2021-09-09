/-  spider
/+  io=ph-io, *strandio
=>
=,  io
|% 
++  strand  strand:spider
++  start-agents
  |=  =ship
  =/  m  (strand ,~)
  ;<  ~  bind:m  (dojo ship "|start %graph-store")
  ;<  ~  bind:m  (dojo ship "|start %graph-push-hook")
  ;<  ~  bind:m  (dojo ship "|start %graph-pull-hook")
  ;<  ~  bind:m  (dojo ship "|start %group-store")
  ;<  ~  bind:m  (dojo ship "|start %group-push-hook")
  ;<  ~  bind:m  (dojo ship "|start %group-pull-hook")
  ;<  ~  bind:m  (dojo ship "|start %metadata-store")
  ;<  ~  bind:m  (dojo ship "|start %metadata-hook")
  ;<  ~  bind:m  (sleep `@dr`300)
  (pure:m ~)
::
++  make-link
  |=  [title=@t url=@t]
  =/  m  (strand ,~)
  ;<  ~  bind:m  (dojo ~bud ":graph-store|add-post [~bud %test] ~[[%text '{(trip title)}'] [%url '{(trip url)}']]") 
  (pure:m ~)
--

^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (start-agents ~bud)
;<  ~  bind:m  (start-agents ~dev)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (dojo ~bud "-graph-create [%create [~bud %test] 'test' '' `%graph-validator-link [%policy [%open ~ ~]] 'link']")
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (dojo ~dev "-graph-join [%join [~bud %test] ~bud]")
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (poke-our %aqua noun+!>([%pause-events ~[~dev]]))
;<  ~  bind:m  (make-link 'one' 'one')
;<  ~  bind:m  (make-link 'two' 'one')
;<  ~  bind:m  (make-link 'thre' 'one')
;<  ~  bind:m  (make-link 'four' 'one')
;<  ~  bind:m  (make-link 'five' 'one')
;<  ~  bind:m  (make-link 'six' 'one')
;<  ~  bind:m  (make-link 'seven' 'one')
;<  ~  bind:m  (sleep ~s40)
::  five unacked events is sufficent to cause a clog, and by extension a 
::  %kick
;<  ~  bind:m  (poke-our %aqua noun+!>([%unpause-events ~[~dev]]))
;<  ~  bind:m  (sleep ~s10)
;<  ~  bind:m  (make-link 'eight' 'one')
;<  ~  bind:m  (make-link 'nine' 'one')
;<  ~  bind:m  (sleep ~s10)
;<  ~  bind:m  (dojo ~dev ":graph-pull-hook +dbug %bowl")
;<  ~  bind:m  (dojo ~dev ":graph-store +dbug")
;<  ~  bind:m  (dojo ~bud ":graph-push-hook +dbug %bowl")
;<  ~  bind:m  (dojo ~bud ":graph-store +dbug")
;<  ~  bind:m  end
(pure:m *vase)
::(pure:m *vase)
