/-  spider, aquarium
/+  *ph-io
/*  test-agent  %hoon  /tests/app/running-flub/hoon
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (init-ship ~bud fake=&)
;<  ~  bind:m  (dojo ~bud "|pass [%a %load %mesa]")
;<  ~  bind:m  (init-ship ~dev fake=&)
;<  ~  bind:m  (dojo ~dev "|pass [%a %load %mesa]")
;<  ~  bind:m  (dojo ~bud "|mount %base")
;<  ~  bind:m  (dojo ~dev "|mount %base")
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (copy-file ~bud /app/lost/hoon test-agent)
::
::  Queue a local move for a non-running agent, then send it a remote poke.
::  The local move must not make Gall report the unrelated remote plea as
::  blocked. Once the agent starts, Ames must redeliver the pending poke.
::
=/  =aqua-event:aquarium
  :+  %event  ~bud
  [/g/aqua/watch/lost %deal [~bud ~bud /] %lost %watch /http]
;<  ~  bind:m  (send-events aqua-event ~)
=/  =aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/poke/lost %deal [~dev ~bud /] %lost %poke %noun !>(~)]
;<  ~  bind:m  (send-events aqua-event ~)
;<  ~  bind:m  (dojo ~bud "|start %lost")
;<  ~  bind:m
  %+  wait-for-output  ~bud
  "running-flub-received"
;<  ~  bind:m  end
(pure:m *vase)
