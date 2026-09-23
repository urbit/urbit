/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
/*  gall-raw   %hoon  /sys/vane/gall/hoon
=,  strand=strand:spider
=>  |%  ++  gate
          |*  [typ=mold exp=noun]
          |=  [=mark val=noun]
          ~&  [exp val mark]
          ?+  mark  %.n
              %noun  =(exp ;;(typ val))
          ==
    --
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list ?(%mesa %ames))  ~[%ames]
|-  ^-  form:m
?~  cores  (pure:m *vase)
;<  ~  bind:m  start-simple
::
=*  loop  $
;<  ~  bind:m  (init-ship ~bud fake=&)
;<  ~  bind:m  (dojo ~bud "|pass [%a %load {<i.cores>}]")
;<  ~  bind:m  (init-ship ~dev fake=&)
;<  ~  bind:m  (dojo ~dev "|pass [%a %load {<i.cores>}]")
::
;<  ~  bind:m  (dojo ~bud "|mount %base")
;<  ~  bind:m  (dojo ~dev "|mount %base")
::
;<  ~  bind:m  (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~bud "|pass [%g %spew %odd ~]")
;<  ~  bind:m  (dojo ~dev "|pass [%g %spew %odd ~]")
::
;<  ~  bind:m  (send-hi ~bud ~dev)  ::  creates flow 4
::
;<  ~  bind:m  (copy-file ~bud /app/sub/hoon sub-agent)
::  subscribe to the %sub agent to check every received fact
::
=/  =aqua-event:aquarium
  :+  %event  ~bud
  [/g/aqua/watch/sub %deal [~bud ~bud /] %sub %watch /aqua]  ::  creates flow 8
  ::
;<  ~  bind:m  (send-events aqua-event ~)
::  poke a non-running agent
::    (we do this before starting the agent so we hit +mo-clear-queue)
::
;<  ~  bind:m  (dojo ~bud ":sub [%pok ~dev %pub]")  :: big poke
;<  ~  bind:m  (dojo ~bud "|start %sub")
::
::  %prod, so we resend the /gf plea that whas dropped on first contact
::
;<  ~  bind:m  (dojo ~bud "|pass [%a %prod [~dev]~]")
::  check that ~dev has halted this flow
::
;<  ~  bind:m  (wait-for-has-halt ~dev ~bud %pub)  :: XX
::  check that remote flubs are received
::
;<  ~  bind:m  (wait-for-flub ~bud ~dev %pub)
::  XX check that proding doesn't actually send the flubbed poke again
::
;<  ~  bind:m  (dojo ~bud "|pass [%a %prod [~dev]~]")
::
;<  ~  bind:m  (copy-file ~dev /app/pub/hoon pub-agent)
::  start %goading the flow; sends remote %spur
::
::  subscribe of ~dev for %pub gifts
::
=/  =aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/watch/pub %deal [~dev ~dev /] %pub %watch /aqua]
;<  ~  bind:m  (send-events aqua-event ~)
;<  ~  bind:m  (dojo ~dev "|start %pub")
::   check that the flow is not halted anymore
::
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::   check that the %spur is sent
::
;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
::
;<  *  bind:m
  (wait-for-fact ~dev %noun /aqua/watch/pub (gate ,_~ ~))
::
;<  ~  bind:m  end
$(cores t.cores)
