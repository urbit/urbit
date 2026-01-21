/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
/*  gall-raw   %hoon  /sys/vane/gall/hoon
=,  strand=strand:spider
=>  |%  ++  gate
          |*  [typ=mold exp=noun]
          |=  [=mark val=noun]
          ~|  [exp val]
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
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
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
::  initial fact (1) that will go into the blocked queue
::
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 1]~")
::  start %goading the flow; sends remote %spur
::
;<  ~  bind:m  (dojo ~dev "|start %pub")
::   check that the flow is not halted anymore
::
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::   check that the %spur is sent
::
;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
:: XX we should scry into ~bud for no entries in the .pit
::
:: ;<  ~  bind:m  (sleep ~s1)
::  subscribe of ~dev for %pub gifts
::
=/  =aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/watch/pub %deal [~dev ~dev /] %pub %watch /subs]
;<  ~      bind:m  (send-events aqua-event ~)
::  check that ~bud receives the gift
::
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
::
::  now we are going to give the second (2) fact and wait for it
::
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/adios 2]~")
;<  =noun  bind:m
  (wait-for-fact snd=~dev %noun /aqua/watch/pub (gate ,(list [path @]) [/adios 2]~))
::
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/adios 2]~))
::  suspendend subscriber agent; send (3) %fact
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.n %sub]")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/cucu 3]~")
::  check that ~bud emits the gift
::
;<  *  bind:m
  (wait-for-fact rcv=~dev %noun /aqua/watch/pub (gate ,(list [path @]) [/cucu 3]~))
::  the gift should be now in the blocked queue
::
;<  ~  bind:m  (sleep ~s3)
::  revive agent
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.y %sub]")
::  check that ~bud receives the gift
::
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/cucu 3]~))
::
::  leave the subscription and resubscribe (will make a new subscription flow)
::
;<  ~  bind:m  (dojo ~bud ":sub [%bye ~dev %pub]")
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
::  suspend the agent before the %leave %watch are acknowledged
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.n %sub]")
::  enqueue new facts (1) (2) (3) into the blocked queue
::    (the publisher will reuse flow number 8 so the facts should
::     trigger the stale %fact case, since we have increased the nonce)
::
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 1]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 2]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 3]~")
::
::  revive subscriber agent
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.y %sub]")
::  check that flow 8 is corked on both (first on the publisher)
::
;<  ~  bind:m  (wait-for-cork ~dev ~bud flow=9)  :: XX for |mesa this would be [8 %bak]
;<  ~  bind:m  (dojo ~bud "|rein %base [%.n %sub]")
::  send facts again
::
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 1]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 2]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 3]~")
::  XX give some time so this %facts end up in the blocked queue
::
;<  ~  bind:m  (sleep ~s1)
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.y %sub]")
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/new 1]~))
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/new 2]~))
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/new 3]~))
::
;<  ~  bind:m  end
$(cores t.cores)
