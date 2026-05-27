/-  spider, aquarium
/+  ph=ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
/*  gall-raw   %hoon  /sys/vane/gall/hoon
=,  strand=strand:spider
=>  |%  ++  fact-gate
          |*  [typ=mold exp=noun]
          |=  [=mark val=noun]
          ^-  ?
          ~|  [exp val]
          ?+  mark  %.n
            %noun  =(exp ;;(typ val))
          ==
    --
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=+  ~(. ph %lago)
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
|-  ^-  form:m
?~  cores  (pure:m *vase)
=|  tids=drivers
;<  =_tids  bind:m  start-lago
::
=*  loop  $
;<  ~  bind:m  (aqua-setup ahoy-on/|)
;<  ~  bind:m  (aqua-setup %ames-retry ~s2)
;<  ~  bind:m  (switch-network-core i.cores)
;<  ~  bind:m  (init-ship ~bud fake=&)
;<  ~  bind:m  (init-ship ~dev fake=&)
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
  [/g/aqua/watch/sub %deal [~bud ~bud /] %sub %watch /aqua]
  ::
;<  ~  bind:m  (send-events aqua-event ~)
::  poke a non-running agent
::    (we do this before starting the agent so we hit +mo-clear-queue)
::
::  subscribe to the %pub agent to check subscription-ack
::
=/  =aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/watch/pub %deal [~dev ~dev /] %sub %watch /aqua]
;<  ~  bind:m  (send-events aqua-event ~)
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
~&  >  "start %goading the flow; sends remote %spur"
::  start %goading the flow; sends remote %spur
::
;<  ~  bind:m  (dojo ~dev "|start %pub")
::   check that the flow is not halted anymore
::
~&  >  "check that the flow is not halted anymore"
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::   check that the %spur is sent
::
~&  >  "check that the %spur is sent"
;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
::  XX  we need to wait for the subscription to be established before
::  giving any fact
::
~&  >  "prod to re-send the %watch $plea"
::  XX don't prod; remove when virtual %behn is in app/lago
::
;<  ~  bind:m  (dojo ~bud "|pass [%a %prod [~dev]~]")
::
:: ~&  >  "waiting for subscription ack"
::  check that subscription is succesful
::
:: ;<  *  bind:m
::   =+  fact=subscribed/~bud
::   (wait-for-fact ~dev %noun /aqua/watch/pub (fact-gate ,_fact fact))
::
~&  >  "sending fact [/hola 1]~"
:: ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 1]~")
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m
  ::  don't wait for ack
  ::
  %^  send-raw-card  %pass  /poke
  [%agent [our.bowl %lago] %poke %aqua-events !>((dojo:util ~dev ":pub send+`(list [path @])`[/hola 1]~"))]
::  check that ~bud receives the gift
::
~&  >  "waiting for fact [/hola 1]~"
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/hola 1]~))
::
::  now we are going to give the second (2) fact and wait for it
::

~&  >  "sending fact [/adios 2]~"
:: ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/adios 2]~")
::  don't wait for this poke-ack
::
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m
  %^  send-raw-card  %pass  /poke
  [%agent [our.bowl %lago] %poke %aqua-events !>((dojo:util ~dev ":pub send+`(list [path @])`[/adios 2]~"))]
::
:: ~&  >  "waiting for fact send [/adios 2]~"
:: ;<  *  bind:m
::   (wait-for-fact snd=~dev %noun /aqua/watch/pub (fact-gate ,(list [path @]) [/adios 2]~))
:: ::
~&  >  "waiting for fact rcvr [/adios 2]~"
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/adios 2]~))
::  suspendend subscriber agent; send (3) %fact
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.n %sub]")
:: ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/cucu 3]~")
::  don't wait for this poke-ack
::
;<  =bowl:spider  bind:m  get-bowl
;<  ~  bind:m
  %^  send-raw-card  %pass  /poke
  [%agent [our.bowl %lago] %poke %aqua-events !>((dojo:util ~dev ":pub send+`(list [path @])`[/cucu 3]~"))]
::  check that ~dev send the gift
::
:: ;<  *  bind:m
::   (wait-for-fact rcv=~dev %noun /aqua/watch/pub (fact-gate ,(list [path @]) [/cucu 3]~))
::  the gift should be now in the blocked queue
::
;<  ~  bind:m  (sleep ~s3)
::  revive agent
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.y %sub]")
::  check that ~bud receives the gift
::
;<  *  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/cucu 3]~))
::
::  leave the subscription and resubscribe (will make a new subscription flow)
::
~&  >>  "leave the subscription and resubscribe"
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
~&  >>  "sending 3 facts"
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 1]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 2]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 3]~")
::
::  revive subscriber agent
::
;<  ~  bind:m  (sleep ~s2)
;<  ~  bind:m  (dojo ~bud "|rein %base [%.n %sub]")
::  send facts again
::
~&  >>  "sending 3 more facts"
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 1]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 2]~")
;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/new 3]~")
::  XX give some time so this %facts end up in the blocked queue
::
;<  ~  bind:m  (sleep ~s1)
::
;<  ~  bind:m  (dojo ~bud "|rein %base [%.y %sub]")
;<  noun-1=^noun  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/new 1]~))
;<  noun-2=^noun  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/new 2]~))
;<  noun-3=^noun  bind:m
  (wait-for-fact rcv=~bud %noun /aqua/watch/sub (fact-gate ,(list [path @]) [/new 3]~))
::
::  check that flow 8 is corked on both
::
~&  >>  "check that flow 8 is corked on subscriber"
;<  corked-for=?  bind:m
  %^  peek-for-cork  ~bud  ~dev
  ?:  ?=(%ames i.cores)  &+8
  |+[8 %for]
~&  >>  "check that flow 8 is corked on publisher"
;<  ~  bind:m  (dojo ~dev "|pass [%a %prod [~bud]~]")
;<  corked-bak=?  bind:m
  %^  peek-for-cork  ~dev  ~bud
  ?:  ?=(%ames i.cores)  &+9
  |+[8 %bak]
~|  [corked-for corked-bak]
?>  =([corked-for corked-bak] [& &])
;<  ~  bind:m  (end tids)
;<  ~  bind:m  (send-events ~[reset-routing/~])
$(cores t.cores)
