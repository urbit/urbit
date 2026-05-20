::  a gall-ames desync; or how i learned to love %aqua:
::
::    - start a subscription flow
::
:: seq = 1 (~bud -> %watch -> ~dev)
::         (~dev -> %ack   -> ~bud)
::
::    - inmediately leave the subscription
:: seq = 2 (~bud -> %leave -> ~dev)  ; %leave outstanding
::
::    - before %leave arrives, %kick the subscriber
::         (~dev -> %kick -> ~pub)   ; %kick outstanding
::
::    - suspend %pub agent; %leave arrives and gets flubbed
::         (~bud -> %leave -> ~dev (suspended))  ; %flub
::
::    - before %leave arrives, %kick the subscriber
::         (~dev -> %kick -> ~pub)   ; %kick gets handled
::         (~bud -> %ack  -> ~dev)   ; when %leave is outstanding
:: seq = 3 (~bud -> %cork  -> ~dev)  ; triggers cork
::
::    - revive %pub agent; remote %spur is sent
::         (~dev -> %spur -> ~pub)
::         (~pub -> %ack -> ~dev)
::
::    - %goad resumes forward flow; cogestion triggers sending both messages:
:: seq = 2 (~bud -> %leave -> ~dev)
:: seq = 3 (~bud -> %cork -> ~dev)
::
::    - both %leave and %cork get handled in order
:: seq = 2 (~dev -> %ack -> XX)    ; leave %ack gets lost
:: seq = 3 (~dev -> %ack -> ~bud)  ; %cork deletes the flow on the backward flow
::
::    - %ack for cork arrives, but %leave still outstanding
:: seq = 3 (~dev -> %ack -> ~bud)  ; %cork %ack gets confused with %leave %ack
::                                 ; %cork  (seq = 3) gets cleared from queue
::                                 ; %leave (seq = 2) outstanding
::
::    - %leave is resent, but the flow is corked; automatic %ack
:: seq = 2 (~bud -> %leave -> ~dev)
:: seq = 2 (~dev -> %ack -> ~bud)  ; always %ack if the flow is corked
::
/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
/*  gall-raw   %hoon  /sys/vane/gall/hoon
=,  strand=strand:spider
=>  |%  ++  gate
          |*  [typ=mold exp=noun]
          |=  [=mark val=noun]
          ?+  mark  %.n
              %noun  =(exp ;;(typ val))
          ==
    --
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list ?(%mesa %ames))  ~[%ames]  :: XX %mesa
|-  ^-  form:m
?~  cores  (pure:m *vase)
=|  tids=drivers
;<  =_tids  bind:m  start-simple
::
=*  loop  $
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
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (send-hi ~dev ~bud)
::
;<  ~  bind:m  (copy-file ~bud /app/sub/hoon sub-agent)
;<  ~  bind:m  (copy-file ~dev /app/pub/hoon pub-agent)
::  subscribe to the %sub agent to check every received fact
::
=/  sub-watch=aqua-event:aquarium
  :+  %event  ~bud
  [/g/aqua/watch/sub %deal [~bud ~bud /] %sub %watch /aqua]
::
::  subscribe of ~dev for %pub gifts
::
=/  pub-watch=aqua-event:aquarium
  :+  %event  ~dev
  [/g/aqua/watch/pub %deal [~dev ~dev /] %pub %watch /aqua]
;<  ~  bind:m  (send-events sub-watch pub-watch ~)
::  poke a non-running agent
::    (we do this before starting the agent so we hit +mo-clear-queue)
::
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")  :: creates flow 4 (/gf)
;<  ~  bind:m  (dojo ~bud "|start %sub")            :: creates flow 8 (/ge)
;<  ~  bind:m  (dojo ~dev "|start %pub")
::  check that subscription is succesful
::
;<  *  bind:m
  =+  fact=subscribed/~bud
  (wait-for-fact ~dev %noun /aqua/watch/pub (gate ,_fact fact))
::  wait for the %ack to arrive
::
;<  ~  bind:m  (sleep ~s1)
::  from now drop everything that ~bud sends
::
;<  ~  bind:m  (poke-our %aqua %aqua-rule !>([%drop-link ~bud ~dev]))
;<  ~  bind:m  (sleep ~s1)
::  enqueue %leave (it will be dropped)
::
;<  ~  bind:m  (dojo ~bud ":sub [%bye ~dev %pub]")  :: flow 9 seq = 1
::  drop the %kick on ~bud
::
:: ;<  ~  bind:m  (poke-our %aqua %aqua-rule !>([%drop-next ~dev ~bud 1]))
:: ;<  ~  bind:m  (sleep ~s1)
::  kick the subscriber, and then suspend the agent
::
;<  ~  bind:m  (dojo ~dev ":pub [%bye ~bud]")       :: flow 8 seq = 2
;<  ~  bind:m  (sleep ~s1)
;<  ~  bind:m  (dojo ~dev "|rein %base [%.n %pub]")
::
;<  ~  bind:m  (poke-our %aqua %aqua-rule !>([%clear-rules ~bud ~dev]))
::  check that ~dev has halted this flow
::
;<  ~  bind:m  (wait-for-has-halt ~dev ~bud %pub)
::  check that remote flubs are received
::
;<  ~  bind:m  (wait-for-flub ~bud ~dev %pub)
::  up to this point the %leave and the %kick are both
::  outstanding and un-acked
::
;<  ~  bind:m  (dojo ~dev "|rein %base [%.y %pub]")
::  start %goading the flow; sends remote %spur
::
::   check that the flow is not halted anymore
::
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::   check that the %spur is sent
::
;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
::
~&  >>  "check that flow 8 is corked on publisher"
;<  *  bind:m
  ?:  ?=(%ames i.cores)  (wait-for-cork ~dev ~bud &+9)
  (peek-for-cork ~bud ~dev |+[8 %bak])
::
;<  ~  bind:m  (end tids)
$(cores t.cores)
