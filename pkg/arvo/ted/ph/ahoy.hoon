/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
=>  |%  ++  gate
          |*  [typ=mold exp=noun]
          |=  [=mark val=noun]
          ~|  [exp val]
          ?+  mark  %.n
              %noun  =(exp ;;(typ val))
          ==
    --
=<  all
|%
++  all
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ;<  ~  bind:m  boot-ames-mesa
  ;<  ~  bind:m  boot-mesa-ames
  ;<  ~  bind:m  boot-with-ames-and-breach
  (pure:m *vase)
  ::
::
++  init
  =/  m  (strand ,~)
  ;<  ~  bind:m  start-azimuth
  ;<  ~  bind:m  (spawn ~bud)
  ;<  ~  bind:m  (spawn ~dev)
  (pure:m ~)
::
++  setup
  |=  [who=@p proto=?(%mesa %ames)]
  =/  m  (strand ,~)
  =/  =aqua-event:aquarium
    :+  %event  who
    [/g/aqua/watch/sub %deal [who who /] %sub %watch /aqua]
  ::
  ;<  ~  bind:m  (init-ship who fake=|)
  ;<  ~  bind:m  (dojo who "|pass [%a %load {<proto>}]")
  ;<  ~  bind:m  (dojo who "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
  ;<  ~  bind:m  (dojo who "|mount %base")
  ;<  ~  bind:m  (copy-file who /app/sub/hoon sub-agent)
  ;<  ~  bind:m  (copy-file who /app/pub/hoon pub-agent)
  ;<  ~  bind:m  (dojo who "|start %sub")
  ;<  ~  bind:m  (dojo who "|start %pub")
  ::  subscribe to who for %pub gifts
  ::
  ;<  ~  bind:m  (send-events aqua-event ~)
  (pure:m ~)
::
++  boot-with-ames-and-breach
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  first both ships start communication using %ames
  ::
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (setup ~dev %ames)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  (sleep ~s2)
  ::  subscribe before breaching
  ::
  ;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ::
  ::  now we breach ~bud. since ~dev has %ames as the default core
  ::  it will remain as %known, with no flow state.
  ::
  ::  ~bud will start again using %mesa as the default core
  ::
  ;<  ~  bind:m  (breach ~bud)
  ;<  ~  bind:m  (setup ~bud %mesa)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ::  subscribe again and send fact
  ::
  ;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 1]~")
  ::  check that ~bud receives the gift
  ::
  ;<  =noun  bind:m
    (wait-for-fact ~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  boot-ames-mesa
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  the sender sends an %ames packet, the receiver will:
  ::    - drop the packet
  ::    - ask jael for the keys
  ::    - enqueue an %ahoy $plea
  ::
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (setup ~dev %mesa)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 1]~")
  ::  check that ~bud receives the gift
  ::
  ;<  =noun  bind:m
    (wait-for-fact ~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  boot-mesa-ames
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  the sender sends a %mesa packet, the receiver will:
  ::    - move the peer into chums
  ::    - drop the packet
  ::    - ask jael for the keys
  ::
  ;<  ~  bind:m  (setup ~bud %mesa)
  ;<  ~  bind:m  (setup ~dev %ames)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ;<  ~  bind:m  (dojo ~dev ":pub send+`(list [path @])`[/hola 1]~")
  ::  check that ~bud receives the gift
  ::
  ;<  =noun  bind:m
    (wait-for-fact ~bud %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ;<  ~  bind:m  end
  ;<  ~  bind:m  end
  (pure:m ~)
::
--
