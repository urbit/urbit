/-  spider
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
=<  all
|%
++  all
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ;<  ~  bind:m  boot-both-with-ames
  :: ;<  ~  bind:m  boot-ames-mesa
  :: ;<  ~  bind:m  boot-mesa-ames
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
  ;<  ~  bind:m  (init-ship who fake=|)
  ;<  ~  bind:m  (dojo who "|pass [%a %load {<proto>}]")
  ;<  ~  bind:m  (dojo who "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
  (pure:m ~)
::
++  boot-both-with-ames
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  first both ships start communication using %ames
  ::
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (setup ~dev %ames)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  (sleep ~s2)
  ::  now we breach ~bud. since ~dev has %ames as the default core
  ::  it will remain as %known, with no flow state.
  ::
  ::  ~bud will start again using %mesa as the default core
  ::
  ;<  ~  bind:m  (breach ~bud)
  ;<  ~  bind:m  (setup ~bud %mesa)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  boot-ames-mesa
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  the sender send an %ames packet, the receiver will:
  ::    - drop the packet
  ::    - ask jael for the keys
  ::    - enqueue an %ahoy $plea
  ::
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (setup ~dev %mesa)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  boot-mesa-ames
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  the sender send a %mesa packet, the receiver will:
  ::    - drop the packet
  ::    - ask jael for the keys
  ::    - enqueue an %ahoy $plea
  ::
  ;<  ~  bind:m  (setup ~bud %mesa)
  ;<  ~  bind:m  (setup ~dev %ames)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  end
  (pure:m ~)
::
--
