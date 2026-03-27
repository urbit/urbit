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
  ::  XX  still can't run all tests at the same time..
  ::      XX aqua cleanup missing?
  ::
  ;<  ~  bind:m  test-mesa-ames-1
  ;<  ~  bind:m  test-ames-mesa-1
  ;<  ~  bind:m  test-mesa-ames-2
  ;<  ~  bind:m  test-ames-mesa-2
  ;<  ~  bind:m  test-mesa-ames-3
  ;<  ~  bind:m  boot-with-ames-and-breach  :: XX this waits for two ~m2 retries...
  ::  TODO
  ::
  :: ;<  ~  bind:m  (boot-ames-mesa ~dev comet)
  :: ;<  ~  bind:m  boot-moon
  :: ;<  ~  bind:m  boot-planet
  ;<  ~  bind:m  end
  (pure:m *vase)
::
++  test-mesa-ames-1
  =/  m  (strand ,~)
  ::  ~bud will send a %mesa packet to ~dev, that has %ames as
  ::  default network core, it will handle it and move ~bud to .chums
  ::
  ;<  ~  bind:m  (boot-core ~bud ~dev %mesa %ames)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  test-ames-mesa-1
  =/  m  (strand ,~)
  ::  ~bud will send an %ames packet to ~dev, that has %mesa as
  ::  default network core, it will handle it and enqueue an %ahoy
  ::  $plea, and when acked, move ~bud to .chums
  ::
  ;<  ~  bind:m  (boot-core ~bud ~dev %ames %mesa)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  test-mesa-ames-2
  =/  m  (strand ,~)
  ::  comet will send a %mesa packet to ~bud, that has %ames as
  ::  default network core
  ::
  =/  comet=@p
    ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
  ;<  ~  bind:m  (boot-core ~bud comet %mesa %ames)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  test-ames-mesa-2
  =/  m  (strand ,~)
  ::  ~dev will have todos in it alien agenda when hearing the
  ::  attestation proof. .comet has %mesa as its network core so
  ::  it should handle the packet and enqueue the $ahoy %plea
  ::
  :: =/  comet=@p
  ::   ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
  :: ;<  ~  bind:m  (boot-core ~dev comet %ames %mesa)  :: XX bail:evil
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  test-mesa-ames-3
  =/  m  (strand ,~)
  ::  ~dev will have todos in it alien agenda when hearing the
  ::  attestation proof. .comet has %ames as its network core so
  ::  it should handle the %mesa packet and make an entry in .chums
  ::
  =/  comet=@p
    ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
  ;<  ~  bind:m  (boot-core ~dev comet %mesa %ames)
  ;<  ~  bind:m  end
  (pure:m ~)
::
++  init
  =/  m  (strand ,~)
  ;<  ~  bind:m  start-azimuth
  ;<  ~  bind:m  (spawn ~bud)
  ;<  ~  bind:m  (spawn ~dev)
  ;<  ~  bind:m  (spawn ~marbud)
  (pure:m ~)
::
++  setup
  |=  [who=@p proto=?(%mesa %ames)]
  =/  m  (strand ,~)
  =/  =aqua-event:aquarium
    :+  %event  who
    [/g/aqua/watch/sub %deal [who who /] %sub %watch /aqua]
  ::
  ;<  ~  bind:m
    ?.  ?=(%pawn (clan:title who))
      (init-ship who fake=|)
    (init-comet who)
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
++  boot-core
  |=  [sndr=@p rcvr=@p core-s=?(%ames %mesa) core-r=?(%ames %mesa)]
  =/  m  (strand ,~)
  ;<  ~  bind:m  init
  ::  if sender has %ames as .core, the receiver will:
  ::    - drop the packet
  ::    - ask jael for the keys
  ::    - enqueue an %ahoy $plea (%mesa is default core)
  ::    - packet is processed using %ames
  ::
  ::  if receiver has %mesa as .core, the receiver will:
  ::    - move the peer into chums
  ::    - drop the packet
  ::    - ask jael for the keys
  ::
  ;<  ~  bind:m  (setup sndr core-s)
  ;<  ~  bind:m  (setup rcvr core-r)
  ;<  ~  bind:m
    ?.  ?=(%ames core-s)
      ^-  form:m
      (pure:m ~)
    ;<  =bowl:spider  bind:m  get-bowl
    =/  aqua-pax
      :-  %i
      /(scot %p sndr)/cz/(scot %p sndr)/kids/(scot %da now.bowl)/noun
    =+  ;;  hash=@uvi
        (need (scry-aqua:util (unit @uvi) our.bowl now.bowl aqua-pax))
    ::  load hood/ahoy hash
    ::
    ^-  form:m
    (dojo rcvr ":hood &ahoy-set-hash {<hash>}")
  ;<  ~  bind:m  (send-hi sndr rcvr)
  ::
  ;<  ~  bind:m  (dojo sndr ":sub [%sub {<rcvr>} %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ;<  ~  bind:m  (dojo rcvr ":pub send+`(list [path @])`[/hola 1]~")
  ::  check that sndr receives the gift
  ::
  ;<  =noun  bind:m
    (wait-for-fact sndr %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ::  XX wait for migration confirmation
  ::    this could be a spurious print coming from a "migrated" %ahoy $plea. these
  ::    %mesa $pleas are always acked, and the actual migration no-ops since the peer
  ::    is no longer in .peers.ames-state.
  ::
  ;<  ~  bind:m
    ?.  ?=(%ames core-s)
      ^-  form:m
      (pure:m ~)
    (wait-for-output rcvr "ahoy: %mesa migration completed for {<sndr>}")
  ;<  ~  bind:m  end
  (pure:m ~)
::
--
