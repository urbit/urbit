/-  spider, aquarium
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
=/  comet=@p
  ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
=>  |%
    ++  gate
      |*  [typ=mold exp=noun]
      |=  [=mark val=noun]
      ~|  [exp val]
      ?+  mark  %.n
          %noun  =(exp ;;(typ val))
      ==
    ::
    ++  load-migration-hash
      |=  [sndr=@p rcvr=@p]
      =/  m  (strand ,~)
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
    ::
    --
=<  all
|%
++  all
  ^-  thread:spider
  |=  vase
  =/  m  (strand ,vase)
  ::  XX  still can't run all tests at the same time..
  ::  XX aqua cleanup missing?
  ::  XX looks like it's working now most of the times
  ::  the ones that don't, comets seem to be involved somehow
  ::
  ::  enable ahoy-probbing for all ships
  ::
  ;<  ~          bind:m  (aqua-setup ahoy-on/&)
  ;<  ~          bind:m  test-mesa-ames-1
  ;<  ~          bind:m  test-ames-mesa-1
  ;<  ~          bind:m  (boot-with-core-and-breach %mesa)
  ;<  ~          bind:m  (boot-with-core-and-breach %ames)
  ::  comet tests
  ::
  ::  comet -> sponsor
  ::
  ;<  ~          bind:m  test-simple-comet-1  :: "comet -> ~bud"  > :ping
  ;<  ~          bind:m  test-simple-comet-2  :: "comet -> ~bud"  > :ping
                                              ::    (kids hash loaded later)
  ;<  ~          bind:m  test-simple-comet-3  :: "comet -> ~bud"  > |hi after migration
  ;<  ~          bind:m  test-simple-comet-4  :: "comet -> ~bud"  > :sub pre-migration
                                              ::                  > :pub post-migration
  ::  galaxy -> comet
  ;<  ~          bind:m  test-simple-comet-5  :: "comet -> ~bud"  > :ping
                                              :: "~bud -> comet"  > |hi
                                              :: comet %ahoy-probs galaxy
  ;<  ~          bind:m  test-simple-comet-6  :: all in %mesa
                                              :: "comet -> ~bud"  > :ping
                                              :: "~bud -> comet"  > |hi
  ;<  ~          bind:m  test-simple-comet-7  :: comet in %mesa, galaxy in %ames
                                              ::  galaxy has %ames TODOS
                                              :: "comet -> ~bud"  > :ping
                                              :: "~bud -> comet"  > |hi
  ;<  ~          bind:m  test-simple-comet-8  :: ~dev -> comet
                                              ::  galaxy has %ames TODOS
                                              :: "comet -> ~dev"  > :ping
                                              :: "~dev -> comet"  > |hi
  ::  TODO
  ::
  :: ;<  ~          bind:m  boot-moon
  :: ;<  ~          bind:m  boot-planet
  (pure:m *vase)
::
++  test-mesa-ames-1
  =/  m  (strand ,~)
  ::  ~bud will send a %mesa packet to ~dev, that has %ames as
  ::  default network core, it will handle it and move ~bud to .chums
  ::
  ;<  ~  bind:m  (boot-core ~bud ~dev %mesa %ames)
  (pure:m ~)
::
++  test-ames-mesa-1
  =/  m  (strand ,~)
  ::  ~bud will send an %ames packet to ~dev, that has %mesa as
  ::  default network core, it will handle it and enqueue an %ahoy
  ::  $plea, and when acked, move ~bud to .chums
  ::
  ;<  ~  bind:m  (boot-core ~bud ~dev %ames %mesa)
  (pure:m ~)
::  botn comet and sponsor in %ames
::
++  test-simple-comet-1
  =/  m  (strand ,~)
  ::  comet boots with %ames and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  galaxy will start %ahoy probing after hearing the %plea
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (load-migration-hash comet ~bud)
  ;<  ~  bind:m  (dojo ~bud ":hood &ahoy-verb ~")
  ::
  ;<  ~  bind:m  (setup comet %ames)
  ::
  ;<  ~  bind:m  (wait-for-output ~bud "ahoy: %mesa migration completed for {<comet>}")
  ;<  ~          bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-2
  =/  m  (strand ,~)
  ::  comet boots with %ames and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  galaxy will start %ahoy probing after hearing the %plea
  ::  here we load the migration hash a bit later
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (dojo ~bud ":hood &ahoy-verb ~")
  ::
  ;<  ~  bind:m  (setup comet %ames)
  ::
  ;<  ~  bind:m  (load-migration-hash comet ~bud)
  ;<  ~  bind:m  (wait-for-output ~bud "ahoy: %mesa migration completed for {<comet>}")
  ;<  ~          bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-3
  =/  m  (strand ,~)
  ::  comet boots with %ames and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  galaxy will start %ahoy probing after hearing the %plea
  ::  here we communicate using %mesa after migration
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (load-migration-hash comet ~bud)
  ;<  ~  bind:m  (dojo ~bud ":hood &ahoy-verb ~")
  ::
  ;<  ~  bind:m  (setup comet %ames)
  ::
  ;<  ~  bind:m  (wait-for-output ~bud "ahoy: %mesa migration completed for {<comet>}")
  ;<  ~  bind:m  (send-hi comet ~bud)
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-4
  =/  m  (strand ,~)
  ::  comet boots with %ames and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  galaxy will start %ahoy probing after hearing the %plea
  ::  here we start a subscription flow _before_ migration and
  ::  receive a fact _after_ migration
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (dojo ~bud ":hood &ahoy-verb ~")
  ::
  ;<  ~  bind:m  (setup comet %ames)
  ;<  ~  bind:m  (dojo comet ":sub [%sub {<~bud>} %pub]")
  ::
  ;<  ~  bind:m  (load-migration-hash comet ~bud)
  ;<  ~  bind:m  (wait-for-output ~bud "ahoy: %mesa migration completed for {<comet>}")
  ;<  ~  bind:m  (send-hi comet ~bud)
  ;<  ~  bind:m  (dojo ~bud ":pub send+`(list [path @])`[/hola 1]~")
  ;<  =noun  bind:m
    (wait-for-fact comet %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-5
  =/  m  (strand ,~)
  ::  comet boots with %ames and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  comet will start %ahoy probing after hearing the /gf %plea
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ::  the galaxy won't migrate the comet
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/|)
  ;<  ~  bind:m  (setup ~bud %ames)
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/&)
  ;<  ~  bind:m  (setup comet %ames)
  ;<  ~  bind:m  (load-migration-hash from=~bud to=comet)
  ;<  ~  bind:m  (dojo comet ":hood &ahoy-verb ~")
  ::
  ;<  ~  bind:m  (wait-for-output comet "ahoy: %mesa migration completed for ~bud")
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-6
  =/  m  (strand ,~)
  ::  comet boots with %mesa and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  the galaxy will save the comet in .chums
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ::  the galaxy won't migrate the comet
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/|)
  ;<  ~  bind:m  (setup ~bud %ames)
  ::  the comet will send %mesa packets first
  ::
  ;<  ~  bind:m  (switch-network-core %mesa)
  ;<  ~  bind:m  (aqua-setup ahoy-on/&)
  ;<  ~  bind:m  (setup comet %mesa)
  ::  both galxy and comet should handle mesa packets now
  ::
  ;<  ~  bind:m  (send-hi comet ~bud)
  ::
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-7
  =/  m  (strand ,~)
  ::  comet boots with %mesa and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  the galaxy will have %ames TODOS, will on-the-spot migrate
  ::  %alien TODOS (/gf & /hi) to .chums and start peeking for attestation
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ::  the galaxy won't migrate the comet
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/|)
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (dojo ~bud "|hi {(scow %p comet)}")
  ::  the comet will send %mesa packets first
  ::
  ;<  ~  bind:m  (switch-network-core %mesa)
  ;<  ~  bind:m  (aqua-setup ahoy-on/&)
  ;<  ~  bind:m  (setup comet %mesa)
  ::  both galxy and comet should handle mesa packets now
  ::
  ;<  ~  bind:m  (send-hi comet ~bud)
  ::
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-simple-comet-8
  =/  m  (strand ,~)
  ::  comet boots with %mesa and talks to its sponsor
  ::  (will inmediately start %pinging)
  ::  the galaxy will have %ames TODOS, will on-the-spot migrate
  ::  %alien TODOS (/gf & /hi) to .chums and start peeking for attestation
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (switch-network-core %ames)
  ::  the galaxy won't migrate the comet
  ::
  ;<  ~  bind:m  (aqua-setup ahoy-on/|)
  ::  also setup ~bud so aqua doesn't crash when scrying
  ::
  ;<  ~  bind:m  (setup ~bud %ames)
  ;<  ~  bind:m  (setup ~dev %ames)
  ;<  ~  bind:m  (dojo ~dev "|hi {(scow %p comet)}")
  ::  the comet will send %mesa packets first
  ::
  ;<  ~  bind:m  (switch-network-core %mesa)
  ;<  ~  bind:m  (aqua-setup ahoy-on/&)
  ;<  ~  bind:m  (setup comet %mesa)
  ::  both galxy and comet should handle mesa packets now
  ::
  ;<  ~  bind:m  (send-hi comet ~dev)
  ::
  ;<  ~  bind:m  (end t)
  (pure:m ~)
::
++  test-mesa-ames-3
  =/  m  (strand ,~)
  ::  ~dev will have todos in it alien agenda when hearing the
  ::  attestation proof. .comet has %ames as its network core so
  ::  it should handle the %mesa packet and make an entry in .chums
  ::
  ;<  ~  bind:m  (boot-core ~dev comet %mesa %ames)
  (pure:m ~)
::  init: start all io threads and subscribe to /effect
::
++  init
  =/  m  (strand ,drivers)
  ;<  t=drivers  bind:m  start-azimuth
  ::  only spawn once (i.e. set up keys once, and broadcast them as eth logs)
  ::
  ;<  ~          bind:m  (spawn ~bud)
  ;<  ~          bind:m  (spawn ~dev)
  ;<  ~          bind:m  (spawn ~marbud)
  (pure:m t)
::
++  setup
  |=  [who=@p proto=?(%mesa %ames)]
  =/  m  (strand ,~)
  ::  for every test we initialize the ship
  ::    - send %init-ship to aqua: boot from pill
  ::    - load network protocol core in ames.hoon
  ::
  ;<  ~  bind:m
    ?.  ?=(%pawn (clan:title who))
      (init-ship who fake=|)
    (init-comet who)
  ;<  ~  bind:m  (dojo who "|pass [%a %load {<proto>}]")
  :: ;<  ~  bind:m  (dojo who "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
  ;<  ~  bind:m  (dojo who "|mount %base")
  ;<  ~  bind:m  (copy-file who /app/sub/hoon sub-agent)
  ;<  ~  bind:m  (copy-file who /app/pub/hoon pub-agent)
  ;<  ~  bind:m  (dojo who "|start %sub")
  ;<  ~  bind:m  (dojo who "|start %pub")
  ::  subscribe to the %sub test agent to capture received %facts from %pub
  ::
  =/  =aqua-event:aquarium
    :+  %event  who
    [/g/aqua/watch/sub %deal [who who /] %sub %watch /aqua]
  ;<  ~  bind:m  (send-events aqua-event ~)
  (pure:m ~)
::
++  boot-with-core-and-breach
  |=  core=?(%mesa %ames)
  =/  m  (strand ,~)
  :: ;<  ~  bind:m  init
  ::  first both ships start communication using %ames
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m  (setup ~bud core)
  ;<  ~  bind:m  (setup ~dev core)
  ;<  ~  bind:m  (send-hi ~bud ~dev)
  ;<  ~  bind:m  (sleep ~s2)
  ::  load migration hash for both
  ::
  ;<  ~  bind:m  (load-migration-hash ~bud ~dev)
  ;<  ~  bind:m  (load-migration-hash ~dev ~bud)
  ::  subscribe before breaching
  ::
  ;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
  ;<  ~  bind:m  (sleep ~s2)
  ::
  ::  now we breach ~bud. if ~dev will remain as %known either In
  ::  .chums or .peers.ames state (based on the default protocol)
  ::
   ;<  ~  bind:m  (breach ~bud)
  ::  ~bud will start again using the other protocol as default core
  ::
  ;<  ~  bind:m  (setup ~bud ?:(?=(%mesa core) %ames %mesa))
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
  ;<  ~          bind:m  (end t)
  (pure:m ~)
::
++  boot-core
  |=  [sndr=@p rcvr=@p core-s=?(%ames %mesa) core-r=?(%ames %mesa)]
  =/  m  (strand ,~)
  ::  if sender has %ames as .core, the receiver will:
  ::    - drop the packet
  ::    - ask jael for the keys
  ::    - enqueue an %ahoy $plea (%mesa is default core)
  ::    - packet is processed using %ames
  ::
  ::  if receiver has %mesa as .core, the receiver will:
  ::    (if nothing outstanding and %peer as %alien)
  ::    - move the peer into chums
  ::    - drop the packet
  ::    - ask jael for the keys
  ::
  ;<  t=drivers  bind:m  init
  ;<  ~  bind:m
    ::  setup sender (and galaxies) first so %aqua doesn't crash when scrying
    ::  setup network core for the sender and receivers
    ::
    ?.  ?=(%pawn (clan:title sndr))
      ::  XX do this per-ship
      ::
      ;<  ~  bind:m  (switch-network-core core-s)
      ;<  ~  bind:m  (setup sndr core-s)
      ;<  ~  bind:m  (switch-network-core core-r)
      (setup rcvr core-r)
    ;<  ~  bind:m
      ?:  =(~bud rcvr)
        (pure:m ~)
      ::  we need to setup ~bud as well if the comet tries to talk to other galaxies
      ::
      (setup ~bud core-s)
    ;<  ~  bind:m  (switch-network-core core-r)
    ;<  ~  bind:m  (setup rcvr core-r)
    ;<  ~  bind:m  (switch-network-core core-s)
    (setup sndr core-s)
  ::  load migration hashes
  ::
  ;<  ~  bind:m  (load-migration-hash sndr rcvr)
  ;<  ~  bind:m  (load-migration-hash rcvr sndr)
  ::  first plea would be the /gf system flow plea       :: (bone = 0)
  ::
  ;<  ~  bind:m  (send-hi sndr rcvr)                     :: (bone = 4)
  ::
  ;<  ~  bind:m  (dojo sndr ":sub [%sub {<rcvr>} %pub]") :: (bone = 8)
  ::
  ::  XX wait for migration confirmation
  ::    this could be a spurious print coming from a "migrated" %ahoy $plea. these
  ::    %mesa $pleas are always acked, and the actual migration no-ops since the peer
  ::    is no longer in .peers.ames-state.
  ::
  ;<  ~  bind:m
    ?:  ?=(%mesa core-s)
      ^-  form:m
      (pure:m ~)
    (wait-for-output rcvr "ahoy: %mesa migration completed for {<sndr>}")
  ::  wait for the subscription to happen
  ::
  ;<  ~  bind:m  (sleep ~s2)
  ;<  ~  bind:m  (dojo rcvr ":pub send+`(list [path @])`[/hola 1]~")
  ::  check that sndr receives the gift after migration
  ::
  ;<  =noun  bind:m
    (wait-for-fact sndr %noun /aqua/watch/sub (gate ,(list [path @]) [/hola 1]~))
  ;<  ~      bind:m  (end t)
  (pure:m ~)
::
--
