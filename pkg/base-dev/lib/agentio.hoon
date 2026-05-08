=>
  |%
  ++  card  card:agent:gall
  --
::
|_  =bowl:gall
++  scry
  |=  [desk=@tas =path]
  %+  weld
    /(scot %p our.bowl)/[desk]/(scot %da now.bowl)
  path
::
++  pass
  |_  =wire
  ++  poke
    |=  [=dock =cage]
    [%pass wire %agent dock %poke cage]
  ::
  ++  poke-our
    |=  [app=term =cage]
    ^-  card
    (poke [our.bowl app] cage)
  ::
  ++  poke-self
    |=  =cage
    ^-  card
    (poke-our dap.bowl cage)
  ::
  ++  arvo
    |=  task=task-user-v1:gall
    ^-  card
    [%pass wire %arvo task]
  ::
  ++  watch
    |=  [=dock =path]
    [%pass (watch-wire path) %agent dock %watch path]
  ::
  ++  watch-our
    |=  [app=term =path]
    (watch [our.bowl app] path)
  ::
  ++  watch-wire
    |=  =path
    ^+  wire
    ?.  ?=(~ wire)
      wire
    agentio-watch+path
  ::
  ++  leave
    |=  =dock
    [%pass wire %agent dock %leave ~]
  ::
  ++  leave-our
    |=  app=term
    (leave our.bowl app)
  ::
  ++  leave-path
    |=  [=dock =path]
    =.  wire
      (watch-wire path)
    (leave dock)
  ::
  ++  wait
    |=  p=@da
    (arvo %behn %wait p)
  ::
  ++  rest
    |=  p=@da
    (arvo %behn %rest p)
  ::
  ++  warp
    |=  [id=* wer=ship =riff:clay]
    ?~  q.riff  !!
    (arvo %clay %read id wer p.riff u.q.riff)  ::  review
  ::
  ++  warp-our
    |=  [id=* =riff:clay]
    (warp id our.bowl riff)
  ::
  ::  right here, right now
  ++  warp-slim
    |=  [genre=?(%sing %next) =care:clay =path id=*]
    =/  =mood:clay
      [care r.byk.bowl path]
    =/  =rave:clay
      ?:(?=(%sing genre) [genre mood] [genre mood])
    (warp-our id q.byk.bowl `rave)
  ::
  ++  tire
    (arvo %clay %tire `~)
  ::
  ++  connect
    |=  [=binding:eyre app=term]
    (arvo %eyre %connect binding app)
  --
::
++  fact-kick
  |=  [=path =cage]
  ^-  (list card)
  :~  (fact cage ~[path])
      (kick ~[path])
  ==
::
++  fact-init
  |=  =cage
  ^-  card
  [%give %fact ~ cage]
::
++  fact-init-kick
  |=  =cage
  ^-  (list card)
  :~  (fact cage ~)
      (kick ~)
  ==
::
++  fact
  |=  [=cage paths=(list path)]
  ^-  card
  [%give %fact paths cage]
::
++  fact-all
  |=  =cage
  ^-  (unit card)
  =/  paths=(set path)
    %-  ~(gas in *(set path))
    %+  turn  ~(tap by sup.bowl)
    |=([duct ship =path] path)
  ?:  =(~ paths)  ~
  `(fact cage ~(tap in paths))
::
++  kick
  |=  paths=(list path)
  [%give %kick paths ~]
::
++  kick-only
  |=  [=ship paths=(list path)]
  [%give %kick paths `ship]
--
