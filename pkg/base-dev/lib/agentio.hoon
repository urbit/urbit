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
    |=  =note-arvo
    ^-  card
    [%pass wire %arvo note-arvo]
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
    (arvo %b %wait p)
  ::
  ++  rest
    |=  p=@da
    (arvo %b %rest p)
  ::
  ++  warp
    |=  [wer=ship =riff:clay]
    (arvo %c %warp wer riff)
  ::
  ++  warp-our
    |=  =riff:clay
    (warp our.bowl riff)
  ::
  ::  right here, right now
  ++  warp-slim
    |=  [genre=?(%sing %next) =care:clay =path]
    =/  =mood:clay
      [care r.byk.bowl path]
    =/  =rave:clay
      ?:(?=(%sing genre) [genre mood] [genre mood])
    (warp-our q.byk.bowl `rave)
  ::
  ++  connect
    |=  [=binding:eyre app=term]
    (arvo %e %connect binding app)
  --
::
++  fact-curry
  |*  [=mark =mold]
  |=  [paths=(list path) fac=mold]
  (fact mark^!>(fac) paths)
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
