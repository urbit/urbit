=>
  |%
  ++  card  card:agent:gall
  --
::
|_  =bowl:gall
++  scry
  |*  [desk=@tas =path]
  ?>  ?=(^ path)
  ?>  ?=(^ t.path)
  %+  weld
    /(scot %p our.bowl)/[desk]/(scot %da now.bowl)
  t.t.path
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
    (arvo %b %wait p)
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
++  fact
  |=  [=cage paths=(list path)]
  ^-  card
  [%give %fact paths cage]
::
++  kick
  |=  paths=(list path)
  [%give %kick paths ~]
::
++  kick-only
  |=  [=ship paths=(list path)]
  [%give %kick paths `ship]
--
