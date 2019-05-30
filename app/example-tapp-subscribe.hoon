/+  tapp, stdio
=>
  |%
  +$  subscription-state
    $:  target=[her=ship app=term]
        =path
    ==
  +$  state
    $:  subscription=(unit subscription-state)
    ==
  +$  in-poke-data   [%noun =cord]
  +$  out-poke-data  [%noun =cord]
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
    ==
  ++  tapp   (^tapp state in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
=,  trad=trad:tapp
=,  tapp-trad=tapp-trad:tapp
=,  stdio
%-  create-tapp-poke-diff:tapp
^-  tapp-core-poke-diff:tapp
|_  [=bowl:gall state]
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-trad
  ^-  form:m
  ?:  =(cord.in-poke-data 'pull')
    ?~  subscription
      (trad-fail %no-subscription ~)
    ;<  ~  bind:m  (pull-app [target path]:u.subscription)
    (pure:m ~)
  ;<  ~  bind:m  (poke-app [our.bowl %example-tapp-fetch] %noun 'print')
  ;<  ~  bind:m  (peer-app [our.bowl %example-tapp-fetch] /comments)
  =.  subscription  `[[our.bowl %example-tapp-fetch] /comments]
  ;<  ~  bind:m  (wait (add now.bowl ~s3))
  (pure:m subscription)
::
++  handle-diff
  |=  [[her=ship app=term] =path data=in-peer-data]
  =/  m  tapp-trad
  ^-  form:m
  ?>  ?=(%comments -.data)
  ~&  subscriber-got-data=(lent comments.data)
  (pure:m subscription)
--
