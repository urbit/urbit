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
  +$  peek-data  _!!
  +$  in-poke-data   [%noun =cord]
  +$  out-poke-data  [%noun =cord]
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
    ==
  ++  tapp   (^tapp state peek-data in-poke-data out-poke-data in-peer-data out-peer-data)
  ++  stdio  (^stdio out-poke-data out-peer-data)
  --
=,  async=async:tapp
=,  tapp-async=tapp-async:tapp
=,  stdio
%-  create-tapp-poke-diff:tapp
^-  tapp-core-poke-diff:tapp
|_  [=bowl:gall state]
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?:  =(cord.in-poke-data 'pull')
    ?~  subscription
      (async-fail %no-subscription ~)
    ;<  ~  bind:m  (pull-app [target path]:u.subscription)
    (pure:m ~)
  =/  target  [our.bowl %example-tapp-fetch]
  ;<  ~  bind:m  (poke-app target %noun 'print')
  ;<  ~  bind:m  (peer-app target /comments)
  =.  subscription  `[target /comments]
  ;<  ~  bind:m  (wait (add now.bowl ~s3))
  (pure:m subscription)
::
++  handle-diff
  |=  [[her=ship app=term] =path data=in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ?>  ?=(%comments -.data)
  ~&  subscriber-got-data=(lent comments.data)
  (pure:m subscription)
--
