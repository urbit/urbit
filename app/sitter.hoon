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
  +$  command  [%noun =cord]
  +$  poke-data
    $%  [%noun cord]
    ==
  +$  out-peer-data  ~
  +$  in-peer-data
    $%  [%comments comments=(list tape)]
    ==
  ++  tapp   (^tapp state command poke-data out-peer-data in-peer-data)
  ++  stdio  (^stdio poke-data out-peer-data)
  --
=,  trad=trad:tapp
=,  tapp-trad=tapp-trad:tapp
=,  stdio
%-  create-tapp-poke-diff:tapp
^-  tapp-core-poke-diff:tapp
|_  [=bowl:gall state]
++  handle-command
  |=  =command
  =/  m  tapp-trad
  ^-  form:m
  ?:  =(cord.command 'pull')
    ?~  subscription
      (trad-fail %no-subscription ~)
    ;<  ~  bind:m  (pull-app [target path]:u.subscription)
    (pure:m ~)
  ;<  ~  bind:m  (poke-app [our.bowl %baby] %noun 'print')
  ;<  ~  bind:m  (peer-app [our.bowl %baby] /comments)
  =.  subscription  `[[our.bowl %baby] /comments]
  ;<  ~  bind:m  (wait (add now.bowl ~s3))
  (pure:m subscription)
::
++  handle-diff
  |=  [[her=ship app=term] =path data=in-peer-data]
  =/  m  tapp-trad
  ^-  form:m
  ?>  ?=(%comments -.data)
  ~&  sitter-got-data=(lent comments.data)
  (pure:m subscription)
--
