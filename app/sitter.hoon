/+  tapp, stdio
=>
  |%
  +$  subscription-state
    $:  her=ship
        app=term
    ==
  +$  state
    $:  subscription=(unit subscription-state)
    ==
  +$  command  cord
  +$  poke-data
    $%  [%noun cord]
    ==
  ++  tapp   (^tapp state command poke-data)
  ++  stdio  (^stdio poke-data)
  --
=,  trad=trad:tapp
=,  tapp-trad=tapp-trad:tapp
=,  stdio
%-  create-tapp:tapp
^-  tapp-core:tapp
|_  [=bowl:gall state]
++  handle-command
  |=  =command
  =/  m  tapp-trad
  ^-  form:m
  ;<  ~  bind:m  (poke-app [our.bowl %baby] %noun 'print')
  (pure:m subscription)
--
