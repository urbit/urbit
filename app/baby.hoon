/+  tapp
::
::  Preamble
::
=>
  |%
  +$  state
    $:  count=@ud
    ==
  +$  command  cord
  ++  tapp  (^tapp state command)
  --
=,  create-tapp=create-tapp:tapp
=,  trad=trad:tapp
=,  tapp-core=tapp-core:tapp
=,  tapp-trad=tapp-trad:tapp
=,  helpers:tapp
::
::  The app
::
%-  create-tapp
^-  tapp-core
|_  [=bowl:gall state]
++  handle-command
  |=  =command
  =/  m  tapp-trad
  ^-  form:m
  =/  =hiss:eyre
    :*  purl=(rash command auri:de-purl:html)
        meth=%get
        math=~
        body=~
    ==
  ;<  ~           bind:m  (send-hiss hiss)
  ;<  =httr:eyre  bind:m  expect-sigh
  ~&  [%fetched count httr]
  (pure:m +(count))
--
