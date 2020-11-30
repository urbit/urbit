/-  spider
/+  *strandio
=>  
|%
++  strand  strand:spider
::
++  supported-apps
  ^-  (list term)
  :~  %graph-pull-hook
      %group-pull-hook
  ==
::
++  poke-all-sane
  =/  m  (strand ,~)
  ^-  form:m
  =/  apps  supported-apps
  |-  =*  loop  $
  ?~  apps
    (pure:m ~)
  =*  app  i.apps
  ;<  ~   bind:m  (poke-our app sane+!>(%sane))
  loop(apps t.apps)
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  poke-all-sane 
;<  ~  bind:m  (poke-our %sane noun+!>(%fix))
(pure:m !>("Done"))
