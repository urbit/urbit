/-  spider
/+  *strandio
=>  
|%
++  strand  strand:spider
+$  input  ?(%fix %check)
::
++  supported-apps
  ^-  (list term)
  ~[%group-store]
::
++  poke-all-sane
  |=  =input
  =/  m  (strand ,~)
  ^-  form:m
  =/  apps  supported-apps
  |-  =*  loop  $
  ?~  apps
    (pure:m ~)
  =*  app  i.apps
  ;<  ~   bind:m  (poke-our app sane+!>(input))
  loop(apps t.apps)
--
::
^-  thread:spider
|=  vas=vase
=/  m  (strand ,vase)
=+  !<([~ in=input] vas)
;<  ~  bind:m  (poke-all-sane in)
;<  ~  bind:m  (poke-our %sane noun+!>(in))
(pure:m !>("Done"))
