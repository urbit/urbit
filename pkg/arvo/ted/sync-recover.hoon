/-  spider 
/+  strandio, resource
=,  strand=strand:spider
|%
+$  app   ?(%graph %metadata %group %contact)
++  poke-our  poke-our:strandio
++  poke      poke:strandio
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =app rid=resource] arg)
=/  pull-hook
  (crip "{(trip app)}-pull-hook")
=/  push-hook
  (crip "{(trip app)}-push-hook")
;<  ~  bind:m  (poke-our pull-hook hook-desync+!>(rid))
;<  ~  bind:m  (poke [entity.rid push-hook] hook-desync+!>(rid))
;<  ~  bind:m  (poke-our pull-hook hook-resync+!>(rid))
(pure:m *vase)

