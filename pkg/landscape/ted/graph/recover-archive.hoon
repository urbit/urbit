/-  spider, graph=graph-store, met=metadata-store, *group, group-store, push-hook
/+  strandio, resource, graph-view
=>
|%
++  strand  strand:spider
++  poke  poke:strandio
++  poke-our   poke-our:strandio
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ pax=path] arg)
;<  =bowl:spider  bind:m  get-bowl:strandio
=+  .^([rid=resource mar=marked-graph:graph] %cx pax)
;<  ~  bind:m 
  (poke-our:strandio %graph-store %graph-update-3 !>([now.bowl %add-graph p.mar q.mar &]))
(pure:m *vase)
