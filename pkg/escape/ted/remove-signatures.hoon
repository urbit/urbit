/-  spider
/+  strandio, store=graph-store, gra=graph
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand:spider ,vase)
^-  form:m
=+  !<([~ =update:store] arg)
?>  ?=(%remove-signatures -.q.update)
=*  poke-our  poke-our:strandio
;<  ~  bind:m
  (poke-our:strandio %graph-push-hook %graph-update-3 !>(update))
(pure:m *vase)
