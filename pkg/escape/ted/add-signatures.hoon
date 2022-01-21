/-  spider
/+  strandio, store=graph-store, gra=graph, graph-view, sig=signatures
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand:spider ,vase)
^-  form:m
=+  !<([~ =update:store] arg)
?>  ?=(%add-signatures -.q.update)
=*  poke-our  poke-our:strandio
;<  =bowl:spider  bind:m  get-bowl:strandio
=|  gall=bowl:gall
=.  gall  gall(our our.bowl, now now.bowl)
=/  =node:store  (~(got-node gra gall) uid.q.update)
?>  ?=(%& -.post.node)
=/  =signature:store
  (sign:sig our.bowl now.bowl (need hash.p.post.node))
=.  signatures.q.update  (silt signature ~)
;<  ~  bind:m
  (poke-our:strandio %graph-push-hook %graph-update-3 !>(update))
(pure:m *vase)
