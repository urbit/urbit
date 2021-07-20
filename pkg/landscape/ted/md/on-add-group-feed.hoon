/-  spider, graph-view, met=metadata-store
/+  strandio
::
=*  strand        strand:spider
=*  poke-our      poke-our:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =update:met] arg)
?.  ?=(%add -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
?:  =(our.bowl entity.group.update)
  (pure:m !>(~))
?.  ?=(%group -.config.metadatum.update)
  (pure:m !>(~))
?~  feed.config.metadatum.update
  (pure:m !>(~))
?~  u.feed.config.metadatum.update
  (pure:m !>(~))
=*  feed  u.u.feed.config.metadatum.update
;<  ~  bind:m
  %+  poke-our  %spider
  =-  spider-start+!>([`tid.bowl ~ byk.bowl %graph-join -])
  %+  slop  !>(~)
  !>  ^-  action:graph-view
  [%join resource.feed entity.resource.feed]
(pure:m !>(~))

