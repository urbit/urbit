/-  spider, grp=group-store
/+  strandio, res=resource
::
=*  strand    strand:spider
=*  raw-poke  raw-poke:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =update:grp] arg)
?.  ?=(%remove-members -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
?.  (~(has in ships.update) our.bowl)
  (pure:m !>(~))
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %group-store]
  :-  %group-action
  !>  ^-  action:grp
  [%remove-group resource.update ~]
(pure:m !>(~))
