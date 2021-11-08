/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    push-hook
/+  strandio, resource, view=group-view
=>
|%
++  strand    strand:spider
++  poke      poke:strandio
++  poke-our  poke-our:strandio
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:view] arg)
?>  ?=(%create -.action)
?>  ((sane %tas) name.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
::  Add graph to graph-store
::
=/  rid=resource
  [our.bowl name.action]
=/  group-act=action:store
  [%add-group rid policy.action %.n]
;<  ~  bind:m  (poke-our %group-store %group-action !>(group-act))
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m  (poke-our %group-store %group-action !>([%add-members rid (sy our.bowl ~)]))
=/  push-hook-act=cage
  :-  %push-hook-action
  !>  ^-  action:push-hook
  [%add rid]
;<  ~  bind:m
  (poke-our %group-push-hook push-hook-act)
=/  =metadatum:metadata
  %*  .  *metadatum:metadata
    title         title.action
    description   description.action
    date-created  now.bowl
    creator       our.bowl
    config        [%group ~]
    hidden        %.n
  ==
=/  met-action=action:metadata
  [%add rid groups+rid metadatum]
;<  ~  bind:m  (poke-our %metadata-store %metadata-action !>(met-action))
;<  ~  bind:m  (poke-our %metadata-push-hook push-hook-act)
;<  ~  bind:m  (poke-our %contact-push-hook push-hook-act)
(pure:m !>(~))


