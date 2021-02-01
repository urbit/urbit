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
++  cleanup-md
  |=  rid=resouce
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =associations:metadata  bind:m  
    %+  scry:strandio  associations:metadata
    %+  weld  /metadata-store/group 
    (snoc (en-path:resource rid) %noun)
  =/  assocs=(list [=md-resource:metadata association:metadata])
    ~(tap by associations) 
  |-  
  =*  loop  $
  ?~  assocs
    (pure:m ~)
  ;<  ~  bind:m
    (poke-our %metadata-store metadata-action+!>([%remove rid md-resource.i.assocs]))
  loop(assocs t.assocs)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
=+  !<([~ =action:view] arg)
?>  ?=(%remove -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(our.bowl entity.resource.action)
=/  push-hook-act=cage
  :-  %push-hook-action
  !>  ^-  action:push-hook
  [%remove resource.action]
;<  ~  bind:m  (cleanup-md:view rid)
;<  ~  bind:m  (poke-our %group-store %group-update !>([%remove-group rid ~]))
;<  ~  bind:m  (poke-our %metadata-push-hook push-hook-act)
;<  ~  bind:m  (poke-our %group-push-hook push-hook-act)
(pure:m !>(~))
