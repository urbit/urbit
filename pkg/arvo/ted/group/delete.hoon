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
=+  !<([~ =action:view] arg)
?>  ?=(%remove -.action)
=*  rid  resource.action
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(our.bowl entity.rid)
=/  push-hook-act=cage
  :-  %push-hook-action
  !>  ^-  action:push-hook
  [%remove resource.action]
;<  ~  bind:m  (cleanup-md:view rid)
;<  ~  bind:m  (poke-our %group-store %group-update-0 !>([%remove-group rid ~]))
;<  ~  bind:m  (poke-our %metadata-push-hook push-hook-act)
;<  ~  bind:m  (poke-our %contact-push-hook push-hook-act)
;<  ~  bind:m  (poke-our %group-push-hook push-hook-act)
(pure:m !>(~))
