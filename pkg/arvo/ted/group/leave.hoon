/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    pull-hook
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
?>  ?=(%leave -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
=*  rid  resource.action
=/  pull-hook-act=cage
  :-  %pull-hook-action
  !>  ^-  action:pull-hook
  [%remove rid]
;<  ~  bind:m  (poke-our %contact-pull-hook pull-hook-act)
;<  ~  bind:m  (poke-our %metadata-pull-hook pull-hook-act)
;<  ~  bind:m  (poke-our %group-pull-hook pull-hook-act)
;<  ~  bind:m  (poke-our %group-store %group-update !>([%remove-group rid ~]))
;<  ~  bind:m  (cleanup-md:view rid)
(pure:m !>(~))
