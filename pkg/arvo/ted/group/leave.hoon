/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    pull-hook
/+  strandio, resource, view=group-view
=>
|%
++  strand        strand:spider
++  poke          poke:strandio
++  raw-poke-our  raw-poke-our:strandio
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
;<  ~  bind:m  (raw-poke-our %contact-pull-hook pull-hook-act)
;<  ~  bind:m  (raw-poke-our %metadata-pull-hook pull-hook-act)
;<  ~  bind:m  (raw-poke-our %group-pull-hook pull-hook-act)
;<  ~  bind:m  (raw-poke-our %group-store %group-update-0 !>([%remove-group rid ~]))
;<  ~  bind:m  (cleanup-md:view rid)
(pure:m !>(~))
