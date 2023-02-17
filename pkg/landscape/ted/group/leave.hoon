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
=/  leave=cage
  :-  %group-update-0
  !>  ^-  update:store
  [%remove-members rid (silt our.bowl ~)]
=/  remove=cage
  :-  %group-update-0
  !>  ^-  update:store
  [%remove-group rid ~]
;<  ~  bind:m
  (raw-poke-our %group-push-hook leave)
;<  ~  bind:m
  (raw-poke-our %group-pull-hook pull-hook-act)
;<  ~  bind:m
  (raw-poke-our %contact-pull-hook pull-hook-act)
;<  ~  bind:m
  (raw-poke-our %group-store remove)
;<  ~  bind:m
  (raw-poke-our %group-view group-view-action+!>([%done rid]))
(pure:m !>(~))
