/-  spider,
    metadata=metadata-store,
    *group,
    inv=invite-store,
    store=group-store,
    push-hook
/+  strandio, resource, view=group-view, grpl=group
=>
|%
++  strand    strand:spider
++  poke      poke:strandio
++  poke-our  poke-our:strandio
++  gallify-bowl
  |=  =bowl:spider
  ^-  bowl:gall
  :*  [our src %$]:bowl
      [~ ~]
      [0 eny now byk]:bowl
  ==
::
++  invite-ships
  |=  [ships=(set ship) rid=resource description=cord]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  =/  =action:inv
    :^  %invites  %groups  (shaf %group-uid eny.bowl)
    ^-  multi-invite:inv
    [our.bowl %group-push-hook rid ships description]
  ;<  ~  bind:m  (poke-our %invite-hook invite-action+!>(action))
  (pure:m ~)
::
++  add-pending
  |=  [ships=(set ship) rid=resource]
  =/  m  (strand ,~)
  ^-  form:m
  =/  =action:store
    [%change-policy rid %invite %add-invites ships]
  ;<  ~  bind:m  (poke-our %group-push-hook %group-update-0 !>(action))
  (pure:m ~)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:view] arg)
?>  ?=(%invite -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  =bowl:gall  (gallify-bowl bowl)
?>  (~(is-admin grpl bowl) our.bowl resource.action)
;<  ~  bind:m 
  (invite-ships [ships resource description]:action)
=/  =group
  (need (~(scry-group grpl bowl) resource.action))
?:  ?=(%open -.policy.group)
  (pure:m !>(~))
;<  ~  bind:m  (add-pending [ships resource]:action)
(pure:m !>(~))
