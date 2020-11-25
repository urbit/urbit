/-  spider,
    graph=graph-store,
    *metadata-store,
    *group,
    group-store,
    inv=invite-store
/+  strandio, resource, graph-view
=>
|% 
++  strand  strand:spider
++  poke  poke:strandio
++  poke-our   poke-our:strandio
::
++  handle-group
  |=  [rid=resource =associated:graph-view]
  =/  m  (strand ,resource)
  ?:  ?=(%group -.associated)
    (pure:m rid.associated)
  =/  =action:group-store
    [%add-group rid policy.associated %&]
  ;<  ~  bind:m  (poke-our %group-store %group-action !>(action))
  ;<  ~  bind:m
    (poke-our %group-push-hook %push-hook-action !>([%add rid]))
  (pure:m rid)
--
::
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([=action:graph-view ~] arg)
?>  ?=(%create -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
::
::  Add graph to graph-store
::
?.  =(our.bowl entity.rid.action)
  (strand-fail:strandio %bad-request ~)
=/  =update:graph
  [%0 now.bowl %add-graph rid.action *graph:graph mark.action %.n]
;<  ~  bind:m
  (poke-our %graph-store graph-update+!>(update))
;<  ~  bind:m
  (poke-our %graph-push-hook %push-hook-action !>([%add rid.action]))
::
::  Add group, if graph is unmanaged
::
;<  group=resource  bind:m
  (handle-group rid.action associated.action)
=/  group-path=path
  (en-path:resource group)
::
::  Setup metadata
::
=/  =metadata
  %*  .  *metadata
    title         title.action
    description   description.action
    date-created  now.bowl
    creator       our.bowl
    module        module.action
  ==
=/  =metadata-action
  [%add group-path graph+(en-path:resource rid.action) metadata]
;<  ~  bind:m
  (poke-our %metadata-hook %metadata-action !>(metadata-action))
;<  ~  bind:m
  (poke-our %metadata-hook %metadata-hook-action !>([%add-owned group-path]))
::
::  Send invites
::
?:  ?=(%group -.associated.action)
  (pure:m !>(~))
?-    -.policy.associated.action
    %open  (pure:m !>(~))
    %invite
  =/  inv-action=action:inv
    :^  %invites  %graph  (shaf %graph-uid eny.bowl)
    ^-  multi-invite:inv
    :*  our.bowl
        %graph-push-hook
        rid.action
        pending.policy.associated.action
        description.action
    ==
  ;<  ~  bind:m
    (poke-our %invite-hook %invite-action !>(inv-action))
  (pure:m !>(~))
==
