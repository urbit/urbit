/-  spider, graph=graph-store, *metadata-store, *group, group-store
/+  strandio, resource, graph-view
=>
|%
++  strand  strand:spider
++  poke  poke:strandio
++  poke-our   poke-our:strandio
--
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<
      [~ rid=resource title=@t description=@t group=resource module=@t ~]
    arg
;<  =bowl:spider  bind:m  get-bowl:strandio
::  unarchive graph and share it
;<  ~  bind:m
  (poke-our %graph-store %graph-update !>([%0 now.bowl %unarchive-graph rid]))
;<  ~  bind:m
  (poke-our %graph-push-hook %push-hook-action !>([%add rid]))
::
::  Setup metadata
::
=/  =metadata
  %*  .  *metadata
    title         title
    description   description
    date-created  now.bowl
    creator       our.bowl
    module        module
  ==
=/  act=metadata-action
  [%add (en-path:resource group) graph+(en-path:resource rid) metadata]
;<  ~  bind:m  (poke-our %metadata-hook %metadata-action !>(act))
;<  ~  bind:m
  (poke-our %metadata-hook %metadata-hook-action !>([%add-owned (en-path:resource group)]))
(pure:m !>(~))
