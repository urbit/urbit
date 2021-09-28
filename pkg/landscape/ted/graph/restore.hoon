/-  spider, graph=graph-store, met=metadata-store, *group, group-store, push-hook
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
  (poke-our %graph-store %graph-update-3 !>([now.bowl %unarchive-graph rid]))
;<  ~  bind:m
  (poke-our %graph-push-hook %push-hook-action !>([%add rid]))
::
::  Setup metadata
::
=/  =metadatum:met
  %*  .  *metadatum:met
    title         title
    description   description
    date-created  now.bowl
    creator       our.bowl
    config        [%graph module]
  ==
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %metadata-action
  !>  ^-  action:met
  [%add group [%graph rid] metadatum]
;<  ~  bind:m
  %+  poke-our  %metadata-push-hook
  :-  %push-hook-action
  !>  ^-  action:push-hook
  [%add group]
(pure:m !>(~))
