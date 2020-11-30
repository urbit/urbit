/-  spider, grp=group-store, gra=graph-store, met=metadata-store, hook=metadata-hook
/+  strandio, res=resource
::
=*  strand    strand:spider
=*  raw-poke  raw-poke:strandio
=*  scry      scry:strandio
::
=>
|%
++  remove-metadata
  |=  entries=(list [g=group-path:met m=md-resource:met])
  ^-  form:m
  |-  ^-  form:m
  =*  loop  $
  ?~  entries
    (pure:m !>(~))
  ;<  ~  bind:m
    %+  raw-poke
      [our.bowl %metadata-store]
    :-  %metadata-action
    !>  ^-  metadata-action:met
    [%remove g.i.entries m.i.entries]
  loop(entries t.entries)
::
++  archive-graphs
  |=  graphs=(list path)
  |-  ^-  form:m
  =*  loop  $
  ?~  graphs
    (pure:m !>(~))
  ;<  ~  bind:m
    %+  raw-poke
      [our.bowl %graph-store]
    :-  %graph-update
    !>  ^-  update:gra
    [%0 now.bowl [%archive-graph (de-path:res i.graphs)]]
  loop(graphs t.graphs)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([=update:grp ~] arg)
?.  ?=(%remove-group -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %metadata-hook]
  :-  %metadata-hook-action
  !>  ^-  metadata-hook-action:hook
  [%remove (en-path:res resource.update)]
::
::  get metadata associated with group and remove it
;<  =associations:met  bind:m
  %+  scry  associations:met
  ;:  weld
    /gx/metadata-store/group
    (en-path:res resource.update)
    /noun
  ==
;<  ~  bind:m
  (remove-metadata ~(tap in ~(key by associations)))
=/  graphs=(list path)
  %+  turn  ~(tap in ~(key by associations))
  |=  [g=group-path:met m=md-resource:met]
  ^-  path
  app-path.m
(archive-graphs graphs)
