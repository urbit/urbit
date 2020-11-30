/-  spider, grp=group-store, gra=graph-store, met=metadata-store, hook=metadata-hook
/+  strandio, res=resource
::
=*  strand    strand:spider
=*  raw-poke  raw-poke:strandio
=*  scry      scry:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([=update:grp ~] arg)
?.  ?=(%remove-group -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
::  stop serving or syncing metadata associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %metadata-hook]
  :-  %metadata-hook-action
  !>  ^-  metadata-hook-action:hook
  [%remove (en-path:res resource.update)]
::
::  get metadata associated with group
;<  =associations:met  bind:m
  %+  scry  associations:met
  ;:  weld
    /gx/metadata-store/group
    (en-path:res resource.update)
    /noun
  ==
=/  entries=(list [g=group-path:met m=md-resource:met])
  ~(tap in ~(key by associations))
|-  ^-  form:m
=*  loop  $
?~  entries
  (pure:m !>(~))
::  remove metadata associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %metadata-store]
  :-  %metadata-action
  !>  ^-  metadata-action:met
  [%remove g.i.entries m.i.entries]
::  archive graph associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %graph-store]
  :-  %graph-update
  !>  ^-  update:gra
  [%0 now.bowl [%archive-graph (de-path:res app-path.m.i.entries)]]
loop(entries t.entries)
