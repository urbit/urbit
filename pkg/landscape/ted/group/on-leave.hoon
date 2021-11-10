/-  spider, grp=group-store, gra=graph-store, met=metadata-store
/+  strandio, res=resource
::
=*  strand    strand:spider
=*  raw-poke  raw-poke:strandio
=*  raw-poke-our  raw-poke-our:strandio
=*  scry      scry:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =update:grp] arg)
?.  ?=(%remove-group -.update)
  (pure:m !>(~))
;<  =bowl:spider  bind:m  get-bowl:strandio
::  tell group host to remove us as member
::
;<  ~  bind:m
  %+  raw-poke
    [entity.resource.update %group-push-hook]
  :-  %group-update-0
  !>  ^-  update:grp
  [%remove-members resource.update (silt [our.bowl ~])]
::  stop serving or syncing group updates
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %group-push-hook]
  :-  %push-hook-action
  !>([%remove resource.update])
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %group-pull-hook]
  :-  %pull-hook-action
  !>([%remove resource.update])
::  stop serving or syncing metadata associated with group
::
;<  ~  bind:m
  %-  raw-poke-our
  ?:  =(our.bowl entity.resource.update)
    :-  %metadata-push-hook
    push-hook-action+!>([%remove resource.update])
  :-  %metadata-pull-hook
  pull-hook-action+!>([%remove resource.update])
;<  =associations:met  bind:m
  %+  scry  associations:met
  ;:  weld
    /gx/metadata-store/group
    (en-path:res resource.update)
    /noun
  ==
=/  entries=(list [m=md-resource:met g=resource:res *])
  ~(tap by associations)
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
  !>  ^-  action:met
  [%remove g.i.entries m.i.entries]
::  archive graph associated with group
::
=*  app-resource  resource.m.i.entries
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %graph-store]
  :-  %graph-update-3
  !>  ^-  update:gra
  [now.bowl [%archive-graph app-resource]]
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %graph-pull-hook]
  :-  %pull-hook-action
  !>([%remove app-resource])
loop(entries t.entries)
