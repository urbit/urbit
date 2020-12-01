/-  spider, grp=group-store, gra=graph-store, met=metadata-store, con=contact-store
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
::  tell group host to remove us as member
::
;<  ~  bind:m
  %+  raw-poke
    [entity.resource.update %group-push-hook]
  :-  %group-update
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
::  stop serving or syncing contacts associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %contact-hook]
  :-  %contact-hook-action
  !>([%remove (en-path:res resource.update)])
::  remove contact data associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %contact-store]
  :-  %contact-action
  !>  ^-  contact-action:con
  [%delete (en-path:res resource.update)]
::  stop serving or syncing metadata associated with group
::
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %metadata-hook]
  :-  %metadata-hook-action
  !>([%remove (en-path:res resource.update)])
::  get metadata associated with group
::
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
;<  ~  bind:m
  %+  raw-poke
    [our.bowl %graph-pull-hook]
  :-  %pull-hook-action
  !>([%remove (de-path:res app-path.m.i.entries)])
loop(entries t.entries)
