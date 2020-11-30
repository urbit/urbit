/-  spider, grp=group-store, gra=graph-store, met=metadata-store
/+  strandio, res=resource
::
=*  strand    strand:spider
=*  poke-our  poke-our:strandio
=*  scry      scry:strandio
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([=update:grp ~] arg)
?.  ?=(%remove-group -.update)
  (pure:m !>(~))
::
::  get graphs associated with group and archive them
;<  =associations:met  bind:m
  %+  scry  associations:met
  ;:  weld
    /gx/metadata-store/resource/group
    (en-path:res resource.update)
    /noun
  ==
=/  graphs=(list path)
  %+  turn  ~(tap in ~(key by associations))
  |=  [g=group-path:met m=md-resource:met]
  ^-  path
  app-path.m
;<  =bowl:spider  bind:m  get-bowl:strandio
|-  ^-  form:m
=*  loop  $
?~  graphs
  (pure:m !>(~))
;<  ~  bind:m
  %^    poke-our
      %graph-store
    %graph-update
  !>  ^-  update:gra
  [%0 now.bowl [%archive-graph (de-path:res i.graphs)]]
loop(graphs t.graphs)
