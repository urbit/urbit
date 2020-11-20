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
=/  graphs=(list path)
  %+  turn
    %~  tap  in
    %~  key  by
    ^-  associations:met
    %+  scry
      %noun
    (weld /group (en-path:res resource.update))
  |=  [g=group-path:met m=md-resource:met]
  ^-  path
  app-path.m
|-
?~  groups
  (pure:m !>(~))
;<  ~  bind:m
  %+  poke-our  %graph-store
  !>  ^-  update:graph-store
  [%archive-graph (de-path:res i.groups)]
$(groups t.groups)
