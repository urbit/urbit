/-  spider, graph-view, graph=graph-store, *metadata-store, *group
/+  strandio, resource
=>
|% 
++  strand  strand:spider
++  poke  poke:strandio
++  raw-poke-our   raw-poke-our:strandio
::
++  scry-metadata
  |=  rid=resource
  %+  scry:strandio   ,(unit resource)
  ;:  weld
    /gx/metadata-store/resource/graph
    (en-path:resource rid)
    /noun
  ==
::
++  scry-group
  |=  rid=resource
  %+  scry:strandio   ,(unit group)
  ;:  weld
    /gx/group-store/groups
    (en-path:resource rid)
    /noun
  ==
::
++  delete-graph
  |=  [now=time rid=resource]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m
    (raw-poke-our %graph-pull-hook %pull-hook-action !>([%remove rid]))
  ;<  ~  bind:m
    (raw-poke-our %graph-store %graph-update-3 !>([now [%remove-graph rid]]))
  (pure:m ~)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%leave -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?:  =(our.bowl entity.rid.action)
  (strand-fail:strandio %bad-request ~)
;<  group-rid=(unit resource)  bind:m  (scry-metadata rid.action)
?~  group-rid  (pure:m !>(~))
;<  g=(unit group)  bind:m  (scry-group u.group-rid)
?~  g  (pure:m !>(~))
;<  ~  bind:m  (delete-graph now.bowl rid.action)
?.  hidden.u.g
  (pure:m !>(~))
;<  =thread-result:strandio  bind:m
  (await-thread:strandio %group-leave !>([~ [%leave rid.action]]))
(pure:m !>(~))
