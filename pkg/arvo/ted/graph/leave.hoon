/-  spider, graph-view, graph=graph-store, *metadata-store, *group
/+  strandio, resource
=>
|% 
++  strand  strand:spider
++  poke  poke:strandio
++  poke-our   poke-our:strandio
::
++  scry-metadata
  |=  rid=resource
  =/  m  (strand ,resource)
  ^-  form:m
  ;<  group=(unit resource)  bind:m
    %+  scry:strandio   ,(unit resource)
    ;:  weld
      /gx/metadata-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  (pure:m (need group))
::
++  scry-group
  |=  rid=resource
  =/  m  (strand ,group)
  ^-  form:m
  ;<  ugroup=(unit group)  bind:m
    %+  scry:strandio   ,(unit group)
    ;:  weld
      /gx/group-store/groups
      (en-path:resource rid)
      /noun
    ==
  (pure:m (need ugroup))
::
++  delete-graph
  |=  [now=time rid=resource]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  ~  bind:m
    (poke-our %graph-pull-hook %pull-hook-action !>([%remove rid]))
  ;<  ~  bind:m
    (poke-our %graph-store %graph-update-1 !>([now [%remove-graph rid]]))
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
;<  group-rid=resource  bind:m  (scry-metadata rid.action)
;<  g=group  bind:m  (scry-group group-rid)
;<  ~  bind:m  (delete-graph now.bowl rid.action)
?.  hidden.g
  (pure:m !>(~))
;<  =thread-result:strandio  bind:m
  (await-thread:strandio %group-leave !>([~ [%leave rid.action]]))
(pure:m !>(~))
