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
  =/  m  (strand ,(unit resource))
  ;<  pax=(unit path)  bind:m
    %+  scry:strandio   ,(unit path)
    ;:  weld
      /gx/metadata-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  (pure:m (bind pax de-path:resource))
::
++  scry-group
  |=  rid=resource
  =/  m  (strand ,(unit resource))
  ;<  ugroup=(unit group)  bind:m
    %+  scry:strandio   ,(unit group)
    ;:  weld
      /gx/group-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  (pure:m (need ugroup))
::
++  delete-graph
  |=  rid=resource
  =/  m  (strand ,~)
  ;<  ~  bind:m
    (poke-our %graph-pull-hook %pull-hook-action [%remove rid.action])
  ;<  ~  bind:m
    (poke-our %graph-store %graph-update [%archive-graph rid.action])
  (pure:m ~)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([=action:graph-view ~] arg)
?>  ?=(%leave -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?<  =(our.bowl entity.rid.action)
;<  ugroup-rid=(unit resource)  bind:m  
  (scry-metadata rid.action)
?~  ugroup-rid  (fail:m %nonexistent)
;<  ugroup=(unit group)
  (scry-group u.ugroup-rid)
?~  ugroup  (fail:m %nonexistent)
?.  hidden.u.ugroup
  (delete-graph rid.action)
;<  ~  bind:m
  (poke-our %group-push-hook %pull-hook-action [%remove rid.action])
;<  ~  bind:m
  (poke-our %group-store %group-action !>([%remove-group rid.action])
;<  ~  bind:m  (delete-graph rid.action)
(pure:m !>(~))
