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
  ;<  paxs=(unit (set path))  bind:m
    %+  scry:strandio   ,(unit (set path))
    ;:  weld
      /gx/metadata-store/resource/publish
      (en-path:resource rid)
      /noun
    ==
  ?~  paxs  (pure:m ~)
  ?~  u.paxs  (pure:m ~)
  (pure:m `(de-path:resource n.u.paxs))
::
++  scry-group
  |=  rid=resource
  =/  m  (strand ,group)
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
  |=  [group=resource rid=resource app=app-name:graph-view]
  =/  m  (strand ,~)
  ::;<  ~  bind:m
  ::  (poke-our %graph-push-hook %push-hook-action [%remove rid.action])
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ;<  ~  bind:m
    (poke-our %graph-store %graph-update !>([%0 now.bowl %archive-graph rid]))
  (pure:m ~)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<(=action:graph-view arg)
?>  ?=(%delete -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?>  =(our.bowl entity.rid.action)
;<  ugroup-rid=(unit resource)  bind:m  
  (scry-metadata rid.action)
?~  ugroup-rid  !!
;<  =group  bind:m
  (scry-group u.ugroup-rid)
?.  hidden.group
  ;<  ~  bind:m
    (delete-graph rid.action)
  (pure:m !>(~))
;<  ~  bind:m
  (poke-our %group-push-hook %push-hook-action !>([%remove rid.action]))
;<  ~  bind:m
  (poke-our %group-store %group-action !>([%remove-group rid.action]))
;<  ~  bind:m  (delete-graph rid.action)
(pure:m !>(~))
