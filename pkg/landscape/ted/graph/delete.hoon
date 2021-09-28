/-  spider, graph-view, graph=graph-store, metadata=metadata-store, *group, group-store
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
  |=  [group-rid=resource rid=resource]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ;<  ~  bind:m
    (poke-our %graph-store %graph-update-3 !>([now.bowl %remove-graph rid]))
  ;<  ~  bind:m
    (poke-our %graph-push-hook %push-hook-action !>([%remove rid]))
  ;<  ~  bind:m
    %+  poke-our  %metadata-push-hook
    :-  %metadata-update-2
    !>  ^-  action:metadata
    [%remove group-rid [%graph rid]]
  (pure:m ~)
::
++  delete-tags
  |=  [graph=resource grp-rid=resource =group]
  =/  m  (strand ,~)
  ^-  form:m
  =/  tags=(list [=tag tagged=(set ship)])
    %+  skim  ~(tap by tags.group) 
    |=  [=tag tagged=(set ship)]
    ?@  tag  %.n
    ?&  =(app.tag %graph)
        =(resource.tag graph)
    ==
  |-  =*  loop  $
  ^-  form:m
  ?~  tags
    (pure:m ~)
  ;<  ~  bind:m
    %+  poke  [entity.grp-rid %group-push-hook]
    :-  %group-update-0
    !>  ^-  update:group-store
    [%remove-tag grp-rid tag.i.tags tagged.i.tags]
  loop(tags t.tags)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%delete -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?.  =(our.bowl entity.rid.action)
  (strand-fail:strandio %bad-request ~)
;<  group-rid=resource  bind:m  
  (scry-metadata rid.action)
;<  =group  bind:m
  (scry-group group-rid)
;<  ~  bind:m
  (delete-tags rid.action group-rid group)
;<  ~  bind:m
  (delete-graph group-rid rid.action)
?.  hidden.group
  (pure:m !>(~))
;<  =thread-result:strandio  bind:m
  (await-thread:strandio %group-delete !>(`[%remove rid.action]))
(pure:m !>(~))

