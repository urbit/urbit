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
      /gx/metadata-store/resource/graph
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
  |=  [group-rid=resource rid=resource]
  =/  m  (strand ,~)
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ;<  ~  bind:m
    (poke-our %graph-store %graph-update !>([%0 now.bowl %remove-graph rid]))
  ;<  ~  bind:m
    (poke-our %graph-push-hook %push-hook-action !>([%remove rid]))
  ;<  ~  bind:m
    %+  poke-our  %metadata-hook
    :-  %metadata-action
    !>  :+  %remove 
      (en-path:resource group-rid)
    [%graph (en-path:resource rid)]
  (pure:m ~)
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
;<  ugroup-rid=(unit resource)  bind:m  
  (scry-metadata rid.action)
?~  ugroup-rid  !!
;<  =group  bind:m
  (scry-group u.ugroup-rid)
;<  ~  bind:m
  (delete-graph u.ugroup-rid rid.action)
?.  hidden.group
  (pure:m !>(~))
;<  =thread-result:strandio  bind:m
  (await-thread:strandio %group-delete !>([%delete rid.action]))
(pure:m !>(~))

