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
  ;<  pax=(unit (set path))  bind:m
    %+  scry:strandio   ,(unit (set path))
    ;:  weld
      /gx/metadata-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  ?>  ?=(^ pax)
  ?>  ?=(^ u.pax)
  (pure:m (de-path:resource n.u.pax))
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
    (poke-our %graph-store %graph-update !>([%0 now [%remove-graph rid]]))
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
?:  =(our.bowl entity.rid.action)
  (strand-fail:strandio %bad-request ~)
;<  group-rid=resource  bind:m  (scry-metadata rid.action)
;<  g=group  bind:m  (scry-group group-rid)
?.  hidden.g
  ;<  ~  bind:m  (delete-graph now.bowl rid.action)
  (pure:m !>(~))
;<  ~  bind:m
  %+  poke-our  %metadata-hook
  metadata-hook-action+!>([%remove (en-path:resource rid.action)])
;<  ~  bind:m
  %+  poke-our  %metadata-store
  :-  %metadata-action
  !>  :+  %remove 
    (en-path:resource rid.action)
  [%graph (en-path:resource rid.action)]
;<  ~  bind:m
  (poke-our %group-pull-hook %pull-hook-action !>([%remove rid.action]))
;<  ~  bind:m
  (poke-our %group-store %group-action !>([%remove-group rid.action ~]))
;<  ~  bind:m  (delete-graph now.bowl rid.action)
(pure:m !>(~))
