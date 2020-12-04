/-  spider, graph-view, graph=graph-store, *metadata-store, *group, *metadata-store
/+  strandio, resource
=>
|% 
++  strand  strand:spider
++  poke  poke:strandio
++  poke-our   poke-our:strandio
::
++  check-live
  |=  who=ship
  =/  m  (strand ,~)
  ^-  form:m
  %+  (set-timeout:strandio ,~)  ~s20
  ;<  ~  bind:m
    (poke [who %hood] %helm-hi !>(~))
  (pure:m ~)
::
++  scry-group
  |=  rid=resource
  =/  m  (strand ,group)
  ^-  form:m
  ;<  ugroup=(unit group)  bind:m
    %+  scry:strandio  (unit group)
    %+  weld  /gx/group-store/groups
    (snoc (en-path:resource rid) %noun)
  ?>  ?=(^ ugroup)
  (pure:m u.ugroup)
::
++  scry-metadatum
  |=  rid=resource
  =/  m  (strand ,metadata)
  ^-  form:m
  =/  enc-path=@t  (scot %t (spat (en-path:resource rid)))
  ;<  umeta=(unit metadata)  bind:m
    %+  scry:strandio  (unit metadata)
    %+  weld  /gx/metadata-store/metadata
    /[enc-path]/graph/[enc-path]/noun
  ?>  ?=(^ umeta)
  (pure:m u.umeta)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%groupify -.action)
;<  =group  bind:m  (scry-group rid.action)
?.  hidden.group
  (strand-fail:strandio %bad-request ~)
;<  =metadata  bind:m  (scry-metadatum rid.action)
?~  to.action
  ;<  ~  bind:m
    %+  poke-our  %contact-view
    :-  %contact-view-action
    !>([%groupify rid.action title.metadata description.metadata])
  (pure:m !>(~))
;<  new=^group  bind:m  (scry-group u.to.action)
?<  hidden.new
=/  new-path  (en-path:resource u.to.action)
=/  app-path  (en-path:resource rid.action)
=/  add-md=metadata-action
  [%add new-path graph+app-path metadata]
;<  ~  bind:m
  (poke-our %metadata-store metadata-action+!>(add-md))
;<  ~  bind:m
  %+  poke-our  %metadata-store
  metadata-action+!>([%remove app-path graph+app-path])
;<  ~  bind:m
  (poke-our %group-store %group-update !>([%remove-group rid.action ~]))
(pure:m !>(~))
