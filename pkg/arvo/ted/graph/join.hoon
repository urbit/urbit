/-  spider, graph-view, graph=graph-store, *metadata-store, *group
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
++  scry-metadata
  |=  [app=app-name:graph-view rid=resource]
  =/  m  (strand ,(unit resource))
  ^-  form:m
  ;<  pax=(unit (set path))  bind:m
    %+  scry:strandio   ,(unit (set path))
    ;:  weld
      /gx/metadata-store/resource/[app]
      (en-path:resource rid)
      /noun
    ==
  %-  pure:m
  ?~  pax  ~
  ?~  u.pax  ~
  `(de-path:resource n.u.pax)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<(=action:graph-view arg)
?>  ?=(%join -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?<  =(our.bowl entity.rid.action)
;<  group=(unit resource)  bind:m  
  (scry-metadata app.action rid.action)
;<  ~  bind:m  (check-live entity.rid.action)
?^  group
  ::  We have group, graph is managed
  ;<  ~  bind:m  
     %+  poke-our   %graph-pull-hook
     pull-hook-action+!>([%add ship.action rid.action])
  (pure:m !>(~))
:: Else, add group then join
;<  ~  bind:m  
  %+  poke
    [ship.action %group-push-hook]
  group-update+!>([%add-members rid.action (sy our.bowl ~)])
::
;<  ~  bind:m
  %+  poke-our  %group-pull-hook
  pull-hook-action+!>([%add ship.action rid.action])
::
;<  ~  bind:m
  %+  poke-our  %metadata-hook
  metadata-hook-action+!>([%add-synced ship.action rid.action])
::
;<  ~  bind:m  
  %+  poke-our  %graph-pull-hook
  pull-hook-action+!>([%add ship.action rid.action])
(pure:m !>(~))
