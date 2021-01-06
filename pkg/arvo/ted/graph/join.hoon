/-  spider, graph-view, graph=graph-store, *metadata-store, *group
/+  strandio, resource
=>
|% 
++  strand  strand:spider
++  fail    strand-fail:strand
++  poke  poke:strandio
++  poke-our   poke-our:strandio
::
++  scry-metadata
  |=  rid=resource
  =/  m  (strand ,(unit resource))
  ^-  form:m
  ;<  pax=(unit (set path))  bind:m
    %+  scry:strandio   ,(unit (set path))
    ;:  weld
      /gx/metadata-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  %-  pure:m
  ?~  pax  ~
  ?~  u.pax  ~
  `(de-path:resource n.u.pax)
::
++  wait-for-group-join
  |=  rid=resource
  =/  m  (strand ,~)
  ^-  form:m
  =/  pax
    (en-path:resource rid)
  =/  hold=@dr  ~s0..8000
  |-  ^-  form:m
  =*  loop  $
  ;<  u-group=(unit group)  bind:m  
    (scry:strandio ,(unit group) (weld /gx/group-store/groups (snoc pax %noun)))
  ?^  u-group
    (pure:m ~)
  ;<  ~  bind:m  (sleep:strandio hold)
  =.  hold  (min (mul hold 2) ~m5)
  loop
::
++  wait-for-md
  |=  rid=resource
  =/  m  (strand ,~)
  ^-  form:m
  =/  pax
    (en-path:resource rid)
  =/  hold=@dr  ~s0..8000
  |-  ^-  form:m
  =*  loop  $
  ;<  groups=(set path)  bind:m  
    (scry:strandio ,(set path) /gy/metadata-store/group-indices) 
  ?:  (~(has in groups) pax)
    (pure:m ~)
  ;<  ~  bind:m  (sleep:strandio hold)
  =.  hold  (min (mul hold 2) ~m5)
  loop
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =action:graph-view] arg)
?>  ?=(%join -.action)
;<  =bowl:spider  bind:m  get-bowl:strandio
?:  =(our.bowl entity.rid.action)
  (fail %bad-request ~)
;<  group=(unit resource)  bind:m  (scry-metadata rid.action)
?^  group
  ::  We have group, graph is managed
  ;<  ~  bind:m  
     %+  poke-our  %graph-pull-hook
     pull-hook-action+!>([%add ship.action rid.action])
  (pure:m !>(~))
:: Else, add group then join
;<  ~  bind:m  
  %+  (map-err:strandio ,~)  |=(* [%forbidden ~])
  %+  poke
    [ship.action %group-push-hook]
  group-update+!>([%add-members rid.action (sy our.bowl ~)])
::
;<  ~  bind:m
  %+  poke-our  %group-pull-hook
  pull-hook-action+!>([%add ship.action rid.action])
;<  ~  bind:m  (wait-for-group-join rid.action)
::
;<  ~  bind:m
  %+  poke-our  %metadata-hook
  metadata-hook-action+!>([%add-synced ship.action (en-path:resource rid.action)])
::
;<  ~  bind:m  (wait-for-md rid.action)
::
;<  ~  bind:m  
  %+  poke-our  %graph-pull-hook
  pull-hook-action+!>([%add ship.action rid.action])
(pure:m !>(~))
