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
  ;<  res=(unit resource)  bind:m
    %+  scry:strandio   ,(unit resource)
    ;:  weld
      /gx/metadata-store/resource/graph
      (en-path:resource rid)
      /noun
    ==
  (pure:m res)
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
?>  ?=(^ group)
::  We have group, graph is managed
;<  ~  bind:m  
   %+  poke-our  %graph-pull-hook
   pull-hook-action+!>([%add ship.action rid.action])
(pure:m !>(~))

