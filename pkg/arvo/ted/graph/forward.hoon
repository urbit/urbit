/-  spider, graph-view, graph=graph-store, *metadata-store, *group
/+  strandio, resource
=>
|% 
++  strand    strand:spider
++  poke      poke:strandio
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<(=action:graph-view arg)
?>  ?=(%forward -.action)
;<  ~  bind:m  
  %+  (map-err:strandio ,~)  |=(* [%forbidden ~])
  %+  poke
    [entity.rid.action %graph-push-hook]
  [%graph-update !>(update.action)]
(pure:m !>(~))
