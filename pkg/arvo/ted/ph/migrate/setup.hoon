/-  spider,
    graph-store,
    graph-view,
    post,
    *resource,
    *group
/+  *ph-io, strandio
=,  strand=strand:spider
=>
|%
::
++  create-group
  |=  our=@p
  %^  dojo-thread  our  %group-create 
  :-  %group-view-action 
  :*  %create 
      %group-1
      [%open ~ ~]
      'Test Group'
      'A description'
  ==
::
++  hang
  =/  m  (strand ,~)
  ^-  form:m
  |=  tin=strand-input:strand
  `[%wait ~]
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  hang
(pure:m *vase)



