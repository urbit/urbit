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
++  join-group
  |=  our=@p
  %^  poke-app  our  %group-view
  :-  %group-view-action 
  :*  %join 
      [~zod %group-1]
      ~zod
  ==
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (sleep ~s10)
;<  ~  bind:m  end
(pure:m *vase)



