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
;<  ~  bind:m  (create-group ~zod)
;<  ~  bind:m  (join-group ~bus)
;<  ~  bind:m  (join-group ~web)
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~web ~bus)
(pure:m *vase)



