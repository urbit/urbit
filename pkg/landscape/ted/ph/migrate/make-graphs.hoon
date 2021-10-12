/-  spider,
    graph-store,
    graph-view,
    post,
    *resource
/+  *ph-io, strandio
=,  strand=strand:spider
=>
|%
::
++  graph-post
  |=  [our=@p wen=@da rid=resource body=cord id=@]
  =/  =index:post  [id]~
  =/  =post:post  [our index wen [%text body]~ ~ ~]
  =/  =node:graph-store  [[%& post] %empty ~]
  =/  act=update:graph-store  [wen %add-nodes rid (my [index node] ~)]
  (poke-app our %graph-push-hook %graph-update-3 act)
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
::  create graphs
::
=/  group-rid   [~zod %group-1]
=/  group-path  /ship/~zod/group-1
=/  create-1=action:graph-view
  :*  %create
      [~zod %graph-1]
      'graph 1'
      'desc 1'
      `%graph-validator-chat
      [%group group-rid]
      'fake'
  ==
::
=/  create-2=action:graph-view
  :*  %create
      [~bus %graph-2]
      'graph 2'
      'desc 2'
      `%graph-validator-chat
      [%group group-rid]
      'fake'
  ==
::
=/  create-3=action:graph-view
  :*  %create
      [~web %graph-3]
      'graph 3'
      'desc 3'
      `%graph-validator-chat
      [%policy %invite (sy ~zod ~bus ~)]
      'fake'
  ==
::
;<  ~  bind:m  (poke-app ~zod %group-store %group-update-0 [%add-tag group-rid %admin (sy ~bus ~)])
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~web ~bus)

;<  ~  bind:m  (dojo-thread ~zod %graph-create %graph-view-action create-1)
;<  ~  bind:m  (dojo-thread ~bus %graph-create %graph-view-action create-2)
;<  ~  bind:m  (dojo-thread ~web %graph-create %graph-view-action create-3)
;<  ~  bind:m  (sleep ~s30)
::
::  join graphs
::
=/  join-1=action:graph-view
  [%join [~zod %graph-1] ~zod]
=/  join-2=action:graph-view
  [%join [~bus %graph-2] ~bus]
=/  join-3=action:graph-view
  [%join [~web %graph-3] ~web]
::
;<  ~  bind:m  (sleep ~s10)
;<  ~  bind:m  (poke-app ~zod %group-view %group-view-action join-3)
;<  ~  bind:m  (poke-app ~bus %group-view %group-view-action join-3)
;<  ~  bind:m  (dojo-thread ~web %graph-join %graph-view-action join-1)
;<  ~  bind:m  (dojo-thread ~bus %graph-join %graph-view-action join-1)
;<  ~  bind:m  (dojo-thread ~zod %graph-join %graph-view-action join-2)
;<  ~  bind:m  (dojo-thread ~web %graph-join %graph-view-action join-2)
;<  ~  bind:m  (sleep ~s30)
::
::  make posts
::
;<  ~  bind:m  (graph-post ~zod now.bol [~zod %graph-1] 'post 1' 1)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~zod %graph-1] 'post 2' 2)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~zod %graph-1] 'post 3' 3)
;<  ~  bind:m  (sleep ~s5)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~bus %graph-2] 'post 4' 4)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~bus %graph-2] 'post 5' 5)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~bus %graph-2] 'post 6' 6)
;<  ~  bind:m  (sleep ~s5)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~web %graph-3] 'post 7' 7)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~web %graph-3] 'post 8' 8)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~web %graph-3] 'post 9' 9)
;<  ~  bind:m  (sleep ~s5)
::
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~web ~bus)
::
(pure:m *vase)
