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
::  make posts
::
;<  ~  bind:m  (graph-post ~zod now.bol [~zod %graph-1] 'post 10' 10)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~zod %graph-1] 'post 20' 20)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~zod %graph-1] 'post 30' 30)
;<  ~  bind:m  (sleep ~s5)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~bus %graph-2] 'post 40' 40)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~bus %graph-2] 'post 50' 50)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~bus %graph-2] 'post 60' 60)
;<  ~  bind:m  (sleep ~s5)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~web %graph-3] 'post 70' 70)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~bus now.bol [~web %graph-3] 'post 80' 80)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (graph-post ~web now.bol [~web %graph-3] 'post 90' 90)
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
