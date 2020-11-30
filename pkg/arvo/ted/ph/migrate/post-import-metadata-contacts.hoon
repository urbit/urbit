/-  spider,
    contact-view,
    contact-store,
    group-store,
    metadata-store,
    post,
    graph-store,
    *resource
/+  *ph-io, strandio
=,  strand=strand:spider
::
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
::  test metadata import
::
=/  change-group-1=metadata-action:metadata-store
  :*  %add
      /ship/~zod/group-1
      [%contacts /ship/~zod/group-1]
      'New Group 1 Title'
      'new description'
      0x0
      now.bol
      ~zod
      'fake'
  ==
=/  change-web-book=metadata-action:metadata-store
  :*  %add
      /ship/~web/graph-3
      [%graph /ship/~web/graph-3]
      'New Graph 3 Title'
      'new description'
      0x0
      now.bol
      ~web
      'fake'
  ==
;<  ~  bind:m  (poke-app ~zod %metadata-hook %metadata-action change-group-1)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (poke-app ~web %metadata-hook %metadata-action change-web-book)
;<  ~  bind:m  (sleep ~s5)
::
::  test contacts import
::
=/  add-zod=contact-action:contact-store
  :*  %add  /ship/~zod/group-1  ~zod
      'ZOD'  ''  ''  ''  ''  0x0  ~
  ==
=/  add-bus=contact-action:contact-store
  :*  %add  /ship/~zod/group-2  ~bus
      'BUS'  ''  ''  ''  ''  0x0  ~
  ==
;<  ~  bind:m  (poke-app ~zod %contact-hook %contact-action add-zod)
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m  (poke-app ~bus %contact-hook %contact-action add-bus)
(pure:m *vase)
