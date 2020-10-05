/-  spider,
    chat-view,
    publish,
    contact-view,
    chat-store,
    graph-store,
    post,
    *resource,
    metadata-store
/+  *ph-io, strandio
=,  strand=strand:spider
=>
|%
++  chat-message
  |=  [our=@p =path wen=@da mes=cord]
  =/  act=action:chat-store
    :*  %message  path  `@uvH`(sham [our path mes])
        0  our  wen  [%text mes]
    ==
  (poke-app our %chat-hook %chat-action act)
::
++  publish-note
  |=  [our=@p host=@p book=@tas note=@tas title=@t body=@t]
  =/  act=action:publish  [%new-note host book note title body]
  (poke-app our %publish %publish-action act)
::
++  publish-comment
  |=  [our=@p host=@p book=@tas note=@tas body=@t]
  =/  act=action:publish  [%new-comment host book note body]
  (poke-app our %publish %publish-action act)
::
++  graph-post
  |=  [our=@p wen=@da rid=resource body=cord id=@]
  =/  =index:post  [id]~
  =/  =post:post  [our index wen [%text body]~ ~ ~]
  =/  =node:graph-store  [post %empty ~]
  =/  act=update:graph-store  [%0 wen %add-nodes rid (my [index node] ~)]
  (poke-app our %graph-push-hook %graph-update act)
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  az=tid:spider  bind:m  start-azimuth
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
::  group setup
::  - ~zod creates an open group
::  - ~zod creates and invite-only group, and invites ~bus and ~web
::  - ~bus and ~web join the first, but not the second group, to keep
::    invite-store populated
::
=/  group-1=contact-view-action:contact-view
  :*  %create
      %group-1
      [%open ~ ~]
      'Group 1'
      'this is group 1'
  ==
=/  group-2=contact-view-action:contact-view
  :*  %create
      %group-2
      [%invite (sy ~bus ~web ~)]
      'Group 2'
      'this is group 2'
  ==
=/  join=contact-view-action:contact-view  [%join ~zod %group-1]
;<  ~  bind:m  (poke-app ~zod %contact-view %contact-view-action group-1)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~zod %contact-view %contact-view-action group-2)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~bus %contact-view %contact-view-action join)
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~web %contact-view %contact-view-action join)
;<  ~  bind:m  (wait-for-output ~web ">=")
;<  ~  bind:m  (sleep ~s20)
::
::  chat setup
::  - ~zod creates a chat associated with group-1
::  - ~bus creates a chat associated with group-1
::  - ~web creates a dm with ~zod
::
=/  chat-1=action:chat-view
  :*  %create
      'Chat 1'  ''
      /~zod/chat-1
      /ship/~zod/group-1
      [%invite ~]
      ~
      %.y
      %.n
  ==
=/  chat-2=action:chat-view
  :*  %create
      'Chat 2'  ''
      /~bus/chat-2
      /ship/~zod/group-1
      [%invite ~]
      ~
      %.y
      %.n
  ==
=/  web-zod-dm=action:chat-view
  :*  %create
      '~web <-> ~zod'  ''
      /~web/dm--zod
      /ship/~web/dm--zod
      [%invite (sy ~zod ~)]
      (sy ~zod ~)
      %.y
      %.n
  ==
=/  join-1  [%join ~zod /~zod/chat-1 %.y]
=/  join-2  [%join ~bus /~bus/chat-2 %.y]
=/  join-3  [%join ~web /~web/dm--zod %.y]
;<  ~  bind:m  (poke-app ~zod %chat-view %chat-view-action chat-1)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~bus %chat-view %chat-view-action chat-2)
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~web %chat-view %chat-view-action web-zod-dm)
;<  ~  bind:m  (wait-for-output ~web ">=")
;<  ~  bind:m  (sleep ~s20)
::
;<  ~  bind:m  (poke-app ~bus %chat-view %chat-view-action join-1)
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~web %chat-view %chat-view-action join-1)
;<  ~  bind:m  (wait-for-output ~web ">=")
::
;<  ~  bind:m  (poke-app ~zod %chat-view %chat-view-action join-2)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~web %chat-view %chat-view-action join-2)
;<  ~  bind:m  (wait-for-output ~web ">=")
::
;<  ~  bind:m  (poke-app ~zod %chat-view %chat-view-action join-3)
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (sleep ~s20)
::
;<  ~  bind:m  (chat-message ~zod /~zod/chat-1 now.bol 'message 1')
;<  ~  bind:m  (chat-message ~bus /~zod/chat-1 now.bol 'message 2')
;<  ~  bind:m  (chat-message ~web /~bus/chat-2 now.bol 'message 3')
;<  ~  bind:m  (chat-message ~zod /~web/dm--zod now.bol 'message 4')
::
::  publish setup
::
=/  book-1=action:publish
  :*  %new-book  %book-1  'Book 1'  ''  %.y
      [/ship/~zod/group-1 ~ %.y %.n]
  ==
=/  book-2=action:publish
  :*  %new-book  %book-2  'Book 2'  ''  %.y
      [/ship/~zod/group-1 ~ %.y %.n]
  ==
=/  book-3=action:publish
  :*  %new-book  %book-3  'Book 3'  ''  %.y
      [/ship/~web/book-3 (sy ~zod ~bus ~) %.n %.n]
  ==
;<  ~  bind:m  (poke-app ~zod %publish %publish-action book-1)
;<  ~  bind:m  (poke-app ~bus %publish %publish-action book-2)
;<  ~  bind:m  (poke-app ~web %publish %publish-action book-3)
;<  ~  bind:m  (sleep ~s60)
::
;<  ~  bind:m  (poke-app ~bus %publish %publish-action [%subscribe ~zod %book-1])
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~web %publish %publish-action [%subscribe ~zod %book-1])
;<  ~  bind:m  (wait-for-output ~web ">=")
::
;<  ~  bind:m  (poke-app ~zod %publish %publish-action [%subscribe ~bus %book-2])
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (poke-app ~web %publish %publish-action [%subscribe ~bus %book-2])
;<  ~  bind:m  (wait-for-output ~web ">=")
::
;<  ~  bind:m  (poke-app ~bus %publish %publish-action [%subscribe ~web %book-3])
;<  ~  bind:m  (wait-for-output ~bus ">=")
;<  ~  bind:m  (poke-app ~zod %publish %publish-action [%subscribe ~web %book-3])
;<  ~  bind:m  (wait-for-output ~zod ">=")
;<  ~  bind:m  (sleep ~s30)
::
;<  ~  bind:m  (publish-note ~zod ~zod %book-1 %note-1 'note 1' 'note 1')
;<  ~  bind:m  (publish-note ~bus ~bus %book-2 %note-2 'note 2' 'note 2')
;<  ~  bind:m  (publish-note ~web ~web %book-3 %note-3 'note 3' 'note 3')
;<  ~  bind:m  (sleep ~s30)
;<  ~  bind:m  (publish-comment ~web ~bus %book-2 %note-2 'comment 2')
;<  ~  bind:m  (publish-comment ~bus ~zod %book-1 %note-1 'comment 1')
;<  ~  bind:m  (publish-comment ~zod ~web %book-3 %note-3 'comment 3')
::
::  graph setup
::
=/  group-path  /ship/~zod/group-1
=/  graph-1=update:graph-store
  [%0 now.bol %add-graph [~zod %graph-1] *graph:graph-store ~]
=/  graph-2=update:graph-store
  [%0 now.bol %add-graph [~bus %graph-2] *graph:graph-store ~]
=/  g1-meta=metadata-action:metadata-store
  :*  %add  group-path  /graph/ship/~zod/graph-1
      ['graph-1' '' 0x0 now.bol ~zod %foo]
  ==
=/  g2-meta=metadata-action:metadata-store
  :*  %add  group-path  /graph/ship/~bus/graph-2
      ['graph-2' '' 0x0 now.bol ~bus %foo]
  ==
;<  ~  bind:m  (poke-app ~zod %graph-store %graph-update graph-1)
;<  ~  bind:m
  (poke-app ~zod %graph-push-hook %push-hook-action [%add ~zod %graph-1])
;<  ~  bind:m  (poke-app ~zod %metadata-hook %metadata-action g1-meta)
;<  ~  bind:m
  (poke-app ~zod %metadata-hook %metadata-hook-action [%add-owned group-path])
::
;<  ~  bind:m  (poke-app ~bus %graph-store %graph-update graph-2)
;<  ~  bind:m
  (poke-app ~bus %graph-push-hook %push-hook-action [%add ~bus %graph-2])
;<  ~  bind:m  (poke-app ~bus %metadata-hook %metadata-action g2-meta)
;<  ~  bind:m
  (poke-app ~bus %metadata-hook %metadata-hook-action [%add-owned group-path])
::
;<  ~  bind:m  (sleep ~s5)
;<  ~  bind:m
  (poke-app ~bus %graph-pull-hook %pull-hook-action [%add ~zod [~zod %graph-1]])
;<  ~  bind:m
  (poke-app ~web %graph-pull-hook %pull-hook-action [%add ~zod [~zod %graph-1]])
;<  ~  bind:m
  (poke-app ~zod %graph-pull-hook %pull-hook-action [%add ~bus [~bus %graph-2]])
;<  ~  bind:m
  (poke-app ~web %graph-pull-hook %pull-hook-action [%add ~bus [~bus %graph-2]])
;<  ~  bind:m  (sleep ~s30)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~zod %graph-1] 'post 1' 1)
;<  ~  bind:m  (graph-post ~bus now.bol [~zod %graph-1] 'post 2' 2)
;<  ~  bind:m  (graph-post ~web now.bol [~zod %graph-1] 'post 3' 3)
::
;<  ~  bind:m  (graph-post ~zod now.bol [~bus %graph-2] 'post 4' 4)
;<  ~  bind:m  (graph-post ~bus now.bol [~bus %graph-2] 'post 5' 5)
;<  ~  bind:m  (graph-post ~web now.bol [~bus %graph-2] 'post 6' 6)
::
;<  ~  bind:m  (wait-for-output ~web "XXXX")
;<  ~  bind:m  end-azimuth
(pure:m *vase)
