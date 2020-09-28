/-  spider,
    chat-view,
    publish,
    link-view,
    contact-view,
    link-listen-hook,
    chat-store,
    link-store
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
++  make-link
  |=  [our=@p =path title=@t url=@t]
  =/  act=action:link-store  [%save path title url]
  (poke-app our %link-store %link-action act)
::
++  link-comment
  |=  [our=@p =path url=@t body=@t]
  =/  act=action:link-store  [%note path url body]
  (poke-app our %link-store %link-action act)
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
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
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
::  links setup
::
::=/  link-1=action:link-view
::  :*  %create
::      /link-1  'Link 1'  ''
::      [%group /ship/~zod/group-1]
::      %.n
::  ==
::=/  link-2=action:link-view
::  :*  %create
::      /link-2  'Link 1'  ''
::      [%group /ship/~zod/group-1]
::      %.n
::  ==
::=/  link-3=action:link-view
::  :*  %create
::      /link-3  'Link 1'  ''
::      [%ships (sy ~zod ~bus ~)]
::      %.n
::  ==
::=/  join-1=action:link-listen-hook  [%watch /link-1]
::=/  join-2=action:link-listen-hook  [%watch /link-2]
::=/  join-3=action:link-listen-hook  [%watch /link-3]
::::
::;<  ~  bind:m  (poke-app ~zod %link-view %link-view-action link-1)
::;<  ~  bind:m  (poke-app ~bus %link-view %link-view-action link-2)
::;<  ~  bind:m  (poke-app ~web %link-view %link-view-action link-3)
::;<  ~  bind:m  (sleep ~s30)
::::
::;<  ~  bind:m  (poke-app ~bus %link-listen-hook %link-listen-action join-1)
::;<  ~  bind:m  (poke-app ~web %link-listen-hook %link-listen-action join-1)
::::
::;<  ~  bind:m  (poke-app ~zod %link-listen-hook %link-listen-action join-2)
::;<  ~  bind:m  (poke-app ~web %link-listen-hook %link-listen-action join-2)
::::
::;<  ~  bind:m  (poke-app ~zod %link-listen-hook %link-listen-action join-3)
::;<  ~  bind:m  (poke-app ~bus %link-listen-hook %link-listen-action join-3)
::;<  ~  bind:m  (sleep ~s30)
::::
::;<  ~  bind:m  (make-link ~bus /link-1 'link 1' 'link1.com')
::;<  ~  bind:m  (make-link ~web /link-2 'link 2' 'link2.com')
::;<  ~  bind:m  (make-link ~zod /link-3 'link 3' 'link3.com')
::;<  ~  bind:m  (sleep ~s30)
::;<  ~  bind:m  (link-comment ~web /link-1 'link1.com' 'comment 1')
::;<  ~  bind:m  (link-comment ~zod /link-2 'link2.com' 'comment 2')
::;<  ~  bind:m  (link-comment ~bus /link-3 'link3.com' 'comment 3')
::
;<  ~  bind:m  (wait-for-output ~web "foo")
(pure:m *vase)
