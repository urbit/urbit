/-  spider,
    chat-view,
    *resource,
    chat-store
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
--
::
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  bol=bowl:spider  bind:m  get-bowl:strandio
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
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~web ~bus)
::
(pure:m *vase)
