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
;<  ~  bind:m  (chat-message ~zod /~zod/chat-1 now.bol 'message 5')
;<  ~  bind:m  (chat-message ~bus /~zod/chat-1 now.bol 'message 6')
;<  ~  bind:m  (chat-message ~web /~bus/chat-2 now.bol 'message 7')
;<  ~  bind:m  (chat-message ~zod /~web/dm--zod now.bol 'message 8')
::
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~web ~bus)
::
(pure:m *vase)
