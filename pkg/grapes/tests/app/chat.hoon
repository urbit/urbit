/+  *test
/=  agent  /app/chat
|%
++  chat-1  [~zod %test]
++  bowl
  =|  run=@ud
  ^-  bowl:gall
  :*  [~zod ~zod %chat]
      [~ ~]
      [run `@uvJ`(shax run) (add (mul run ~s1) *time) [~zod %garden ud+run]]
  ==

--
|%
++  zod  `agent:gall`agent
++  test-setup
  =/  [cards=(list card) new-zod=agent:gall]
    (~(on-poke zod bowl) %resource !>(chat-1))
  =/  [=mark =vase]  
    (need (need (~(on-peek new-zod bowl) /x/chat/~zod/test)))
  (expect-eq !>(&) vase)
--
