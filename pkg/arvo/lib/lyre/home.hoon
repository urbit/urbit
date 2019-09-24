/-  *lyre, hall
/+  *lyre
^-  dom
=/  dep=dependencies
  [~ ~[[%chat-lyre-view /chat/-50/0/chat1]] ~ %chat]
=/  act=action
  [%change-deps %home dep]
=/  chat-poke=poke
  [%lyre %lyre-action (action:enjs act)]
:*  %padding  20  20  20  20
  :-  %vertical
  :~  [%button [%text 'chat'] chat-poke]
  ==
==
