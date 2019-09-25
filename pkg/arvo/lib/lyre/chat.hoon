/-  *lyre, chat-store
/+  *lyre
|=  [con=configs:chat-store nom=path env=(list envelope:chat-store)]
^-  dom
:*  %padding  20  20  20  20
  :-  %vertical
  ^-  (list dom)
  :-  [%padding 0 20 0 0 [%button [%text 'home'] (link %home)]]
  %+  snoc
    %+  turn  env
    |=  ev=envelope:chat-store
    ^-  dom
    ?+    -.letter.ev
        [%text 'Unhandled Letter Type']
      %text
        :-  %horizontal
        :~  [%text (cat 3 (scot %p author.ev) ':')]
            [%padding 0 0 10 0 [%text text.letter.ev]]
        ==
    ==
  :*  %form  %chat-lyre-view  %json
    %-  my
    :~  [%chat (spat nom)]
        [%who (scot %p owner:(~(got by con) nom))]
    ==
    :-  %horizontal
    :~  [%text-input %message]
        [%submit [%padding 0 0 10 0 [%text 'poast']]]
    ==
  ==
==
