/-  *lyre, inbox
|=  mail=mailbox:inbox
^-  dom
:*  %padding  20  20  20  20
  :-  %vertical
  ^-  (list dom)
  %+  snoc 
    %+  turn  envelopes:mail
    |=  env=envelope:inbox
    ^-  dom
    ?+    -.letter.env
        [%text 'Unhandled Letter Type']
      %text  
        :-  %horizontal
        :~  [%text (cat 3 (scot %p author.env) ':')]
            [%padding 0 0 20 0 [%text text.letter.env]]
        ==
    ==
  :^  %form  %inbox  %json
  :-  %horizontal
  :~  [%text-input %chat-input]
      [%submit [%padding 0 0 10 0 [%text 'poast']]]
  ==
==
