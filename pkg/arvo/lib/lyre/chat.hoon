/-  *lyre, inbox
|=  [con=configs:inbox nom=path env=(list envelope:inbox)]
^-  dom
:*  %padding  20  20  20  20
  :-  %vertical
  ^-  (list dom)
  %+  snoc 
    %+  turn  env
    |=  ev=envelope:inbox
    ^-  dom
    ?+    -.letter.ev
        [%text 'Unhandled Letter Type']
      %text  
        :-  %horizontal
        :~  [%text (cat 3 (scot %p author.ev) ':')]
            [%padding 0 0 10 0 [%text text.letter.ev]]
        ==
    ==
  :*  %form  %chat-view  %json
    %-  my 
    :~  [%chat (spat nom)]
        [%who (scot %p owner:(~(got by con) nom))]
    ==
    :-  %horizontal
    :~  [%text-input %message]
  ::      [%data %here-is-the-key [%s 'here is the value']]
        [%submit [%padding 0 0 10 0 [%text 'poast']]]
    ==
  ==
==
