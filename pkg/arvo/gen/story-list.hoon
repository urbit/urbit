::  story: List unordered commit messages for the given desk, including orphans
::
::::
  ::
/-  *story
/$  story-to-txt  %story  %txt
::
:-  %say
|=  $:  [now=@da tick=@ud @ bec=beak]
        [[~] =desk ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=/  cas                   r.bec  :: use case from beak
=/  pax=path  (en-pick now tick (en-bema [our desk cas] /story))
=+  .^(desks=(set ^desk) %cd (en-bema [our %$ [da+now ud+tick]] /))
?.  (~(has in desks) desk)
  tang+[leaf+"Error: desk {<desk>} does not exist." ~]
?.  .^(? %cu pax)
  tang+['Error: No story file found. Please use |story-init to create one.' ~]
=/  tale       .^(story %cx pax)
=/  tale-text  (flop (story-to-txt tale))
[%tang tale-text]
