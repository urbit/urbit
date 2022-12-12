::  story: List unordered commit messages for the given desk, including orphans
::
::::
  ::
/-  *story
/$  story-to-txt  %story  %txt
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[~] =desk ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=/  cas                   r.bec  :: use case from beak
=/  pax                   /(scot %p our)/[desk]/(scot cas)/story
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  tang+[leaf+"Error: desk {<desk>} does not exist." ~]
?:  !.^(? %cu pax)
  tang+['Error: No story file found. Please use |story-init to create one.' ~]
=/  tale        .^(story %cx pax)
=/  tale-text  (flop (story-to-txt tale))
tang+tale-text
