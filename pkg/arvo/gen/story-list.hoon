::  story: List unordered commit messages for the given desk, including orphans
::
::::
  ::
/-  *story
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
=/  story-to-mime  
  .^($-(story mime) %cf /(scot %p our)/[desk]/(scot cas)/story/mime)
::
=/  tale=story  .^(story %cx pax)
=/  tale-mime   (story-to-mime tale)
=/  tale-text   `@t`q.q.tale-mime
:-  %tang
[tale-text ~]