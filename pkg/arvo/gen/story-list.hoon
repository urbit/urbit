::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[~] =desk ~]
    ==
::
::  (Internal) List all commit messages for the given desk, 
::  including orphans (i.e. ones that have no connection to the file graph)
::
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=/  tale=story            .^(story %cx /(scot %p our)/[desk]/(scot %da now)/story)
=/  story-to-mime         .^($-(story mime) %cf /(scot %p our)/[desk]/(scot %da now)/story/mime)
=/  tale-mime             (story-to-mime tale)
=/  tale-text             `@t`q.q.tale-mime
:-  %tang
[tale-text ~]