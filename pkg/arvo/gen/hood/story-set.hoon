/-  *story
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[syd=desk =prose ~] cas=case ~]
    ==
::
::  Add a commit message to a given case on a given desk (modifies in place) 
::
=/  our  p.bec
=?  cas  =(*case cas)  [%da now]  :: set current commit when cas not provided
=/  tak  .^(tako:clay %cs /(scot %p our)/[syd]/(scot cas)/tako/~)
=/  tale=story  .^(story %cx /(scot %p our)/[syd]/(scot %da now)/story)
=.  tale  (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info syd %& [/story %ins story+!>(tale)]~]]