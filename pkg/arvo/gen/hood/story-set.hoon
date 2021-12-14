::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[syd=desk [title=@t body=@t] ~] cas=case ~]
    ==
::
::  Add a commit message to a given case on a given desk (modifies in place) 
::
=/  our  p.bec
=?  cas  =(*case cas)  [%da now]  :: set current commit when cas not provided
=/  tak  .^(tako:clay %cs /(scot %p our)/base/(scot cas)/tako/~)
=/  tale=(map tako:clay [@t @t])
  .^((map tako:clay [@t @t]) %cx /(scot %p our)/[syd]/(scot %da now)/story)
=.  tale  (~(put by tale) tak [title body])
:-  %helm-pass
[%c [%info syd %& [/story %ins story+!>(tale)]~]]