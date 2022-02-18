::  story: Attach a commit message to the commit associated with a given case on a given desk
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[=prose ~] =desk cas=cash ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec   :: use current desk if user didn't provide
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  ~&  >>  "Error: desk {<desk>} does not exist."
  helm-pass+[%d %noop ~]
=?  cas   =(*case cas)    da+now  :: use current commit if cas not provided
::  TODO case existence check
=/  tak
  ?:  ?=([%tako tako:clay] cas)
    p.cas
  .^(tako:clay %cs /(scot %p our)/[desk]/(scot cas)/tako/~)
::
=/  tale=story  .^(story %cx /(scot %p our)/[desk]/(scot %da now)/story)
=.  tale        (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]