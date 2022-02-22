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
=?  cas   =(*case cas)    r.bec   :: use case from beak if cas not provided
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  ~&  >>  "Error: desk {<desk>} does not exist."
  helm-pass+[%d %noop ~]
?:  !.^(? %cs /(scot %p our)/[desk]/(scot cas)/case)
  ~&  >>  "Error: invalid case {<cas>} provided"
  helm-pass+[%d %noop ~]
=/  tak
  ?:  ?=([%tako tako:clay] cas)
    p.cas
  .^(tako:clay %cs /(scot %p our)/[desk]/(scot cas)/tako/~)
::
=/  pax            /(scot %p our)/[desk]/(scot cas)/story
?:  !.^(? %cu pax)
  ~&  >>  "Error: desk {<desk>} does not exist."
  helm-pass+[%d %noop ~]
=/  tale=story     .^(story %cx /(scot %p our)/[desk]/(scot %da now)/story)
=.  tale           (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]