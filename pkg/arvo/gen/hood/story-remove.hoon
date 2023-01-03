::  story: Remove any commit message(s) for a given commit
::
::  Optionally targeting a specific desk or prose
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[cas=cash ~] =desk prz=prose ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=?  cas   =(*case cas)    r.bec  :: use case from beak if cas not provided
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  ~&  >>  "Error: desk {<desk>} does not exist."
  helm-pass+[%d %noop ~]
=/  tak=tako:clay
  ?:  ?=([%tako tako:clay] cas)
    p.cas
  ?:  !.^(? %cs /(scot %p our)/[desk]/(scot cas)/case)
    ~&  >>  "Error: invalid case {<cas>} provided"
    !!
  .^(tako:clay %cs /(scot %p our)/[desk]/(scot cas)/tako/~)
::
=/  pax            /(scot %p our)/[desk]/(scot %da now)/story
?:  !.^(? %cu pax)
  ~&  >>  "Error: No story file found. Please use |story-init to create one."
  helm-pass+[%d %noop ~]
=/  tale=story     .^(story %cx pax)
=.  tale
  ?:  =(*prose prz)
    (~(del by tale) tak)
  (~(del ju tale) tak prz)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]