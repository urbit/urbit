::  story: Attach a commit message (to the last commit by default)
::
::  Optionally takes a case and desk
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[title=@t body=$@(~ [p=@t ~])] =desk cas=cash ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec   :: use current desk if user didn't provide
=?  cas   =(*case cas)    r.bec   :: use case from beak if cas not provided
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  helm-pass+[%d %flog %text "Error: desk {<desk>} does not exist."]
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
  helm-pass+[%d %flog %text "Error: No story file found. Please use |story-init to create one."]
=/  tale=story     .^(story %cx /(scot %p our)/[desk]/(scot %da now)/story)
=/  =prose         [title ?~(body '' p.body)]
=.  tale           (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]
