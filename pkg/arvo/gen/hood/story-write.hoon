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
=/  tale=story     .^(story %cx /(scot %p our)/[desk]/(scot %da now)/story)
=/  =prose         [title ?~(body '' p.body)]
=.  tale           (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]