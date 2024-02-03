::  story: Attach a commit message (to the last commit by default)
::
::  Optionally takes a case and desk
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da tick=@ud @ bec=beak]
        [[title=@t body=$@(~ [p=@t ~])] =desk cas=cash ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec   :: use current desk if user didn't provide
=?  cas   =(*case cas)    r.bec   :: use case from beak if cas not provided
=+  .^(desks=(set ^desk) %cd (en-bema [our %$ [da+now ud+tick]] /))
?.  (~(has in desks) desk)
  helm-pass+[%d %flog %text "Error: desk {<desk>} does not exist."]
=/  tak=tako:clay
  ?:  ?=([%tako tako:clay] cas)
    p.cas
  ?.  .^(? %cs (en-pick now tick (en-bema [our desk cas] /case)))
    ~&  >>  "Error: invalid case {<cas>} provided"
    !!
  .^(tako:clay %cs (en-pick now tick (en-bema [our desk cas] /tako/~)))
::
=/  pax=path  (en-bema [our desk [da+now ud+tick]] /story)
?.  .^(? %cu pax)
  helm-pass+[%d %flog %text "Error: No story file found. Please use |story-init to create one."]
=+  .^(tale=story %cx pax)
=/  =prose  [title ?~(body '' p.body)]
=.  tale    (~(put ju tale) tak prose)
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]
