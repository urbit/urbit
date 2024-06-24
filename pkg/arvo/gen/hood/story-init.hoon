::  story: Create a story file for a given desk, optionally overwriting
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da tick=@ud @ bec=beak]
        [[~] [=desk overwrite=_| ~]]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=+  .^(desks=(set ^desk) %cd (en-bema [p.bec %$ [da+now ud+tick]] /))
?.  (~(has in desks) desk)
  helm-pass+[%d %flog %text "Error: desk {<desk>} does not exist."]
=/  existing-story
  .^(? %cu (en-bema [p.bec desk [da+now ud+tick]] /story))
?:  &(existing-story !overwrite)
  :-  %helm-pass
  [%d %flog %text "Error: /{(trip (slav %tas desk))}/story already exists. To forcibly overwrite, use `=overwrite %.y`"]
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(*story)]~]]
