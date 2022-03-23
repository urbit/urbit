::  story: Create a story file for a given desk, optionally overwriting
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[~] =desk overwrite=_| ~]
    ==
=/  our                   p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
?:  !(~(has in .^((set ^desk) %cd /(scot %p our)/$/(scot %da now))) desk)
  ~&  >>  "Error: desk {<desk>} does not exist."
  helm-pass+[%d %noop ~]
=/  existing-story        .^(? %cu /(scot %p our)/[desk]/(scot %da now)/story)
?:  ?&(existing-story !overwrite)
  ~&  >>  "Error: /{(trip (slav %tas desk))}/story already exists."
  ~&  >>  "To forcibly overwrite, use `=overwrite %.y`"
  ::  XX could use a better way to noop
  helm-pass+[%d %noop ~]
=|  tale=story
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]