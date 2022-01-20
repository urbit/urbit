/-  *story
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[~] =desk overwrite=_| ~]
    ==
::
::  Create a story file for a given desk 
::
=/  our  p.bec
=?  desk  =(*^desk desk)  q.bec  :: use current desk if user didn't provide
=/  existing-story  .^(? %cu /(scot %p our)/[desk]/(scot %da now)/story)::
?:  ?&(existing-story !overwrite)
  ~&  "Error: {<desk>}/story already exists. Set the optional parameter `overwrite` to `%.y` to forcibly wipe"
  ::  XX dumb hack to no-op because apparently type system doesn't like it when you mix %kiln-info and %helm-pass
  :-  %helm-pass  [%d %noop ~]
=|  tale=story
:-  %helm-pass
[%c [%info desk %& [/story %ins story+!>(tale)]~]]