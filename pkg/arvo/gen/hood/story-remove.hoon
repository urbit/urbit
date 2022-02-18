::  story: Remove any commit message(s) for a given desk, optionally targeting a specific case or prose
::
::::
  ::
/-  *story
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [[syd=desk ~] cas=case prz=prose ~]
    ==
::
::  XX: story set and story init both have desk and case as optional.
::      however, it seems a bit odd to allow both optional here
::      since we're dealing with a more hazardous operation.
::      should we allow a bare `|story-remove` to remove the last commit message on the current desk?
::      leaning towards no, and potentially even making the case non-optional
=/  our                p.bec
=?  cas  =(*case cas)  da+now  :: use current commit if cas not provided
=/  tak                .^(tako:clay %cs /(scot %p our)/[syd]/(scot cas)/tako/~)
=/  tale=story         .^(story %cx /(scot %p our)/[syd]/(scot %da now)/story)
=.  tale
  ?:  =(*prose prz)
    (~(del by tale) tak)
  (~(del ju tale) tak prz)
:-  %helm-pass
[%c [%info syd %& [/story %ins story+!>(tale)]~]]