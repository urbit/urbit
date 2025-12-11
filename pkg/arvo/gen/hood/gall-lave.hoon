::  Deletes all stale incoming gall (re) subscriptions
::
::    It runs in dry mode by default, printing the number of stale subscriptions
::    To actually delete the subscriptions, run with |gall-lave, =dry |
::
::    |gall-lave, =veb %1  :: for each subscription path, print bone info
::    |gall-lave, =veb %2  :: number of stale subscriptions per app
::    |gall-lave, =veb %3  :: subscription only in %gall (corked in %ames)
::
/=  gall-raw  /sys/vane/gall
::
=>  |%
    +$  flow  [=bone corked=? =ship app=term =duct]
    +$  subs  (jar key=[subscriber=@p app=@ta =path] flow)
    +$  veb   ?(%1 %2 %3 %31 %32 %4 %x)
    --
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=~ peer=(unit @p) dry=? =veb]
::
=/  our-gall  (gall-raw p.bec)
=/  gall-yokes
  .^((map dude:gall yoke:our-gall) %gy /(scot %p p.bec)//(scot %da now)/$)
=;  flows=(list flow)
  :-  %helm-gall-lave
  ~?  dry  "#{<(lent flows)>} stale incoming subscriptions"
  ~?  =(veb %3)
    =;  ames-corks=@
      "#{<ames-corks>} only in %gall (corked in %ames)"
    %+  roll  flows
    |=  [flow acc=@]
    ?.(corked acc +(acc))
  :-  dry
  =-  ::  XX only handle "local" subscription desync (i.e. don't %cork)
      ::
      (skim - |=(v=[?(%g %a) *] ?=(%g v)))
  ::  a %g tag signals that the subscription exists only in %gall
  ::  tagging it with %a signals that the flow also exists in %ames
  ::
  (turn flows |=(flow :_(ship^app^duct ?:(corked %g %a))))
::
%+  roll  ~(tap by gall-yokes)
|=  [[=dude:gall =yoke:our-gall] ducts=(list flow)]
|^
%-  ~(rep by incoming)
|=  [[[=^ship app=@ta =path] ducts=(list flow)] d=_ducts]
?.  (gth (lent ducts) 1)  d
%+  weld  d
=-  ~?  ?=(%2 veb)  "{<(lent -)>} stale subscriptions on {<ship^app=app^path>}"
    ~?  ?=(%1 veb)  (turn - |=(flow "{<`flow`bone^corked^ship^app^~>}"))
    -
::  latest bone (i.e. subscription) is in the head
::
(tail (sort ducts |=([a=flow b=flow] (gth -.a -.b))))
::
++  incoming
  ?>  ?=(%live -.yoke)
  %+  roll  ~(tap by bitt.yoke)
  |=  [[=duct =ship =path] =subs]
  ?:  &(?=(^ peer) !=(u.peer ship))
    subs
  ?.  ?=([[%gall %sys %req @ @ *] *] duct)
    subs
  ?.  ?=([^ [%ames %bone @ @ @ ~] *] duct)
    subs
  =*  app    &5:i.duct
  :: =*  rift   &4:i.t.duct  :: XX ?
  =/  [=bone scry-path=^path]
    =/  =bone
      (slav %ud &5:i.t.duct)
    ::
    =+  .^  ahoyed=?  %ax
          [(scot %p p.bec) %$ (scot %da now) %ahoyed (scot %p ship) ~]
        ==
    =.  bone  ?.(ahoyed bone (mix 0b1 bone))
    :-  bone
    :*  (scot %p p.bec)  %$  (scot %da now)
        %corked   (scot %p ship)
        ?.  ahoyed
          [(scot %ud bone) ~]
        [%bak (scot %ud bone) ~]
    ==
  =+  .^(corked=? %ax scry-path)
  (~(add ja subs) [ship `@ta`app path] [bone corked ship app duct])
--
