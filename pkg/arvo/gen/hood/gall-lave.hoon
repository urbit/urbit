::  Deletes all stale incoming gall (re) subscriptions
::
::    It runs in dry mode by default, printing the number of stale subscriptions
::    To actually delete the subscriptions, run with |gall-lave, =dry |
::
::    |gall-lave, =veb %1  :: for each subscription path, print bone info
::    |gall-lave, =veb %2  :: number of stale subscriptions per app
::    |gall-lave, =veb %3  :: subscription only present in %gall
::    |gall-lave, =veb %31 :: ... since N are already in closing in %ames
::    |gall-lave, =veb %32 :: ... since N are already corked in %ames
::
/=  gall-raw  /sys/vane/gall
::
=>  |%
    +$  flow  [=bone closing=? corked=? =ship app=term =duct]
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
      "#{<ames-corks>} only in %gall (closing or corked in %ames)"
    %+  roll  flows
    |=  [flow acc=@]
    ?.(|(closing corked) acc +(acc))
  ~?  =(veb %31)
    =;  ames-corks=@
      "#{<ames-corks>} in closing"
    (roll flows |=([flow acc=@] ?.(closing acc +(acc))))
  ~?  =(veb %32)
    =;  ames-corks=@
      "#{<ames-corks>} flows already corked"
    (roll flows |=([flow acc=@] ?.(corked acc +(acc))))
  :-  dry
  ::  a %g tag signals that the subscription exists only in %gall
  ::  tagging it with %a signals that the flow also exists in %ames
  ::
  (turn flows |=(flow :_(ship^app^duct ?:(|(closing corked) %a %g))))
::
%+  roll  ~(tap by gall-yokes)
|=  [[=dude:gall =yoke:our-gall] ducts=(list flow)]
|^
%-  ~(rep by incoming)
|=  [[[=^ship app=@ta =path] ducts=(list flow)] d=_ducts]
?.  (gth (lent ducts) 1)  d
%+  weld  d
=-  ~?  ?=(%2 veb)  "{<(lent -)>} stale subscriptions on {<ship^app=app^path>}"
    ~?  ?=(%1 veb)  (turn - |=(flow "{<`flow`bone^closing^corked^ship^app^~>}"))
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
  ?.  ?=([[%gall %sys %req @ @ *] [%ames %bone @ @ @ *] *] duct)
    subs
  =*  app    &5:i.duct
  =*  rift   &4:i.t.duct
  =/  =bone  (slav %ud &5:i.t.duct)
  =/  scry
    |=  =term
    /(scot %p p.bec)//(scot %da now)/[term]/(scot %p ship)/(scot %ud bone)
  =+  .^(closing=? %ax (scry %closing))
  =+  .^(corked=? %ax (scry %corked))
  %+  ~(add ja subs)  [ship `@ta`app path]
  [bone closing corked ship app=&5:i.duct duct]
--
