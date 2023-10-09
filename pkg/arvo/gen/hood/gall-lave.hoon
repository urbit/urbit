::  Deletes all stale incoming gall (re) subscriptions
::
::    It runs in dry mode by default, printing the number of stale subscriptions
::    To actually delete the subscriptions, run with |gall-lave, =dry |
::
/=  gall-raw  /sys/vane/gall
::
=>  |%
    +$  subs  (jar [@ta path] [duct bone])
    --
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=~ peer=(unit @p) dry=? veb=?(%1 %2 %3 %x)]
::
=/  our-gall  (gall-raw p.bec)
=/  gall-yokes
  .^((map dude:gall yoke:our-gall) %gy /(scot %p p.bec)//(scot %da now)/$)
=;  ducts=(list [duct bone])
  :-  %helm-gall-lave
  ~?  dry  "#{<(lent ducts)>} stale incoming subscriptions"
  ~?  ?=(%1 veb)  ducts
  dry^~
::
%+  roll  ~(tap by gall-yokes)
|=  [[=dude:gall =yoke:our-gall] ducts=(list [duct bone])]
|^
%-  ~(rep by incoming)
|=  [[[app=@ta =path] ducts=(list [duct bone])] d=_ducts]
?.  (gth (lent ducts) 1)  d
%+  weld  d
=-  ~?  ?=(%2 veb)  "{<(lent -)>} stale subscriptions {<[app=app path]>}"
    ~?  ?=(%3 veb)  "{<(turn - |=([duct =bone] app=app^path^bone))>}"
    -
::  latest bone (i.e. subscription) is in the head
::
%-  tail
(sort ducts |=([a=[duct bone] b=[duct bone]] (gth +.a +.b)))
::
++  incoming
  ?>  ?=(%live -.yoke)
  %+  roll  ~(tap by bitt.yoke)
  |=  [[=duct =ship =path] subs=(jar [@ta path] [duct bone])]
  ?:  &(?=(^ peer) !=(u.peer ship))
    subs
  ?.  ?=([[%gall %sys %req @ @ *] [%ames %bone @ @ @ *] *] duct)
    subs
  =*  app   &5:i.duct
  =*  rift  &4:i.t.duct
  =*  bone  &5:i.t.duct
  (~(add ja subs) [`@ta`app path] [duct (slav %ud bone)])
--
