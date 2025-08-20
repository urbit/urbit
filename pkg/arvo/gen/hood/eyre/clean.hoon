::  Deletes all stale incoming http subscriptions
::
::    It runs in dry mode by default, printing the number of stale subscriptions
::    To actually delete the subscriptions, run with |eyre/clean, =dry |
::
/=  gall-raw  /sys/vane/gall
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=~ dry=? veb=?(%1 %2 ~)]
=/  m-gall  (gall-raw p.bec)
=+  .^(yokes=(map dude:gall yoke:m-gall) %gy /(scot %p p.bec)//(scot %da now)/$)
=+  .^(=channel-state:eyre %e /(scot %p p.bec)/channel-state/(scot %da now)/$)
=;  subs=(list [%g ship term duct])
  :-  %helm-eyre-lave
  ~?  dry  "#{<(lent subs)>} stale incoming subscriptions"
  dry^subs
::
%+  roll  ~(tap by yokes)
|=  [[=dude:gall =yoke:m-gall] subs=(list [%g ship term duct])]
?>  ?=(%live -.yoke)
%+  roll  ~(tap by bitt.yoke)
|=  [[=duct =ship =path] s=_subs]
?.  ?=([[%eyre %channel %subscription session=@ sub=@ ship=@ app=@ *] *] duct)
  s
=*  chan-wir  |2:i.duct
=*  ses-id    `@ta`&4:i.duct
=*  sub-id    &5:i.duct
=*  http-duct  t.duct
?~  chan=(~(get by session.channel-state) ses-id)
  ~?  >>  =(veb %1)  stale/ses-id^(rash sub-id dem)
  [g/ship^dude^duct s]
?^  sub=(~(get by subscriptions.u.chan) (rash sub-id dem))
  ~?  >  =(veb %2)  sub-exists/ses-id^(rash sub-id dem)
  s
~?  >>  =(veb %1)  stale/ses-id^(rash sub-id dem)
[g/ship^dude^duct s]
