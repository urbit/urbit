::  Display twitter feed
::
::::  /hoon/feed/twit/gen
  ::
/?    310
/-    twitter
/+  old-zuse
=,  old-zuse
::
::::  ~fyr
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bek/beak}
        {{who/iden $~} typ/?($user $home)}
    ==
=+  pax=/(scot %p p.bek)/twit/(scot %da now)/[typ]/[who]/twit-feed
:-  %tang
%+  turn   (flop .^((list post:twitter) %gx pax))
|=  post:twitter  ^-  tank
rose+[": " `~]^~[leaf+"{<now>} @{(trip who)}" leaf+(trip txt)]
