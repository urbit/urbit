::  Display twitter feed
::
::::  /hoon/feed/twit/gen
  ::
/-    twitter
!:
::::  ~fyr
  ::
:-  %say
|=  $:  {now/@da eny/@uvJ bek/beak}
        {{who/iden $~} typ/?($home $user)}
    ==
=+  pax=/(scot %p p.bek)/twit/(scot %da now)/[typ]/[who]
:-  %tang
%+  turn   (flop .^((list post:twitter) %gx pax))
|=  post:twitter  ^-  tank
rose+[": " `~]^~[leaf+"{<now>} @{(trip who)}" leaf+(trip txt)]
