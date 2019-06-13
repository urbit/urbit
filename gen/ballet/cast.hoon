/-  *ballet
/+  ring
::
=/  people=(list @p)
  [~littel-ponnys ~rovnys-ricfer ~palfun-foslup ~rapfyr-diglyt ~poldec-tonteg ~]
::  Creates a 
::
:-  %say
|=  $:  {now/@da eny/@uvJ byk/beak}
        [[=id who=@p vote=@u ~] ~]
    ==
::  todo: these are fake keys
::
=/  keys=(list @udpoint)
  (turn people public-key-for-ship:ring)
=/  electorate-keys=(set @udpoint)  (sy keys)
::  ballot
::
=/  vote-to-cast=^vote
  [id [[%radio vote] ~]]

::  sign our ballet
::
=/  signature=ring-signature:ring
  %-  sign:ring  :*
    message=vote-to-cast
    link-scope=[~ [%election id]]
    anonymity-set=electorate-keys
    my-public-key=(public-key-for-ship:ring who)
    my-private-key=`@udscalar`who
    eny=eny
  ==
::
~&  [%signature signature]
::
:*  %ballot-cast
    id
    signature
    vote-to-cast
==
