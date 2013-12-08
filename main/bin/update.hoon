!:
::  /=main=/bin/install/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time eny=@uw]
|=  ~
^-  bowl
?:  =(~zod who)  [~ ~]
=+  bos==+(bos=(sein who) ?.(=(bos who) bos ~zod))
=+  ^=  syn  ^-  (list ,@tas)
    [%main %arvo %try ~]
:_  ~
^-  (list gift)
:-  [%la %leaf "updating..."]
%+  turn  syn
|=  des=@tas
=+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/=))
=+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/=))
=+  sab=`saba`[bos des [0 let.der] (flop (turn hit.der |=(a=frog q.a)))]
=+  lum=(~(auld ze est owr) est %fine sab)
?~  lum
  `gift`[%la %leaf "{(trip des)} failed to merge"]
?~  u.lum
  `gift`[%la %leaf "{(trip des)} is up to date"]
`gift`[%ok des u.u.lum]
