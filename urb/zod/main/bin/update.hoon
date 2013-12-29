!:
::  /=main=/bin/update/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time eny=@uw]
|=  ~
^-  bowl
?:  =(~zod who)  [~ ~]
=+  bos==+(bos=(sein who) ?.(=(bos who) bos ~zod))
=+  wen=(scot %da (sub est ~s5))
=+  ^=  syn  ^-  (list ,@tas)
    [%main %arvo %try ~]
:_  ~
^-  (list gift)
:-  [%la %leaf "updating..."]
%+  turn  syn
|=  des=@tas
~&  [%reading `path`/(scot %p bos)/[des]/[wen]]
=+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/[wen]))
~&  [%reading `path`/(scot %p who)/[des]/[wen]]
=+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/[wen]))
=+  sab=`saba`[bos des [0 let.der] (flop (turn hit.der |=(a=frog q.a)))]
=+  lum=(~(auld ze est owr) est %fine sab)
?~  lum
  `gift`[%la %leaf "{(trip des)} failed to merge"]
?~  u.lum
  `gift`[%la %leaf "{(trip des)} is up to date"]
`gift`[%ok des u.u.lum]
