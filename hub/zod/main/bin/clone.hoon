!:
::  /=main=/bin/clone/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time eny=@uw]
|=  [src=path rex=$|(~ [dez=@tas ~])]
^-  bowl
=.  src  (scag 2 src)
?>  ?=([@ @ ~] src)
=+  bos=`@p`(need (slaw %p i.src))
?:  =(bos who)  [~ ~]
:_  ~
=+  dez=?^(rex dez.rex i.t.src)
=+  syt=(scot %da est)
=+  dyn=/(scot %p bos)/[i.t.src]/=
=+  oyn=/(scot %p who)/[dez]/=
=+  der=((hard dome) .^(%cv dyn))
=+  owr=((hard dome) .^(%cv oyn))
=+  sab=`saba`[bos i.t.src [0 let.der] (flop (turn hit.der |=(a=frog q.a)))]
=+  lum=(~(auld ze est owr) est %fine sab)
=+  dem="{(scow %p bos)}/{(trip i.t.src)}"
=+  owm="{(scow %p who)}/{(trip dez)}"
^-  (list gift)
?~  lum  
  [`gift`[%la %leaf "{dem} failed to merge"] ~]
?~  u.lum  
  [`gift`[%la %leaf "{dem} has not changed"] ~]
:~  `gift`[%la %leaf "updated {owm} to {dem}/{(scow %da est)}"]
    `gift`[%ok dez u.u.lum]
==
