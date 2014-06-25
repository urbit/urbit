!:
::  /=main=/bin/update/hoon
::
=>  .(-< `who=@p`-<)
=>  %=    .
        +
      =>  +
      |%
      ++  merge
        |=  [gem=germ who=@p bos=@p est=time]
        |=  [der=dome owr=dome des=desk]
        ^-  gift
        =+  sab=`saba`[bos des [0 let.der] (flop (turn hit.der |=(a=frog q.a)))]
        =+  lum=(~(auld ze est owr) gem who des sab)
        ?~  lum
          ^-  gift
          :+  %la  %leaf
          "{(trip des)} failed to apply, please rerun with a merge option"
        ?~  u.lum
          `gift`[%la %leaf "{(trip des)} is up to date"]
        `gift`[%ok des u.u.lum]
      --
    ==
|=  [est=time eny=@uw]
|=  gem=$|([germ ~] ~)
=+  wen=(scot %da (need (slaw %da +>-:/===))) :: heinous
?:  =(~zod who)  [~ ~]
=+  bos==+(bos=(sein who) ?:(=(bos who) ~zod bos))
=+  syn=`(list ,@tas)`~[%main %arvo %try]
=+  ^=  desks
%+  turn  syn
|=  des=desk
=+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/[wen]))
=+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/[wen]))
[der owr des]
=+  gifts=`(list gift)`(turn desks (merge ?~(gem %fine -.gem) who bos est))
`bowl`[[[%la %leaf "updating..."] gifts] ~]
