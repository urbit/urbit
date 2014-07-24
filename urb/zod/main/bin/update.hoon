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
        |=  [der=dome owr=dome ran=rang des=desk]
        ^-  gift
        ~&  der
        =+  sab=`saba`[bos des [0 let.der] der]
        =+  lum=`(unit (unit mizu))`(~(auld ze est owr ran) gem who des sab)
        ?~  lum
          ^-  gift
          :+  %la  %leaf
          "{(trip des)} failed to apply, please rerun with a merge option"
        ?~  u.lum
          `gift`[%la %leaf "{(trip des)} is up to date"]
        `gift`[%og des u.u.lum]
      --
    ==
|=  [est=time eny=@uw]
|=  gem=$|([germ ~] ~)
=+  wen=(scot %da (need (slaw %da +>-:/===))) :: heinous
?:  =(~zod who)  [~ ~]
=+  bos==+(bos=(sein who) ?:(=(bos who) ~zod bos))
=+  syn=`(list ,@tas)`~[%main %try %arvo]
=+  ^=  desks
%+  turn  syn
    |=  des=desk
    =+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/[wen]))
    =+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/[wen]))
    =+  ran=((hard rang) .^(%cu /(scot %p bos)/[des]/[wen]))    :: global store
    [der owr ran des]
=+  gifts=`(list gift)`(turn desks (merge ?~(gem %fine -.gem) who bos est))
`bowl`[[[%la %leaf "updating..."] gifts] ~]
