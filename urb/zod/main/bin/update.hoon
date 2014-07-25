!:
::  /=main=/bin/update/hoon
::
=>  .(-< `who=@p`-<)
=>  %=    .
        +
      =>  +
      |%
      ++  posh                                                  ::  pause until
        |=  end=@da
        |=  wop=$+(@da bowl)
        ^-  bowl
        :-  ~  :-  ~
        :-  ^-  (list slip)
            :~  [~ %wa end]
            ==
        |=  [now=@da pax=path nut=note]
        (wop now)
      ::
      ++  merge
        |=  [gem=germ who=@p bos=@p est=time ran=rang]
        |=  [der=dome owr=dome des=desk]
        ^-  gift
        ~&  der
        =+  sab=`saba`[bos des [0 let.der] der]
        =+  lum=`(unit (unit mizu))`(~(auld ze est owr ran) gem who des sab est)
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
=+  syn=`(list ,@tas)`~[%main %try]
=+  ^=  desks
%+  turn  syn
    |=  des=desk
    =+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/[wen]))
    =+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/[wen]))
    [der owr des]
~&  %start-posh
%-  (posh (add ~s1 est))                                      ::  hack
|=  tim=@da
~&  %get-ran
=+  ran=((hard rang) .^(%cu /(scot %p who)/main/(scot %da tim)))       ::  global store
~&  %got-ran
=+  gifts=`(list gift)`(turn desks (merge ?~(gem %fine -.gem) who bos est ran))
`bowl`[[[%la %leaf "updating..."] gifts] ~]
