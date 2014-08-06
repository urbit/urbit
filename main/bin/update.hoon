!:
::  /=main=/bin/update/hoon
::
=>  .(-< `who=@p`-<)
=>  %=    .
        +
      =>  +
      |%
      ::  from pony.hoon, so we don't have to pull it to start subs
      ++  pomo  |=([gud=gift bol=bowl] [[gud p.bol] q.bol])
      ++  pomp  |=([tix=tape bol=bowl] (pomo la/leaf/tix bol))
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
        =+  sab=`saba`[bos des [0 let.der] der]
        =+  lum=`(unit (unit mizu))`(~(auld ze est owr ran) gem who des sab est)
        ?~  lum
          ^-  gift
          :^  %la  %rose  [": " "" ""]
          :~  
            leaf/"{(trip des)} failed to apply, please rerun with a merge option"
            (skol -:!>(_germ))
          ==
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
    ~&  [%fetch des]
    =+  der=((hard dome) .^(%cv /(scot %p bos)/[des]/[wen]))
    =+  owr=((hard dome) .^(%cv /(scot %p who)/[des]/[wen]))
    [der owr des]
%+  pomp  "fetched new objects, merging"
%-  (posh est)                                      ::  hack
|=  tim=@da
=+  ran=((hard rang) .^(%cu /(scot %p who)/main/(scot %da tim)))       ::  global store
=+  gifts=`(list gift)`(turn desks (merge ?~(gem %fine -.gem) who bos est ran))
`bowl`[[[%la %leaf "updating..."] gifts] ~]
