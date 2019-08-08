::  Create a private key-file for a random (or specified) moon
::
::::  /hoon/moon/hood/gen
  ::
/-  *sole
/+  *generators
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [mon=@p ~]
        =life
        public-key=pass
    ==
:-  %helm-moon
^-  (unit [=ship =udiff:point:able:jael])
=*  our  p.bec
=/  ran  (clan:title our)
?:  ?=({?($earl $pawn)} ran)
  %-  %-  slog  :_  ~
      leaf+"can't manage a moon from a {?:(?=($earl ran) "moon" "comet")}"
  ~
=/  seg=ship  (sein:title our now mon)
?.  =(our seg)
  %-  %-  slog  :_  ~
      :-  %leaf
      "can't create keys for {(scow %p mon)}, which belongs to {(scow %p seg)}"
  ~
=/  =^life
  ?.  =(*^life life)
    life
  +(.^(^life j+/(scot %p our)/life/(scot %da now)/(scot %p mon)))
=/  =pass
  ?.  =(*pass public-key)
    public-key
  =/  cub  (pit:nu:crub:crypto 512 (shaz (jam mon life eny)))
  =/  =seed:able:jael
    [mon 1 sec:ex:cub ~]
  %-  %-  slog
      :~  leaf+"moon: {(scow %p mon)}"
          leaf+(scow %uw (jam seed))
      ==
  pub:ex:cub
`[mon *id:block:able:jael %keys life 1 pass]
