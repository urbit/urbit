::  change the keys of a moon
::
::::  /hoon/moon-cycle-keys/hood/gen
  ::
/-  *sole
/+  *generators
::
::::
  ::
:-  %say
|=  $:  [now=@da tick=@ud eny=@uvJ our=@p ^]
        [mon=@p ~]
        =life
        public-key=pass
    ==
:-  %helm-moon
^-  (unit [=ship =udiff:point:jael])
=/  ran  (clan:title our)
?:  ?=([?(%earl %pawn)] ran)
  %-  %-  slog  :_  ~
      leaf+"can't manage a moon from a {?:(?=(%earl ran) "moon" "comet")}"
  ~
=/  seg=ship  (sein:title our now tick mon)
?.  =(our seg)
  %-  %-  slog  :_  ~
      :-  %leaf
      "can't create keys for {(scow %p mon)}, which belongs to {(scow %p seg)}"
  ~
=/  =^life
  ?.  =(*^life life)
    life
  +(.^(^life %j (en-bema [our %life [da+now ud+tick]] /(scot %p mon))))
=/  =pass
  ?.  =(*pass public-key)
    public-key
  =/  cub  (pit:nu:crub:crypto 512 (shaz (jam mon life eny)))
  =/  =seed:jael
    [mon life sec:ex:cub ~]
  %-  %-  slog
      :~  leaf+"moon: {(scow %p mon)}"
          leaf+(scow %uw (jam seed))
      ==
  pub:ex:cub
`[mon *id:block:jael %keys [life 1 pass] %.n]
