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
        arg=?(~ [mon=@p ~])
        public-key=pass
    ==
:-  %helm-moon
^-  (unit [=ship =udiff:point:able:jael])
=*  our  p.bec
=/  ran  (clan:title our)
?:  ?=({?($earl $pawn)} ran)
  %-  %-  slog  :_  ~
      leaf+"can't create a moon from a {?:(?=($earl ran) "moon" "comet")}"
  ~
=/  mon=ship
  ?^  arg
    mon.arg
  (add our (lsh 5 1 (end 5 1 (shaz eny))))
=/  seg=ship  (sein:title our now mon)
?.  =(our seg)
  %-  %-  slog  :_  ~
      :-  %leaf
      "can't create keys for {(scow %p mon)}, which belongs to {(scow %p seg)}"
  ~
=/  =pass
  ?.  =(*pass public-key)
    public-key
  =/  cub  (pit:nu:crub:crypto 512 (shaz (jam mon life=1 eny)))
  =/  =seed:able:jael
    [mon 1 sec:ex:cub ~]
  %-  %-  slog
      :~  leaf+"moon: {(scow %p mon)}"
          leaf+(scow %uw (jam seed))
      ==
  pub:ex:cub
`[mon *id:block:able:jael %keys 1 1 pass]
