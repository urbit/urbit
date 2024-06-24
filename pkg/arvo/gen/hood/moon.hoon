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
|=  $:  [now=@da tick=@ud eny=@uvJ our=@p ^]
        arg=?(~ [mon=@p ~])
        public-key=pass
    ==
:-  %helm-moon
^-  (unit [=ship =udiff:point:jael])
=/  ran  (clan:title our)
?:  ?=([?(%earl %pawn)] ran)
  %-  %-  slog  :_  ~
      leaf+"can't create a moon from a {?:(?=(%earl ran) "moon" "comet")}"
  ~
=/  mon=ship
  ?^  arg
    mon.arg
  (add our (lsh 5 (end 5 (shaz eny))))
=/  ryf=(unit rift)
  .^((unit rift) %j (en-bema [our %ryft da+now ud+tick] /(scot %p mon)))
?^  ryf
  %.  ~
  %-  slog
  :~  leaf+"can't create {(scow %p mon)}, it already exists."
      'use |moon-breach and/or |moon-cycle-keys instead.'
  ==
=/  seg=ship  (sein:title our now tick mon)
?.  =(our seg)
  %-  %-  slog  :_  ~
      :-  %leaf
      "can't create keys for {(scow %p mon)}, which belongs to {(scow %p seg)}"
  ~
=/  =pass
  ?.  =(*pass public-key)
    public-key
  =/  cub  (pit:nu:crub:crypto 512 (shaz (jam mon life=1 eny)))
  =/  =seed:jael
    [mon 1 sec:ex:cub ~]
  %-  %-  slog
      :~  leaf+"moon: {(scow %p mon)}"
          leaf+(scow %uw (jam seed))
      ==
  pub:ex:cub
`[mon *id:block:jael %keys [1 1 pass] %.n]
