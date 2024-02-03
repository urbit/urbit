::  breach (factory reset) a moon
::
::::  /hoon/moon-breach/hood/gen
  ::
/-  *sole
/+  *generators
::
::::
  ::
:-  %say
|=  $:  [now=@da tick=@ud @ our=@p ^]
        [mon=@p ~]
        =rift
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
=/  =^rift
  ?.  =(*^rift rift)
    rift
  +(.^(^rift %j (en-bema [our %rift [da+now ud+tick]] /(scot %p mon))))
`[mon *id:block:jael %rift rift %.n]
