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
        =rift
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
=/  =^rift
  ?.  =(*^rift rift)
    rift
  +(.^(^rift j+/(scot %p our)/rift/(scot %da now)/(scot %p mon)))
`[mon *id:block:able:jael %rift rift]
