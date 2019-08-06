::  Create a private key-file for a random (or specified) moon
::
::::  /hoon/moon/gen
  ::
/?    310
/-  *sole
/+  *generators
::
::::
  ::
:-  %ask
|=  $:  [now=@da eny=@uvJ bec=beak]
        arg=?(~ [mon=@p ~])
        ~
    ==
=*  our  p.bec
=/  ran  (clan:title our)
?:  ?=({?($earl $pawn)} ran)
  %-  produce
  :-  %tang  :_  ~
  leaf+"can't create a moon from a {?:(?=($earl ran) "moon" "comet")}"
=/  mon=ship
  ?^  arg
    mon.arg
  (add our (lsh 5 1 (end 5 1 (shaz eny))))
=/  seg=ship  (sein:title our now mon)
?.  =(our seg)
  %-  produce
  :-  %tang  :_  ~
  leaf+"can't create keys for {(scow %p mon)}, which belongs to {(scow %p seg)}"
=/  lyf=life  .^(@ud j+/(scot %p our)/life/(scot %da now)/(scot %p our))
=/  sed=seed:able:jael
  .^  seed:able:jael
      j+/(scot %p our)/earl/(scot %da now)/(scot %p mon)/(scot %ud lyf)
  ==
%+  print    leaf+"moon: {(scow %p mon)}"
%-  produce  [%atom (scot %uw (jam sed))]
