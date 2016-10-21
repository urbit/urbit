::  Generate random moon and corresponding ticket
::
::::  /hoon/moon/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        $~
        $~
    ==
:-  %tang  :_  ~  :-  %leaf
=+  ran=(clan p.bec)
?:  ?=({?($earl $pawn)} ran)
  "can't create a moon from a {?:(?=($earl ran) "moon" "comet")}"
=+  mon=(mix (lsh 5 1 (end 5 1 eny)) p.bec)
=+  tic=.^(@ /a/(scot %p p.bec)/tick/(scot %da now)/(scot %p mon))
"moon: {<`@p`mon>}; ticket: {<`@p`tic>}"
