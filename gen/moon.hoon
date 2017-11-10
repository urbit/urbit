::  Generate random moon and corresponding ticket
::
::::  /hoon/moon/gen
  ::
/?    310
/-  sole
/+  old-zuse
=,  old-zuse
[. sole]
::
::::
  ::
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        $~
        $~
    ==
=/  ran  (clan p.bec)
?:  ?=({?($earl $pawn)} ran)
  %-  sole-so
  :-  %tang  :_  ~
  leaf+"can't create a moon from a {?:(?=($earl ran) "moon" "comet")}"
=/  mon  (mix (lsh 5 1 (end 5 1 eny)) p.bec)
=/  tic  .^(@ /a/(scot %p p.bec)/tick/(scot %da now)/(scot %p mon))
%+  sole-yo
  leaf+"(see https://github.com/urbit/arvo/issues/327 for details)"
%+  sole-yo
  :-  %leaf
  ;:  weld
    "WARNING: linking a moon to your "
    ?-(ran $czar "galaxy", $king "star", $duke "planet")
    " can cause networking bugs"
  ==
%+  sole-lo
  [& %$ "enter y/yes to continue: "]
|=  inp/tape
?.  |(=("y" inp) =("yes" inp))
  (sole-so [%tang leaf+"canceled" ~])
(sole-so [%tang leaf+"moon: {<`@p`mon>}; ticket: {<`@p`tic>}" leaf+"" ~])
