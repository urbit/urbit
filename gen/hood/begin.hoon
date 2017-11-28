::  Redeem ticket to replace current urbit with full one  XX DEPRECATED
::
::::  /hoon/begin/hood/gen
  ::
/?    310
/-  sole
/+  old-zuse
=,  old-zuse
::
::::
  ::
=>  |%
    ++  begs  {his/@p tic/@p yen/@t ges/gens}
    ++  scug  |*({a/@ b/(pole)} ?~(b ~ ?~(a ~ [-.b $(b +.b, a (dec a))])))
    --
=,  sole
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/_(scug *@ *{his/@p tic/@p $~})}
        safety/?($off $on)
    ==
^-  (sole-result (cask begs))
?.  =(safety %off)
  %+  sole-yo
    :-  %leaf
    "|begin is deprecated, please invoke urbit with -w [name] -t [ticket]"
  sole-no
=-  -
%+  sole-lo
  [%& %helm-begin "your urbit: ~"]
%+  sole-go  fed:ag
|=  his/@p
%+  sole-lo
  [%& %helm-ticket "your ticket: ~"]
%+  sole-go  fed:ag
|=  tic/@p
%+  sole-lo
  [%& %helm-entropy "some entropy: "]
%+  sole-go  (boss 256 (more gon qit))
|=  yen/@t
=+  ney=(shax yen)
%+  sole-yo  `tank`[%leaf "entropy check: {(scow %p `@p`(mug ney))}"]
%+  sole-so  %helm-begin
:*  his
    tic
    ney
::
    ^-  gens
    :-  %en
    =+  can=(clan his)
    ?-  can
      $czar  [%czar ~]
      $duke  [%duke %anon ~]
      $earl  [%earl (scot %p his)]
      $king  [%king (scot %p his)]
      $pawn  [%pawn ~]
    ==
==
