::  Redeem ticket to replace current urbit with full one  XX DEPRECATED
::
::::  /hoon/begin/hood/gen
  ::
/?    310
/-  sole
/+  generators, old-zuse
=,  old-zuse
::
::::
  ::
=>  |%
    ++  begs  {his/@p tic/@p yen/@t ges/gens}
    ++  scug  |*({a/@ b/(pole)} ?~(b ~ ?~(a ~ [-.b $(b +.b, a (dec a))])))
    --
=,  generators
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/_(scug *@ *{his/@p tic/@p ~})}
        safety/?($off $on)
    ==
^-  (sole-result:sole (cask begs))
?.  =(safety %off)
  %+  print
    :-  %leaf
    "|begin is deprecated, please invoke urbit with -w [name] -t [ticket]"
  no-product
=-  -
%+  prompt
  [%& %helm-begin "your urbit: ~"]
%+  parse  fed:ag
|=  his/@p
%+  prompt
  [%& %helm-ticket "your ticket: ~"]
%+  parse  fed:ag
|=  tic/@p
%+  prompt
  [%& %helm-entropy "some entropy: "]
%+  parse  (boss 256 (more gon qit))
|=  yen/@t
=+  ney=(shax yen)
%+  print  `tank`[%leaf "entropy check: {(scow %p `@p`(mug ney))}"]
%+  produce  %helm-begin
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
