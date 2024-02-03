::  +desk-requests: count pending requests for a desk
::
:-  %say
|=  $:  [now=@da tick=@ud @ our=@p ^]
        [=desk ~]
        ~
    ==
:-  %tang
^-  tang
=/  cul=(list [@p rave:clay])
  %~  tap  in
  .^  (set [@p rave:clay])
      %cx  (en-bema [our %$ [da+now ud+tick]] /cult/[desk])
  ==
::
=/  [loc=_cul inc=_cul]
  (skid cul |=([=@p rave:clay] =(p our)))
=/  syc=_cul
  =/  nex=@ud
    +(ud:.^(cass:clay %cw (en-bema [our desk [da+now ud+tick]] /)))
  (skim inc |=([@p =rave:clay] =([%sing %w ud+nex /] rave)))
::
%-  flop
:~  leaf+"total:     {<(lent cul)>}"
    leaf+"- local:     {<(lent loc)>}"
    leaf+"- incoming:  {<(lent inc)>}"
    leaf+"  - for next:  {<(lent syc)>}"
==
