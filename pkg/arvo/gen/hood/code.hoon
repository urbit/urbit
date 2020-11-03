::  Helm: query or reset login code for web
::
::::  /hoon/code/hood/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=?(~ [%reset ~]) ~]
    ==
=*  our  p.bec
:-  %helm-code
?~  arg
  =/  code=tape
    %+  slag  1
    %+  scow  %p
    .^(@p %j /(scot %p our)/code/(scot %da now)/(scot %p our))
  =/  step=tape
    %+  scow  %ud
    .^(@ud %j /(scot %p our)/step/(scot %da now)/(scot %p our))
  %-  %-  slog
  :~  [%leaf code]
      [%leaf (weld "current step=" step)]
      [%leaf "use |code %reset to invalidate this and generate a new code"]
  ==
  ~
?>  =(%reset -.arg)
%reset
