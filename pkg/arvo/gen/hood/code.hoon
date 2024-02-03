::  Helm: query or reset login code for web
::
::::  /hoon/code/hood/gen
  ::
/?    310
/-  *sole
/+  *generators
:-  %ask
|=  $:  [now=@da tick=@ud @ our=@p ^]
        [arg=?(~ [%reset ~]) ~]
    ==
^-  (sole-result [%helm-code ?(~ %reset)])
?~  arg
  =/  code=tape
    %+  slag  1
    %+  scow  %p
    .^(@p %j (en-bema [our %code [da+now ud+tick]] /(scot %p our)))
  =/  step=tape
    %+  scow  %ud
    .^(@ud %j (en-bema [our %step [da+now ud+tick]] /(scot %p our)))
  ::
  %+  print  'use |code %reset to invalidate this and generate a new code'
  %+  print  leaf+(weld "current step=" step)
  %+  print  leaf+code
  (produce [%helm-code ~])
::
?>  =(%reset -.arg)
%+  print  'continue?'
%+  print  'warning: resetting your code closes all web sessions'
%+  prompt
  [%& %project "y/n: "]
%+  parse
  ;~  pose
    (cold %.y (mask "yY"))
    (cold %.n (mask "nN"))
  ==
|=  reset=?
?.  reset
  no-product
(produce [%helm-code %reset])
