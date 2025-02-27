::  Helm: send message to an urbit
::
::::  /hoon/hi/hood/gen
  ::
/?    310
:-  %say
|=([^ [who=ship mez=$@(~ [a=@ ~])] ~] helm-send-hi+[who ?~(mez ~ a.mez)])
