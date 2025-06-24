::  Test connectivity to a ship with optional message
::
::::  /hoon/hi/hood/gen
  ::
/?    310
:-  %say
|=([^ [who=ship mez=$@(~ [a=@ ~])] ~] helm-send-hi+[who ?~(mez ~ a.mez)])
