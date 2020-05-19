::  Print keys for a ship
::
:-  %say
|=  [[now=time @ our=ship ^] [her=ship ~] ~]
=/  our  (scot %p our)
=/  now  (scot %da now)
=/  her  (scot %p ship)
:*  %noun
    life=.^((unit @ud) %j /[our]/lyfe/[now]/[her])
    rift=.^((unit @ud) %j /[our]/ryft/[now]/[her])
==
