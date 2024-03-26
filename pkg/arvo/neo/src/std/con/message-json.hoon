/@  message
:-  [%message %json]
|=  mes=message
=,  enjs:format
%-  pairs
:~  from/s/(scot %p from.mes)
    when/(time now.mes)
    contents/s/contents.mes
==
