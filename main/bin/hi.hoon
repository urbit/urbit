!:
::  /=main=/toy/hi/hoon
::
|=  *
|=  [bud=@p ebb=$|(~ [tex=tape ~])]
^-  bowl
:-  :~  [%sq bud %hi /request ?~(ebb '' (rap 3 tex.ebb))]
    ==
:-  ~
:-  ^-  (list slip)
    :~  [/request [%ow ~]]
        [/prompt [%up %none "waiting for {(scow %p bud)}" ~]]
    ==
|=  [now=@da pax=path nut=note]
?>  =(/request pax)
?>  ?=(%ow -.nut)
:-  ?:  =(%good p.nut)
      ~ 
    :_  ~
    :-  %ha
    :-  %leaf
    "hi: {(scow %p bud)} rejected a message"
~
