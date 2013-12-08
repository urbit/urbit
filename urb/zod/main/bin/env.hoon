!:
::  /=main=/bin/env/hoon
::
=>  .(- [who=`@p`-< how=`path`->])
|=  [est=time eny=@uw]
|=  arg=(list)
^-  bowl
:_  ~
=+  ^=  voy  ^-  (list tape)
    :~  "who: {<who>}"
        "how: {<how>}"
        "est: {<est>}"
        "eny: {<eny>}"
    ==
::
=+  ^=  gar
    =+  nix=1
    |-  ^-  (list tape)
    ?~  arg  ~
    :_  $(arg t.arg, nix +(nix))
    "arg: {(pave !>(nix))}: {(pave !>(i.arg))}"
::
(turn (weld voy gar) |=(a=tape [%la %leaf a]))
