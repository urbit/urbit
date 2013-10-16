!:
::  /=main=/bin/chat/hoon
::
=>  .(-< `who=@p`-<)
=>  .(+ =>(+ ^/===/lib/pony))
|=  [est=time *]
|=  ~
=+  bud=(sein who)
^-  bowl
:-  ~  :-  ~
:-  ^-  (list slip)
    :~  [/hi [%lq %hi]]
        [/yu [%lq %yu]]
        [/up [%up %text ": " ""]]
        [/re [%ow ~]]
    ==
|=  [now=@da pax=path nut=note]
^-  bowl
%-  pogo
:_  ^$
^-  bowl
?+    pax  !!
    /re
  ?>  ?=(%ow -.nut)
  [~ ~]
::
    /up 
  ?>  ?=(%up -.nut)
  :_(~ :_(~ [%sq bud %ye /re p.nut]))
::
    /hi  
  ?>  ?=(%lq -.nut)
  =+  msg=?:(=(0 r.nut) "remains quietly present" (trip ((hard ,@) r.nut)))
  :_(~ `(list gift)`:_(~ [%la %leaf "< {(trip (numb p.nut now))}: {msg}"]))
::
    /yu
  ?>  ?=(%lq -.nut)
  =+  dof=((hard ,[p=@p q=@t]) r.nut)
  =+  msg=?:(=(0 q.dof) "remains quietly present" (trip q.dof))
  :_(~ :_(~ [%la %leaf "> {(trip (numb p.dof now))}: {msg}"]))
==
