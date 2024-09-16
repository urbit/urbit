::  print [len] %ames message-pump timers, sorted by number-per-ship
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=$@(~ [len=@ ~]) ~]
:-  %noun
::
=;  who
  ^-  (list [@ta @ud])
  =/  len   ?^(arg len.arg 50)
  (scag len (sort ~(tap by who) |=([[@ a=@ud] @ b=@ud] (gth a b))))
::
=|  who=(map @ta @ud)
=/  tim  .^((list (pair @da duct)) bx+/(scot %p p.bec)//(scot %da now)/debug/timers)
|-  ^+   who
?~  tim  who
?.  &(?=(^ q.i.tim) ?=([%ames %pump ^] i.q.i.tim))
  $(tim t.tim)
=*  her  i.t.t.i.q.i.tim
=/  i  (~(gut by who) her 0)
$(tim t.tim, who (~(put by who) her +(i)))
