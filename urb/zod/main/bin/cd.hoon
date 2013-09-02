!:
::  /=main=/bin/cd/hoon
::
=>  .(-< `who=@p`-<)
|=  [est=time *]
|=  arg=$|(~ [p=path ~])
:_  ~
^-  (list gift)
?~  arg
  :~  [%cc ~]
      [%cp ~]
  ==
?.  ?=(^ p.arg)  ~
?.  =(i.p.arg (scot %p who))  ~|(%bad-ship !!)
?.  ?=(^ t.p.arg)  ~
=+  gav=(slay i.t.p.arg)
?.  ?=([~ %$ %tas *] gav)  ~|(%bad-desk !!)
:-  [%ck q.p.u.gav]
?.  ?=(^ t.t.p.arg)  ~
=+  gov=(slay i.t.t.p.arg)
:-  :-  %cc
    ?+  gov  ~|(%bad-case !!)
      [~ %$ %da *]   ?:(=(est q.p.u.gov) ~ [~ %da q.p.u.gov])
      [~ %$ %ud *]   [~ %ud q.p.u.gov]
      [~ %$ %tas *]  [~ %tas q.p.u.gov]
    ==
[[%cp t.t.t.p.arg] ~]
