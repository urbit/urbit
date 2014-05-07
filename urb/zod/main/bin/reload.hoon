!:
::  /=main=/bin/reload/hoon
::
=>  .(- `[who=@p how=path]`-)
|=  [est=time *]
|=  all=(list ,@tas)
:_  ~
^-  (list gift)
%+  turn  all
|=  nam=@tas
^-  gift
=+  tip=(end 3 1 nam)
=+  pax=[(scot %p who) %arvo (scot %da est) nam %hoon ~]
[%xx %veer ?:(=('z' tip) %$ tip) pax (,@ .^(%cx pax))]
