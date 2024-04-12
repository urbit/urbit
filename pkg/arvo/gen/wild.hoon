::  Generate a $wilt from a given noun's type.
::
::::  /hoon/wild/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  [^ [arg=type ~] ~]
:-  %noun
%.  arg
=>
|%
+$  cape  $@(? [cape cape])    ::  noun mask; & means known, | unknown
+$  sock  [=cape data=*]       ::  core with mask; only & values need to match
+$  wilt  (list [l=* s=sock])  ::  list of labeled masked cores
--
::
|=  [t=type]
=|  gil=(set type)
|-  ^-  ~
?+  t  ~
  [%cell *]
=+  $(t p.t)
$(t q.t)
  [%core *]
~&  p.q.t
~&  ~(key by q.r.q.t)
~&  (~(run by q.r.q.t) |=(a=tome ~(key by q.a)))
$(t p.t)
::  XX infer the type of each arm and continue into it
  [%face *]
$(t q.t)
  [%fork *]
%-  ~(rep in p.t)
|=  [t=type ~]
^$(t t)
  [%hint *]
$(t q.t)
  [%hold *]
?:  (~(has in gil) t)  ~
$(t (~(play ut p.t) q.t), gil (~(put in gil) t))
==
