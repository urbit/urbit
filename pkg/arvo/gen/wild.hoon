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
:: =|  wlt=wilt
|-  ^-  ~
?+    t
    ~
    [%cell *]
  ~&  %cell
  =+  $(t p.t)
  $(t q.t)
    [%core *]
  ::  p.t type  ...
  ::  q.t coil
  ::    p.q.t garb  (trel (unit term) poly vair)
  ::    q.q.t type  ...
  ::    r.q.t (pair seminoun (map term tome))
  ::      p.r.q.t seminoun  "partial noun"
  ::      q.r.q.t (map term tome)
  ::
  ::  +$  tome  (pair what (map term hoon))
  ::  +$  what  (unit (pair cord (list sect)))
  ~&  %core
  ~&  pt+p.t
  ~&  qqt+q.q.t
  ~&  garb+p.q.t
  ~&  chapters+~(key by q.r.q.t)
  ~&  terms+(~(run by q.r.q.t) |=(a=tome p.a))
  ~&  whats+(~(run by q.r.q.t) |=(a=tome ~(key by q.a)))
  $(t p.t)
  ::  XX infer the type of each arm and continue into it
    [%face *]  $(t q.t)
    :: [%fork *]  (~(rep in p.t) |=([t=type ~] ^$(t t)))
    [%fork *]  ~
    [%hint *]  $(t q.t)
    [%hold *]
  ?:  (~(has in gil) t)  ~
  $(t (~(play ut p.t) q.t), gil (~(put in gil) t))
==
