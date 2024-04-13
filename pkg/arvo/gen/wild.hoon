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
  ::    seminoun is battery if already known
  ::    r.q.t (pair seminoun (map term tome))
  ::      p.r.q.t seminoun  "partial noun"
  ::      q.r.q.t (map term tome)
  ::
  ::  +$  tome  (pair what (map term hoon))
  ::  +$  what  (unit (pair cord (list sect)))
  ~&  %core
  :: ~&  pt+p.t      :: type
  :: ~&  qqt+q.q.t   :: coil's type
  :: ~&  garb+p.q.t  :: coil's garb
  :: ~&  chapters+~(key by q.r.q.t)  :: terms of coil's map
  =/  coil-mtt  q.r.q.t
  ~&  terms+(~(run by coil-mtt) |=(tom=tome p.tom)) :: terms of coil's map's tomes
  ~&  whats+(~(run by coil-mtt) |=(tom=tome ~(key by q.tom)))
  $(t p.t)
  ::  XX infer the type of each arm and continue into it
    [%face *]
  ~&  %face
  $(t q.t)
    [%fork *]
  ~&  %fork
  (~(rep in p.t) |=([t=type ~] ^$(t t)))
    [%hint *]
  ~&  %hint
  $(t q.t)
    [%hold *]
  ~&  %hold
  ?:  (~(has in gil) t)  ~
  $(t (~(play ut p.t) q.t), gil (~(put in gil) t))
==
