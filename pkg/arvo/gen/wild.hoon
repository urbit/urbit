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
:: |-  ^-  wilt
|-  ^-  ~
?+    t
    ~
    [%cell *]
  =+  $(t p.t)
  $(t q.t)
    [%core *]
  ~&  [%core]
  ~&  [%type p.t]
  ~&  [%garb p.q.t]
  ::  [%core p=type q=coil]
  ::  p.t type  ...
  ::  q.t coil
  ::    p.q.t garb  (trel (unit term) poly vair)
  ::    q.q.t type  ...
  ::    r.q.t (pair seminoun (map term tome))
  ::      p.r.q.t seminoun  "partial noun" (battery if known)
  ::      q.r.q.t (map term tome)
  ::
  ::  +$  tome  (pair what (map term hoon))
  ::  +$  what  (unit (pair cord (list sect)))
  =/  semi  `seminoun`p.r.q.t
  ~&  semi+`@ux`(mug data.semi)  :: mug of the battery
  =/  toms  `(list tome)`~(val by q.r.q.t)
  =/  mats  `(list (map term hoon))`(turn toms |=(tom=tome q.tom))
  =/  hots  `(list term)`(zing (turn mats |=(mat=(map term hoon) ~(tap in ~(key by mat)))))
  =/  hons  `(list hoon)`(zing (turn mats |=(mat=(map term hoon) ~(val by mat))))
  =/  mins  `(list (pair type nock))`(turn hons |=(hon=hoon (~(mint ut t) %noun hon)))
  =/  mits  `(list type)`(turn mins |=(min=(pair type nock) p.min))
  =+  %+  turn  mits  |=  ty=type
                      ?:  (~(has in gil) ty)  ~
                      ^$(t ty, gil (~(put in gil) ty))
  ~
  :: $(t p.t)
  ::  XX infer the type of each arm and continue into it
    [%face *]
  $(t q.t)
    [%fork *]
  (~(rep in p.t) |=([t=type ~] ^$(t t)))
    [%hint *]
  $(t q.t)
    [%hold *]
  ?:  (~(has in gil) t)  ~
  $(t (~(play ut p.t) q.t), gil (~(put in gil) t))
==
