::  Generate a $wilt from a given noun's type.
::
::::  /hoon/wild/gen
  ::
/?    310
/+  h=hoon  ::  XX remove later
::
::::
  ::
:-  %say
|=  [^ [arg=type ~] ~]
:-  %noun
=>
|%
+$  cape  $@(? [cape cape])
+$  sock  [=cape data=*]
+$  wilt  (list [l=path s=sock])    ::  XX switch back to *
--
%.  arg
=|  cot=(map type wilt)
=|  gil=(set type)                  ::  all types seen
|=  t=type
^-  wilt
?+    t
    (zing ~(val by cot))
    [%cell *]
  ~&  %cell
  (weld $(t p.t) $(t q.t))
    [%core *]
  ::  extract info from type
  =+  [semi chap]=[p q]:r.q.t
  ?~  p.p.q.t
    $(t p.t)
  =/  bell  (need p.p.q.t)
  =/  self=wilt  ~[[l=~[bell] s=[& (mug data.semi)]]]
  ::
  =/  mats=(list (map term hoon))
    (turn ~(val by chap) |=(tom=tome q.tom))
  =/  hops=(list (pair term hoon))
    (zing (turn mats |=(mat=(map term hoon) ~(tap by mat))))
  =/  kits=(list type)  ::  get the types of the minted arms
    %+  skim
      (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
    |=  kit=type
    ^-  ?
    &(?=([%core *] kit) !=(~ p.p.q.kit))
  ::
  ~&  [%kits bell kits]
  $(t p.t)
    [%face *]
  ~&  %face
  $(t q.t)
    [%fork *]
  ~&  %fork
  %+  weld  `wilt`(zing ~(val by cot))  ^-  wilt
  (zing (turn ~(tap in p.t) |=([t=type] ^$(t t))))
    [%hint *]
  ~&  %hint
  $(t q.t)
    [%hold *]
  ~&  %hold
  ?:  (~(has in gil) t)  (zing ~(val by cot))
  $(t (~(play ut.h p.t) q.t), gil (~(put in gil) t))
==
