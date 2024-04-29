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
++  rent
  ::  find the parent %core type of the given %core type
  |=  t=type
  ^-  (unit type)
  ?+    t
      ~
      [%core *]
    ?:  ?=([%core *] p.t)
      `p.t
    ~
  ==
++  kilt
  ::  get a kid's wilt, where p is its papa's label
  |=  [t=type p=term]
  ^-  wilt
  ?.  ?=([%core *] t)
    ~
  =+  [semi chapters]=[p q]:r.q.t
  =/  bell  (need p.p.q.t)  ::  XX
  ~[[l=~[bell p] s=[& (mug data.semi)]]]
--
%.  arg
=|  cot=(map type wilt)
=|  gil=(set type)                  ::  all types seen
=|  wit=wilt
|=  t=type
^-  wilt
?+    t
    wit
    [%cell *]
  ~&  %cell
  (weld $(t p.t) $(t q.t))
    [%core *]
  ::
  =+  [semi chapters]=[p q]:r.q.t
  =/  bell  (need p.p.q.t)  ::  XX
  =/  mats=(list (map term hoon))
    (turn ~(val by chapters) |=(tom=tome q.tom))
  =/  hops=(list (pair term hoon))
    (zing (turn mats |=(mat=(map term hoon) ~(tap by mat))))
  =/  kits=(list type)
    (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
  =/  papa=(unit type)  (rent t)
  =/  self=wilt  ~[[l=~[bell] s=[& (mug data.semi)]]]
  ::
  =/  kids=(list [t=type w=wilt])  (turn kits |=(t=type [t (kilt t bell)]))
  =.  cot  (~(gas by cot) (snoc kids [t self]))
  ::
  ~&  ~
  ~&  [%batt `@ux`(mug data.semi)]
  ~&  [%load p.t]
  ~&  [%bell bell]
  ~&  [%cot cot]
  ?~  papa
    =.  cot
    %-  ~(run by cot)
    |=  w=wilt
    ^-  wilt
    ?~  w
      w
    ?:  =(~[bell] `path`-<.w)
      w
    ~[[l=(weld `path`-<.w ~[bell]) s=->.w]]
    (zing ~(val by cot))
  $(t p.t)
    [%face *]
  ~&  %face
  $(t q.t)
    [%fork *]
  ~&  %fork
  %+  weld  wit  ^-  wilt
  (zing (turn ~(tap in p.t) |=([t=type] ^$(t t))))
    [%hint *]
  ~&  %hint
  $(t q.t)
    [%hold *]
  ~&  %hold
  ?:  (~(has in gil) t)  wit
  $(t (~(play ut.h p.t) q.t), gil (~(put in gil) t))
==
