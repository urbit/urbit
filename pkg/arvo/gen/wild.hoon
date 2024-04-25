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
%.  arg
=>
|%
+$  cape  $@(? [cape cape])
+$  sock  [=cape data=*]
+$  wilt  (list [l=path s=sock])    ::  XX switch back to *
--
::
=|  sirs=(map type [l=path s=sock])  ::  parents of each type
=|  kids=(map type [l=path s=sock])  ::  children of each type
=|  cot=(set type)                   ::  core types seen
=|  gil=(set type)                   ::  all types seen
=|  wit=wilt
|=  t=type
^-  wilt
?+    t
    wit
    [%cell *]
  ~&  %cell
  (weld $(t p.t) $(t q.t))
    [%core *]
  ::  [%core p=type q=coil]
  ::  p.t payload type
  ::  q.t coil
  ::    p.q.t garb  (trel (unit term) poly vair)
  ::    q.q.t sample type where the core was defined
  ::    r.q.t (pair seminoun (map term tome))
  ::      p.r.q.t seminoun  "partial noun" (battery if known)
  ::      q.r.q.t (map term tome)
  ::
  ::  +$  tome  (pair what (map term hoon))
  ::  +$  what  (unit (pair cord (list sect)))
  =+  [semi chapters]=[p q]:r.q.t
  =/  bell  (need p.p.q.t)
  =/  mats=(list (map term hoon))
    (turn ~(val by chapters) |=(tom=tome q.tom))
  =/  hops=(list (pair term hoon))
    (zing (turn mats |=(mat=(map term hoon) ~(tap by mat))))
  =/  kits=(list type)  ::  types of each "kid" (types of minted arms)
    (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
  ::
  ~&  ~
  ~&  [%batt `@ux`(mug data.semi)]
  ~&  [%load p.t]
  ~&  [%bell bell]
  ::
  ::  1. collect the input type's list of parents
  ::  2. collect the input type's children to be processed
  ::  3. collect each children's list of parents
  ::  4. zip the values into full label hierarchies
  ::
  ::  cot=(set type) all core types seen
  ::  gil=(set type) all non-core types seen
  ::  kids=(map type [l=path s=sock]) tracks each type's children
  ::  sirs=(map type [l=path s=sock]) tracks each type's parents
  ::
  ?.  (~(has in cot) t)
    =.  cot  (~(put in cot) t)
    (weld ~[[l=~[bell] s=[& (mug data.semi)]]] $(t p.t))
  wit
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
