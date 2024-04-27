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
=|  cot=(map type path)             ::  core types
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
  =/  bell  (need p.p.q.t)  ::  XX
  =/  mats=(list (map term hoon))
    (turn ~(val by chapters) |=(tom=tome q.tom))
  =/  hops=(list (pair term hoon))
    (zing (turn mats |=(mat=(map term hoon) ~(tap by mat))))
  =/  kits=(list type)  ::  type of each "kid" (types of minted arms)
    (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
  ::
  ~&  ~
  ~&  [%batt `@ux`(mug data.semi)]
  ~&  [%load p.t]
  ~&  [%bell bell]
  ::
  ::  parents
  ::
  =/  self=wilt  ~[[l=~[bell] s=[& (mug data.semi)]]]
  =/  past=wilt  $(t p.t)
  ~&  [%past bell past]
  =/  root=?  =(~ past)
  ?:  root
    =.  cot  (~(put by cot) t ~[bell])
    self
  =/  old=wilt  ~[(snag (dec (lent past)) past)]
  =/  olds=path  -<.old
  %+  weld  past
  ^-  wilt
  ~[[l=(weld `path`-<.self olds) s=[& (mug data.semi)]]]  ::  XX nix mug
  ::
  ::  children
  ::
  :: =/  self=wilt  ~[[l=~[bell] s=[& (mug data.semi)]]]
  :: =/  next=(list wilt)
  ::   (skip (turn kits |=(t=type ^$(t t))) |=(w=wilt =(~ w)))
  :: =/  leaf=?  =(~ next)
  :: ?:  leaf
  ::   =.  cot  (~(put by cot) t -<.self)
  ::   self
  :: %-  zing
  :: %+  turn  next
  :: |=  w=wilt
  :: ^-  wilt
  :: ~[[l=(weld `path`-<.w `path`-<.self) s=[& (mug data.semi)]]]
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
