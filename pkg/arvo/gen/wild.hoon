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
:: +$  wilt  (list [l=* s=sock])
+$  wilt  (list [l=path s=sock])  :: just useful for pretty printing
--
::
=|  gil=(set type)       ::  all seen types
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
  =/  mits=(list type)
    (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
  ::
  ~&  ~
  ~&  [%batt `@ux`(mug data.semi)]
  ~&  [%load p.t]
  ~&  [%bell bell]
  ::
  =.  wit
    %+  weld  wit  ^-  wilt
    %-  zing
    %+  turn  mits
    |=  t=type
    ?:  (~(has in gil) t)  wit
    %=  ^$  ::  traverse minted types
      t    t
      gil  (~(put in gil) t)
      wit  ~[[l=~[bell] s=[& ~]]]
      :: wit  %+  turn  wit
      ::      |=  p=[l=path s=sock]
      ::      [l=(weld l.p ~[bell]) s=s.p]
    ==
  ?:  (~(has in gil) t)  wit
  %+  weld  wit
  %=  $  ::  traverse payload type
    t  p.t
    gil  (~(put in gil) t)
    wit  %+  turn  wit
         |=  p=[l=path s=sock]
         [l=(weld l.p ~[bell]) s=s.p]
  ==
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
