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
%.  [arg ~ |]
=|  cot=(map type wilt)
=|  gil=(set type)
|=  [t=type pops=(unit wilt) pop=?]
^-  [wit=wilt cot=(map type wilt)]
?+    t
    [(zing ~(val by cot)) cot]
    [%cell *]
  ~&  %cell
  =+  [lw lc]=[wit cot]:$(t p.t)
  =+  [rw rc]=[wit cot]:$(t q.t)
  [(weld lw rw) (~(uni by lc) rc)]
    [%core *]
  ::  extract info from type
  =+  [semi chap]=[p q]:r.q.t
  ?~  p.p.q.t
    [wit cot]:$(t p.t)
  =/  bell  (need p.p.q.t)
  =/  dath=path  ?:(=(~ pops) ~ -<:(need pops))
  =/  self=wilt
    ~[[l=(weld ~[bell] dath) s=[& (mug data.semi)]]]
  ::
  =/  mats=(list (map term hoon))
    (turn ~(val by chap) |=(tom=tome q.tom))
  =/  hops=(list (pair term hoon))
    (zing (turn mats |=(mat=(map term hoon) ~(tap by mat))))
  =/  kits=(list type)  ::  get the types of the minted arms
    %+  skim
      (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
    |=  k=type
    ^-  ?
    &(?=([%core *] k) !=(~ p.p.q.k))
  ::
  ~&  ~
  ~&  [%self self]
  ::
  =^  kids=(list [wit=wilt cot=(map type wilt)])  cot
    =/  kips=(list [wit=wilt cot=(map type wilt)])
      (turn kits |=(k=type ^$(t k, pops `self)))
    :-  kips
    %-  ~(uni by cot)
    %+  roll
      (turn kips |=(p=[wit=wilt cot=(map type wilt)] cot.p))
    |=  [a=(map type wilt) b=(map type wilt)]
    ^-  (map type wilt)
    (~(uni by a) b)
  ::
  ?.  =(~ pops)
    =/  dath=path  -<:(need pops)
    =/  silt=wilt  ~[[l=(weld ~[bell] dath) s=[& (mug data.semi)]]]
    :-  silt
    (~(put by cot) [t silt])
  ::
  =/  papa=?  ?!  ?=([%core *] p.t)  ::  XX not thorough
  ?:  pop
    =.  cot
    %-  ~(run by cot)
    |=  w=wilt
    ^-  wilt
    ?~  w
      w
    ?:  |(=(~[bell] `path`-<.w) =(bell (rear `path`-<.w)))
      w
    ~[[l=(weld `path`-<.w ~[bell]) s=->.w]]
    ?:  papa
      [(zing ~(val by cot)) cot]
    $(t p.t, pop &)
  ::
  $(t p.t, pop &)
    [%face *]
  ~&  %face
  $(t q.t)
    [%fork *]
  ~&  %fork
  =/  wit
  %+  weld  `wilt`(zing ~(val by cot))  ^-  wilt
  (zing (turn ~(tap in p.t) |=([t=type] wit:^$(t t))))
  [wit cot]
    [%hint *]
  ~&  %hint
  $(t q.t)
    [%hold *]
  ~&  %hold
  ?:  (~(has in gil) t)  [(zing ~(val by cot)) cot]
  $(t (~(play ut.h p.t) q.t), gil (~(put in gil) t))
==
