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
%.  [arg ~ ~]
=|  cot=(map type wilt)
=|  gil=(set type)                  ::  all types seen
=|  wit=wilt
|=  [t=type brud=(list type) pops=(unit wilt)]
^-  wilt
?+    t
    wit
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
    %+  skip  ::  skip unjetted arms
      (turn hops |=(hop=(pair term hoon) p:(~(mint ut.h t) %noun q.hop)))
    |=  kit=type
    ^-  ?
    &(?=([%core *] kit) =(~ p.p.q.kit))
  ::
  =/  kids=(list [type wilt])
    %+  turn
      kits
    |=  t=type
    ^-  [type wilt]
    [t ^$(t t, pops `self)]
  ~&  [%kids bell (snoc kids [t self])]
  =.  cot  (~(gas by cot) (snoc kids [t self]))
  ::
  ~&  ~
  ~&  [%batt `@ux`(mug data.semi)]
  ~&  [%load p.t]
  ~&  [%bell bell]
  ::
  ?.  =(~ pops)  ::  we know our pops
    =/  dath=path  -<:(need pops)
    =/  rest=wilt  ~[[l=(weld ~[bell] dath) s=[& (mug data.semi)]]]
    ~&  [%rest rest]
    rest
  ::
  =/  papa=(unit type)
    ?:  ?=([%core *] p.t)
      `p.t
    ~
  ?~  papa  ::  we are the papa
    =.  cot
    %-  ~(run by cot)  ::  add our label to cot's entries
    |=  w=wilt
    ^-  wilt
    ?~  w
      w
    ?:  |(=(~[bell] `path`-<.w) =(bell (rear `path`-<.w)))
      w
    ~[[l=(weld `path`-<.w ~[bell]) s=->.w]]
    (zing ~(val by cot))  ::  we're done
  ::
  ?.  =(~ brud)  ::  we know our kids
    =/  sons=(list [t=type w=wilt])
      %+  turn  ::  prepend our label to all our sons
        brud
      |=  b=type
      =/  orig=wilt  (need (~(get by cot) b))
      [b `wilt`~[[l=(weld `path`-<.orig ~[bell]) s=->.orig]]]
    =.  cot  (~(gas by cot) sons)  ::  update sons in cot
    %=  $  ::  recurse with empty brood
      t     p.t
      brud  ~
    ==
  ::
  %=  $  ::  recurse with ~[kits t] as brood
    t     p.t
    brud  `(list type)`(snoc kits t)
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
