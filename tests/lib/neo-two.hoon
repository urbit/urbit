/-  neo
/+  aux=neo-two
/+  *test
=|  [=farm:neo =loam:dirt:neo]
=*  sta  -
|%
++  take-dirt-card
  |=  =card:dirt:neo
  ^+  sta
  =^  gifts=(list gift:dirt:neo)  loam
    (~(call plow:aux loam) card)
  =.  farm  (~(take till:aux [loam farm]) gifts)
  sta
++  oat  *oath:neo
::  ++  reset
++  test-grow
  =.  sta  (take-dirt-card #/foo %grow atom/!>(1) ~ *oath:neo)
  =.  sta  (take-dirt-card #/foo/bar %grow atom/!>(1) ~ *oath:neo)
  =.  sta  (take-dirt-card #/foo/bar/baz %grow atom/!>(1) ~ *oath:neo)
  =.  sta  (take-dirt-card #/foo/bar %grow atom/!>(1) ~ *oath:neo)
  =.  sta  (take-dirt-card #/foo %grow atom/!>(2) ~ *oath:neo)
  =/  want  (~(piek till:aux [loam farm]) %z #/foo)
  =/  have  (~(peek till:aux [loam farm]) %z #/foo)
  ~&  sta
  (expect-eq !>(want) !>(have))
--
