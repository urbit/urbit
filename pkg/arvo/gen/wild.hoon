::  Generate a $wilt from a given noun's type.          ::
::  XX put extra %wild hint in transient compute cores? ::
::                                                      ::
::::  /hoon/wild/gen                                    ::
  ::                                                    ::
/?    310                                               ::
/+  h=hoon                                              ::  XX remove later
::                                                      ::
::::                                                    ::
  ::                                                    ::
:-  %say                                                ::
|=  [^ [arg=type ~] ~]                                  ::
:-  %noun                                               ::
=>                                                      ::
|%                                                      ::
+$  cape  $@(? [cape cape])                             ::
+$  sock  [=cape data=*]                                ::
+$  wilt  (list [l=path s=sock])                        ::
+$  kind  [tags=(list path) fin=(map type (list path))] ::
--                                                      ::
%.  arg                                                 ::
=|  gil=(set type)                                      ::  processing
=|  fin=(map type (list path))                          ::  finished state
=|  dad=path                                            ::  father
|=  t=type                                              ::
^-  kind                                                ::
?:  (~(has in gil) t)                                   ::
  [~ fin]                                               ::
?:  (~(has by fin) t)                                   ::
  [(~(get by fin) t) fin]                               ::
=.  gil  (~(put in gil) t)                              ::
?+    t                                                 ::
    [~ fin]                                             ::
    [%cell *]                                           ::
  =/  l=kind  $(t p.t)                                  ::
  =/  r=kind  $(t q.t, fin fin.l)                       ::
  [(weld tags.l tags.r) fin.r]                          ::
    [%core *]                                           ::
  =+  [semi chap]=[p q]:r.q.t                           ::  seminoun, chapters
  =/  tabs=(list (map term hoon))                       ::  chapter contents
    (turn ~(val by chap) |=(t=tome q.t))                ::
  =*  bell  p.p.q.t                                     ::
  ?~  bell                                              ::
    $(t p.t)                                            ::  XX
  =/  pay=kind  $(t p.t)
  =/  code=(list hoon)                                  ::  arm codes
    %-  zing                                            ::
    (turn tabs |=(t=(map term hoon) ~(val by t)))       ::
  =/  pros=(list type)                                  ::  arm product types
    (turn code |=(c=hoon p:(~(mint ut.h t) %noun c)))   ::
  =/  self=path  [u.bell dad]
  =/  arms=kind
    =|  pax=(list (list path))
    |-  ^-  kind
    ?~  pros
      [(zing pax) fin]
    =/  kid=kind  ^$(t i.pros, dad self)
    %=  $
      pros  t.pros
      pax   [tags.kid pax]
      fin   fin.kid
    ==
  =/  tags=(list path)  [self tags.arms]
  =.  fin  (~(put by fin) t tags)
  [(weld tags tags.pay) fin.pay]
    [%face *]                                           ::
  $(t q.t)                                              ::
    [%fork *]                                           ::
  =|  pax=(list (list path))
  |-  ^-  kind
  ?~  p.t
    [(zing pax) fin]
  =^  n=(list path)  fin  ^$(t n.p.t)
  =^  l=(list path)  fin  $(p.t l.p.t)
  =^  r=(list path)  fin  $(p.t r.p.t)
  [:(weld n l r) fin]
    [%hint *]                                           ::
  $(t q.t)                                              ::
    [%hold *]                                           ::
  $(t p:(~(mint ut.h p.t) %noun q.t))                   ::
==                                                      ::
