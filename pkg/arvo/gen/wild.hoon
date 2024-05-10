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
=|  pop=(unit type)                                     ::  father
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
  =/  bell=(unit term)  p.p.q.t                         ::
  ?~  bell                                              ::
    $(t p.t)                                            ::
  =/  code=(list hoon)                                  ::  arm codes
    %-  zing                                            ::
    (turn tabs |=(t=(map term hoon) ~(val by t)))       ::
  =/  arms=(list type)                                  ::  arm types
    (turn code |=(c=hoon p:(~(mint ut.h t) %noun c)))   ::
  =/  sons=(list kind)                                  ::  sons
    %+  skip
      (turn arms |=(t=type ^$(t t)))
    |=(k=kind &(=(~ tags.k)))
  ?~  sons                                              ::  leaf
    =/  leaf  ~[/[u.bell]]
    =.  fin  (~(put by fin) t leaf)
    [leaf fin]
  =/  kids=(list path)                                  ::
    (zing (turn sons |=(k=kind tags.k)))
  =.  fin                                               ::  append bell
    %+  ~(put by fin)
      t
    (turn kids |=(p=path (snoc p u.bell)))              ::
  =.  fin                                               ::  interpath
    %+  ~(jab by fin)
      t
    |=(tags=(list path) (snoc tags /[u.bell]))
  =/  papa=kind  $(t p.t)
  =/  tags=(list path)
    (weld tags.papa (need (~(get by fin) t)))
  [tags (~(uni by fin.papa) fin)]
    [%face *]                                           ::
  $(t q.t)                                              ::
    [%fork *]                                           ::
  [~ fin]  ::  XX                                       ::
  :: =/  ways  (turn ~(tap in p.t) |=(t=type ^$(t t)))  ::
  :: =/  wit                                            ::
  :: %+  weld  `wilt`(zing ~(val by fin))  ^-  wilt     ::
  :: (zing (turn ~(tap in p.t) |=([t=type] wit:^$(t t)))::
  :: [wit cot]                                          ::
    [%hint *]                                           ::
  $(t q.t)                                              ::
    [%hold *]                                           ::
  ?:  (~(has in gil) t)  [(zing ~(val by fin)) fin]     ::
  $(t (~(play ut.h p.t) q.t), gil (~(put in gil) t))    ::
==                                                      ::
