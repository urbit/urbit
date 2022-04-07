~%  %up-qah  ..part  ~
|*  [k=mold v=mold]
::  molds and mold builders
::
~%  %core  +  ~
|%
::  radix tree-based psq
::
::  * vertical order is priority, horizontal is mug order on keys
::  * no collision resolution in this implementation
::
+$  pri                                               ::  psq
  $@  ~
  $%  [%bin k=k p=@ v=v m=@ l=pri r=pri]
      [%tip k=k p=@ v=v]
  ==
::  bit/mask utilities
::
++  zero                                              ::  mask keeps none
  ~/  %zero
  |=  [m=@ k=k]
  ^-  ?
  =(0 (dis (mug k) m))
::
++  peak                                              ::  max differing bit
  ~/  %peak
  |=  [k=k l=k]
  ^-  @
  (rsh 0 (bex (xeb (mix (mug k) (mug l)))))
::
++  feud                                              ::  high bits differ
  ~/  %feud                                           ::
  |=  [m=@ k=k l=k]                                   ::  m determines 'high'
  ^-  ?
  =/  n  (dis (mix (not 5 1 (dec m)) m) 0x7fff.ffff)
  !=((dis (mug k) n) (dis (mug l) n))
::  tree manipulation
::
++  rule                                               ::  decide branch
  ~/  %rule                                            ::
  |=  [k=k p=@ v=v a=pri b=pri]                        ::  a must be nonempty
  ^-  pri
  =/  l  +<.a
  =/  m  (peak k l)
  ?:  (zero m l)
    [%bin k p v m a b]
  [%bin k p v m b a]
::
++  lex                                               ::  muggish lex order
  ~/  %lex
  |=  [p=@ k=k q=@ l=k]
  ^-  ?
  ?:  =(p q)
    (gor k l)
  (lth p q)
::
++  fuse                                              ::  disjoint tree merge
  ~/  %fuse
  |=  [m=@ l=pri r=pri]
  ^-  pri
  ?~  l  r
  ?-    -.l
      %tip
    ?~  r  l
    :-  %bin
    ?-    -.r
        %tip
      ?:  (lex p.l k.l p.r k.r)
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex p.l k.l p.r k.r)
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ::
      %bin
    ?~  r  l
    :-  %bin
    ?-    -.r
        %tip
      ?:  (lex p.l k.l p.r k.r)
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex p.l k.l p.r k.r)
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ==
::
++  funk                                              ::  shrink node (left)
  ~/  %funk
  |=  [k=k p=@ v=v m=@ l=pri r=pri]
  ^-  pri
  ?~  l
    ?~  r
      [%tip k p v]
    [%bin k p v m ~ r]
  [%bin k p v m l r]
::
++  wane                                              ::  shrink node (right)
  ~/  %wane
  |=  [k=k p=@ v=v m=@ l=pri r=pri]
  ^-  pri
  ?~  r
    ?~  l
      [%tip k p v]
    [%bin k p v m l ~]
  [%bin k p v m l r]
::  api
::
++  get                                               ::  lookup
  ~/  %get
  |=  [a=pri =k]
  ^-  (unit (pair @ v))
  ?~  a  ~
  ?-    -.a
      %tip
    ?.  =(k k.a)  ~
    `[p.a v.a]
  ::
      %bin
    ?:  (feud m.a k k.a)
      ~
    ?:  =(k k.a)
      `[p.a v.a]
    ?:  (zero m.a k)
      $(a l.a)
    $(a r.a)
  ==
::
++  del                                               ::  delete at key k
  ~/  %del
  |=  [a=pri k=k]
  |-  ^-  pri
  ?~  a  ~
  ?-    -.a
      %tip
    ?:  =(k k.a)  ~
    a
  ::
      %bin
    ?:  (feud m.a k k.a)
      a
    ?:  =(k k.a)
      (fuse m.a l.a r.a)
    ?:  (zero m.a k)
      (funk k.a p.a v.a m.a $(a l.a) r.a)
    (wane k.a p.a v.a m.a l.a $(a r.a))
  ==
::
++  raw                                               ::  raw insert
  ~/  %raw                                            ::
  |=  [a=pri k=k p=@ v=v]                             ::  k must not exist in
  ^-  pri                                             ::  queue
  ?~  a  [%tip k p v]
  ?-    -.a
      %tip
    ?:  (lex p k p.a k.a)
      (rule k p v a ~)
    (rule k.a p.a v.a [%tip k p v] ~)
  ::
      %bin
    ?:  (feud m.a k k.a)
      ?:  (lex p k p.a k.a)
        (rule k p v a ~)
      (rule k.a p.a v.a [%tip k p v] (fuse m.a l.a r.a))
    :-  %bin
    ?:  (lex p k p.a k.a)
      ?:  (zero m.a k.a)
        [k p v m.a $(a l.a, k k.a, p p.a, v v.a) r.a]
      [k p v m.a l.a $(a r.a, k k.a, p p.a, v v.a)]
    ?:  (zero m.a k)
      [k.a p.a v.a m.a $(a l.a) r.a]
    [k.a p.a v.a m.a l.a $(a r.a)]
  ==
::
++  put                                               ::  insert
  ~/  %put
  |=  [a=pri k=k p=@ v=v]
  ^-  pri
  (raw (del a k) k p v)
::
++  gas                                               ::  concatenate
  ~/  %gas
  |=  [a=pri b=(list (trel k @ v))]
  |-  ^-  pri
  ?~  b  a
  $(b t.b, a (put a p.i.b q.i.b r.i.b))
::
++  vip                                               ::  very important put
  ~/  %vip
  |=  [a=pri k=k p=@ =v]                              ::  p must be higher
  ^-  pri                                             ::  than resident max
  ?~  a
    [%tip k p v]
  ?-    -.a
      %tip
    ?:  =(k k.a)
      [%tip k p v]
    (rule k.a p.a v.a [%tip k p v] ~)
  ::
      %bin
    ?:  (feud m.a k k.a)
      (rule k.a p.a v.a [%tip k p v] (fuse m.a l.a r.a))
    ?:  =(k k.a)
      ?:  (zero m.a k)
        (fuse m.a (raw l.a k p v) r.a)
      (fuse m.a l.a (raw r.a k p v))
    ?:  (zero m.a k)
      [%bin k.a p.a v.a m.a $(a l.a) r.a]
    [%bin k.a p.a v.a m.a l.a $(a r.a)]
  ==
::
++  bot                                               ::  min-priority view
  ~/   %bot
  |=  a=pri
  ^-  (unit (qual k @ v pri))
  ?~  a  ~
  %-  some
  ?-    -.a
      %tip  [k.a p.a v.a ~]
      %bin  [k.a p.a v.a (fuse m.a l.a r.a)]
  ==
::
++  cut                                               ::  delete min-pri
  ~/  %cut
  |=  a=pri
  ^-  pri
  =/  val  (bot a)
  ?~  val  a
  s.u.val
::
++  see                                               :: get with pri bump
  ~/  %see
  |=  [a=pri =k p=@]                                  :: p must be higher
  ^-  (pair (unit (pair @ v)) pri)                    :: than resident max
  ?~  a  [~ ~]
  ?-    -.a
      %tip
    ?.  =(k k.a)  [~ ~]
    [`[p.a v.a] [%tip k p v.a]]
  ::
      %bin
    ?:  (feud m.a k k.a)
      [~ a]
    ?:  =(k k.a)
      :-  `[p.a v.a]
      ?:  (zero m.a k)
        (fuse m.a (raw l.a k p v.a) r.a)
      (fuse m.a l.a (raw r.a k p v.a))
    ?:  (zero m.a k)
      =/  per  $(a l.a)
      [p.per [%bin k.a p.a v.a m.a q.per r.a]]
    =/  per  $(a r.a)
    [p.per [%bin k.a p.a v.a m.a l.a q.per]]
  ==
--
