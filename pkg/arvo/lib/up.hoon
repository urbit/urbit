::  XX fix jet hints
::
=*  k  @
=*  v  @
~%  %up-lib  ..part  ~
::  molds and mold builders
::
|%
+$  elem  [=k p=@ =v]                                 ::  pri element
::
+$  lnode  [n=elem l=ltree m=k r=ltree]               ::  loser tree node
::
+$  ltree                                             ::  loser tree
  $@  ~
  $%  [%llos s=@ p=lnode]
      [%rlos s=@ p=lnode]
  ==
::
+$  torn                                              ::  tournament view
  $@  ~
  $%  [%sing n=elem]
      [%play l=pro r=pro]
  ==
::
+$  pro  $@(~ [n=elem t=ltree m=k])                   ::  internal psq
::
+$  buc  [=k =v t=pro]                                ::  bucket
::
+$  pri                                               ::  psq
  $@  ~
  $%  [%bin k=@ p=@ v=buc m=@ l=pri r=pri]
      [%tip k=@ p=@ v=buc]
  ==
::  internals
::
++  mega  4                                           ::  balancing factor
::
++  size                                              ::  queue size
  ~/  %size
  |=  t=ltree
  ^-  @
  ?~(t 0 s.t)
::
++  one                                               ::  construct
  ~/  %one
  |=  =elem
  ^-  pro
  [elem ~ k.elem]
::
++  llos                                              ::  left loser
  ~/  %llos
  |=  a=lnode
  ^-  ltree
  [%llos +((add (size l.a) (size r.a))) a]
::
++  rlos                                              ::  right loser
  ~/  %rlos
  |=  a=lnode
  ^-  ltree
  [%rlos +((add (size l.a) (size r.a))) a]
::
++  lbal                                              ::  left balance
  ~/  %lbal
  |=  a=lnode
  ^-  ltree
  ?:  (lth (add (size l.a) (size r.a)) 2)
    (llos a)
  ?:  (gth (size r.a) (mul mega (size l.a)))
    (llbal a)
  ?:  (gth (size l.a) (mul mega (size r.a)))
    (lrbal a)
  (llos a)
::
++  rbal                                              ::  right balance
  ~/  %rbal
  |=  a=lnode
  ^-  ltree
  ?:  (lth (add (size l.a) (size r.a)) 2)
    (rlos a)
  ?:  (gth (size r.a) (mul mega (size l.a)))
    (rlbal a)
  ?:  (gth (size l.a) (mul mega (size r.a)))
    (rrbal a)
  (rlos a)
::
++  llbal                                             ::  left balance-left
  ~/  %llbal
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  ?:  (lth (size l.p.r.a) (size r.p.r.a))
    (llsin a)
  (lldub a)
::
++  lrbal                                             ::  left balance-right
  ~/  %lrbal
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  ?:  (gth (size l.p.l.a) (size r.p.l.a))
    (lrsin a)
  (lrdub a)
::
++  rlbal
  ~/  %rlbal
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  ?:  (lth (size l.p.r.a) (size r.p.r.a))
    (rlsin a)
  (rldub a)
::
++  rrbal
  ~/  %rrbal
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  ?:  (gth (size l.p.l.a) (size r.p.l.a))
    (rrsin a)
  (rrdub a)
::
++  llsin                                             ::  left single-left
  ~/  %llsin
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  =/  b  p.r.a
  ?-  -.r.a
      %llos
    ?:  (win [p.n.a k.n.a] [p.n.b k.n.b])
      (llos n.a (rlos n.b l.a m.a l.b) m.b r.b)
    (llos n.b (llos n.a l.a m.a l.b) m.b r.b)
      %rlos
    (rlos n.b (llos n.a l.a m.a l.b) m.b r.b)
  ==
::
++  rlsin
  ~/  %rlsin
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  =/  b  p.r.a
  ?-  -.r.a
    %llos  (rlos n.a (rlos n.b l.a m.a l.b) m.b r.b)
    %rlos  (rlos n.b (rlos n.a l.a m.a l.b) m.b r.b)
  ==
::
++  lrsin
  ~/  %lrsin
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  =/  b  p.l.a
  ?-  -.l.a
    %llos  (llos n.b l.b m.b (llos n.a r.b m.a r.a))
    %rlos  (llos n.a l.b m.b (llos n.b r.b m.a r.a))
  ==
::
++  rrsin
  ~/  %rrsin
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  =/  b  p.l.a
  ?-  -.l.a
      %llos
    (llos n.b l.b m.b (rlos n.a r.b m.a r.a))
      %rlos
    ?:  (win [p.n.a k.n.a] [p.n.b k.n.b])
      (rlos n.a l.b m.b (llos n.b r.b m.a r.a))
    (rlos n.b l.b m.b (rlos n.a r.b m.a r.a))
  ==
::
++  lldub                                             ::  left double-left
  ~/  %lldub
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  ?-  -.r.a
    %llos  (llsin n.a l.a m.a (lrsin p.r.a))
    %rlos  (llsin n.a l.a m.a (rrsin p.r.a))
  ==
::
++  lrdub
  ~/  %lrdub
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  ?-  -.l.a
    %llos  (lrsin n.a (llsin p.l.a) m.a r.a)
    %rlos  (lrsin n.a (rlsin p.l.a) m.a r.a)
  ==
::
++  rldub
  ~/  %rldub
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ r.a)
  ?-  -.r.a
    %llos  (rlsin n.a l.a m.a (lrsin p.r.a))
    %rlos  (rlsin n.a l.a m.a (rrsin p.r.a))
  ==
::
++  rrdub
  ~/  %rrdub
  |=  a=lnode
  ^-  ltree
  ?>  ?=(^ l.a)
  ?-  -.l.a
    %llos  (rrsin n.a (llsin p.l.a) m.a r.a)
    %rlos  (rrsin n.a (rlsin p.l.a) m.a r.a)
  ==
::
++  toy                                               ::  play
  ~/  %toy
  |=  [a=pro b=pro]
  ^-  pro
  ?~  a  b
  ?~  b  a
  ?:  (win [p.n.a k.n.a] [p.n.b k.n.b])
    [n.a (rbal n.b t.a m.a t.b) m.b]
  [n.b (lbal n.a t.a m.a t.b) m.b]
::
++  sec                                               ::  second best
  ~/  %sec
  |=  [t=ltree m=k]
  |-  ^-  pro
  ?~  t  ~
  ?-  -.t
    %llos  (toy [n.p.t l.p.t m.p.t] $(t r.p.t))
    %rlos  (toy $(t l.p.t, m m.p.t) [n.p.t r.p.t m])
  ==
::
++  win                                               ::  compare
  ~/  %win
  |=  [[p=@ q=*] [r=@ s=*]]
  ?|  (lth p r)
      ?&(=(p r) (gor q s))
  ==
::  radix tree utilities
::
++  zero                                              ::  zero
  ~/  %zero
  |=  [k=@ m=@]
  ^-  ?
  =(0 (dis k m))
::
++  gone                                              ::  nomatch
  ~/  %gone
  |=  [k=@ l=@ m=@]
  ^-  ?
  =/  n  (mask m)
  !=((dis k n) (dis l n))
::
++  mask                                              ::  maskw
  ~/  %mask
  |=  a=@
  ^-  @
  (mix (not 5 1 (dec a)) a)
::
++  part                                              ::  branch mask
  ~/  %part
  |=  [k=@ l=@]
  ^-  @
  (high (mix k l))
::
++  high                                              ::  highest bitmask
  ~/  %high
  |=  a=@
  ^-  @
  (rsh 0 (bex (xeb a)))
::
++  lex                                               ::  lexicographic order
  ~/  %lex                                            ::  [atom atom]
  |=  [[p=@ k=@] [q=@ l=@]]
  ^-  ?
  ?:  =(p q)
    (lth k l)
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
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m ~ r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ::
      %bin
    ?~  r  l
    :-  %bin
    ?-    -.r
        %tip
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l ~]
    ::
        %bin
      ?:  (lex [p.l k.l] [p.r k.r])
        [k.l p.l v.l m $(m m.l, l l.l, r r.l) r]
      [k.r p.r v.r m l $(m m.r, l l.r, r r.r)]
    ==
  ==
::
++  funk                                              ::  bin shrink-left
  ~/  %funk
  |=  [k=@ p=@ v=buc m=@ l=pri r=pri]
  ^-  pri
  ?~  l
    ?~  r  [%tip k p v]
    :-  %bin
    ?-  -.r
      %tip  [k p v m ~ r]
      %bin  [k p v m ~ r]
    ==
  [%bin k p v m l r]
::
++  wane                                              ::  bin shrink-right
  ~/  %wane
  |=  [k=@ p=@ v=buc m=@ l=pri r=pri]
  ^-  pri
  ?~  r
    ?~  l  [%tip k p v]
    :-  %bin
    ?-  -.l
      %tip  [k p v m l ~]
      %bin  [k p v m l ~]
    ==
  [%bin k p v m l r]
::  collision resolution
::
++  qor
  ~%  %qor  ..qor  ~
  |%
  ++  top                                             ::  maximum key
    ~/  %top
    |=  a=pro
    ?~  a  !!
    m.a
  ::
  ++  see                                             ::  tournament view
    ~/  %see
    |=  a=pro
    ^-  torn
    ?~  a  ~
    ?~  t.a
      [%sing n.a]
    ?-  -.t.a
      %llos  [%play [n.p.t.a l.p.t.a m.p.t.a] [n.a r.p.t.a m.a]]
      %rlos  [%play [n.a l.p.t.a m.p.t.a] [n.p.t.a r.p.t.a m.a]]
    ==
  ::
  ++  put                                             :: add [key pri val]
    ~/  %put
    |=  [a=pro =k p=@ =v]
    |-  ^-  pro
    ?~  a
      (one k p v)
    ?:  ?=(~ t.a)
      ?:  =(k m.a)
        (one k p v)
      ?:  (gor k m.a)
        (toy (one k p v) (one n.a))
      (toy (one n.a) (one k p v))
    ?-    -.t.a
        %rlos
      =/  b=lnode  p.t.a
      ?:  |(=(k m.b) (gor k m.b))
        (toy $(a [n.a l.b m.b]) [n.b r.b m.a])
      (toy [n.a l.b m.b] $(a [n.b r.b m.a]))
    ::
        %llos
      =/  b=lnode  p.t.a
      ?:  |(=(k m.b) (gor k m.b))
        (toy $(a [n.b l.b m.b]) [n.a r.b m.a])
      (toy [n.b l.b m.b] $(a [n.a r.b m.a]))
    ==
  ::
  ++  has                                             ::  contains
    ~/  %has
    |=  [a=pro =k]
    !=(~ (get a k))
  ::
  ++  get                                             ::  lookup
    ~/  %get
    |=  [a=pro =k]
    |-  ^-  (unit (pair @ v))
    ?~  a  ~
    =/  tor=torn  (see a)
    ?~  tor  ~
    ?-    -.tor
        %sing
      ?.  =(k k.n.tor)  ~
      `[p.n.tor v.n.tor]
    ::
        %play
      ?:  |(=(k (top l.tor)) (gor k (top l.tor)))
        $(a l.tor)
      $(a r.tor)
    ==
  ::
  ++  del                                             ::  delete at key k
    ~/  %del
    |=  [a=pro =k]
    |-  ^-  pro
    ?~  a  ~
    ?:  ?=(~ t.a)
      ?:  =(k k.n.a)  ~
      (one n.a)
    ?-    -.t.a
        %rlos
      =/  b=lnode  p.t.a
      ?:  |(=(k m.b) (gor k m.b))
        (toy $(a [n.a l.b m.b]) [n.b r.b m.a])
      (toy [n.a l.b m.b] $(a [n.b r.b m.a]))
    ::
        %llos
      =/  b=lnode  p.t.a
      ?:  |(=(k m.b) (gor k m.b))
        (toy $(a [n.b l.b m.b]) [n.a r.b m.a])
      (toy [n.b l.b m.b] $(a [n.a r.b m.a]))
    ==
  ::
  ++  bot                                             ::  lowest-pro view
    ~/  %bot
    |=  a=pro
    ^-  (unit (pair elem pro))
    ?~  a  ~
    `[n.a (sec t.a m.a)]
  --
::  radix tree logic
::
++  qat
  ~%  %qat  ..qat  ~
  |%
  ::  bucket helpers
  ::
  ++  pour                                            ::  to bucket
    ~/  %pour
    |=  a=pro
    ^-  (unit (pair @ buc))
    =/  val  (bot:qor a)
    ?~  val  ~
    `[p.p.u.val k.p.u.val v.p.u.val q.u.val]
  ::
  ++  make                                            ::  make bucket
    ~/  %make
    |=  [=k p=@ =v a=pro]
    ^-  (pair @ buc)
    =.  a  (put:qor a k p v)
    =/  val  (pour a)
    ?>  ?=(^ val)
    u.val
  ::
  ++  put                                             ::  add [key pri val]
    ~/  %put
    |=  [a=pri k=@ p=@ v=buc]
    ^-  pri
    (raw (del a k) k p v)
  ::
  ++  tie                                             ::  link
    ~/  %tie
    |=  [k=@ p=@ v=buc l=@ a=pri b=pri]
    ^-  pri
    =/  m  (part k l)
    :-  %bin
    ?:  (zero m l)
      [k p v m a b]
    [k p v m b a]
  ::
  ++  del                                             ::  delete at key k
    ~/  %del
    |=  [a=pri k=@]
    |-  ^-  pri
    ?~  a  ~
    ?-    -.a
        %tip
      ?:  =(k k.a)  ~
      a
    ::
        %bin
      ?:  (gone k k.a m.a)
        a
      ?:  =(k k.a)
        (fuse m.a l.a r.a)
      ?:  (zero k m.a)
        (funk k.a p.a v.a m.a $(a l.a) r.a)
      (wane k.a p.a v.a m.a l.a $(a r.a))
    ==
  ::
  ++  jib                                             ::  update at min-pri
    ~/  %jib
    |=  a=pri
    |^  ^-  (pair (unit (trel k @ v)) pri)
    ?~  a
      =/  bee  (help ~)
      :-  p.bee
      ?~  q.bee  ~
      [%tip p.u.q.bee q.u.q.bee r.u.q.bee]
    ?-    -.a
        %tip
      =/  bee  (help `[k.a p.a v.a])
      :-  p.bee
      ?~  q.bee  ~
      [%tip u.q.bee]
    ::
        %bin
      =/  bee  (help `[k.a p.a v.a])
      :-  p.bee
      ?~  q.bee  (fuse m.a l.a r.a)
      =/  t=(trel @ @ buc)  u.q.bee
      ?:  !=(k.a p.t)
        (put (fuse m.a l.a r.a) p.t q.t r.t)
      ?:  (lte q.t p.a)
        [%bin k.a q.t r.t m.a l.a r.a]
      (raw (fuse m.a l.a r.a) k.a q.t r.t)
    ==
    ::
    ++  help
      |=  b=(unit (trel @ @ buc))
      ^-  (pair (unit (trel k @ v)) (unit (trel @ @ buc)))
      ?~  b  [~ ~]
      =/  =buc  r.u.b
      :-  `[k.buc q.u.b v.buc]
      =/  val  (bot:qor t.buc)
      ?~  val  ~
      `[p.u.b p.p.u.val k.p.u.val v.p.u.val q.u.val]
    --
  ::
  ++  pet                                             ::  delete view
    ~/  %pet
    |=  [a=pri k=@]
    |^  ^-  (unit (trel @ buc pri))
    =/  med  (omit a)
    ?~  q.med  ~
    `[p.u.q.med q.u.q.med p.med]
    ::
    ++  omit
      |=  b=pri
      ^-  (pair pri (unit (pair @ buc)))
      ?~  b  [~ ~]
      ?-    -.b
          %tip
        ?:  =(k k.b)
          [~ `[p.b v.b]]
        [b ~]
      ::
          %bin
        ?:  (gone k k.b m.b)
          [b ~]
        ?:  =(k k.b)
          :-  (fuse m.b l.b r.b)
          `[p.b v.b]
        ?:  (zero k m.b)
          =/  med  (omit l.b)
          :-  (funk k.b p.b v.b m.b p.med r.b)
          q.med
        =/  med  (omit r.b)
        :-  (wane k.b p.b v.b m.b l.b p.med)
        q.med
      ==
    --
  ::
  ++  raw                                             ::  put fresh (unsafe)
    ~/  %raw
    |=  [a=pri k=@ p=@ v=buc]                         ::  k must not exist in
    |-  ^-  pri                                       ::  queue
    ?~  a
      [%tip k p v]
    ?-    -.a
        %tip
      ?:  (lex [p k] [p.a k.a])
        (tie k p v k.a a ~)
      (tie k.a p.a v.a k [%tip k p v] ~)
    ::
        %bin
      ?:  (gone k k.a m.a)
        ?:  (lex [p k] [p.a k.a])
          (tie k p v k.a a ~)
        (tie k.a p.a v.a k [%tip k p v] (fuse m.a l.a r.a))
      :-  %bin
      ?:  (lex [p k] [p.a k.a])
        ?:  (zero k.a m.a)
          [k p v m.a $(a l.a, k k.a, p p.a, v v.a) r.a]
        [k p v m.a l.a $(a r.a, k k.a, p p.a, v v.a)]
      ?:  (zero k m.a)
        [k.a p.a v.a m.a $(a l.a) r.a]
      [k.a p.a v.a m.a l.a $(a r.a)]
    ==
  ::
  ++  gun                                             ::  vip view (unsafe)
    ~/  %gun
    |=  [a=pri =k p=@ =v]                             ::  p must be higher
    |^                                                ::  than resident max
    |-  ^-  (pair (unit (pair @ buc)) pri)
    =/  h  (mug k)
    =/  b  [k v ~]
    ?~  a  [~ [%tip h p b]]
    ?-    -.a
        %tip
      ?:  =(h k.a)
        =/  val  (help p.a v.a)
        :-  `[p.a v.a]
        [%tip h p.val q.val]
      :-  ~
      (tie k.a p.a v.a h [%tip h p b] ~)
    ::
        %bin
      ?:  (gone h k.a m.a)
        =/  tee  (fuse m.a l.a r.a)
        :-  ~
        (tie k.a p.a v.a h [%tip h p b] tee)
      ?:  =(h k.a)
        =/  val  (help p.a v.a)
        :-  `[p.a v.a]
        ?:  (zero h m.a)
          (fuse m.a (raw l.a h p.val q.val) r.a)
        (fuse m.a l.a (raw r.a h p.val q.val))
      ?:  (zero h m.a)
        =/  val  $(a l.a)
        :-  p.val
        [%bin k.a p.a v.a m.a q.val r.a]
      =/  val  $(a r.a)
      :-  p.val
      [%bin k.a p.a v.a m.a l.a q.val]
    ==
    ::
    ++  help
      |=  [bp=@ bb=buc]
      ^-  (pair @ buc)
      ?:  =(k k.bb)
        (make k p v t.bb)
      :-  bp
      [k.bb v.bb (put:qor t.bb k p v)]
    --
  ::
  ++  see                                             ::  get hi-pri (unsafe)
    ~/  %see
    |=  [a=pri =k p=@]                                ::  lookup, then pri
    =/  h  (mug k)                                    ::  bump, with p higher
    |^                                                ::  than resident max
    |-  ^-  (pair (unit (pair @ v)) pri)
    ?~  a  [~ ~]
    ?-    -.a
        %tip
      ?:  =(h k.a)
        =/  val  (help p.a v.a)
        :-  p.val
        [%tip h q.val r.val]
      [~ a]
    ::
        %bin
      ?:  (gone h k.a m.a)
        [~ a]
      ?:  =(h k.a)
        =/  val  (help p.a v.a)
        :-  p.val
        ?:  (zero h m.a)
          (fuse m.a (raw l.a h q.val r.val) r.a)
        (fuse m.a l.a (raw r.a h q.val r.val))
      ?:  (zero h m.a)
        =/  val  $(a l.a)
        :-  p.val
        [%bin k.a p.a v.a m.a q.val r.a]
      =/  val  $(a r.a)
      :-  p.val
      [%bin k.a p.a v.a m.a l.a q.val]
    ==
    ::
    ++  help
      |=  [bp=@ bb=buc]
      ^-  (trel (unit (pair @ v)) @ buc)
      ?:  =(k k.bb)
        :-  `[bp v.bb]
        (make k p v.bb t.bb)
      =/  val  (get:qor t.bb k)
      ?~  val  [~ bp bb]
      :+  val
        bp
      [k.bb v.bb (put:qor t.bb k p q.u.val)]
    --
  --
::  pri logic
::
++  put                                               ::  add [key pri val]
  ~/  %put
  |=  [a=pri =k p=@ =v]
  ^-  pri
  =/  h  (mug k)
  |^  ^-  pri
  =/  ped  (pet:qat a h)
  =?  a  ?=(^ ped)
    r.u.ped
  =/  pav
    ?~  ped
      ~
    `[p.u.ped q.u.ped]
  =/  bee  (ins pav)
  ?~  bee  a
  (raw:qat a h p.u.bee q.u.bee)
  ::
  ++  ins
    |=  b=(unit (pair @ buc))
    ^-  (unit (pair @ buc))
    %-  some
    ?~  b  [p k v ~]
    =/  =buc  q.u.b
    ?:  =(k k.buc)
      (make:qat k p v t.buc)
    ?:  |((lth p.u.b p) &(=(p p.u.b) (gor k.buc k)))
      =/  val  (put:qor t.buc k p v)
      [p.u.b k.buc v.buc val]
    =/  val
      ?:  (has:qor t.buc k)
        (put:qor (del:qor t.buc k) k.buc p.u.b v.buc)
      (put:qor t.buc k.buc p.u.b v.buc)
    [p k v val]
  --
::
++  gas                                               ::  concatenate
  ~/  %gas
  |=  [a=pri b=(list (trel k @ v))]
  |-  ^-  pri
  ?~  b  a
  $(b t.b, a (put a p.i.b q.i.b r.i.b))
::
++  bot                                               ::  lowest-pri view
  ~/  %bot
  |=  a=pri
  ^-  (unit (qual k @ v pri))
  =/  val=(pair (unit (trel k @ v)) pri)
    (jib:qat a)
  ?~  p.val  ~
  `[p.u.p.val q.u.p.val r.u.p.val q.val]
::
++  cut                                               ::  delete lowest-pri
  ~/  %cut
  |=  a=pri
  ^-  pri
  =/  val  (bot a)
  ?~  val  a
  s.u.val
::
++  gun                                               ::  vip view (unsafe)
  ~/  %gun
  |=  [a=pri =k p=@ =v]                               ::  p must be higher
  ^-  (pair (unit (pair @ _v)) pri)                   ::  than resident max
  =/  big  (gun:qat a k p v)
  :_  q.big
  ?~  p.big  ~
  =/  bb  q.u.p.big
  ?:  =(k k.bb)
    `[p.u.p.big v.bb]
  (get:qor t.bb k)
::
++  see                                               ::  get hi-pri (unsafe)
  ~/  %see
  |=  [a=pri =k p=@]                                  ::  lookup, then pri
  ^-  (pair (unit (pair @ v)) pri)                    ::  bump, with p higher
  (see:qat a k p)                                     ::  than resident max
--
