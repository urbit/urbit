~%  %up-lib  ..part  ~
|%
++  up
  ~/  %up
  |*  [k=mold v=mold]
  ::  molds and mold builders
  ::
  ~%  %core  +  ~
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
  +$  buc  [=v t=pro]                                   ::  bucket
  ::
  +$  pri                                               ::  psq
    $@  ~
    $%  [%bin k=@ p=@ v=buc m=@ l=pri r=pri]
        [%tip k=@ p=@ v=buc]
    ==
  ::  utilities
  ::
  ::  XX FIXME does this exist?
  ++  lex                                               ::  muggish lex order
    ~/  %lex
    |=  [p=@ k=k q=@ l=k]
    ^-  ?
    ?:  =(p q)
      (gor k l)
    (lth p q)
  ::  radix tree utilities
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
    ~/  %feud
    |=  [m=@ k=k l=k]
    ^-  ?
    =/  n  (mix (mix (dec m) 0x7fff.ffff) m)            ::  31-bit high mask
    !=((dis (mug k) n) (dis (mug l) n))
  ::
  ++  rule                                              ::  decide branch
    ~/  %rule                                           ::
    |=  [k=k p=@ v=buc a=pri b=pri]                     ::  a must be nonempty
    ^-  pri
    =/  l  +<.a
    =/  m  (peak k l)
    ?:  (zero m l)
      [%bin k p v m a b]
    [%bin k p v m b a]
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
    |=  [k=k p=@ v=buc m=@ l=pri r=pri]
    ^-  pri
    ?~  l
      ?~  r
        [%tip k p v]
      [%bin k p v m ~ r]
    [%bin k p v m l r]
  ::
  ++  wane                                              ::  shrink node (right)
    ~/  %wane
    |=  [k=k p=@ v=buc m=@ l=pri r=pri]
    ^-  pri
    ?~  r
      ?~  l
        [%tip k p v]
      [%bin k p v m l ~]
    [%bin k p v m l r]
  ::  collision resolution
  ::
  ++  qor
    ~%  %qor  ..qor  ~
    |%
    ::  loser tree internals
    ::
    ++  size                                            ::  loser tree size
      ~/  %size
      |=  t=ltree
      ^-  @
      ?~(t 0 s.t)
    ::
    ++  llos                                            ::  left loser node
      ~/  %llos
      |=  a=lnode
      ^-  ltree
      [%llos +((add (size l.a) (size r.a))) a]
    ::
    ++  rlos                                            ::  right loser node
      ~/  %rlos
      |=  a=lnode
      ^-  ltree
      [%rlos +((add (size l.a) (size r.a))) a]
    ::
    ++  lbal                                            ::  left-balance
      ~/  %lbal
      |=  a=lnode
      ^-  ltree
      =/  sl  (size l.a)
      =/  sr  (size r.a)
      ?:  (lth (add sl sr) 2)
        (llos a)
      ?:  (gth sr (mul 4 sl))
        ?>  ?=(^ r.a)
        ?:  (lth (size l.p.r.a) (size r.p.r.a))
          (llsin a)
        ?-  -.r.a
          %llos  (llsin n.a l.a m.a (lrsin p.r.a))
          %rlos  (llsin n.a l.a m.a (rrsin p.r.a))
        ==
      ?:  (gth sl (mul 4 sr))
        ?>  ?=(^ l.a)
        ?:  (gth (size l.p.l.a) (size r.p.l.a))
          (lrsin a)
        ?-  -.l.a
          %llos  (lrsin n.a (llsin p.l.a) m.a r.a)
          %rlos  (lrsin n.a (rlsin p.l.a) m.a r.a)
        ==
      (llos a)
    ::
    ++  rbal                                            ::  right-balance
      ~/  %rbal
      |=  a=lnode
      ^-  ltree
      =/  sl  (size l.a)
      =/  sr  (size r.a)
      ?:  (lth (add sl sr) 2)
        (rlos a)
      ?:  (gth sr (mul 4 sl))
        ?>  ?=(^ r.a)
        ?:  (lth (size l.p.r.a) (size r.p.r.a))
          (rlsin a)
        ?-  -.r.a
          %llos  (rlsin n.a l.a m.a (lrsin p.r.a))
          %rlos  (rlsin n.a l.a m.a (rrsin p.r.a))
        ==
      ?:  (gth sl (mul 4 sr))
        ?>  ?=(^ l.a)
        ?:  (gth (size l.p.l.a) (size r.p.l.a))
          (rrsin a)
        ?-  -.l.a
          %llos  (rrsin n.a (llsin p.l.a) m.a r.a)
          %rlos  (rrsin n.a (rlsin p.l.a) m.a r.a)
        ==
      (rlos a)
    ::
    ++  llsin                                           ::  left single-left
      ~/  %llsin
      |=  a=lnode
      ^-  ltree
      ?>  ?=(^ r.a)
      =/  b  p.r.a
      ?-  -.r.a
          %llos
        ?:  (lex p.n.a k.n.a p.n.b k.n.b)
          (llos n.a (rlos n.b l.a m.a l.b) m.b r.b)
        (llos n.b (llos n.a l.a m.a l.b) m.b r.b)
          %rlos
        (rlos n.b (llos n.a l.a m.a l.b) m.b r.b)
      ==
    ::
    ++  rlsin                                           ::  right single-right
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
        ?:  (lex p.n.a k.n.a p.n.b k.n.b)
          (rlos n.a l.b m.b (llos n.b r.b m.a r.a))
        (rlos n.b l.b m.b (rlos n.a r.b m.a r.a))
      ==
    ::
    ++  toy                                             ::  play
      ~/  %toy
      |=  [a=pro b=pro]
      ^-  pro
      ?~  a  b
      ?~  b  a
      ?:  (lex p.n.a k.n.a p.n.b k.n.b)
        [n.a (rbal n.b t.a m.a t.b) m.b]
      [n.b (lbal n.a t.a m.a t.b) m.b]
    ::
    ++  sec                                             ::  second best
      ~/  %sec
      |=  [t=ltree m=k]
      |-  ^-  pro
      ?~  t  ~
      ?-  -.t
        %llos  (toy [n.p.t l.p.t m.p.t] $(t r.p.t))
        %rlos  (toy $(t l.p.t, m m.p.t) [n.p.t r.p.t m])
      ==
    ::
    ++  sink                                            ::  make bucket
      ~/  %sink
      |=  [a=pro =k p=@ =v]
      ^-  (trel _k @ buc)
      =.  a  (put a k p v)
      =/  val  (bot a)
      ?>(?=(^ val) u.val)
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
    ::  XX FIXME remove
    ++  key                                             ::  list of keys
      |=  a=pro
      ^-  (list k)
      (turn (tap a) |=(=elem k.elem))
    ::  XX FIXME remove
    ++  tap                                             ::  convert to list
      |=  a=pro
      =|  b=(list elem)
      |-  ^+  b
      =/  tor  (see a)
      ?~  tor  b
      ?-  -.tor
        %sing  [n.tor b]
        %play  (weld $(a l.tor) $(a r.tor))
      ==
    ::
    ++  put                                             :: add [key pri val]
      ~/  %put
      |=  [a=pro =k p=@ =v]
      |-  ^-  pro
      ?~  a
        [[k p v] ~ k]
      ?:  ?=(~ t.a)
        ?:  =(k m.a)
          [[k p v] ~ k]
        ?:  (gor k m.a)
          (toy [[k p v] ~ k] [n.a ~ k.n.a])
        (toy [n.a ~ k.n.a] [[k p v] ~ k])
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
    ::  FIXME remove
    ++  gas                                             ::  concatenate
      ~/  %gas
      |=  [a=pro b=(list (trel k @ v))]
      |-  ^-  pro
      ?~  b  a
      $(b t.b, a (put a p.i.b q.i.b r.i.b))
    ::
    ++  has                                             ::  contains
      ~/  %has
      |=  [a=pro =k]
      !=(~ (get a k))
    ::  FIXME remove
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
        ?>  ?=(^ l.tor)
        ?:  |(=(k m.l.tor) (gor k m.l.tor))
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
        [n.a ~ k.n.a]
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
      ^-  (unit (trel k @ buc))
      ?~  a  ~
      `[k.n.a p.n.a v.n.a (sec t.a m.a)]
    --
  ::  radix tree logic
  ::
  ++  qat
    ~%  %qat  ..qat  ~
    |%
    ::  XX FIXME remove
    ::
    ++  get                                             ::  lookup
      ~/  %get
      |=  [a=pri =k]
      ^-  (unit (pair @ buc))
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
    ++  del                                             ::  delete at key k
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
        ?:  =((mug k) (mug k.a))                        ::  XX FIXME check
          (fuse m.a l.a r.a)
        ?:  (zero m.a k)
          (funk k.a p.a v.a m.a $(a l.a) r.a)
        (wane k.a p.a v.a m.a l.a $(a r.a))
      ==
    ::
    ++  dew                                             ::  delete view
      ~/  %dew
      |=  [a=pri =k]
      ^-  (unit (qual _k @ buc pri))
      =-  ?~  p.-  ~
          `[p.u.p.- q.u.p.- r.u.p.- q.-]
      |-  ^-  (pair (unit (trel _k @ buc)) pri)
      ?~  a  [~ ~]
      ?-    -.a
          %tip
        ?:  =((mug k) (mug k.a))
          [`[k.a p.a v.a] ~]
        [~ a]
      ::
          %bin
        ?:  (feud m.a k k.a)
          [~ a]
        ?:  =((mug k) (mug k.a))
          :-  `[k.a p.a v.a]
          (fuse m.a l.a r.a)
        ?:  (zero m.a k)
          =/  mud  $(a l.a)
          :-  p.mud
          (funk k.a p.a v.a m.a q.mud r.a)
        =/  mud  $(a r.a)
        :-  p.mud
        (wane k.a p.a v.a m.a l.a q.mud)
      ==
    ::
    ++  raw                                             ::  raw insert
      ~/  %raw                                          ::
      |=  [a=pri k=k p=@ v=buc]                         ::  k must not exist in
      ^-  pri                                           ::  queue
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
    ++  put                                             ::  insert
      ~/  %put
      |=  [a=pri k=k p=@ v=buc]
      ^-  pri
      (raw (del a k) k p v)
    ::
    ++  gas                                             ::  concatenate
      ~/  %gas
      |=  [a=pri b=(list (trel k @ buc))]
      |-  ^-  pri
      ?~  b  a
      $(b t.b, a (put a p.i.b q.i.b r.i.b))
    ::
    ++  bot                                             ::  min-priority view
      ~/   %bot
      |=  a=pri
      ^-  (unit (qual k @ buc pri))
      ?~  a  ~
      %-  some
      ?-    -.a
          %tip  [k.a p.a v.a ~]
          %bin  [k.a p.a v.a (fuse m.a l.a r.a)]
      ==
    ::
    ++  cut                                             ::  delete min-pri
      ~/  %cut
      |=  a=pri
      ^-  pri
      =/  val  (bot a)
      ?~  val  a
      s.u.val
    --
  ::  pri logic
  ::
  ++  put                                               ::  add [key pri val]
    ~/  %put
    |=  [a=pri =k p=@ =v]
    ^-  pri
    =/  ped  (dew:qat a k)
    ?~  ped
      (raw:qat a k p [v ~])
    =/  [l=_k q=@ =buc =pri]  u.ped
    =;  bee=(trel _k @ _buc)
      (raw:qat pri p.bee q.bee r.bee)
    ?:  =((mug k) (mug l))
      (sink:qor t.buc k p v)
    ?:  |((lth q p) &(=(p q) (gor l k)))
      :^  l  q  v.buc
      (put:qor t.buc k p v)
    :^  k  p  v
    ?:  (has:qor t.buc k)
      (put:qor (del:qor t.buc k) l q v.buc)
    (put:qor t.buc l q v.buc)
  ::  FIXME remove
  ::
  ++  gas                                               ::  concatenate
    ~/  %gas
    |=  [a=pri b=(list (trel k @ v))]
    |-  ^-  pri
    ?~  b  a
    $(b t.b, a (put a p.i.b q.i.b r.i.b))
  ::
  ++  cut                                               ::  delete min-pri
    ~/  %cut
    |=  a=pri
    ^-  pri
    ?~  a  ~
    ?-    -.a
        %tip
      =/  hol  (bot:qor t.v.a)
      ?~  hol  ~
      [%tip u.hol]
    ::
        %bin
      =/  hol  (bot:qor t.v.a)
      ?~  hol  (fuse m.a l.a r.a)
      (raw:qat (fuse m.a l.a r.a) u.hol)
    ==
  ::
  ++  vip                                             ::  very important put
    ~/  %vip                                          ::
    |=  [a=pri k=k p=@ =v]                            ::  p must be higher
    ^-  pri                                           ::  than resident max
    ?~  a
      [%tip k p v ~]
    ?-    -.a
        %tip
      ?:  =((mug k) (mug k.a))
        [%tip k.a p.a v.v.a (put:qor t.v.a k p v)]
      (rule k.a p.a v.a [%tip k p v ~] ~)
    ::
        %bin
      ?:  (feud m.a k k.a)
        (rule k.a p.a v.a [%tip k p v ~] (fuse m.a l.a r.a))
      ?:  =((mug k) (mug k.a))
        ?:  (zero m.a k)
          (fuse m.a (raw:qat l.a k.a p.a v.v.a (put:qor t.v.a k p v)) r.a)
        (fuse m.a l.a (raw:qat r.a k.a p.a v.v.a (put:qor t.v.a k p v)))
      ?:  (zero m.a k)
        [%bin k.a p.a v.a m.a $(a l.a) r.a]
      [%bin k.a p.a v.a m.a l.a $(a r.a)]
    ==
  ::
  ++  see                                             ::  get with pri bump
    ~/  %see                                          ::
    |=  [a=pri =k p=@]                                ::  p must be higher
    |^  ^-  (pair (unit (pair @ v)) pri)              ::  than resident max
    ?~  a  [~ ~]
    ?-    -.a
        %tip
      ?.  =((mug k) (mug k.a))
        [~ a]
      =/  mud  (stir k.a p.a v.a)
      :-  p.mud
      [%tip q.mud r.mud s.mud]
    ::
        %bin
      ?:  (feud m.a k k.a)
        ~&  [%feud m=m.a k=k l=k.a]
        [~ a]
      ?:  =((mug k) (mug k.a))
        =/  mud  (stir k.a p.a v.a)
        :-  p.mud
        ?:  (zero m.a k)
          (fuse m.a (raw:qat l.a q.mud r.mud s.mud) r.a)
        (fuse m.a l.a (raw:qat r.a q.mud r.mud s.mud))
      ?:  (zero m.a k)
        =/  val  $(a l.a)
        :-  p.val
        [%bin k.a p.a v.a m.a q.val r.a]
      =/  val  $(a r.a)
      :-  p.val
      [%bin k.a p.a v.a m.a l.a q.val]
    ==
    ::
    ++  stir
      |=  [l=_k q=@ =buc]
      ^-  (qual (unit (pair @ v)) _k @ _buc)
      ?:  =(k l)
        :-  `[q v.buc]
        (sink:qor t.buc k p v.buc)
      =/  val  (get:qor t.buc k)
      ?~  val  [~ k q buc]
      :^    val
          l
        q
      [v.buc (put:qor t.buc k p q.u.val)]
    --
  --
--

