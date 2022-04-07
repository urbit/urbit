~%  %up-qor  ..part  ~
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
::
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
::  api
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
::
++  apt                                             ::  check correctness
  ~/  %apt
  |=  a=pro
  |^  ^-  ?
  ?.  uni
    ~&  %apt-uni
    uni
  ?.  hep
    ~&  %apt-hep
    hep
  ?.  bin
    ~&  %apt-bin
    bin
  ?.  ann
    ~&  %apt-ann
    ann
  &
  ::
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
  ++  key                                             ::  list of keys
    |=  a=pro
    ^-  (list k)
    (turn (tap a) |=(=elem k.elem))
  ::
  ++  lex                                           ::  muggish lex order
    |=  [p=@ k=k q=@ l=k]
    ^-  ?
    ?:  =(p q)
      (gor k l)
    (lth p q)
  ::
  ++  uni                                           ::  has unique keys
    =/  l  (sort (key a) gor)
    =|  a=(unit k)
    |-  ^-  ?
    ?~  l  &
    ?:  =(a (some i.l))  |
    $(l t.l, a (some i.l))
  ::
  ++  hep                                           ::  min-heap prop
    ?~  a  &
    |-  ^-  ?
    ?~  t.a  &
    ?-  -.t.a
        %llos
      =/  b=lnode  p.t.a
      ?&  (lex p.n.a k.n.a p.n.b k.n.b)
          $(k.n.a k.n.b, p.n.a p.n.b, t.a l.b)
          $(t.a r.b)
      ==
        %rlos
      =/  b=lnode  p.t.a
      ?&  (lex p.n.a k.n.a p.n.b k.n.b)
          $(t.a l.b)
          $(k.n.a k.n.b, p.n.a p.n.b, t.a r.b)
      ==
    ==
  ::
  ++  bin                                           ::  binary search tree
    |-  ^-  ?
    =/  tor  (see a)
    ?~  tor  &
    ?-  -.tor
        %sing  &
        %play
      =/  k  (top l.tor)
      ?&  (levy (key l.tor) |=(* (gor +< k)))
          (levy (key r.tor) |=(* |(=(k +<) !(gor +< k))))
          $(a l.tor)
          $(a r.tor)
      ==
    ==
  ::
  ++  ann                                           ::  correct annotations
    =/  calc
      |=  t=ltree
      ^-  @
      ?~  t  0
      ?-  -.t
        %llos  +((add $(t l.p.t) $(t r.p.t)))
        %rlos  +((add $(t l.p.t) $(t r.p.t)))
      ==
    ?~  a  &
    |-  ^-  ?
    ?~  t.a  =(0 (calc t.a))
    ?&  =(s.t.a (calc t.a))
        $(t.a l.p.t.a)
        $(t.a r.p.t.a)
    ==
  --
--
