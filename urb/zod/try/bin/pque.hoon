!:
::  /=main=/bin/app/hoon
::
=>  %=    .
        +
      =>  +
      !:
      |%
      ++  bqno  |*  [a=_,* b=_,*]                             ::  binary skew queno
                $:  r=@u                                      ::  rank/depth
                    k=a                                       ::  priority
                    n=b                                       ::  value
                    c=(bque a b)                              ::  children
                ==                                            ::
      ++  bque  |*  [a=_,* b=_,*]                             ::  binary skew que
                (list (bqno a b))                             ::
      ++  pr  !:                                              ::  priority queue
        |*  [key=$+(* *) val=$+(* *)]
        |=  cmp=$+([key key] ?)                               ::  lte=min, gte=max
        |%
        ++  link
          |=  [p=(bqno key val) q=(bqno key val)]             ::  link eq rank
          ^-  (bqno key val)
          ?>  =(r.p r.q)
            ?:  (cmp k.p k.q)
              [r=+(r.p) k=k.p n=n.p c=[i=q t=c.p]]
            [r=+(r.q) k=k.q n=n.q c=[i=p t=c.q]]
        ++  slink                                             ::  skew link
          |=  [p=(bqno key val) q=(bqno key val) r=(bqno key val)]
          ^-  (bqno key val)
          ~!  p
          ~!  q
          ~!  r
          ?:  &((cmp k.q k.p) (cmp k.q k.r))
            [r=+(r.q) k=k.q n=n.q c=[i=p t=[i=r t=c.q]]]
          ?:  &((cmp k.r k.p) (cmp k.r k.q))
            [r=+(r.r) k=k.r n=n.r c=[i=p t=[i=q t=c.r]]]
          [r=+(r.q) k=k.p n=n.p c=[i=q t=[i=r t=~]]]
        ++  ins                                               ::  internal ins op
          |=  [p=(bqno key val) q=(bque key val)]
          ^-  (bque key val)
          ?~  q  [p ~]
          ?>  (lte r.p r.i.q)
          ?:  (lth r.p r.i.q)
            [i=p t=q]
          $(p (link p i.q), q t.q)
        ++  uniq                                              ::  remove init dup
          |=  q=(bque key val)
          ?~  q  ~
          (ins i.q t.q)
        ++  meuq                                              ::  unique meld
          |=  [p=(bque key val) q=(bque key val)]
          ^-  (bque key val)
          ?~  p  q
          ?~  q  p
          ?:  (lth r.i.p r.i.q)
            [i.p $(p t.p)]
          ?:  (lth r.i.q r.i.p)
            [i.q $(q t.q)]
          (ins (link i.p i.q) $(p t.p, q t.q))
        ++  gmi                                               ::  getmin
          |=  q=(bque key val)
          ^-  [i=(bqno key val) t=(bque key val)]
          ?~  q  ~|(%fatal-gmi-empty !!)
          ?~  t.q  [i=i.q t=~]
          =+  r=$(q t.q)
          ?:  (cmp k.i.q k.i.r)
            [i=i.q t=t.q]
          [i=i.r t=[i.q t.r]]
        ++  spli                                              ::  split
          ::|*  p=(bque) q=(list ,[k=,_+<-.cmp n=*]) r=(bque)
          |=  [p=(bque key val) q=(list ,[k=key n=val]) r=(bque key val)]
          ^-  [t=(bque key val) x=(list ,[k=key n=val])]
          ?~  r
            [t=p x=q]
          ?:  =(0 r.i.r)
            $(q [[k=k.i.r n=n.i.r] q], r t.r)
          $(p [i.r p], r t.r)
        ++  insl                                              ::  insert list
          ::|*  [p=(list, [k=,_+<-.cmp n=*]) q=(bque)]
          |=  [p=(list ,[k=key n=val]) q=(bque key val)]
          ^-  (bque key val)
          ?~  p  q
          ?~  q  p
          $(p t.p, q (insert q i.p))
        ::
        ::                                                    ::  public interface
        ::                                                
        ++  insert                                            ::  real ins
          |=  [q=(bque key val) k=key n=val]
          ^-  (bque key val)
          ?~  q  [i=[r=0 k=k n=n c=~] t=~]
          ?~  t.q  [i=[r=0 k=k n=n c=~] t=q]
          ?:  =(r.i.q r.i.t.q)
            [i=(slink [r=0 k=k n=n c=~] i.q i.t.q) t=t.t.q]
          [i=[r=0 k=k n=n c=~] t=q]
        ++  meld                                              ::  concat
          |=  [p=(bque key val) q=(bque key val)]
          ^-  (bque key val)
          (meuq (uniq p) (uniq q))
        ++  peek                                              ::  find min/max
          |=  q=(bque key val)
          ^-  [k=key n=val]
          ?~  q  ~|(%empty-bque-peek !!)
          ?~  t.q  [k=k.i.q n=n.i.q]
          =+  m=$(q t.q)
          ?:  (cmp k.i.q k.m)  [k=k.i.q n=n.i.q]  m
        ++  pop                                               ::  delete min/max
          |=  q=(bque key val)
          ^-  [r=[k=key n=val] q=(bque key val)]
          ::^-  [q=(bque key val) r=[k=key n=val]]
          ?~  q  ~|(%empty-bque-pop !!)
          =+  m=(gmi q)
          =+  s=(spli ~ ~ c.i.m)
          [q=[k=k.i.m n=n.i.m] r=(insl x.s (meld t.m t.s))]
          ::[q=(insl x.s (meld t.m t.s)) r=[k=k.i.m n=n.i.m]]
        --
      --
    ==
|=  *
|=  ~
^-  bowl
:_  ~  :_  ~
:-  %$
!>
!:
=+  pri=((pr ,@ ,@) lte)
=+  pq=(insert.pri ~ 6 3)
=.  pq  (insert.pri pq 5 2)
=.  pq  (insert.pri pq 2 5)
=+  pq2=(insert.pri ~ 508 1.084)
=.  pq2  (insert.pri pq2 42 75)
=.  pq2  (insert.pri pq2 325 562)
=.  pq2  (insert.pri pq2 41 822)
=.  pq  (meld.pri pq pq2)
=^  r  pq  (pop.pri pq)
~&  r
=^  r  pq  (pop.pri pq)
~&  r
=^  r  pq  (pop.pri pq)
~&  r
=^  r  pq  (pop.pri pq)
~&  r
=^  r  pq  (pop.pri pq)
~&  r
=^  r  pq  (pop.pri pq)
~&  r
pq
