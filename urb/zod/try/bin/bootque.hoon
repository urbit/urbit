!:
::  /=main=/bin/app/hoon
::
=>  %=    .
        +
      =>  +
      =>  ^/===/bin/pque
      |%
      ::  efficient priority queue
      ::  possibly empty
      ++  pque  |*  [a=_,* b=_,*]
                (unit (rque a b))
      ::  internal - nonempty pque
      ++  rque  |*  [a=_,* b=_,*]
                $:  k=a
                    n=b
                    q=(bque a (rque a b))
                ==
      ::  maximally optimal priority queue
      ::  O(1) insert, meld, peek
      ::  O(log n) pop
      :: 
      ::  lte -> min priority queue
      ::  gte -> max priority queue
      ::
      ::  bootstrapped off of ++pr
      ::
      ::  to create, use something like
      ::  ~zod/try=> ((qu ,@ ,@) lte)
      ::
      ::  example operations
      ::  
      ::  =+  pri=((qu ,@ ,@) lte)
      ::  =+  q=~
      ::  =.  q  (insert.pri q 3 2)
      ::  =^  r  q  (pop.pri q)
      ++  qu  !:
        |*  [key=$+(* *) val=$+(* *)]
        |=  cmp=$+([key key] ?)
        =+  bt=((pr key (rque key val)) cmp)
        |%
        ++  insert
          |=  [q=(pque key val) k=key n=val]
          ^-  (pque key val)
          (meld [~ [k=k n=n q=~]] q)
        ++  meld
          |=  [q=(pque key val) p=(pque key val)]
          ^-  (pque key val)
          ?~  p  q
          ?~  q  p
          ?:  (cmp k.u.p k.u.q)
            [~ [k=k.u.p n=n.u.p q=(insert.bt q.u.p [k=k.u.q n=[k.u.q n=n.u.q q=q.u.q]])]]
          [~ [k=k.u.q n=n.u.q q=(insert.bt q.u.q [k=k.u.p n=[k=k.u.p n=n.u.p q=q.u.p]])]]
        ::  errors on empty pque, sigcheck first
        ++  peek
          |=  q=(pque key val)
          ^-  [k=key n=val]
          ?~  q  ~|(%empty-pque-peek !!)
          [k=k.u.q n=n.u.q]
        ::  errors on empty pque, sigcheck first
        ++  pop
          |=  q=(pque key val)
          ^-  [r=[k=key n=val] q=(pque key val)]
          ?~  q  ~|(%empty-pque-pop !!)
          ?~  q.u.q
            [r=(peek q) q=~]     ::  queue is now empty
          =+  s=(pop.bt q.u.q)   ::  [r=[k=key n=rque] q=bque]
          ~!  s
          [r=(peek q) q=[~ [k=k.r.s n=n.n.r.s q=(meld.bt q.n.r.s q.s)]]]
        --
      --
    ==
|=  *
|=  ~
^-  bowl
:_  ~  :_  ~
:-  %$
!>
=+  pri=((qu ,@ ,@) lte)
=+  pq=(insert.pri ~ 6 302)
=.  pq  (insert.pri pq 5 3.897)
=.  pq  (insert.pri pq 2 1)
=+  pq2=(insert.pri ~ 508 542)
=.  pq2  (insert.pri pq2 42 89)
=.  pq2  (insert.pri pq2 325 325)
=.  pq2  (insert.pri pq2 41 37)
=.  pq  (meld.pri pq pq2)
~&  pq
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
=^  r  pq  (pop.pri pq)
~&  r
pq
