!:
::  /=main=/bin/app/hoon
::
=>  %=    .
        +
      =>  +
      |%
      ++  blob  $%  [%delta q=blob r=udon]
                    [%direct q=* r=umph]
                    [%indirect q=* r=udon]
                ==
      ++  yaki  ,[p=(list yaki) q=(map path blob) r=@ t=@ud]    :: later t=@da
      ::
      ::  database helpers
      ::
      ++  hash-blob   :: 
        |=  p=*
        ^-  blob
        [%direct p %c]
      ::
      ++  hash-yaki     ::  zoal
        |=  [p=(list yaki) q=(map path blob) t=@ud]             :: later t=@da
        ^-  yaki
        [p q (mug [(roll (turn p |=(p=yaki r.p)) add) q t]) t]  ::  later quicksort?
      ::
      ++  grab      ::  -> zaul
        |=  p=blob
        ?-   -.p
           %delta  (lump r.p $(p q.p))
           %direct  q.p
           %indirect  q.p
        ==
      ::
      ++  prep      ::  ->  zump
        |=  p=blob 
        ^-  umph
        ?-   -.p
           %delta  p.r.p
           %direct  r.p
           %indirect  p.r.p
        ==
      ::
      ::  utils
      ::
      ++  lath
        |=  [p=(map path ,*) s=(set path)]
        ^-  (set path)
        %+  roll  (~(tap by p) ~)
        |=  [[p=path *] q=_s]
        %.  p  %~  put  in  q
      ::
      ++  luth
        |=  [p=(map path ,*) q=(map path ,*)]
        ^-  (list path)
        %.  ~  
        %~  tap  in
        %+  lath  p 
        %+  lath  q
        _(set path)
      ::
      ::  graph logic
      ::
      ++  zule                                            ::  reachable
        |=  p=yaki                                        ::  pretty much a |=
        ^-  (set yaki)
        =+  t=(~(put in _(set yaki)) p)
        %+  roll  p.p
        |=  [q=yaki s=_t]
        ?:  (~(has in s) q)                               ::  already done
          s                                               ::  hence skip
        (~(uni by s) ^$(p q))                             ::  otherwise traverse
      ::
      ++  zeal                                            ::  merge points
        |=  [p=yaki q=yaki]
        =+  r=(zule p)
        |-  ^-  (set yaki)
        ?:  (~(has in r) q)  (~(put in _(set yaki)) q)    ::  done 
        %+  roll  p.q
        |=  [t=yaki s=(set yaki)]
        ?:  (~(has in r) t)
          (~(put in s) t)                                 ::  found
        (~(uni in s) ^$(q t))                             ::  traverse
      ::
      ::  diff logic
      ::
      ++  zerg
        |=  [p=yaki q=yaki]
        ^-  (map path miso)
        %+  roll  (luth q.p q.q)
        |=  [pat=path yeb=(map path miso)]
        =+  leb=(~(get by q.p) pat)
        =+  lob=(~(get by q.q) pat)
        ?~  leb  (~(put by yeb) pat [%ins (grab (need lob))])
        ?~  lob  (~(put by yeb) pat [%del (grab (need leb))])
        =+  zeq=(grab u.leb)
        =+  zoq=(grab u.lob)
        ?:  =(zeq zoq)
          yeb
        %+  ~(put by yeb)  pat
        :-  %mut
        ((diff (prep u.leb)) zeq zoq)
      ::
      ::  merge logic
      ::
      ++  qael                                          ::  clean
        |=  wig=(urge)
        ^-  (urge)
        ?~  wig  ~
        ?~  t.wig  wig
        ?:  ?=(%& -.i.wig)
          ?:  ?=(%& -.i.t.wig)
            $(wig [[%& (add p.i.wig p.i.t.wig)] t.t.wig])
          [i.wig $(wig t.wig)]
        [i.wig $(wig t.wig)]
      ::
      ++  qaul                                          ::  check no delete
        |=  wig=(urge)
        ^-  ?
        ?~  wig  %.y
        ?-    -.i.wig
          %&  %.n
          %|  ?:  =(p.i.wig 0) 
                $(wig t.wig)
              %.n
        ==
      ::
      ++  qeal                                          ::  merge p,q
        |=  [p=miso q=miso]
        ^-  miso
        ~|  %qeal-fail
        ?>  ?=(%mut -.p)
        ?>  ?=(%mut -.q)
        ?>  ?=(%c -.q.p.p)
        ?>  ?=(%c -.q.p.q)
        =+  s=(qael p.q.p.p)
        =+  t=(qael p.q.p.q)
        :-  %mut
        :-  %c  ::  todo is this p.p.p?
        :-  %c
        |-  ^-  (urge)
        ::?~  s  ?:  (qual t)  t
        ::       ~|  %qail-conflict  !!
        ::?~  t  ?:  (qual s)  s
        ::       ~|  %qail-conflict  !!
        ?~  s  t
        ?~  t  s
        ?-    -.i.s
            %&
          ?-     -.i.t
               %&
             ?:  =(p.i.s p.i.t)
               [i.s $(s t.s, t t.t)]
             ?:  (gth p.i.s p.i.t)
               [i.t $(t t.t, p.i.s (sub p.i.s p.i.t))]
             [i.s $(s t.s, p.i.t (sub p.i.t p.i.s))]
               %|
             ?:  =(i.s (lent p.i.t))
               [i.t $(s t.s, t t.t)]
             ?:  (gth p.i.s (lent p.i.t))
               [i.t $(t t.t, p.i.s (sub p.i.s (lent p.i.t)))]
             ~|  %quil-conflict  !!
          ==
            %|
          ?>  ?=(%& -.i.t)
          ?:  =(i.t (lent p.i.s))
            [i.s $(s t.s, t t.t)]
          ?:  (gth p.i.t (lent p.i.s))
            [i.s $(s t.s, p.i.t (sub p.i.t (lent p.i.s)))]
          ~|  %quil-conflict  !!
        ==
      ++  quil                                          ::  merge p,q
        |=  [p=(unit miso) q=(unit miso)]
        ^-  (unit miso)
        ?~  p  q                                        ::  trivial
        ?~  q  p                                        ::  trivial
        ?.  ?=(%mut -.u.p)
          ~|  %quil-conflict  !!
        ?.  ?=(%mut -.u.q)
          ~|  %quil-conflict  !!
        %-  some
        %+  qeal  u.p                                   ::  merge p,q'
        u.q
      ::
      ++  meld                                          ::  merge p,q from r
        |=  [p=yaki q=yaki r=yaki]
        ^-  (map path blob)
        =+  s=(zerg r p)
        =+  t=(zerg r q)
        ~&  [%diff-s s]
        ~&  [%diff-t t]
        %+  roll  (luth s t)
        |=  [pat=path res=(map path blob)]
        =+  ^=  v
            %-  need
            %+  quil  
              (~(get by s) pat)
            (~(get by t) pat)
        ?-    -.v
            %del  res                                      ::  no longer exists
            %ins                                           ::  new file
          %+  ~(put by res)  pat 
          %-  hash-blob
          p.v
            %mut                                           ::  patch from r
          ~&  [%patch p.v [%orig (~(get by q.r) pat)]]
          %+  ~(put by res)  pat
          %-  hash-blob
          %+  lump  p.v
          %-  grab
          %-  need 
          %-  ~(get by q.r)  pat
        ==
      ::
      ::  merge types
      ::
      ++  mate                                          ::  merge p,q
        |=  [p=yaki q=yaki]                             ::  %mate/%meld
        ^-  (map path blob)
        =+  r=(~(tap in (zeal p q)) ~)
        ?~  r
          ~|(%mate-no-ancestor !!)
        ?:  =(1 (lent r))
          (meld p q i.r)
        ~|(%mate-criss-cross !!)
      ::
      ++  keep                                          ::  %this
        |=  [p=yaki q=yaki]
        ^-  (map path blob)
        q.p
      ++  drop                                          ::  %that
        |=  [p=yaki q=yaki]
        ^-  (map path blob)
        q.q
      ++  forge                                         ::  %forge
        |=  [p=yaki q=yaki]
        ^-  (map path blob)
        =+  r=(~(tap in (zeal p q)) ~)
        ?~  r
          ~|(%forge-no-ancestor !!)
        %^  meld  p  q
        %+  roll  t.r                                   ::  fake ancestor
        |=  [par=yaki for=_i.r]
        (hash-yaki [par for ~] (forge par for) 0)       ::  fake yaki
      ::
      ::  actual merge
      ::
      ++  merge
        |=  [p=yaki q=yaki r=@ud s=$+([yaki yaki] (map path blob))]
        ^-  yaki
        (hash-yaki [p q ~] (s p q) r)
      ::
      --
    ==
|=  *
|=  ~
^-  bowl 
:_  ~  :_  ~
:-  %$
!>
=|  b=(map path blob)
=+  n1=(hash-yaki ~ (~(put by b) ~['test'] (hash-blob 'hi\0ahello\0a')) 1)
=+  n2=(hash-yaki [n1 ~] (~(put by b) ~['test'] (hash-blob 'hi\0ahello\0abye\0a')) 2)
=+  n3=(hash-yaki [n1 ~] (~(put by b) ~['test'] (hash-blob 'help\0ahi\0ahello\0a')) 3)
=+  n4=(hash-yaki [n1 ~] b 4)
=+  n5=(hash-yaki [n3 n4 ~] b 5)  :: merge n3/n4
=+  n6=(hash-yaki [n5 ~] b 6)
=+  n7=(hash-yaki [n3 ~] b 7)
::(zeal n6 n7)
::(zerg n1 n2)
::(mate n2 n3)
:-  [%result ((hard ,@t) (grab (need (~(get by q:(merge n3 n2 8 mate)) ~['test'])))) (merge n2 n3 9 forge)]
[%result ((hard ,@t) (grab (need (~(get by q:(merge n3 n2 8 mate)) ~['test'])))) (merge n3 n2 9 forge)]
