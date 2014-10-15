::
!?  164
::
  |=  pit=vase
  =>  =~
|%
++  bqno  |*  [a=_,* b=_,*]                             ::  binary skew queno
          $:  r=@u                                      ::  rank/depth
              k=a                                       ::  priority
              n=b                                       ::  value
              c=(bque a b)                              ::  children
          ==                                            ::
++  bque  |*  [a=_,* b=_,*]                             ::  binary skew que
          (list (bqno a b))                             ::
++  gift                                                ::  out result <-$
          $%  [%wake ~]                                 ::  wakey-wakey
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%wait p=@da]                             ::  set alarm
              [%wake ~]                                 ::  timer activate
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  note  ,~                                            ::  out request $->
++  sign  ,~                                            ::  in result $<-
--
::
|%
++  pa                                                  ::  priority queue
  =+  [key=,@da val=duct]
  =+  cmp=lte                                           ::  lte=min, gte=max
  =>  |%
      ++  link
        |=  [p=(bqno key val) q=(bqno key val)]         ::  link eq rank
        ^-  (bqno key val)
        ?>  =(r.p r.q)
          ?:  (cmp k.p k.q)
            [r=+(r.p) k=k.p n=n.p c=[i=q t=c.p]]
          [r=+(r.q) k=k.q n=n.q c=[i=p t=c.q]]
      ++  slink                                         ::  skew link
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
      ++  ins                                           ::  internal ins op
        |=  [p=(bqno key val) q=(bque key val)]
        ^-  (bque key val)
        ?~  q  [p ~]
        ?>  (lte r.p r.i.q)
        ?:  (lth r.p r.i.q)
          [i=p t=q]
        $(p (link p i.q), q t.q)
      ++  uniq                                          ::  remove init dup
        |=  q=(bque key val)
        ?~  q  ~
        (ins i.q t.q)
      ++  meuq                                          ::  unique meld
        |=  [p=(bque key val) q=(bque key val)]
        ^-  (bque key val)
        ?~  p  q
        ?~  q  p
        ?:  (lth r.i.p r.i.q)
          [i.p $(p t.p)]
        ?:  (lth r.i.q r.i.p)
          [i.q $(q t.q)]
        (ins (link i.p i.q) $(p t.p, q t.q))
      ++  gmi                                           ::  getmin
        |=  q=(bque key val)
        ^-  p=[(bqno key val) (bque key val)]
        ?~  q  ~|(%fatal-gmi-empty !!)
        ?~  t.q  [i=i.q t=~]
        =+  [l r]=$(q t.q)
        ?:  (cmp k.i.q k.l)
          [i.q t.q]
        [l [i.q r]]
      ++  spli                                          ::  split
        ::|*  p=(bque) q=(list ,[k=,_+<-.cmp n=*]) r=(bque)
        |=  [p=(bque key val) q=(list ,[k=key n=val]) r=(bque key val)]
        ^-  [t=(bque key val) x=(list ,[k=key n=val])]
        ?~  r
          [t=p x=q]
        ?:  =(0 r.i.r)
          $(q [[k=k.i.r n=n.i.r] q], r t.r)
        $(p [i.r p], r t.r)
      --
  |_  a=(bque key val)                                  ::  public interface
  ++  add                                               ::  insert element
    |=  [k=key n=val]
    ^+  a
    ?~  a  [i=[r=0 k=k n=n c=~] t=~]
    ?~  t.a  [i=[r=0 k=k n=n c=~] t=a]
    ?:  =(r.i.a r.i.t.a)
      [i=(slink [r=0 k=k n=n c=~] i.a i.t.a) t=t.t.a]
    [i=[r=0 k=k n=n c=~] t=a]
  ++  pop                                               ::  remove top
    ^+  a
    ::^-  [q=(bque key val) r=[k=key n=val]]
    =+  ?~  a  ~|(%empty-bque-pop !!)
        [l r]=(gmi a)
    =+  [t x]=(spli ~ ~ c.l)
    =.  a  r
    =.  a  (uni t)
    (gas x)
  ++  gas
    |=  b=(list ,[k=key n=val])
    ^+  a
    q:(roll b |=([[k=key n=val] q=_a] (add(a q) k n)))
  ++  top                                               ::  retrieve top
    ^-  [p=key q=val]
    ?~  a  ~|(%empty-bque-peek !!)
    ?~  t.a  [k n]:i.a
    =+  m=top(a t.a)
    ?.((cmp k.i.a p.m) m [k n]:i.a)
  ++  uni                                               ::  merge
    |=  q=(bque key val)
    ^+  a
    (meuq (uniq a) (uniq q))
  --
--
.  ==
=|  $:  %0                                              ::
        tym=(bque ,@da duct)                            ::
    ==                                                  ::
|=  [now=@da eny=@ ski=sled]                            ::  current invocation
^?
|%                                                      ::  poke/peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          hic=(hypo (hobo kiss))
      ==
  ^-  [p=(list move) q=_..^$]
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  kiss
        ?:  ?=(%soft -.q.hic)
          ::  ~&  [%dill-call-soft (,@tas `*`-.p.q.hic)]
          ((hard kiss) p.q.hic)
        ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
        ~&  [%time-call-flub (,@tas `*`-.q.hic)]
        ((hard kiss) q.hic)
      ==
  =^  mof  tym
    ?-    -.q.hic
        %wait  [~ (~(add pa tym) p.q.hic hen)]
        %wake
      |-  ^-  [(list move) (bque ,@da duct)]
      ?:  =(~ tym)  [~ tym]                             ::  XX  TMI
      =+  nex=~(top pa tym)
      ?:  (lte now p.nex)  [~ tym]
      ~!  tym
      =^  mof  tym  $(tym ~(pop pa tym))
      [[`move`[q.nex %give %wake ~] mof] tym]
    ==
  [mof ..^$]
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ?~  tym  ~
  (some p:[~(top pa tym)])
::
++  load
  |=  old=[%0 tym=(bque ,@da duct)]
  ^+  ..^$
  ..^$(tym tym.old)
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  [~ ~ [%tank >tym<]]
::
++  stay  [%0 tym]
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [p=(list move) q=_..^$]
  !!
--
