!:  ::  %behn, just a timer
!?  164
::::
=,  behn
|=  pit/vase
=>  =~
|%
++  sqeu  |*  {a/_* b/_*}                               ::  binary skew queno
          $:  r/@u                                      ::  rank+depth
              k/a                                       ::  priority
              n/b                                       ::  value
              c/(broq a b)                              ::  children
          ==                                            ::
++  broq  |*  {a/_* b/_*}                               ::  brodal skew qeu
          (list (sqeu a b))                             ::
++  move  {p/duct q/(wind note gift:able)}              ::  local move
++  note  $~                                            ::  out request $->
++  sign  $~                                            ::  in result $<-
++  clok  (broq @da duct)                               ::  stored timers
--
::
|%
++  raze
  |=  tym/{p/clok q/clok}
  ^+  tym
  ?~  p.tym  tym
  ?~  q.tym  tym
  ?:  (gth p:~(get up p.tym) p:~(get up q.tym))         ::  killed nonexisting
    ~&  [%snooze-lost del=p:~(get up q.tym) top=p:~(get up p.tym)]
    $(q.tym ~(pop up q.tym))
  ?:  =(~(get up p.tym) ~(get up q.tym))
    $(tym [~(pop up p.tym) ~(pop up q.tym)])
  tym
::
++  up                                                  ::  priority queue
  =+  [key=@da val=duct]
  =+  cmp=lte                                           ::  lte=min, gte=max
  =>  |%
      ++  link
        |=  {p/(sqeu key val) q/(sqeu key val)}         ::  link eq rank
        ^-  (sqeu key val)
        ?>  =(r.p r.q)
          ?:  (cmp k.p k.q)
            [r=+(r.p) k=k.p n=n.p c=[i=q t=c.p]]
          [r=+(r.q) k=k.q n=n.q c=[i=p t=c.q]]
      ::
      ++  sink                                          ::  skew link
        |=  {p/(sqeu key val) q/(sqeu key val) r/(sqeu key val)}
        ^-  (sqeu key val)
        ?:  &((cmp k.q k.p) (cmp k.q k.r))
          [r=+(r.q) k=k.q n=n.q c=[i=p t=[i=r t=c.q]]]
        ?:  &((cmp k.r k.p) (cmp k.r k.q))
          [r=+(r.r) k=k.r n=n.r c=[i=p t=[i=q t=c.r]]]
        [r=+(r.q) k=k.p n=n.p c=[i=q t=[i=r t=~]]]
      ::
      ++  sert                                          ::  internal ins op
        |=  {p/(sqeu key val) q/(broq key val)}
        ^-  (broq key val)
        ?~  q  [p ~]
        ?>  (lte r.p r.i.q)
        ?:  (lth r.p r.i.q)
          [i=p t=q]
        $(p (link p i.q), q t.q)
      ::
      ++  uniq                                          ::  remove init dup
        |=  q/(broq key val)
        ?~  q  ~
        (sert i.q t.q)
      ::
      ++  meek                                          ::  unique meld
        |=  {p/(broq key val) q/(broq key val)}
        ^-  (broq key val)
        ?~  p  q
        ?~  q  p
        ?:  (lth r.i.p r.i.q)
          [i.p $(p t.p)]
        ?:  (lth r.i.q r.i.p)
          [i.q $(q t.q)]
        (sert (link i.p i.q) $(p t.p, q t.q))
      ::
      ++  mini                                           ::  getmin
        |=  q/(broq key val)
        ^-  p/{(sqeu key val) (broq key val)}
        ?~  q  ~|(%fatal-mini-empty !!)
        ?~  t.q  [i=i.q t=~]
        =+  [l r]=$(q t.q)
        ?:  (cmp k.i.q k.l)
          [i.q t.q]
        [l [i.q r]]
      ::
      ++  spit                                          ::  split
        |=  {p/(broq key val) q/(list {k/key n/val}) r/(broq key val)}
        ^-  {t/(broq key val) x/(list {k/key n/val})}
        ?~  r
          [t=p x=q]
        ?:  =(0 r.i.r)
          $(q [[k=k.i.r n=n.i.r] q], r t.r)
        $(p [i.r p], r t.r)
      --
  |_  a/(broq key val)                                  ::  public interface
  ++  put                                               ::  insert element
    |=  {k/key n/val}
    ^+  a
    ?~  a  [i=[r=0 k=k n=n c=~] t=~]
    ?~  t.a  [i=[r=0 k=k n=n c=~] t=a]
    ?:  =(r.i.a r.i.t.a)
      [i=(sink [r=0 k=k n=n c=~] i.a i.t.a) t=t.t.a]
    [i=[r=0 k=k n=n c=~] t=a]
  ::
  ++  pop                                               ::  remove top
    ^+  a
    =+  ?~  a  ~|(%empty-broq-pop !!)
        [l r]=(mini a)
    =+  [t x]=(spit ~ ~ c.l)
    =.  a  r
    =.  a  (uni t)
    (gas x)
  ::
  ++  gas
    |=  b/(list {k/key n/val})
    ^+  a
    (roll b |=({{k/key n/val} q/_a} (put(a q) k n)))
  ::
  ++  tap
    ^-  (list {k/key n/val})
    ?~  a  ~
    [get tap(a pop)]
  ::
  ++  get                                               ::  retrieve top
    ^-  {p/key q/val}
    ?~  a  ~|(%empty-broq-peek !!)
    ?~  t.a  [k n]:i.a
    =+  m=get(a t.a)
    ?.((cmp k.i.a p.m) m [k n]:i.a)
  ::
  ++  uni                                               ::  merge
    |=  q/(broq key val)
    ^+  a
    (meek (uniq a) (uniq q))
  --
--
.  ==
=|  $:  $0                                              ::
        tym/{p/clok q/clok}                             ::  positive+negative
    ==                                                  ::
|=  {now/@da eny/@ ski/sley}                            ::  current invocation
^?
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen/duct
          hic/(hypo (hobo task:able))
      ==
  ^-  {p/(list move) q/_..^$}
  =>  %=    .                                           ::  XX temporary
          q.hic
        ^-  task:able
        ?:  ?=($soft -.q.hic)
          ::  ~&  [%behn-call-soft (,@tas `*`-.p.q.hic)]
          ((hard task:able) p.q.hic)
        ?:  (~(nest ut -:!>(*task:able)) | p.hic)  q.hic
        ~&  [%behn-call-flub (@tas `*`-.q.hic)]
        ((hard task:able) q.hic)
      ==
  =^  mof  tym
    ?-    -.q.hic
        $rest
      =.  q.tym  (~(put up q.tym) p.q.hic hen)
      =.  tym  (raze tym)
      [~ tym]
    ::
        $wait
      =.  p.tym  (~(put up p.tym) p.q.hic hen)
      =.  tym  (raze tym)
      [~ tym]
    ::
        $wake
      |-  ^+  [*(list move) tym]
      =.  tym  (raze tym)
      ?:  =([~ ~] tym)  [~ tym]                         ::  XX  TMI
      ?:  =(~ p.tym)
        ~&  %weird-wake  [~ tym]
      =+  nex=~(get up p.tym)
      ?:  (lte now p.nex)  [~ tym]
      =^  mof  tym  $(p.tym ~(pop up p.tym))
      [[`move`[q.nex %give %wake ~] mof] tym]
    ::
        $wegh
      :_  tym  :_  ~
      :^  hen  %give  %mass
      :-  %behn
      :-  %|
      :~  tym+[%& tym]
      ==
    ==
  [mof ..^$]
::
++  doze
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ?~  p.tym  ~
  (some p:[~(get up p.tym)])
::
++  load
  |=  old/{$0 tym/{clok clok}}
  ^+  ..^$
  ..^$(tym tym.old)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=($& -.why)  ~
  =*  who  p.why
  =+  ^=  liz
      |-  ^-  (list {@da duct})
      =.  tym  (raze tym)
      ?~  p.tym  ~
      [~(get up p.tym) $(p.tym ~(pop up p.tym))]
  [~ ~ %tank !>(>liz<)]
::
++  stay  [%0 tym]
++  take                                                ::  process move
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [p=*(list move) q=..^$]
  !!
--
