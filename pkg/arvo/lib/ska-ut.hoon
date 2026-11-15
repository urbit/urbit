::  ska-ut: the type-directed core of the hoon compiler, for compiled nock
::
::    (mint-stock sut gol gen) is (~(mint ut sut) gol gen): a subject
::    type, a goal and a hoon to a type and a formula.  no jets: every
::    arm is first-order hoon, the state of a compilation is threaded
::    through the calls, and what +ut memoizes on whole types and hoons
::    is kept here against the ids of arms.
::
::    a type ($tip) mirrors $type node for node.  a core names its
::    definition ($dee): a base ($bax: context, arms) and what is known
::    of the battery.  a hold names an arm ($arm) of a core.  a fork is
::    a list sorted by mug.  small fields come first, so that = on two
::    different types fails early.
::
|%
+$  tip                                                 ::  type
  $~  %noun
  $@  ?(%noun %void)
  $%  [%atom p=term q=(unit @)]
      [%cell p=tip q=tip]
      [%core r=vair q=dee p=tip]
      [%face p=$@(term tune) q=tip]
      [%fork p=(list fom)]
      [%hint n=note s=tip q=tip]
      [%hold q=arm p=tip]
  ==
+$  fom  [c=? m=@ t=tip]                                ::  fork member: its
                                                        ::  key, a constant
                                                        ::  at its head (c)
+$  tap  [p=tip q=tip]                                  ::  pair of types
+$  arm                                                 ::  an arm of a base
  $:  i=@                                               ::  id
      x=(list @)                                        ::  id, digits of 4
      nam=term                                          ::  name
      axe=axis                                          ::  in the battery
      gen=hoon
  ==
+$  arx  $@(~ [n=[m=@ a=arm] l=arx r=arx])              ::  arms by name mug
+$  bax                                                 ::  base of a core
  $:  b=@                                               ::  number
      nym=(unit term)
      hud=poly
      ctx=tip                                           ::  context
      dom=(map term tome)
      arx=arx
  ==
+$  sem                                                 ::  battery, known:
  $@  ?(%bunt %lazy %lazn)                              ::  not; on demand,
  $%  [%full dez=*]                                     ::  vet on or off;
      [%sock p=seminoun]                                ::  whole; foreign
  ==
+$  dee  [i=@ sem=sem bas=bax]                          ::  core definition
+$  men  (list [p=tip w=? q=arm])                       ::  cores of an arm
+$  fid                                                 ::  a wing, matched:
  $:  $=  k                                             ::
      $?  %void                                         ::  in no type
          %leg                                          ::  leg under a name:
          %lig                                          ::  vein [0 axe lon]
          %arm                                          ::  leg, vein lon
          %not                                          ::  arm axe of cores
          %syn                                          ::  men, vein lon
      ==                                                ::  no match, axe to
      lon=(list @)                                      ::  skip; made by nok
      axe=@
      typ=tip
      men=men
      nok=nock
  ==
+$  duz                                                 ::  change at a leg
  $%  [%put p=tip]
      [%fuse p=tip]
      [%crop p=tip]
      [%gain p=tip q=skin]
      [%lose p=tip q=skin]
  ==
+$  rec                                                 ::  known of an arm
  $:  dry=[b=(unit tip) l=(unit tip) n=(unit tip) f=(unit tip)]
      wet=hot                                           ::  ^ holds on its
      nes=(list [y=@ p=tip q=tip])                      ::  core, by battery;
      hem=(list [p=tip q=nock])                         ::  on other cores;
      red=hot                                           ::  nests; formulas;
      mis=(list [y=@ p=tip q=tip])                      ::  payloads redone;
  ==                                                    ::  pairs that meet
+$  hot                                                 ::  cores by mug:
  $@  ~                                                 ::  c: the arm, wet,
  [n=[m=@ p=tip c=? q=(unit tip)] l=hot r=hot]          ::  is checked; q:
+$  kip  [a=@ b=@ p=tip q=tip]                          ::  the hold, played
+$  arr  $@(~ [v=rec p=[a=arr b=arr] q=[c=arr d=arr]])  ::  recs by arm id
+$  sat                                                 ::  compilation
  $:  $=  nes                                           ::  of a nest:
      $:  gil=(list kip)                                ::  pairs assumed
          nog=(list kip)                                ::  pairs refuted
          rof=?                                         ::  a ref looped
          sof=?                                         ::  a sut looped
      ==
      rut=(list [a=@ b=@ p=tip q=tip r=?])              ::  nests that were
      fan=(list [i=@ p=tip])                            ::  long; holds in play
      rib=(list [p=tip q=tip r=hoon])                   ::  arms in mull
      nex=[i=@ x=(list @)]                              ::  next arm id
      tab=arr
      bas=(map @ (list bax))                            ::  bases by mug
      xpo=(map @ (list [p=tip b=? q=type]))             ::  exported cores
      mpo=(map @ (list [p=type q=tip]))                 ::  imported cores
  ==
+$  pan                                                 ::  partial noun
  $%  [%full p=*]                                       ::  known
      [%bloc ~]                                         ::  unknown
      [%half p=pan q=pan]                               ::  cell of both
      [%lazy p=dee q=axis]                              ::  of a battery
      [%type p=tip q=(list tip)]                        ::  of a type
  ==
::                                                      ::::  entry points
++  mint-stock                                          ::  as mint:ut
  |=  [sut=type gol=type gen=hoon]
  ^-  [p=type q=nock]
  =/  s  new
  =^  a  s  (mpo s sut)
  =^  b  s  (mpo s gol)
  =^  pro  s  (mint s & a b gen)
  =^  typ  s  (xpo s p.pro |)
  [typ q.pro]
::
++  play-stock                                          ::  as play:ut
  |=  [sut=type gen=hoon]
  ^-  type
  =/  s  new
  =^  a  s  (mpo s sut)
  =^  typ  s  (play s a gen)
  =^  out  s  (xpo s typ |)
  out
::
++  nest-stock                                          ::  as nest:ut
  |=  [sut=type tel=? ref=type]
  ^-  ?
  =/  s  new
  =^  a  s  (mpo s sut)
  =^  b  s  (mpo s ref)
  =^  ok  s  (nest s a tel b)
  ok
::
++  new                                                 ::  empty state
  ^-  sat
  =|  s=sat
  s(nex [1 [1 ~]])
::                                                      ::::  constructors
++  bool                                                ::  loobean
  ^-  tip
  (fork [%atom %f `&] [%atom %f `|] ~)
::
++  nan                                                 ::  repo of %noun
  ^-  tip
  (fork [%atom %$ ~] [%cell %noun %noun] ~)
::
++  cell
  |=  [hed=tip tal=tip]
  ^-  tip
  ?:  =(%void hed)  %void
  ?:  =(%void tal)  %void
  [%cell hed tal]
::
++  core
  |=  [pac=tip r=vair q=dee]
  ^-  tip
  ?:  =(%void pac)  %void
  [%core r q pac]
::
++  face
  |=  [giz=$@(term tune) der=tip]
  ^-  tip
  ?:  =(%void der)  %void
  [%face giz der]
::
++  hint
  |=  [s=tip n=note q=tip]
  ^-  tip
  ?:  =(%void q)  %void
  ?:  =(%noun q)  %noun
  [%hint n s q]
::
++  fork                                                ::  union
  |=  yed=(list tip)
  ^-  tip
  ?~  yed  %void
  ?~  t.yed  i.yed
  (fork-all ~ yed)
::
++  fork-all
  |=  [lez=(list fom) yed=(list tip)]
  ^-  tip
  ?~  yed
    ?~  lez  %void
    ?~  t.lez  t.i.lez
    [%fork lez]
  ?:  =(%void i.yed)  (fork-all lez t.yed)
  ?:  ?=([%fork *] i.yed)
    (fork-all (fork-uni lez p.i.yed) t.yed)
  =/  k  (fky i.yed)
  (fork-all (fork-put c.k m.k i.yed lez) t.yed)
::
++  fky                                                 ::  key of a member:
  |=  t=tip                                             ::  the constant at
  ^-  [c=? m=@]                                         ::  its head, or a
  ?@  t  [| 0]                                          ::  number of its own
  ?-  -.t
    %atom  ?~(q.t [| 1] [& u.q.t])
    %cell  (fky-head p.t)
    %core  [| i.q.t]
    %face  $(t q.t)
    %fork  [| 2]
    %hint  $(t q.t)
    %hold  [| i.q.t]
  ==
::
++  fky-head
  |=  t=tip
  ^-  [c=? m=@]
  ?@  t  [| 3]
  ?:  ?=(%face -.t)  $(t q.t)
  ?:  ?=(%hint -.t)  $(t q.t)
  ?:  ?=([%atom * ^] t)  [& u.q.t]
  [| 3]
::
++  fork-put                                            ::  into a sorted
  |=  [c=? m=@ t=tip lez=(list fom)]                    ::  list: others,
  ^-  (list fom)                                        ::  then constants
  ?~  lez  [[c m t] ~]
  =/  i  i.lez
  ?:  &(=(c c.i) =(m m.i))
    ?:  =(t t.i)  lez
    ?:  (fork-tie t t.i)  [[c m t] lez]
    [i (fork-put c m t t.lez)]
  ?:  ?|  &(!c c.i)
          &(=(c c.i) (lth m m.i))
      ==
    [[c m t] lez]
  [i (fork-put c m t t.lez)]
::
++  fork-tie                                            ::  order of two with
  |=  [a=tip b=tip]                                     ::  one key
  ^-  ?
  =/  x  (mug a)
  =/  y  (mug b)
  ?:  =(x y)  (dor a b)
  (lth x y)
::
++  fork-uni                                            ::  merge sorted lists
  |=  [a=(list fom) b=(list fom)]
  ^-  (list fom)
  ?~  a  b
  ?~  b  a
  =/  x  i.a
  =/  y  i.b
  ?:  &(=(c.x c.y) =(m.x m.y))
    ?:  =(t.x t.y)  [x (fork-uni t.a t.b)]
    ?:  (fork-tie t.x t.y)  [x (fork-uni t.a b)]
    [y (fork-uni a t.b)]
  ?:  ?|  &(!c.x c.y)
          &(=(c.x c.y) (lth m.x m.y))
      ==
    [x (fork-uni t.a b)]
  [y (fork-uni a t.b)]
::
++  busk                                                ::  bridge on subject
  |=  [sut=tip gen=hoon]
  ^-  tip
  [%face [~ [gen ~]] sut]
::                                                      ::::  tables
++  step                                                ::  next id digits
  |=  x=(list @)
  ^-  (list @)
  ?~  x  [1 ~]
  ?:  =(3 i.x)  [0 $(x t.x)]
  [+(i.x) t.x]
::
++  aget                                                ::  rec of an arm id
  |=  [t=arr x=(list @)]
  ^-  rec
  ?~  t  [[~ ~ ~ ~] ~ ~ ~ ~ ~]
  ?~  x  v.t
  ?:  =(0 i.x)  (aget a.p.t t.x)
  ?:  =(1 i.x)  (aget b.p.t t.x)
  ?:  =(2 i.x)  (aget c.q.t t.x)
  (aget d.q.t t.x)
::
++  aput                                                ::  set rec of an id
  |=  [t=arr x=(list @) v=rec]
  ^-  arr
  ?~  t  $(t [[[~ ~ ~ ~] ~ ~ ~ ~ ~] [~ ~] [~ ~]])
  ?~  x  t(v v)
  ?:  =(0 i.x)  t(a.p $(t a.p.t, x t.x))
  ?:  =(1 i.x)  t(b.p $(t b.p.t, x t.x))
  ?:  =(2 i.x)  t(c.q $(t c.q.t, x t.x))
  t(d.q $(t d.q.t, x t.x))
::                                                      ::::  cores
++  arx-get                                             ::  arm by name
  |=  [m=@ nam=term a=arx]
  ^-  $@(~ arm)
  ?~  a  ~
  ?:  =(m m.n.a)
    ?:  =(nam nam.a.n.a)  a.n.a
    (arx-get m nam r.a)
  ?:  (lth m m.n.a)  (arx-get m nam l.a)
  (arx-get m nam r.a)
::
++  arx-put                                             ::  first of a name
  |=  [a=arx m=@ new=arm]                               ::  stays
  ^-  arx
  ?~  a  [[m new] ~ ~]
  ?:  =(m m.n.a)
    ?:  =(nam.new nam.a.n.a)  a
    a(r $(a r.a))
  ?:  (lth m m.n.a)  a(l $(a l.a))
  a(r $(a r.a))
::
++  arx-dom                                             ::  arms of chapters
  |=  [nex=[i=@ x=(list @)] dom=(map term tome)]
  ^-  [arx [i=@ x=(list @)]]
  =|  a=arx
  =/  axe=axis  1
  |-  ^-  [arx [i=@ x=(list @)]]
  ?~  dom  [a nex]
  ?:  &(?=(~ l.dom) ?=(~ r.dom))
    (arx-tom a nex axe q.n.dom)
  =^  b  nex  (arx-tom a nex (peg axe 2) q.n.dom)
  =.  a  b
  ?~  l.dom  $(dom r.dom, axe (peg axe 3))
  ?~  r.dom  $(dom l.dom, axe (peg axe 3))
  =^  c  nex  $(dom l.dom, axe (peg axe 6))
  =.  a  c
  $(dom r.dom, axe (peg axe 7))
::
++  arx-tom                                             ::  arms of a chapter
  |=  [a=arx nex=[i=@ x=(list @)] axe=axis dab=(map term hoon)]
  ^-  [arx [i=@ x=(list @)]]
  ?~  dab  [a nex]
  =/  two  &(?=(~ l.dab) ?=(~ r.dab))
  =/  new=arm  [i.nex x.nex p.n.dab ?:(two axe (peg axe 2)) q.n.dab]
  =.  a  (arx-put a (mug p.n.dab) new)
  =.  nex  [+(i.nex) (step x.nex)]
  ?:  two  [a nex]
  ?~  l.dab  $(dab r.dab, axe (peg axe 3))
  ?~  r.dab  $(dab l.dab, axe (peg axe 3))
  =^  b  nex  $(dab l.dab, axe (peg axe 6))
  =.  a  b
  $(dab r.dab, axe (peg axe 7))
::
++  arx-gen                                             ::  arm by its hoon
  |=  [a=arx gen=hoon]
  ^-  $@(~ arm)
  ?~  a  ~
  ?:  =(gen gen.a.n.a)  a.n.a
  =/  l  $(a l.a)
  ?^  l  l
  $(a r.a)
::
++  dom-at                                              ::  arm at an axis
  |=  [dom=(map term tome) axe=axis]
  ^-  (unit hoon)
  ?~  dom  ~
  ?:  &(?=(~ l.dom) ?=(~ r.dom))  (tom-at q.n.dom axe)
  ?:  =(1 axe)  ~
  ?:  =(2 (cap axe))  (tom-at q.n.dom (mas axe))
  =.  axe  (mas axe)
  ?~  l.dom  $(dom r.dom)
  ?~  r.dom  $(dom l.dom)
  ?:  =(1 axe)  ~
  ?:  =(2 (cap axe))  $(dom l.dom, axe (mas axe))
  $(dom r.dom, axe (mas axe))
::
++  tom-at
  |=  [dab=(map term hoon) axe=axis]
  ^-  (unit hoon)
  ?~  dab  ~
  ?:  &(?=(~ l.dab) ?=(~ r.dab))  ?:(=(1 axe) `q.n.dab ~)
  ?:  =(1 axe)  ~
  ?:  =(2 (cap axe))  ?:(=(1 (mas axe)) `q.n.dab ~)
  =.  axe  (mas axe)
  ?~  l.dab  $(dab r.dab)
  ?~  r.dab  $(dab l.dab)
  ?:  =(1 axe)  ~
  ?:  =(2 (cap axe))  $(dab l.dab, axe (mas axe))
  $(dab r.dab, axe (mas axe))
::
++  base                                                ::  intern a base
  |=  [s=sat ctx=tip nym=(unit term) hud=poly dom=(map term tome)]
  ^-  [bax sat]
  =/  key  (mug [ctx dom])
  =/  lis=(list bax)  (fall (~(get by bas.s) key) ~)
  =/  hit  (bass ctx nym hud dom lis)
  ?^  hit  [hit s]
  =/  yan  (arx-dom nex.s dom)
  =/  nex  ?:(=(i.nex.s i.+.yan) [+(i.nex.s) (step x.nex.s)] +.yan)
  =/  b=bax  [i.nex.s nym hud ctx dom -.yan]
  [b s(nex nex, bas (~(put by bas.s) key [b lis]))]
::
++  bass
  |=  $:  ctx=tip  nym=(unit term)  hud=poly  dom=(map term tome)
          l=(list bax)
      ==
  ^-  $@(~ bax)
  ?~  l  ~
  ?:  ?&  =(hud hud.i.l)
          =(nym nym.i.l)
          =(ctx ctx.i.l)
          =(dom dom.i.l)
      ==
    i.l
  (bass ctx nym hud dom t.l)
::
++  deck                                                ::  definition
  |=  [b=bax m=sem]
  ^-  dee
  =/  two  (add b.b b.b)
  =/  for  (add two two)
  =/  k
    ?@  m  ?-(m %bunt 0, %lazy 1, %lazn 2)
    ?-(-.m %full 3, %sock 4)
  [(add k (add for for)) m b]
::
++  self                                                ::  a core, original
  |=  q=dee
  ^-  tip
  [%core %gold q ctx.bas.q]
::                                                      ::::  holds
++  repo                                                ::  one step open
  |=  [s=sat sut=tip]
  ^-  [tip sat]
  ?+  sut      ~>(%mean.'repo-fltt' !!)
    %noun      [nan s]
    [%core *]  [[%cell %noun p.sut] s]
    [%face *]  [q.sut s]
    [%hint *]  [q.sut s]
    [%hold *]  (rest s sut)
  ==
::
++  rest                                                ::  play a hold, once
  |=  [s=sat sut=tip]
  ^-  [tip sat]
  ?>  ?=([%hold *] sut)
  =/  p  p.sut
  =/  a  q.sut
  =/  r  (aget tab.s x.a)
  ::  k: the hold is on the core as it was made, the battery known as
  ::  0 %bunt, 1 %lazy, 2 %lazn, 3 %full; else 4
  ::
  =/  k=@
    ?.  ?=([%core %gold *] p)  4
    ?.  =(p.p ctx.bas.q.p)  4
    =/  m  sem.q.p
    ?@  m  ?-(m %bunt 0, %lazy 1, %lazn 2)
    ?:(?=(%full -.m) 3 4)
  =/  hit=(unit tip)
    ?:  =(0 k)  b.dry.r
    ?:  =(1 k)  l.dry.r
    ?:  =(2 k)  n.dry.r
    ?:  =(3 k)  f.dry.r
    =/  e  (hot-get (mug p) p wet.r)
    ?~(e ~ q.e)
  ?^  hit  [u.hit s]
  ?:  (fans i.a p fan.s)  ~>(%mean.'rest-loop' !!)
  =/  old  fan.s
  =.  fan.s  [[i.a p] fan.s]
  =^  typ  s  (play s p gen.a)
  =.  fan.s  old
  =.  r  (aget tab.s x.a)
  =.  r
    ?:  =(0 k)  r(b.dry `typ)
    ?:  =(1 k)  r(l.dry `typ)
    ?:  =(2 k)  r(n.dry `typ)
    ?:  =(3 k)  r(f.dry `typ)
    =/  m  (mug p)
    =/  e  (hot-get m p wet.r)
    r(wet (hot-put m p ?~(e | c.e) `typ wet.r))
  [typ s(tab (aput tab.s x.a r))]
::
++  fans                                                ::  hold in play
  |=  [i=@ p=tip l=(list [i=@ p=tip])]
  ^-  ?
  ?~  l  |
  ?:  &(=(i i.i.l) =(p p.i.l))  &
  $(l t.l)
::
++  hot-get                                             ::  core by mug
  |=  [m=@ p=tip h=hot]
  ^-  $@(~ [m=@ p=tip c=? q=(unit tip)])
  ?~  h  ~
  ?:  =(m m.n.h)
    ?:  =(p p.n.h)  n.h
    (hot-get m p r.h)
  ?:  (lth m m.n.h)  (hot-get m p l.h)
  (hot-get m p r.h)
::
++  hot-put                                             ::  set core by mug
  |=  [m=@ p=tip c=? q=(unit tip) h=hot]
  ^-  hot
  ?~  h  [[m p c q] ~ ~]
  ?:  =(m m.n.h)
    ?:  =(p p.n.h)  h(n [m p c q])
    h(r $(h r.h))
  ?:  (lth m m.n.h)  h(l $(h l.h))
  h(r $(h r.h))
::
++  kee                                                 ::  a small number
  |=  t=tip                                             ::  that differs for
  ^-  @                                                 ::  most other types
  ?@  t  1
  ?-  -.t
    %atom  2
    %cell  3
    %core  i.q.t
    %face  $(t q.t)
    %fork  ?~(p.t 4 m.i.p.t)
    %hint  $(t q.t)
    %hold  i.q.t
  ==
::
++  kips                                                ::  pair in a list
  |=  [a=@ b=@ sut=tip ref=tip l=(list kip)]
  ^-  ?
  ?~  l  |
  ?:  &(=(a a.i.l) =(b b.i.l) =(sut p.i.l) =(ref q.i.l))  &
  (kips a b sut ref t.l)
::
++  hath                                                ::  in a list
  |=  [sut=tip l=(list tip)]
  ^-  ?
  ?~  l  |
  ?:  =(i.l sut)  &
  (hath sut t.l)
::
++  hath-two                                            ::  pair in a list
  |=  [sut=tip ref=tip l=(list tap)]
  ^-  ?
  ?~  l  |
  ?:  &(=(p.i.l sut) =(q.i.l ref))  &
  (hath-two sut ref t.l)
::                                                      ::::  play
++  play                                                ::  type of a hoon
  |=  [s=sat sut=tip gen=hoon]
  ^-  [tip sat]
  ?+    gen
    =/  doz  ~(open ap gen)
    ?:  =(doz gen)  ~>(%mean.'play-open' !!)
    (play s sut doz)
  ::
      [^ *]
    =^  hed  s  (play s sut p.gen)
    =^  tal  s  (play s sut q.gen)
    [(cell hed tal) s]
  ::
      [%brcn *]
    =^  b  s  (base s sut p.gen %dry q.gen)
    [(core sut %gold (deck b %bunt)) s]
  ::
      [%brpt *]
    =^  b  s  (base s sut p.gen %wet q.gen)
    [(core sut %gold (deck b %bunt)) s]
  ::
      [%$ *]     (peek s sut %read p.gen)
      [%cnts *]  (epla s sut p.gen q.gen)
      [%limb *]  (epla s sut [p.gen ~] ~)
      [%wing *]  (epla s sut p.gen ~)
      [%dtkt *]  (play s sut [%kttr p.gen])
      [%dtls *]  [[%atom %$ ~] s]
      [%rock *]  [(rock p.gen q.gen) s]
      [%sand *]
    ?^  q.gen  [(rock p.gen q.gen) s]
    ?:  =(%n p.gen)  ?>(=(0 q.gen) [[%atom p.gen `q.gen] s])
    ?:  =(%f p.gen)  ?>((lte q.gen 1) [bool s])
    [[%atom p.gen ~] s]
  ::
      [%tune *]  [(face p.gen sut) s]
      [%dttr *]  [%noun s]
      [%dtts *]  [bool s]
      [%dtwt *]  [bool s]
      [%hand *]  (mpo s p.gen)
      [%ktbr *]
    =^  typ  s  (play s sut p.gen)
    (wrap s %iron typ)
  ::
      [%ktcb *]  (play s sut q.gen)
      [%ktls *]  (play s sut p.gen)
      [%ktpm *]
    =^  typ  s  (play s sut p.gen)
    (wrap s %zinc typ)
  ::
      [%ktsg *]  (play s sut p.gen)
      [%ktwt *]
    =^  typ  s  (play s sut p.gen)
    (wrap s %lead typ)
  ::
      [%note *]
    =^  typ  s  (play s sut q.gen)
    [(hint sut p.gen typ) s]
  ::
      [%sgzp *]  (play s sut q.gen)
      [%sggr *]  (play s sut q.gen)
      [%tsgr *]
    =^  typ  s  (play s sut p.gen)
    (play s typ q.gen)
  ::
      [%tscm *]  (play s (busk sut p.gen) q.gen)
      [%wtcl *]
    =^  fex  s  (chip s | sut & p.gen)
    =^  wux  s  (chip s | sut | p.gen)
    =^  hiq  s  ?:(=(%void fex) [%void s] (play s fex q.gen))
    =^  ran  s  ?:(=(%void wux) [%void s] (play s wux r.gen))
    [(fork hiq ran ~) s]
  ::
      [%fits *]  [bool s]
      [%wthx *]  [bool s]
      [%dbug *]  ~_((show %o p.gen) (play s sut q.gen))
      [%zpcm *]  (play s sut p.gen)
      [%lost *]  [%void s]
      [%zpmc *]
    =^  hed  s  (play s sut p.gen)
    =^  tal  s  (play s sut q.gen)
    [(cell hed tal) s]
  ::
      [%zpgl *]  (play s sut [%kttr p.gen])
      [%zpts *]  [%noun s]
      [%zppt *]
    =^  ok  s  (feel s | sut p.gen)
    ?.(ok (play s sut q.gen) (play s sut r.gen))
  ::
      [%zpzp *]  [%void s]
  ==
::
++  rock                                                ::  type of a constant
  |=  [p=term q=*]
  ^-  tip
  ?@  q  [%atom p `q]
  [%cell $(q -.q) $(q +.q)]
::                                                      ::::  wings
++  epla                                                ::  play:et
  |=  [s=sat sut=tip hyp=wing rig=(list (pair wing hoon))]
  ^-  [tip sat]
  =^  lug  s  (find s | sut %read hyp)
  ?:  ?=(%syn k.lug)  ?>(?=(~ rig) [typ.lug s])
  ?:  ?=(%arm k.lug)
    ?~  rig  (fire s | sut men.lug)
    (epla-arm s sut men.lug rig)
  ?~  rig  [typ.lug s]
  (epla-leg s sut typ.lug rig)
::
++  epla-leg
  |=  [s=sat sut=tip typ=tip rig=(list (pair wing hoon))]
  ^-  [tip sat]
  ?~  rig  [typ s]
  =^  zil  s  (play s sut q.i.rig)
  =^  dar  s  (tack s | typ p.i.rig zil)
  (epla-leg s sut q.dar t.rig)
::
++  epla-arm
  |=  [s=sat sut=tip hag=men rig=(list (pair wing hoon))]
  ^-  [tip sat]
  ?~  rig  (fire s | sut hag)
  =^  zil  s  (play s sut q.i.rig)
  =^  dix  s  (toss s | p.i.rig zil hag)
  (epla-arm s sut q.dix t.rig)
::
++  emin                                                ::  mint:et
  |=  [s=sat vet=? sut=tip gol=tip hyp=wing rig=(list (pair wing hoon))]
  ^-  [[p=tip q=nock] sat]
  =^  lug  s  (find s vet sut %read hyp)
  =^  pro  s
    ^-  [[p=tip q=nock] sat]
    ?:  ?=(%syn k.lug)  ?>(?=(~ rig) [[typ.lug nok.lug] s])
    =/  axe  (axil k.lug lon.lug axe.lug)
    ?:  ?=(%arm k.lug)
      ?~  rig
        =^  typ  s  (fire s vet sut men.lug)
        [[typ [%9 axe.lug %0 axe]] s]
      (emin-arm s vet sut axe axe.lug men.lug ~ rig)
    ?~  rig  [[typ.lug [%0 axe]] s]
    (emin-leg s vet sut axe typ.lug ~ rig)
  ?.  vet  [pro s]
  =^  ok  s  (nest s gol & p.pro)
  [pro s]
::
++  emin-leg
  |=  $:  s=sat  vet=?  sut=tip
          axe=axis  typ=tip  hej=(list (pair axis nock))
          rig=(list (pair wing hoon))
      ==
  ^-  [[p=tip q=nock] sat]
  ?~  rig  [[typ (hike axe hej)] s]
  =^  zil  s  (mint s vet sut %noun q.i.rig)
  =^  dar  s  (tack s vet typ p.i.rig p.zil)
  (emin-leg s vet sut axe q.dar [[p.dar q.zil] hej] t.rig)
::
++  emin-arm
  |=  $:  s=sat  vet=?  sut=tip
          axe=axis  arm=axis  hag=men  hej=(list (pair axis nock))
          rig=(list (pair wing hoon))
      ==
  ^-  [[p=tip q=nock] sat]
  ?~  rig
    =^  typ  s  (fire s vet sut hag)
    [[typ [%9 arm (hike axe hej)]] s]
  =^  zil  s  (mint s vet sut %noun q.i.rig)
  =^  dix  s  (toss s vet p.i.rig p.zil hag)
  (emin-arm s vet sut axe arm q.dix [[p.dix q.zil] hej] t.rig)
::
++  emul                                                ::  mull:et
  |=  $:  s=sat  vet=?  sut=tip  gol=tip  dox=tip
          hyp=wing  rig=(list (pair wing hoon))
      ==
  ^-  [[p=tip q=tip] sat]
  =^  lug  s  (find s vet sut %read hyp)
  =^  dug  s  (find s vet dox %read hyp)
  =^  pro  s
    ^-  [[p=tip q=tip] sat]
    ?:  ?=(%syn k.lug)
      ?>  &(?=(%syn k.dug) ?=(~ rig))
      [[typ.lug typ.dug] s]
    ?<  ?=(%syn k.dug)
    ?.  ?=(%arm k.lug)
      ?<  ?=(%arm k.dug)
      =/  typ  typ.lug
      =/  dyp  typ.dug
      |-  ^-  [[p=tip q=tip] sat]
      ?~  rig  [[typ dyp] s]
      =^  zil  s  (mull s vet sut %noun dox q.i.rig)
      =^  dar  s  (tack s vet typ p.i.rig p.zil)
      =^  dor  s  (tack s vet dyp p.i.rig q.zil)
      ?>  =(p.dar p.dor)
      $(rig t.rig, typ q.dar, dyp q.dor)
    ?>  ?=(%arm k.dug)
    ?>  =(axe.lug axe.dug)
    =/  hag  men.lug
    =/  dag  men.dug
    |-  ^-  [[p=tip q=tip] sat]
    ?~  rig
      =^  typ  s  (fire s vet sut hag)
      =^  dyp  s  (fire s | sut dag)
      [[typ dyp] s]
    =^  zil  s  (mull s vet sut %noun dox q.i.rig)
    =^  dix  s  (toss s vet p.i.rig p.zil hag)
    =^  dux  s  (toss s vet p.i.rig q.zil dag)
    ?>  =(p.dix p.dux)
    $(rig t.rig, hag q.dix, dag q.dux)
  ?.  vet  [pro s]
  =^  ok  s  (nest s gol & p.pro)
  [pro s]
::
++  tend                                                ::  axis of a vein
  |=  vit=(list @)
  ^-  axis
  ?~  vit  1
  ?:  =(0 i.vit)  $(vit t.vit)
  ?~  t.vit  i.vit
  (peg $(vit t.vit) i.vit)
::
++  tack                                                ::  put at a wing
  |=  [s=sat vet=? sut=tip hyp=wing mur=tip]
  ^-  [[p=axis q=tip] sat]
  =^  f  s  (find s vet sut %rite hyp)
  ?<  ?=(%syn k.f)
  (take s vet sut (vine k.f lon.f axe.f) [%put mur])
::
++  toss                                                ::  put in each core
  |=  [s=sat vet=? hyp=wing mur=tip hag=men]
  ^-  [[p=axis q=men] sat]
  ?~  hag  ~>(%mean.'toss-none' !!)
  =^  geq  s  (tack s vet p.i.hag hyp mur)
  ?~  t.hag  [[p.geq [[q.geq w.i.hag q.i.hag] ~]] s]
  (toss-all s vet hyp mur p.geq [[q.geq w.i.hag q.i.hag] ~] t.hag)
::
++  toss-all
  |=  [s=sat vet=? hyp=wing mur=tip axe=axis out=men hag=men]
  ^-  [[p=axis q=men] sat]
  ?~  hag  [[axe (flop out)] s]
  =^  geq  s  (tack s vet p.i.hag hyp mur)
  ?>  =(axe p.geq)
  (toss-all s vet hyp mur axe [[q.geq w.i.hag q.i.hag] out] t.hag)
::
++  take                                                ::  change at a vein
  |=  [s=sat vet=? sut=tip vit=(list @) d=duz]
  ^-  [[p=axis q=tip] sat]
  =/  axe  (tend vit)
  =^  typ  s  (take-in s vet d (flop vit) sut)
  [[axe typ] s]
::
++  take-in
  |=  [s=sat vet=? d=duz vit=(list @) sut=tip]
  ^-  [tip sat]
  ?~  vit
    ?-  -.d
      %put   [p.d s]
      %fuse  (fuse s sut p.d)
      %crop  (crop s sut p.d)
      %gain  (gain:ar s vet p.d sut q.d)
      %lose  (lose:ar s vet p.d sut q.d)
    ==
  ?.  =(0 i.vit)  (take-ax s vet i.vit t.vit d ~ sut)
  ?+    sut  (take-in s vet d t.vit sut)
      [%face *]
    =^  x  s  (take-in s vet d t.vit q.sut)
    [(face p.sut x) s]
  ::
      [%hint *]
    =^  x  s  (take-in s vet d vit q.sut)
    [(hint s.sut n.sut x) s]
  ::
      [%fork *]  (take-in-fork s vet vit d ~ p.sut)
      [%hold *]
    =^  x  s  (repo s sut)
    (take-in s vet d vit x)
  ==
::
++  take-in-fork
  |=  [s=sat vet=? vit=(list @) d=duz out=(list tip) yed=(list fom)]
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  =^  x  s  (take-in s vet d vit t.i.yed)
  (take-in-fork s vet vit d [x out] t.yed)
::
++  take-ax
  |=  [s=sat vet=? axe=axis vit=(list @) d=duz vil=(list tip) sut=tip]
  ^-  [tip sat]
  ?:  =(1 axe)  (take-in s vet d vit sut)
  =/  now  (cap axe)
  =/  lat  (mas axe)
  ?-    sut
      %noun      (take-ax s vet axe vit d vil [%cell %noun %noun])
      %void      [%void s]
      [%atom *]  [%void s]
      [%cell *]
    ?:  =(2 now)
      =^  x  s  (take-ax s vet lat vit d vil p.sut)
      [(cell x q.sut) s]
    =^  x  s  (take-ax s vet lat vit d vil q.sut)
    [(cell p.sut x) s]
  ::
      [%core *]
    ?:  =(2 now)
      =^  x  s  (repo s sut)
      (take-ax s vet axe vit d vil x)
    =^  x  s  (take-ax s vet lat vit d vil p.sut)
    [(core x r.sut q.sut) s]
  ::
      [%face *]
    =^  x  s  (take-ax s vet axe vit d vil q.sut)
    [(face p.sut x) s]
  ::
      [%fork *]  (take-ax-fork s vet axe vit d vil ~ p.sut)
      [%hint *]
    =^  x  s  (take-ax s vet axe vit d vil q.sut)
    [(hint s.sut n.sut x) s]
  ::
      [%hold *]
    ?:  (hath sut vil)  [%void s]
    =^  x  s  (repo s sut)
    (take-ax s vet axe vit d [sut vil] x)
  ==
::
++  take-ax-fork
  |=  $:  s=sat  vet=?  axe=axis  vit=(list @)  d=duz
          vil=(list tip)  out=(list tip)  yed=(list fom)
      ==
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  =^  x  s  (take-ax s vet axe vit d vil t.i.yed)
  (take-ax-fork s vet axe vit d vil [x out] t.yed)
::
++  wrap                                                ::  variance
  |=  [s=sat yoz=?(%lead %iron %zinc) sut=tip]
  ^-  [tip sat]
  ?+    sut  [sut s]
      [%cell *]
    =^  a  s  (wrap s yoz p.sut)
    =^  b  s  (wrap s yoz q.sut)
    [(cell a b) s]
  ::
      [%core *]
    ?>  |(=(%gold r.sut) =(%lead yoz))
    [sut(r yoz) s]
  ::
      [%face *]
    =^  a  s  (wrap s yoz q.sut)
    [(face p.sut a) s]
  ::
      [%fork *]  (wrap-fork s yoz ~ p.sut)
      [%hint *]
    =^  a  s  (wrap s yoz q.sut)
    [(hint s.sut n.sut a) s]
  ::
      [%hold *]
    =^  a  s  (repo s sut)
    (wrap s yoz a)
  ==
::
++  wrap-fork
  |=  [s=sat yoz=?(%lead %iron %zinc) out=(list tip) yed=(list fom)]
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  =^  x  s  (wrap s yoz t.i.yed)
  (wrap-fork s yoz [x out] t.yed)
::
++  peel                                                ::  payload access
  |=  [way=vial met=vair]
  ^-  [sam=? con=?]
  ?:  ?=(%gold met)  [& &]
  ?-  way
    %both  [| |]
    %free  [& &]
    %read  [?=(%zinc met) |]
    %rite  [?=(%iron met) |]
  ==
::
++  peek                                                ::  type at an axis
  |=  [s=sat sut=tip way=vial axe=axis]
  ^-  [tip sat]
  ?:  =(1 axe)  [sut s]
  (peek-in s way (cap axe) (mas axe) ~ sut)
::
++  peek-in
  |=  [s=sat way=vial now=@ lat=axis gil=(list tip) sut=tip]
  ^-  [tip sat]
  ?-    sut
      [%atom *]  [%void s]
      [%cell *]  (peek s ?:(=(2 now) p.sut q.sut) way lat)
      [%core *]
    ?.  =(3 now)  [%noun s]
    ?:  ?=(%gold r.sut)  (peek s p.sut way lat)
    =/  pec  (peel way r.sut)
    =/  tow  ?:(=(1 lat) 1 (cap lat))
    ?:  ?|  &(sam.pec con.pec)
            &(sam.pec =(tow 2))
            &(con.pec =(tow 3))
        ==
      (peek s p.sut way lat)
    ?.  =(way %read)  ~>(%mean.'payload-block' !!)
    =^  hed  s  ?.(sam.pec [%noun s] (peek s p.sut way 2))
    =^  tal  s  ?.(con.pec [%noun s] (peek s p.sut way 3))
    (peek s (cell hed tal) way lat)
  ::
      [%fork *]  (peek-fork s way now lat gil ~ p.sut)
      [%hold *]
    ?:  (hath sut gil)  [%void s]
    =^  x  s  (repo s sut)
    (peek-in s way now lat [sut gil] x)
  ::
      %void  [%void s]
      %noun  [%noun s]
      [%face *]  (peek-in s way now lat gil q.sut)
      [%hint *]  (peek-in s way now lat gil q.sut)
  ==
::
++  peek-fork
  |=  $:  s=sat  way=vial  now=@  lat=axis
          gil=(list tip)  out=(list tip)  yed=(list fom)
      ==
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  =^  x  s  (peek-in s way now lat gil t.i.yed)
  (peek-fork s way now lat gil [x out] t.yed)
::                                                      ::::  names
++  find                                                ::  resolve a wing
  |=  [s=sat vet=? sut=tip way=vial hyp=wing]
  ^-  [fid sat]
  =^  pon  s  (fond s vet sut way hyp)
  ?:  ?=(%void k.pon)  ~>(%mean.'find-none' !!)
  ?:  ?=(%not k.pon)  ~>(%mean.'find-limb' !!)
  [pon s]
::
++  vine                                                ::  vein of a match
  |=  [k=@ lon=(list @) axe=@]
  ^-  (list @)
  ?:(=(%leg k) [0 axe lon] lon)
::
++  axil                                                ::  axis of a match
  |=  [k=@ lon=(list @) axe=@]
  ^-  axis
  ?.  =(%leg k)  (tend lon)
  ?~  lon  axe
  (peg (tend lon) axe)
::
++  fend                                                ::  a leg's type, axis
  |=  [s=sat vet=? sut=tip way=vial hyp=wing]
  ^-  [[p=tip q=axis] sat]
  =^  f  s  (find s vet sut way hyp)
  ?.  ?=(?(%leg %lig) k.f)  ~>(%mean.'fend-fragment' !!)
  [[typ.f (axil k.f lon.f axe.f)] s]
::
++  fund                                                ::  resolve an alias
  |=  [s=sat vet=? sut=tip way=vial gen=hoon]
  ^-  [fid sat]
  =/  hup  ~(reek ap gen)
  ?^  hup  (find s vet sut way u.hup)
  =^  x  s  (mint s vet sut %noun gen)
  [[%syn ~ 0 p.x ~ q.x] s]
::
++  fine                                                ::  match to product
  |=  [s=sat vet=? sut=tip f=fid]
  ^-  [[p=tip q=nock] sat]
  ?:  ?=(%syn k.f)  [[typ.f nok.f] s]
  ?.  ?=(%arm k.f)
    [[typ.f [%0 (axil k.f lon.f axe.f)]] s]
  =^  typ  s  (fire s vet sut men.f)
  [[typ [%9 axe.f %0 (tend lon.f)]] s]
::
++  feel                                                ::  do wings resolve
  |=  [s=sat vet=? sut=tip rot=(list wing)]
  ^-  [? sat]
  =.  rot  (flop rot)
  |-  ^-  [? sat]
  ?~  rot  [& s]
  =^  yep  s  (fond s vet sut %free i.rot)
  ?:  ?=(?(%void %not) k.yep)  [| s]
  =^  x  s  (fine s vet sut yep)
  $(rot t.rot, sut p.x)
::
++  fond                                                ::  raw match
  |=  [s=sat vet=? sut=tip way=vial hyp=wing]
  ^-  [fid sat]
  ?~  hyp  [[%lig ~ 0 sut ~ [%0 0]] s]
  ?~  t.hyp  (lime s vet sut way ~ i.hyp)
  =^  mor  s  $(hyp t.hyp)
  ?-    k.mor
      %void  ~>(%mean.'find-void' !!)
      %not   [mor s]
      %leg   (lime s vet typ.mor way [0 axe.mor lon.mor] i.hyp)
      %lig   (lime s vet typ.mor way lon.mor i.hyp)
      %syn
    =^  fex  s  (mint s vet typ.mor %noun [%wing i.hyp ~])
    [[%syn ~ 0 p.fex ~ (comb nok.mor q.fex)] s]
  ::
      %arm   (lime s vet (fork (cors men.mor)) way lon.mor i.hyp)
  ==
::
++  cors                                                ::  cores of an arm
  |=  hag=men
  ^-  (list tip)
  ?~  hag  ~
  [p.i.hag (cors t.hag)]
::
++  lime                                                ::  one limb
  |=  [s=sat vet=? sut=tip way=vial lon=(list @) lim=limb]
  ^-  [fid sat]
  ?@  lim
    (seek s vet way lim 0 1 lon 0 ~ sut)
  ?:  ?=(%& -.lim)
    =^  typ  s  (peek s sut way p.lim)
    [[%lig [p.lim lon] 0 typ ~ [%0 0]] s]
  ?~  q.lim  (seek-any s 1 lon p.lim ~ sut)
  (seek s vet way u.q.lim 0 1 lon p.lim ~ sut)
::
++  seek                                                ::  search by name;
  |=  $:  s=sat  vet=?  way=vial  nam=term  m=@         ::  m: its mug, or 0
          axe=axis  lon=(list @)  ski=@ud  gil=(list tip)  sut=tip
      ==
  ^-  [fid sat]
  ?-    sut
      %void      [[%void ~ 0 %void ~ [%0 0]] s]
      %noun      [[%not ~ ski %void ~ [%0 0]] s]
      [%atom *]  [[%not ~ ski %void ~ [%0 0]] s]
      [%cell *]
    =^  taf  s  (seek s vet way nam m (add axe axe) lon ski gil p.sut)
    ?.  ?=(%not k.taf)  [taf s]
    (seek s vet way nam m +((add axe axe)) lon axe.taf gil q.sut)
  ::
      [%core *]
    =?  m  =(0 m)  (mug nam)
    =/  zem  (arx-get m nam arx.bas.q.sut)
    ?:  &(?=(^ zem) =(0 ski))
      :_  s
      :*  %arm  [axe lon]  (peg 2 axe.zem)  %void
          [[sut =(%wet hud.bas.q.sut) zem] ~]
          [%0 0]
      ==
    =?  ski  ?=(^ zem)  (dec ski)
    ?:  ?=(%gold r.sut)
      (seek s vet way nam m +((add axe axe)) lon ski gil p.sut)
    =/  pec  (peel way r.sut)
    ?.  sam.pec  [[%not ~ ski %void ~ [%0 0]] s]
    ?:  con.pec  (seek s vet way nam m +((add axe axe)) lon ski gil p.sut)
    =^  typ  s  (peek s p.sut way 2)
    (seek s vet way nam m (peg axe 6) lon ski gil typ)
  ::
      [%hint *]  (seek s vet way nam m axe lon ski gil q.sut)
      [%face *]
    ?@  p.sut
      ?.  =(nam p.sut)  [[%not ~ ski %void ~ [%0 0]] s]
      ?:  =(0 ski)  [[%leg lon axe q.sut ~ [%0 0]] s]
      [[%not ~ (dec ski) %void ~ [%0 0]] s]
    =/  tyr  (~(get by p.p.sut) nam)
    ?~  tyr
      (next s vet way nam m sut axe lon ski gil q.p.sut)
    ?~  u.tyr
      (seek s vet way nam m axe [0 lon] +(ski) gil q.sut)
    ?.  =(0 ski)
      (next s vet way nam m sut axe lon (dec ski) gil q.p.sut)
    =^  tor  s  (fund s vet sut way u.u.tyr)
    =/  von  (weld (vine k.tor lon.tor axe.tor) `(list @)`[0 axe lon])
    :_  s
    ?:  ?=(%syn k.tor)  [%syn ~ 0 typ.tor ~ (comb [%0 axe] nok.tor)]
    ?:  ?=(%arm k.tor)  [%arm von axe.tor %void men.tor [%0 0]]
    [%lig von 0 typ.tor ~ [%0 0]]
  ::
      [%fork *]
    =/  yed  p.sut
    ?~  yed  [[%void ~ 0 %void ~ [%0 0]] s]
    =^  one  s  (seek s vet way nam m axe lon ski gil t.i.yed)
    (seek-fork s vet way nam m axe lon ski gil one t.yed)
  ::
      [%hold *]
    ?:  (hath sut gil)  [[%void ~ 0 %void ~ [%0 0]] s]
    =^  typ  s  (repo s sut)
    (seek s vet way nam m axe lon ski [sut gil] typ)
  ==
::
++  seek-fork
  |=  $:  s=sat  vet=?  way=vial  nam=term  m=@
          axe=axis  lon=(list @)  ski=@ud  gil=(list tip)
          one=fid  yed=(list fom)
      ==
  ^-  [fid sat]
  ?~  yed  [[k.one lon.one axe.one typ.one men.one nok.one] s]
  =^  two  s  (seek s vet way nam m axe lon ski gil t.i.yed)
  (seek-fork s vet way nam m axe lon ski gil (twin one two) t.yed)
::
++  seek-any                                            ::  search, no name
  |=  [s=sat axe=axis lon=(list @) ski=@ud gil=(list tip) sut=tip]
  ^-  [fid sat]
  ?+    sut
    ?:  =(0 ski)  [[%leg lon axe sut ~ [%0 0]] s]
    [[%not ~ (dec ski) %void ~ [%0 0]] s]
  ::
      %void      [[%void ~ 0 %void ~ [%0 0]] s]
      [%hint *]  (seek-any s axe lon ski gil q.sut)
      [%face *]
    ?:  =(0 ski)  [[%leg lon axe q.sut ~ [%0 0]] s]
    [[%not ~ (dec ski) %void ~ [%0 0]] s]
  ::
      [%fork *]
    =/  yed  p.sut
    ?~  yed  [[%void ~ 0 %void ~ [%0 0]] s]
    =^  one  s  (seek-any s axe lon ski gil t.i.yed)
    (seek-any-fork s axe lon ski gil one t.yed)
  ::
      [%hold *]
    ?:  (hath sut gil)  [[%void ~ 0 %void ~ [%0 0]] s]
    =^  typ  s  (repo s sut)
    (seek-any s axe lon ski [sut gil] typ)
  ==
::
++  seek-any-fork
  |=  $:  s=sat  axe=axis  lon=(list @)  ski=@ud  gil=(list tip)
          one=fid  yed=(list fom)
      ==
  ^-  [fid sat]
  ?~  yed  [[k.one lon.one axe.one typ.one men.one nok.one] s]
  =^  two  s  (seek-any s axe lon ski gil t.i.yed)
  (seek-any-fork s axe lon ski gil (twin one two) t.yed)
::
++  next                                                ::  through bridges
  |=  $:  s=sat  vet=?  way=vial  nam=term  m=@
          sut=tip  axe=axis  lon=(list @)  ski=@ud  gil=(list tip)
          zot=(list hoon)
      ==
  ^-  [fid sat]
  ?>  ?=([%face *] sut)
  ?~  zot  (seek s vet way nam m axe [0 lon] ski gil q.sut)
  =^  tiv  s  (mint s vet q.sut %noun i.zot)
  =^  f  s  (seek s vet way nam m 1 ~ ski ~ p.tiv)
  ?:  ?=(%void k.f)  [f s]
  ?:  ?=(%not k.f)  (next s vet way nam m sut axe lon axe.f gil t.zot)
  =^  vat  s  (fine s vet sut f)
  [[%syn ~ 0 p.vat ~ (comb (comb [%0 axe] q.tiv) q.vat)] s]
::
++  twin                                                ::  merge matches
  |=  [hax=fid yor=fid]
  ^-  fid
  ?:  ?=(%void k.hax)  [k.yor lon.yor axe.yor typ.yor men.yor nok.yor]
  ?:  ?=(%void k.yor)  [k.hax lon.hax axe.hax typ.hax men.hax nok.hax]
  ~>  %mean.'find-fork'
  ?-    k.hax
      %void  !!
      %not
    ?>  &(?=(%not k.yor) =(axe.hax axe.yor))
    [%not ~ axe.hax %void ~ [%0 0]]
  ::
      %syn
    ?>  &(?=(%syn k.yor) =(nok.hax nok.yor))
    [%syn ~ 0 (fork typ.hax typ.yor ~) ~ nok.hax]
  ::
      ?(%leg %lig)
    ?>  ?=(?(%leg %lig) k.yor)
    =/  von  (vine k.hax lon.hax axe.hax)
    ?>  =(von (vine k.yor lon.yor axe.yor))
    [%lig von 0 (fork typ.hax typ.yor ~) ~ [%0 0]]
  ::
      %arm
    ?>  &(?=(%arm k.yor) =(lon.hax lon.yor) =(axe.hax axe.yor))
    =/  out  men.hax
    =/  mor  men.yor
    |-  ^-  fid
    ?~  mor  [%arm lon.hax axe.hax %void out [%0 0]]
    ?:  (lien out |=(a=[p=tip w=? q=arm] =(a i.mor)))
      $(mor t.mor)
    $(mor t.mor, out [i.mor out])
  ==
::
++  fire                                                ::  type of an arm
  |=  [s=sat vet=? sut=tip hag=men]
  ^-  [tip sat]
  ?~  hag  [%void s]
  ?:  &(?=(~ t.hag) w.i.hag =([~ 1] gen.q.i.hag))
    [p.i.hag s]
  =^  one  s  (fire-one s vet sut q.i.hag w.i.hag p.i.hag)
  ?~  t.hag  [one s]
  (fire-all s vet sut [one ~] t.hag)
::
++  fire-all
  |=  [s=sat vet=? sut=tip out=(list tip) hag=men]
  ^-  [tip sat]
  ?~  hag  [(fork out) s]
  =^  one  s  (fire-one s vet sut q.i.hag w.i.hag p.i.hag)
  (fire-all s vet sut [one out] t.hag)
::
++  fire-one                                            ::  of one core
  |=  [s=sat vet=? sut=tip q=arm w=? p=tip]
  ^-  [tip sat]
  ?.  ?=([%core *] p)  ~>(%mean.'fire-core' !!)
  =/  ctx  ctx.bas.q.p
  =/  dox=tip  [%core %gold q.p ctx]
  ?.  w
    ?.  vet  [[%hold q dox] s]
    =^  ok  s  (nest s ctx & p.p)
    [[%hold q dox] s]
  ::  the payload with the faces of the original is kept by the payload,
  ::  at the first arm of the base
  ::
  =/  rot  arx.bas.q.p
  ?~  rot  ~>(%mean.'fire-core' !!)
  =/  bx  x.a.n.rot
  =/  pm  (mug p.p)
  =/  pe  (hot-get pm p.p red:(aget tab.s bx))
  =^  pay  s
    ?:  &(?=(^ pe) ?=(^ q.pe))  [u.q.pe s]
    =^  y  s  (redo s p.p ctx)
    =/  br  (aget tab.s bx)
    [y s(tab (aput tab.s bx br(red (hot-put pm p.p & `y red.br))))]
  =/  new=tip  [%core r.p q.p pay]
  ?.  vet  [[%hold q new] s]
  ?:  (ribs sut dox gen.q rib.s)  [[%hold q new] s]
  =/  m  (mug new)
  =/  e  (hot-get m new wet:(aget tab.s x.q))
  ?:  &(?=(^ e) c.e)  [[%hold q new] s]
  =/  old  rib.s
  =.  rib.s  [[sut dox gen.q] rib.s]
  =^  x  s  (mull s & new %noun dox gen.q)
  =.  rib.s  old
  =/  r  (aget tab.s x.q)
  =/  e  (hot-get m new wet.r)
  =.  tab.s  (aput tab.s x.q r(wet (hot-put m new & ?~(e ~ q.e) wet.r)))
  [[%hold q new] s]
::
++  ribs                                                ::  arm in mull
  |=  [sut=tip dox=tip gen=hoon l=(list [p=tip q=tip r=hoon])]
  ^-  ?
  ?~  l  |
  ?:  &(=(r.i.l gen) =(q.i.l dox) =(p.i.l sut))  &
  $(l t.l)
::                                                      ::::  nest
++  nest                                                ::  ref in sut
  |=  [s=sat sut=tip tel=? ref=tip]
  ^-  [? sat]
  ?:  =(sut ref)  [& s]
  ?:  ?=(%noun sut)  [& s]
  =/  ka  (kee sut)
  =/  kb  (kee ref)
  =/  hit  (ruts ka kb sut ref rut.s)
  ?:  =(1 hit)  [& s]
  ?:  =(2 hit)  ?.(tel [| s] ~>(%mean.'nest-fail' !!))
  =/  old  nes.s
  =.  nes.s  [~ ~ | |]
  =^  ok  s  (dext s ~ ~ sut ref)
  =?  s  &(ok !rof.nes.s ?=(^ gil.nes.s))  (keep s gil.nes.s)
  =?  rut.s  ?|  ?=([* * * * * * * * *] gil.nes.s)
                 ?=([* * * * * * * * *] nog.nes.s)
             ==
    [[ka kb sut ref ok] (scag 15 rut.s)]
  =.  nes.s  old
  ?:  ok  [& s]
  ?.  tel  [| s]
  ~>(%mean.'nest-fail' !!)
::
++  ruts                                                ::  a long nest, known
  |=  [a=@ b=@ sut=tip ref=tip l=(list [a=@ b=@ p=tip q=tip r=?])]
  ^-  @
  ?~  l  0
  ?:  &(=(a a.i.l) =(b b.i.l) =(sut p.i.l) =(ref q.i.l))
    ?:(r.i.l 1 2)
  $(l t.l)
::
++  keep                                                ::  remember pairs
  |=  [s=sat l=(list kip)]
  ^-  sat
  ?~  l  s
  =/  sut  p.i.l
  =/  ref  q.i.l
  ?.  &(?=([%hold *] sut) ?=([%hold *] ref))  $(l t.l)
  =/  r  (aget tab.s x.q.sut)
  $(l t.l, tab.s (aput tab.s x.q.sut r(nes [[i.q.ref sut ref] nes.r])))
::
++  long                                                ::  a refuted frame
  |=  [old=(list kip) nog=(list kip) gil=(list kip) now=(list kip)]
  ^-  ?                                                 ::  kept a pair of
  ?~  gil  &                                            ::  its own
  ?|  !=(old t.gil)
      !=(nog now)
  ==
::
++  ness                                                ::  pair remembered
  |=  [y=@ sut=tip ref=tip l=(list [y=@ p=tip q=tip])]
  ^-  ?
  ?~  l  |
  ?:  &(=(y y.i.l) =(ref q.i.l) =(sut p.i.l))  &
  $(l t.l)
::
++  meet                                                ::  each in the other
  |=  [s=sat sut=tip ref=tip]
  ^-  [? sat]
  =^  a  s  (nest s sut | ref)
  ?.  a  [| s]
  (nest s ref | sut)
::
++  dext                                                ::  by the need
  |=  [s=sat seg=(list tip) reg=(list tip) sut=tip ref=tip]
  ^-  [? sat]
  ?:  =(sut ref)  [& s]
  ?-    sut
      %void  (sint s seg reg sut ref)
      %noun  [& s]
      [%atom *]
    ?.  ?=([%atom *] ref)  (sint s seg reg sut ref)
    :_  s
    ?&  (fitz p.sut p.ref)
        |(?=(~ q.sut) =(q.sut q.ref))
    ==
  ::
      [%cell *]
    ?.  ?=([%cell *] ref)  (sint s seg reg sut ref)
    =^  a  s  (dext s ~ ~ p.sut p.ref)
    ?.  a  [| s]
    (dext s ~ ~ q.sut q.ref)
  ::
      [%core *]
    ?.  ?=([%core *] ref)  (sint s seg reg sut ref)
    ?:  &(=(r.sut r.ref) =(q.sut q.ref))
      (dext s seg reg p.sut p.ref)
    ?.  =(hud.bas.q.sut hud.bas.q.ref)  [| s]
    =/  sux  ctx.bas.q.sut
    =/  rex  ctx.bas.q.ref
    =^  a  s  (dext s seg reg sux p.sut)
    ?.  a  [| s]
    =^  b  s  (dext s seg reg p.sut sux)
    ?.  b  [| s]
    =^  c  s  (dext s seg reg rex p.ref)
    ?.  c  [| s]
    =^  d  s  (deem s sux rex seg reg r.sut r.ref)
    ?.  d  [| s]
    ?:  =(%wet hud.bas.q.sut)
      [=(dom.bas.q.sut dom.bas.q.ref) s]
    ::  the same base with the battery known differently: the arms of
    ::  one are the arms of the other, and need no walk
    ::
    ?:  =(b.bas.q.sut b.bas.q.ref)  [& s]
    =/  ka  (kee sut)
    =/  kb  (kee ref)
    ?:  (kips ka kb sut ref gil.nes.s)  [& s]
    =/  old  gil.nes.s
    =.  gil.nes.s  [[ka kb sut ref] gil.nes.s]
    =^  e  s
      %:  deep
        s
        [%core %gold q.sut sux]
        [%core %gold q.ref rex]
        seg
        reg
        dom.bas.q.sut
        dom.bas.q.ref
      ==
    ?:  e  [& s]
    [| s(gil.nes old)]
  ::
      [%face *]  (dext s seg reg q.sut ref)
      [%fork *]
    ?.  ?=(?([%atom *] %noun [%cell *] [%core *]) ref)
      (sint s seg reg sut ref)
    =/  k  (fky ref)
    (lien-dext s ref c.k m.k seg reg p.sut)
  ::
      [%hint *]  (dext s seg reg q.sut ref)
      [%hold *]
    ?:  (hath sut seg)  [| s(sof.nes &)]
    =/  ka  i.q.sut
    =/  kb  (kee ref)
    ?:  (kips ka kb sut ref gil.nes.s)  [& s]
    ?:  (kips ka kb sut ref nog.nes.s)  [| s]
    ?:  ?&  ?=([%hold *] ref)
            (ness kb sut ref nes:(aget tab.s x.q.sut))
        ==
      [& s]
    =/  old  gil.nes.s
    =/  nog  nog.nes.s
    =/  sof  sof.nes.s
    =.  s  s(gil.nes [[ka kb sut ref] gil.nes.s], sof.nes |)
    =^  typ  s  (repo s sut)
    =^  a  s  (dext s [sut seg] reg typ ref)
    ?:  a  [& s(sof.nes |(sof sof.nes.s))]
    ::  refuted: remembered if it took more than a look
    ::
    =?  nog.nes.s  &(!sof.nes.s (long old nog gil.nes.s nog.nes.s))
      [[ka kb sut ref] nog.nes.s]
    [| s(gil.nes old, sof.nes |(sof sof.nes.s))]
  ==
::
++  lien-dext                                           ::  ref in a member;
  |=  $:  s=sat  ref=tip  rc=?  rk=@                    ::  rc: ref has the
          seg=(list tip)  reg=(list tip)  yed=(list fom)::  constant rk at
      ==                                                ::  its head
  ^-  [? sat]
  ?~  yed  [| s]
  ?:  &(rc c.i.yed)
    ::  constants are sorted, and one that differs can't hold the ref
    ::
    ?:  =(rk m.i.yed)
      =^  a  s  (dext s seg reg t.i.yed ref)
      ?:  a  [& s]
      (lien-dext s ref rc rk seg reg t.yed)
    ?:  (gth m.i.yed rk)  [| s]
    (lien-dext s ref rc rk seg reg t.yed)
  ?:  (miff t.i.yed ref)  (lien-dext s ref rc rk seg reg t.yed)
  =^  a  s  (dext s seg reg t.i.yed ref)
  ?:  a  [& s]
  (lien-dext s ref rc rk seg reg t.yed)
::
++  levy-dext                                           ::  each member in sut
  |=  [s=sat sut=tip seg=(list tip) reg=(list tip) yed=(list fom)]
  ^-  [? sat]
  ?~  yed  [& s]
  =^  a  s  (dext s seg reg sut t.i.yed)
  ?.  a  [| s]
  (levy-dext s sut seg reg t.yed)
::
++  miff                                                ::  a fork member and
  |=  [mem=tip ref=tip]                                 ::  a ref that differ
  ^-  ?                                                 ::  at a glance
  ?@  mem  |
  ?@  ref  |
  ?:  ?=(%face -.mem)  $(mem q.mem)
  ?:  ?=(%hint -.mem)  $(mem q.mem)
  ?:  ?=(%face -.ref)  $(ref q.ref)
  ?:  ?=(%hint -.ref)  $(ref q.ref)
  ?:  ?=(%cell -.mem)
    ?:  ?=(%atom -.ref)  &
    ?.  ?=(%cell -.ref)  |
    =/  a  p.mem
    =/  b  p.ref
    ?&  ?=([%atom * ^] a)
        ?=([%atom * ^] b)
        !=(u.q.a u.q.b)
    ==
  ?.  ?=(%atom -.mem)  |
  ?:  ?=(%cell -.ref)  &
  ?&  ?=(%atom -.ref)
      ?=(^ q.mem)
      ?=(^ q.ref)
      !=(u.q.mem u.q.ref)
  ==
::
++  sint                                                ::  by the have
  |=  [s=sat seg=(list tip) reg=(list tip) sut=tip ref=tip]
  ^-  [? sat]
  ?-    ref
      %noun      [| s]
      %void      [& s]
      [%atom *]  [| s]
      [%cell *]  [| s]
      [%core *]  (dext s seg reg sut [%cell %noun p.ref])
      [%face *]  (dext s seg reg sut q.ref)
      [%fork *]
    ?.  ?=([%fork *] sut)  (levy-dext s sut seg reg p.ref)
    ::  fork in fork: remembered, as each side multiplies the other
    ::
    =/  ka  (kee sut)
    =/  kb  (kee ref)
    ?:  (kips ka kb sut ref gil.nes.s)  [& s]
    ?:  (kips ka kb sut ref nog.nes.s)  [| s]
    =/  sof  sof.nes.s
    =.  sof.nes.s  |
    =^  a  s  (levy-dext s sut seg reg p.ref)
    ?:  a  [& s(gil.nes [[ka kb sut ref] gil.nes.s], sof.nes |(sof sof.nes.s))]
    =?  nog.nes.s  !sof.nes.s  [[ka kb sut ref] nog.nes.s]
    [| s(sof.nes |(sof sof.nes.s))]
  ::
      [%hint *]  (dext s seg reg sut q.ref)
      [%hold *]
    ?:  (hath ref reg)  [& s(rof.nes &)]
    =/  ka  (kee sut)
    =/  kb  i.q.ref
    ?:  (kips ka kb sut ref gil.nes.s)  [& s]
    ?:  (kips ka kb sut ref nog.nes.s)  [| s]
    =/  old  gil.nes.s
    =/  nog  nog.nes.s
    =/  sof  sof.nes.s
    =.  s  s(gil.nes [[ka kb sut ref] gil.nes.s], sof.nes |)
    =^  typ  s  (repo s ref)
    =^  a  s  (dext s seg [ref reg] sut typ)
    ?:  a  [& s(sof.nes |(sof sof.nes.s))]
    =?  nog.nes.s  &(!sof.nes.s (long old nog gil.nes.s nog.nes.s))
      [[ka kb sut ref] nog.nes.s]
    [| s(gil.nes old, sof.nes |(sof sof.nes.s))]
  ==
::
++  deem                                                ::  variance
  |=  $:  s=sat  sut=tip  ref=tip  seg=(list tip)  reg=(list tip)
          mel=vair  ram=vair
      ==
  ^-  [? sat]
  ?.  |(=(mel ram) =(%lead mel) =(%gold ram))  [| s]
  ?-    mel
      %lead  [& s]
      %gold
    =^  a  s  (dext s seg reg sut ref)
    ?.  a  [| s]
    (dext s seg reg ref sut)
  ::
      %iron
    =^  a  s  (peek s ref %rite 2)
    =^  b  s  (peek s sut %rite 2)
    (dext s seg reg a b)
  ::
      %zinc
    =^  a  s  (peek s sut %read 2)
    =^  b  s  (peek s ref %read 2)
    (dext s seg reg a b)
  ==
::
++  deep                                                ::  arms of cores
  |=  $:  s=sat  sut=tip  ref=tip  seg=(list tip)  reg=(list tip)
          dom=(map term tome)  vim=(map term tome)
      ==
  ^-  [? sat]
  ?~  dom  [=(vim ~) s]
  ?~  vim  [| s]
  ?.  =(p.n.dom p.n.vim)  [| s]
  =^  a  s  $(dom l.dom, vim l.vim)
  ?.  a  [| s]
  =^  b  s  $(dom r.dom, vim r.vim)
  ?.  b  [| s]
  =/  dab  q.n.dom
  =/  hem  q.n.vim
  |-  ^-  [? sat]
  ?~  dab  [=(hem ~) s]
  ?~  hem  [| s]
  ?.  =(p.n.dab p.n.hem)  [| s]
  =^  a  s  $(dab l.dab, hem l.hem)
  ?.  a  [| s]
  =^  b  s  $(dab r.dab, hem r.hem)
  ?.  b  [| s]
  =^  x  s  (play s sut q.n.dab)
  =^  y  s  (play s ref q.n.hem)
  (dext s seg reg x y)
::                                                      ::::  refinement
++  crop                                                ::  sut less ref
  |=  [s=sat sut=tip ref=tip]
  ^-  [tip sat]
  (crop-dext s ~ sut ref)
::
++  crop-dext
  |=  [s=sat bix=(list tap) sut=tip ref=tip]
  ^-  [tip sat]
  ?:  |(=(sut ref) =(%noun ref))  [%void s]
  ?:  =(%void ref)  [sut s]
  ?-    sut
      [%atom *]
    ?+    ref  (crop-sint s bix sut ref)
        [%cell *]  [sut s]
        [%atom *]
      :_  s
      ?^  q.sut
        ?^(q.ref ?:(=(q.ref q.sut) %void sut) %void)
      ?^(q.ref sut %void)
    ==
  ::
      [%cell *]
    ?+    ref  (crop-sint s bix sut ref)
        [%atom *]  [sut s]
        [%cell *]
      =^  ok  s  (nest s p.ref | p.sut)
      ?.  ok  [sut s]
      =^  x  s  (crop-dext s bix q.sut q.ref)
      [(cell p.sut x) s]
    ==
  ::
      [%core *]
    ?:  ?=(?([%atom *] [%cell *]) ref)  [sut s]
    (crop-sint s bix sut ref)
  ::
      [%face *]
    =^  x  s  (crop-dext s bix q.sut ref)
    [(face p.sut x) s]
  ::
      [%fork *]  (crop-fork s ref bix ~ p.sut)
  ::
      [%hint *]
    =^  x  s  (crop-dext s bix q.sut ref)
    [(hint s.sut n.sut x) s]
  ::
      [%hold *]
    ?<  (hath-two sut ref bix)
    =^  x  s  (repo s sut)
    (crop-dext s [[sut ref] bix] x ref)
  ::
      %noun
    =^  x  s  (repo s sut)
    (crop-dext s bix x ref)
  ::
      %void  [%void s]
  ==
::
++  crop-fork
  |=  [s=sat ref=tip bix=(list tap) out=(list tip) yed=(list fom)]
  ^-  [tip sat]
  =/  k  (fky ref)
  (crop-all s ref c.k m.k bix out yed)
::
++  crop-all
  |=  $:  s=sat  ref=tip  rc=?  rk=@
          bix=(list tap)  out=(list tip)  yed=(list fom)
      ==
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  ?:  &(rc c.i.yed !=(rk m.i.yed))
    (crop-all s ref rc rk bix [t.i.yed out] t.yed)
  =^  x  s  (crop-dext s bix t.i.yed ref)
  (crop-all s ref rc rk bix [x out] t.yed)
::
++  crop-sint
  |=  [s=sat bix=(list tap) sut=tip ref=tip]
  ^-  [tip sat]
  ?+    ref  !!
      [%core *]  [sut s]
      [%face *]  (crop-dext s bix sut q.ref)
      [%fork *]  (crop-each s sut bix p.ref)
  ::
      [%hint *]  (crop-dext s bix sut q.ref)
      [%hold *]
    =^  x  s  (repo s ref)
    (crop-dext s bix sut x)
  ==
::
++  crop-each                                           ::  less each member
  |=  [s=sat sut=tip bix=(list tap) yed=(list fom)]
  ^-  [tip sat]
  ?~  yed  [sut s]
  =^  x  s  (crop-dext s bix sut t.i.yed)
  (crop-each s x bix t.yed)
::
++  fuse                                                ::  sut and ref
  |=  [s=sat sut=tip ref=tip]
  ^-  [tip sat]
  (fuse-in s ~ sut ref)
::
++  fuse-in
  |=  [s=sat bix=(list tap) sut=tip ref=tip]
  ^-  [tip sat]
  ?:  |(=(sut ref) =(%noun ref))  [sut s]
  ?-    sut
      [%atom *]
    ?+    ref  (fuse-in s bix ref sut)
        [%cell *]  [%void s]
        [%atom *]
      =/  foc  ?:((fitz p.ref p.sut) p.sut p.ref)
      :_  s
      ?^  q.sut
        ?^  q.ref
          ?:  =(q.sut q.ref)  [%atom foc q.sut]
          %void
        [%atom foc q.sut]
      [%atom foc q.ref]
    ==
  ::
      [%cell *]
    ?.  ?=([%cell *] ref)  (fuse-in s bix ref sut)
    =^  a  s  (fuse-in s bix p.sut p.ref)
    =^  b  s  (fuse-in s bix q.sut q.ref)
    [(cell a b) s]
  ::
      [%core *]  (fuse-in s bix [%cell %noun p.sut] ref)
      [%face *]
    =^  x  s  (fuse-in s bix q.sut ref)
    [(face p.sut x) s]
  ::
      [%fork *]  (fuse-fork s ref bix ~ p.sut)
      [%hint *]
    =^  x  s  (fuse-in s bix q.sut ref)
    [(hint s.sut n.sut x) s]
  ::
      [%hold *]
    ?:  (hath-two sut ref bix)  ~>(%mean.'fuse-loop' !!)
    =^  x  s  (repo s sut)
    (fuse-in s [[sut ref] bix] x ref)
  ::
      %noun  [ref s]
      %void  [%void s]
  ==
::
++  fuse-fork
  |=  [s=sat ref=tip bix=(list tap) out=(list tip) yed=(list fom)]
  ^-  [tip sat]
  =/  k  (fky ref)
  (fuse-each s ref c.k m.k bix out yed)
::
++  fuse-each
  |=  $:  s=sat  ref=tip  rc=?  rk=@
          bix=(list tap)  out=(list tip)  yed=(list fom)
      ==
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  ?:  &(rc c.i.yed !=(rk m.i.yed))
    ?:  (gth m.i.yed rk)  [(fork out) s]
    (fuse-each s ref rc rk bix out t.yed)
  =^  x  s  (fuse-in s bix t.i.yed ref)
  (fuse-each s ref rc rk bix [x out] t.yed)
::
++  chip                                                ::  sut if gen is how
  |=  [s=sat vet=? sut=tip how=? gen=hoon]
  ^-  [tip sat]
  ?:  ?=([%wtts *] gen)
    =^  ref  s  (play s sut ~(example ax p.gen))
    =^  f  s  (find s vet sut %both q.gen)
    ?:  ?=(%syn k.f)  [sut s]
    =^  x  s
      %:  take
        s  vet  sut
        (vine k.f lon.f axe.f)
        ?:(how [%fuse ref] [%crop ref])
      ==
    [q.x s]
  ?:  ?=([%wthx *] gen)
    =^  f  s  (find s vet sut %both q.gen)
    ?:  ?=(%syn k.f)  [sut s]
    =^  x  s
      %:  take
        s  vet  sut
        (vine k.f lon.f axe.f)
        ?:(how [%gain sut p.gen] [%lose sut p.gen])
      ==
    [q.x s]
  ?:  &(how ?=([%wtpm *] gen))  (chip-all s vet sut how p.gen)
  ?:  &(!how ?=([%wtbr *] gen))  (chip-all s vet sut how p.gen)
  ?:  ?=([%wtzp *] gen)  (chip s vet sut !how p.gen)
  =/  neg  ~(open ap gen)
  ?:(=(neg gen) [sut s] (chip s vet sut how neg))
::
++  chip-all                                            ::  each in turn
  |=  [s=sat vet=? sut=tip how=? l=(list hoon)]
  ^-  [tip sat]
  ?~  l  [sut s]
  =^  x  s  (chip s vet sut how i.l)
  (chip-all s vet x how t.l)
::
++  fish                                                ::  test for a type
  |=  [s=sat sut=tip axe=axis]
  ^-  [nock sat]
  (fish-in s axe ~ sut)
::
++  fish-in
  |=  [s=sat axe=axis vot=(list tip) sut=tip]
  ^-  [nock sat]
  ?-    sut
      %void  [[%1 1] s]
      %noun  [[%1 0] s]
      [%atom *]
    :_  s
    ?~  q.sut  (flip [%3 %0 axe])
    [%5 [%1 u.q.sut] [%0 axe]]
  ::
      [%cell *]
    =^  a  s  (fish-in s (peg axe 2) vot p.sut)
    =^  b  s  (fish-in s (peg axe 3) vot q.sut)
    [(flan [%3 %0 axe] (flan a b)) s]
  ::
      [%core *]  ~>(%mean.'fish-core' !!)
      [%face *]  (fish-in s axe vot q.sut)
      [%fork *]
    =^  yed  s  (rank s p.sut)
    (fish-fork s axe vot yed)
  ::
      [%hint *]  (fish-in s axe vot q.sut)
      [%hold *]
    ?:  (hath sut vot)  ~>(%mean.'fish-loop' !!)
    =^  x  s  (repo s sut)
    (fish-in s axe [sut vot] x)
  ==
::
++  fish-fork
  |=  [s=sat axe=axis vot=(list tip) yed=(list tip)]
  ^-  [nock sat]
  ?~  yed  [[%1 1] s]
  =^  a  s  (fish-in s axe vot i.yed)
  =^  b  s  (fish-fork s axe vot t.yed)
  [(flor a b) s]
::
++  rank                                                ::  a fork as +tap:in
  |=  [s=sat yed=(list fom)]                    ::  of a set of types
  ^-  [(list tip) sat]
  =|  out=(list [m=@ x=type t=tip])
  |-  ^-  [(list tip) sat]
  ?~  yed
    :_  s
    |-  ^-  (list tip)
    ?~  out  ~
    [t.i.out $(out t.out)]
  =^  x  s  (xpo s t.i.yed |)
  =/  m  (mug x)
  =/  new  [m x t.i.yed]
  %=    $
      yed  t.yed
      out
    |-  ^-  (list [m=@ x=type t=tip])
    ?~  out  [new ~]
    ?:  ?|  (gth m m.i.out)
            &(=(m m.i.out) (dor x.i.out x))
        ==
      [new out]
    [i.out $(out t.out)]
  ==
::
::
::                                                      ::::  formulas
++  comb                                                ::  compose
  |=  [mal=nock buz=nock]
  ^-  nock
  ?:  ?&(?=([%0 *] mal) !=(0 p.mal))
    ?:  ?&(?=([%0 *] buz) !=(0 p.buz))
      [%0 (peg p.mal p.buz)]
    ?:  ?=([%2 [%0 *] [%0 *]] buz)
      [%2 [%0 (peg p.mal p.p.buz)] [%0 (peg p.mal p.q.buz)]]
    [%7 mal buz]
  ?:  ?=([^ [%0 %1]] mal)
    [%8 p.mal buz]
  ?:  =([%0 %1] buz)
    mal
  [%7 mal buz]
::
++  cond                                                ::  if
  |=  [pex=nock yom=nock woq=nock]
  ^-  nock
  ?:  =([%1 &] pex)  yom
  ?:  =([%1 |] pex)  woq
  ?:  =([%0 0] pex)  pex
  [%6 pex yom woq]
::
++  cons                                                ::  cell
  |=  [vur=nock sed=nock]
  ^-  nock
  ?:  ?=([[%1 *] [%1 *]] +<)
    [%1 p.vur p.sed]
  [vur sed]
::
++  fitz                                                ::  auras fit
  |=  [yaz=term wix=term]
  ^-  ?
  ?:  =(yaz wix)  &
  =/  yoz  (fiz yaz)
  =/  wux  (fiz wix)
  ?&  ?|  =(0 p.yoz)
          =(0 p.wux)
          &(!=(0 p.wux) (lte p.wux p.yoz))
      ==
      |-  ?|  =(%$ q.yoz)
              =(%$ q.wux)
              ?&  =((end 3 q.yoz) (end 3 q.wux))
                  $(q.yoz (rsh 3 q.yoz), q.wux (rsh 3 q.wux))
              ==
          ==
  ==
::
++  fiz
  |=  mot=@ta
  ^-  [p=@ q=@ta]
  =/  len  (met 3 mot)
  ?:  =(0 len)  [0 %$]
  =/  tyl  (rsh [3 (dec len)] mot)
  ?:  &((gte tyl 'A') (lte tyl 'Z'))
    [(sub tyl 64) (end [3 (dec len)] mot)]
  [0 mot]
::
++  flan                                                ::  and
  |=  [bos=nock nif=nock]
  ^-  nock
  ?:  ?|  =(bos nif)
          =([%1 |] bos)
          =([%1 &] nif)
          =([%0 0] bos)
      ==
    bos
  ?:  ?|  =([%1 &] bos)
          =([%1 |] nif)
          =([%0 0] nif)
      ==
    nif
  [%6 bos nif [%1 |]]
::
++  flip                                                ::  not
  |=  dyr=nock
  ^-  nock
  ?:  =([%1 &] dyr)  [%1 |]
  ?:  =([%1 |] dyr)  [%1 &]
  ?:  =([%0 0] dyr)  dyr
  [%6 dyr [%1 |] %1 &]
::
++  flor                                                ::  or
  |=  [bos=nock nif=nock]
  ^-  nock
  ?:  ?|  =(bos nif)
          =([%1 &] bos)
          =([%1 |] nif)
          =([%0 0] bos)
      ==
    bos
  ?:  ?|  =([%1 |] bos)
          =([%1 &] nif)
          =([%0 0] nif)
      ==
    nif
  [%6 bos [%1 &] nif]
::
++  hike                                                ::  edits at axes
  |=  [a=axis pac=(list (pair axis nock))]
  ^-  nock
  =|  rel=(list (pair axis nock))
  |-  ^-  nock
  ?^  pac  $(pac t.pac, rel (hike-in rel p.i.pac q.i.pac))
  =|  ord=(list (pair axis nock))
  |-  ^-  nock
  ?^  rel  $(rel t.rel, ord (hike-up ord i.rel))
  |-  ^-  nock
  ?~  ord  [%0 a]
  [%10 [p.i.ord q.i.ord] $(ord t.ord)]
::
++  hike-up                                             ::  sort, big first
  |=  [ord=(list (pair axis nock)) e=(pair axis nock)]
  ^-  (list (pair axis nock))
  ?~  ord  [e ~]
  ?:  (gth p.e p.i.ord)  [e ord]
  [i.ord $(ord t.ord)]
::
++  hike-in                                             ::  add an edit
  |=  [n=(list (pair axis nock)) axe=axis fol=nock]
  ^-  (list (pair axis nock))
  ?:  =/  a=axis  axe
      |-  ^-  ?
      ?:  =(1 a)  |
      ?:  =/  l  n
          |-  ^-  ?
          ?~  l  |
          ?:  =(a p.i.l)  &
          $(l t.l)
        &
      $(a (rsh 0 a))
    n
  =.  n
    =/  big  (met 0 axe)
    |-  ^-  (list (pair axis nock))
    ?~  n  ~
    =/  small  (met 0 p.i.n)
    ?:  ?&  (gth small big)
            =(axe (rsh [0 (sub small big)] p.i.n))
        ==
      $(n t.n)
    [i.n $(n t.n)]
  =/  sib=axis  ?:(=(0 (end 0 axe)) +(axe) (dec axe))
  =/  un=(unit nock)
    =/  l  n
    |-  ^-  (unit nock)
    ?~  l  ~
    ?:  =(sib p.i.l)  `q.i.l
    $(l t.l)
  ?~  un  [[axe fol] n]
  %=    $
      axe  (rsh 0 sib)
      fol  ?:((gth sib axe) (cons fol u.un) (cons u.un fol))
      n
    |-  ^-  (list (pair axis nock))
    ?~  n  ~
    ?:  =(sib p.i.n)  t.n
    [i.n $(n t.n)]
  ==
::                                                      ::::  skins
++  ar
  |%
  ++  fish                                              ::  test for a skin
    |=  [s=sat vet=? sut=tip ref=tip =skin =axis]
    ^-  [nock sat]
    ?@  skin  $(skin spec+[[%like [skin]~ ~] [%base %noun]])
    ?-    -.skin
        %base
      ?-    base.skin
          %cell  $(skin [%cell [%base %noun] [%base %noun]])
          %noun  [[%1 &] s]
          %null  $(skin [%leaf %n ~])
          %void  [[%1 |] s]
          %flag
        =^  ok  s  (nest s bool | ref)
        ?:  ok  [[%1 &] s]
        =^  a  s  $(skin [%base %atom %$])
        :_  s
        %+  flan  a
        (flor [%5 [%0 axis] [%1 &]] [%5 [%0 axis] [%1 |]])
      ::
          [%atom *]
        =^  ok  s  (nest s [%atom %$ ~] | ref)
        ?:  ok  [[%1 &] s]
        =^  no  s  (nest s [%cell %noun %noun] | ref)
        ?:  no  [[%1 |] s]
        [(flip [%3 %0 axis]) s]
      ==
    ::
        %cell
      =^  ok  s  (nest s [%atom %$ ~] | ref)
      ?:  ok  [[%1 |] s]
      =^  ce  s  (nest s [%cell %noun %noun] | ref)
      =^  hed  s  (peek s ref %free 2)
      =^  a  s  $(ref hed, axis (peg axis 2), skin skin.skin)
      =^  tal  s  (peek s ref %free 3)
      =^  b  s  $(ref tal, axis (peg axis 3), skin ^skin.skin)
      [(flan ?:(ce [%1 &] [%3 %0 axis]) (flan a b)) s]
    ::
        %leaf
      =^  ok  s  (nest s [%atom %$ `atom.skin] | ref)
      ?:  ok  [[%1 &] s]
      [[%5 [%1 atom.skin] [%0 axis]] s]
    ::
        %dbug  $(skin skin.skin)
        %name  $(skin skin.skin)
        %over
      =^  fid  s  (fend s vet sut %read wing.skin)
      $(sut p.fid, axis (peg axis q.fid), skin skin.skin)
    ::
        %spec
      =^  hit  s  (play s sut ~(example ax spec.skin))
      =^  ok  s  (nest s hit & ref)
      $(skin skin.skin)
    ::
        %wash  [[%1 &] s]
    ==
  ::
  ++  gain                                              ::  ref as the skin
    |=  [s=sat vet=? sut=tip ref=tip =skin]
    ^-  [tip sat]
    ?@  skin  $(skin spec+[[%like [skin]~ ~] [%base %noun]])
    ?-    -.skin
        %base
      ?-    base.skin
          %cell  $(skin [%cell [%base %noun] [%base %noun]])
          %null  $(skin [%leaf %n ~])
          %void  [%void s]
          %flag
        =^  a  s  $(skin [%leaf %f &])
        =^  b  s  $(skin [%leaf %f |])
        [(fork a b ~) s]
      ::
          %noun
        =^  ok  s  (nest s %void | ref)
        [?:(ok %void ref) s]
      ::
          [%atom *]  (walk s vet sut ref [%gat p.base.skin] ~)
      ==
    ::
        %cell  (walk s vet sut ref [%gce skin.skin ^skin.skin] ~)
        %leaf  (walk s vet sut ref [%gle aura.skin atom.skin] ~)
        %dbug  $(skin skin.skin)
        %name
      =^  x  s  $(skin skin.skin)
      [(face term.skin x) s]
    ::
        %over
      =^  typ  s  (play s sut [%wing wing.skin])
      $(skin skin.skin, sut typ)
    ::
        %spec
      =^  hit  s  (play s sut ~(example ax spec.skin))
      =^  x  s  $(skin skin.skin)
      =^  ok  s  (nest s hit & x)
      (fuse s ref hit)
    ::
        %wash  ~>(%mean.'gain-wash' !!)
    ==
  ::
  ++  lose                                              ::  ref not the skin
    |=  [s=sat vet=? sut=tip ref=tip =skin]
    ^-  [tip sat]
    ?@  skin  $(skin spec+[[%like [skin]~ ~] [%base %noun]])
    ?-    -.skin
        %base
      ?-    base.skin
          %cell  $(skin [%cell [%base %noun] [%base %noun]])
          %null  $(skin [%leaf %n ~])
          %void  [ref s]
          %noun  [%void s]
          %flag
        =^  a  s  $(skin [%leaf %f &])
        $(ref a, skin [%leaf %f |])
      ::
          [%atom *]  (walk s vet sut ref [%lat ~] ~)
      ==
    ::
        %cell
      (walk s vet sut ref [%lce skin.skin ^skin.skin] ~)
    ::
        %leaf  (walk s vet sut ref [%lle atom.skin] ~)
        %dbug  $(skin skin.skin)
        %name  $(skin skin.skin)
        %over
      =^  typ  s  (play s sut [%wing wing.skin])
      $(skin skin.skin, sut typ)
    ::
        %spec
      =^  hit  s  (play s sut ~(example ax spec.skin))
      =^  x  s  $(skin skin.skin)
      =^  ok  s  (nest s hit & x)
      (crop s ref hit)
    ::
        %wash  [ref s]
    ==
  ::
  ++  walk                                              ::  through wrappers
    |=  $:  s=sat  vet=?  sut=tip  ref=tip
            $=  how
            $%  [%gat p=term]
                [%gce p=skin q=skin]
                [%gle p=term q=@]
                [%lat ~]
                [%lce p=skin q=skin]
                [%lle p=@]
            ==
            gil=(list tip)
        ==
    ^-  [tip sat]
    =/  los  ?=(?(%lat %lce %lle) -.how)
    ?-    ref
        %void  [%void s]
        [%face *]
      =^  x  s  $(ref q.ref)
      ?:  |(?=(%lat -.how) ?=(%lle -.how))
        [(face p.ref x) s]
      [x s]
    ::
        [%fork *]
      =/  yed  p.ref
      =|  out=(list tip)
      |-  ^-  [tip sat]
      ?~  yed  [(fork out) s]
      =^  x  s  (walk s vet sut t.i.yed how gil)
      $(yed t.yed, out [x out])
    ::
        [%hint *]
      =^  x  s  $(ref q.ref)
      [(hint s.ref n.ref x) s]
    ::
        [%hold *]
      ?:  (hath ref gil)  [%void s]
      =^  x  s  (repo s ref)
      $(gil [ref gil], ref x)
    ::
        %noun
      ?-    -.how
          %gat  [[%atom p.how ~] s]
          %gle  [[%atom p.how `q.how] s]
          %lat  [[%cell %noun %noun] s]
          %lle  [%noun s]
          %lce
        ?.  &(=([%base %noun] p.how) =([%base %noun] q.how))  [ref s]
        [[%atom %$ ~] s]
      ::
          %gce
        =^  a  s  (gain s vet sut ref p.how)
        ?:  =(%void a)  [%void s]
        =^  b  s  (gain s vet sut ref q.how)
        [(cell a b) s]
      ==
    ::
        [%atom *]
      ?-    -.how
          %gce  [%void s]
          %lat  [%void s]
          %lce  [ref s]
          %lle  [?:(=(q.ref `p.how) %void ref) s]
          %gat
        ?.  (fitz p.how p.ref)  ~>(%mean.'atom-mismatch' !!)
        [[%atom (max p.how p.ref) q.ref] s]
      ::
          %gle
        ?:  &(?=(^ q.ref) !=(q.how u.q.ref))  [%void s]
        ?.  (fitz p.how p.ref)  ~>(%mean.'atom-mismatch' !!)
        [[%atom (max p.how p.ref) `q.how] s]
      ==
    ::
        [%cell *]
      ?-    -.how
          %gat  [%void s]
          %gle  [%void s]
          %lat  [ref s]
          %lle  [ref s]
          %gce
        =^  a  s  (gain s vet sut p.ref p.how)
        ?:  =(%void a)  [%void s]
        =^  b  s  (gain s vet sut q.ref q.how)
        [(cell a b) s]
      ::
          %lce
        =^  lef  s  (lose s vet sut p.ref p.how)
        =^  rig  s  (lose s vet sut q.ref q.how)
        [(fork (cell lef rig) (cell lef q.ref) (cell p.ref rig) ~) s]
      ==
    ::
        [%core *]
      ?-    -.how
          %gat  [%void s]
          %gle  [%void s]
          %lat  [ref s]
          %lle  [ref s]
          ?(%gce %lce)
        =^  a  s
          ?:  los  (lose s vet sut p.ref p.how)
          (gain s vet sut p.ref p.how)
        ?:  =(%void a)  [%void s]
        ?:  =(%noun q.how)  [[%core r.ref q.ref a] s]
        =^  b  s
          ?:  los  (lose s vet sut %noun q.how)
          (gain s vet sut %noun q.how)
        [(cell a b) s]
      ==
    ==
  --
::                                                      ::::  mint
++  nice                                                ::  typ in the goal
  |=  [s=sat vet=? gol=tip typ=tip]
  ^-  [tip sat]
  ?.  vet  [typ s]
  =^  ok  s  (nest s gol & typ)
  [typ s]
::
++  mint                                                ::  type and formula
  |=  [s=sat vet=? sut=tip gol=tip gen=hoon]
  ^-  [[p=tip q=nock] sat]
  ?:  ?=([%dbug *] gen)
    ~_  (show %o p.gen)
    =^  hum  s  (mint s vet sut gol q.gen)
    [[p.hum [%11 [%spot %1 p.gen] q.hum]] s]
  ?:  =(%void sut)
    ?.  |(!vet ?=([%lost *] gen) ?=([%zpzp *] gen))
      ~>(%mean.'mint-vain' !!)
    [[%void %0 0] s]
  ?+    gen
    =/  doz  ~(open ap gen)
    ?:  =(doz gen)  ~>(%mean.'mint-open' !!)
    (mint s vet sut gol doz)
  ::
      [^ *]
    =^  hed  s  (mint s vet sut %noun p.gen)
    =^  tal  s  (mint s vet sut %noun q.gen)
    =^  typ  s  (nice s vet gol (cell p.hed p.tal))
    [[typ (cons q.hed q.tal)] s]
  ::
      [%brcn *]  (grow s vet sut gol p.gen %dry q.gen)
      [%brpt *]  (grow s vet sut gol p.gen %wet q.gen)
      [%$ *]
    =^  typ  s  (peek s sut %read p.gen)
    =^  x  s  (nice s vet gol typ)
    [[typ [%0 p.gen]] s]
  ::
      [%cnts *]  (emin s vet sut gol p.gen q.gen)
      [%limb *]  (emin s vet sut gol [p.gen ~] ~)
      [%wing *]  (emin s vet sut gol p.gen ~)
      [%dtkt *]
    =^  nef  s  (mint s vet sut gol [%kttr p.gen])
    =^  val  s  (mint s vet sut %noun q.gen)
    =^  typ  s  (xpo s p.nef |)
    [[p.nef [%12 [%1 hoon-version typ] q.val]] s]
  ::
      [%dtls *]
    =^  typ  s  (nice s vet gol [%atom %$ ~])
    =^  val  s  (mint s vet sut [%atom %$ ~] p.gen)
    [[typ [%4 q.val]] s]
  ::
      [%sand *]
    =^  t  s  (play s sut gen)
    =^  typ  s  (nice s vet gol t)
    [[typ [%1 q.gen]] s]
  ::
      [%rock *]
    =^  typ  s  (nice s vet gol (rock p.gen q.gen))
    [[typ [%1 q.gen]] s]
  ::
      [%dttr *]
    =^  typ  s  (nice s vet gol %noun)
    =^  a  s  (mint s vet sut %noun p.gen)
    =^  b  s  (mint s vet sut %noun q.gen)
    [[typ [%2 q.a q.b]] s]
  ::
      [%dtts *]
    =^  typ  s  (nice s vet gol bool)
    =^  a  s  (mint s vet sut %noun p.gen)
    =^  b  s  (mint s vet sut %noun q.gen)
    [[typ [%5 q.a q.b]] s]
  ::
      [%dtwt *]
    =^  typ  s  (nice s vet gol bool)
    =^  a  s  (mint s vet sut %noun p.gen)
    [[typ [%3 q.a]] s]
  ::
      [%hand *]
    =^  typ  s  (mpo s p.gen)
    [[typ q.gen] s]
  ::
      [%ktbr *]
    =^  vat  s  (mint s vet sut gol p.gen)
    =^  w  s  (wrap s %iron p.vat)
    =^  typ  s  (nice s vet gol w)
    [[typ q.vat] s]
  ::
      [%ktpm *]
    =^  vat  s  (mint s vet sut gol p.gen)
    =^  w  s  (wrap s %zinc p.vat)
    =^  typ  s  (nice s vet gol w)
    [[typ q.vat] s]
  ::
      [%ktwt *]
    =^  vat  s  (mint s vet sut gol p.gen)
    =^  w  s  (wrap s %lead p.vat)
    =^  typ  s  (nice s vet gol w)
    [[typ q.vat] s]
  ::
      [%ktls *]
    =^  t  s  (play s sut p.gen)
    =^  hif  s  (nice s vet gol t)
    =^  val  s  (mint s vet sut hif q.gen)
    [[hif q.val] s]
  ::
      [%ktcb *]
    =^  t  s  (play s sut p.gen)
    =^  hif  s  (nice s vet gol t)
    (mint s vet sut hif q.gen)
  ::
      [%ktsg *]
    =^  pro  s  (mint s vet sut gol p.gen)
    =^  jon  s  (mush s sut q.pro)
    ?~  jon  [pro s]
    [[p.pro [%1 u.jon]] s]
  ::
      [%tune *]  [[(face p.gen sut) [%0 %1]] s]
      [%note *]
    =^  hum  s  (mint s vet sut gol q.gen)
    [[(hint sut p.gen p.hum) q.hum] s]
  ::
      [%sgzp *]  (mint s vet sut gol q.gen)
      [%sggr *]
    =^  hum  s  (mint s vet sut gol q.gen)
    ?@  p.gen  [[p.hum [%11 p.gen q.hum]] s]
    =^  hin  s  (mint s vet sut %noun q.p.gen)
    [[p.hum [%11 [p.p.gen q.hin] q.hum]] s]
  ::
      [%tsgr *]
    =^  fid  s  (mint s vet sut %noun p.gen)
    =^  dov  s  (mint s vet p.fid gol q.gen)
    [[p.dov (comb q.fid q.dov)] s]
  ::
      [%tscm *]  (mint s vet (busk sut p.gen) gol q.gen)
      [%wtcl *]
    =^  nor  s  (mint s vet sut bool p.gen)
    =^  fex  s  (chip s vet sut & p.gen)
    =^  wux  s  (chip s vet sut | p.gen)
    =/  duy=[p=? q=nock]
      ?:  =(%void fex)
        ?:(=(%void wux) [| [%0 0]] [& [%1 |]])
      ?:(=(%void wux) [& [%1 &]] [| q.nor])
    =^  hiq  s  (mint s vet fex gol q.gen)
    =^  ran  s  (mint s vet wux gol r.gen)
    =/  fol  (cond q.duy q.hiq q.ran)
    :_  s
    [(fork p.hiq p.ran ~) ?.(p.duy fol [%11 [%toss q.nor] fol])]
  ::
      [%wthx *]
    =^  typ  s  (nice s vet gol bool)
    =^  fid  s  (fend s vet sut %read [[%& 1] q.gen])
    =^  nok  s  (fish:ar s vet sut p.fid p.gen q.fid)
    [[typ nok] s]
  ::
      [%fits *]
    =^  typ  s  (nice s vet gol bool)
    =^  ref  s  (play s sut p.gen)
    =^  f  s  (find s vet sut %read q.gen)
    ?:  ?=(?(%leg %lig) k.f)
      =^  nok  s  (fish s ref (axil k.f lon.f axe.f))
      [[typ nok] s]
    =^  vat  s  (fine s vet sut f)
    =^  nok  s  (fish s ref 1)
    [[typ [%7 q.vat nok]] s]
  ::
      [%zpcm *]
    =^  t  s  (play s sut p.gen)
    =^  typ  s  (nice s vet gol t)
    [[typ [%1 q.gen]] s]
  ::
      [%lost *]
    ?:  vet  ~>(%mean.'mint-lost' !!)
    [[%void [%0 0]] s]
  ::
      [%zpmc *]
    =^  vos  s  (mint s vet sut %noun q.gen)
    =^  ref  s  (mint s vet sut %noun p.gen)
    =^  typ  s  (nice s vet gol (cell p.ref p.vos))
    =^  bur  s  (xpo s p.vos &)
    [[typ (cons [%1 bur] q.vos)] s]
  ::
      [%zpgl *]
    =^  t  s  (play s sut [%kttr p.gen])
    =^  typ  s  (nice s vet gol t)
    =^  val  s
      %=    $
          gol  %noun
          gen
        :^    %wtcl
            :+  %cncl  [%limb %levi]
            :~  [%tsgr [%zpgr [%kttr p.gen]] [%$ 2]]
                [%tsgr q.gen [%$ 2]]
            ==
          [%tsgr q.gen [%$ 3]]
        [%zpzp ~]
      ==
    [[typ q.val] s]
  ::
      [%zpts *]
    =^  typ  s  (nice s vet gol %noun)
    =^  val  s  (mint s | sut gol p.gen)
    [[typ [%1 q.val]] s]
  ::
      [%zppt *]
    =^  ok  s  (feel s vet sut p.gen)
    ?.(ok (mint s vet sut gol q.gen) (mint s vet sut gol r.gen))
  ::
      [%zpzp *]  [[%void [%0 0]] s]
  ==
::
++  grow                                                ::  mint a core
  |=  $:  s=sat  vet=?  sut=tip  gol=tip
          nym=(unit term)  hud=poly  dom=(map term tome)
      ==
  ^-  [[p=tip q=nock] sat]
  ::  what the goal says of the core
  ::
  =^  log  s  (gold s gol)
  ?>  (chap log dom)
  =/  dog=(unit (map term tome))
    ?:(?=([%core *] log) `dom.bas.q.log ~)
  =^  b  s  (base s sut nym hud dom)
  =/  yet  (self (deck b ?:(vet %lazy %lazn)))
  =^  dez  s  (mine s vet yet log dog hud dom)
  =^  typ  s  (nice s vet gol (core sut %gold (deck b [%full dez])))
  [[typ (cons [%1 dez] [%0 1])] s]
::
++  gold                                                ::  goal for a core
  |=  [s=sat log=tip]
  ^-  [tip sat]
  ?+    log
    =^  x  s  (repo s log)
    $(log x)
  ::
      %noun      [log s]
      %void      ~>(%mean.'core-nice' !!)
      [%atom *]  ~>(%mean.'core-nice' !!)
      [%cell *]
    =^  ok  s  (nest s p.log & %noun)
    [log s]
  ::
      [%core *]  [log(r %gold) s]
      [%fork *]
    =/  yed  p.log
    =|  out=(list tip)
    |-  ^-  [tip sat]
    ?~  yed  [[%fork (turn out |=(t=tip [| 0 t]))] s]
    =^  x  s  (gold s t.i.yed)
    $(yed t.yed, out [x out])
  ==
::
++  chap                                                ::  chapters agree
  |=  [log=tip dom=(map term tome)]
  ^-  ?
  ?+    log  &
      [%core *]
    ?:  =(~(wyt by dom) ~(wyt by dom.bas.q.log))  &
    ~>(%mean.'core-number-of-chapters' !!)
  ::
      [%fork *]
    =/  yed  p.log
    |-  ^-  ?
    ?~  yed  &
    ?>  (chap t.i.yed dom)
    $(yed t.yed)
  ==
::
++  mine                                                ::  mint the arms
  |=  $:  s=sat  vet=?  sut=tip  log=tip
          dog=(unit (map term tome))  hud=poly  dom=(map term tome)
      ==
  ^-  [* sat]
  ?>  ?=([%core *] sut)
  ?~  dom  [~ s]
  =^  dov  s
    =/  dab=(map term hoon)  q.n.dom
    =/  dag=(unit (map term hoon))
      ?~  dog  ~
      =/  hav  (~(get by u.dog) p.n.dom)
      ?~  hav  ~>(%mean.'unexpected-chapter' !!)
      ?.  =(~(wyt by u.hav) ~(wyt by dab))
        ~>(%mean.'core-number-of-arms' !!)
      hav
    |-  ^-  [* sat]
    ?~  dab  [~ s]
    =^  gog  s
      ^-  [tip sat]
      ?~  dag  [%noun s]
      =/  gen  (~(get by u.dag) p.n.dab)
      ?~  gen  ~>(%mean.'unexpected-arm' !!)
      (play s log u.gen)
    =/  a  (arx-get (mug p.n.dab) p.n.dab arx.bas.q.sut)
    =^  vad  s
      (hemp s sut hud gog ?:(&(?=(^ a) =(gen.a q.n.dab)) a ~) q.n.dab)
    ?~  l.dab
      ?~  r.dab  [vad s]
      =^  r  s  $(dab r.dab)
      [[vad r] s]
    ?~  r.dab
      =^  l  s  $(dab l.dab)
      [[vad l] s]
    =^  l  s  $(dab l.dab)
    =^  r  s  $(dab r.dab)
    [[vad l r] s]
  ?~  l.dom
    ?~  r.dom  [dov s]
    =^  r  s  $(dom r.dom)
    [[dov r] s]
  ?~  r.dom
    =^  l  s  $(dom l.dom)
    [[dov l] s]
  =^  l  s  $(dom l.dom)
  =^  r  s  $(dom r.dom)
  [[dov l r] s]
::
++  hemp                                                ::  formula of an arm
  |=  [s=sat sut=tip hud=poly gol=tip a=$@(~ arm) gen=hoon]
  ^-  [nock sat]                                        ::  of a lazy core
  ?>  ?=([%core *] sut)
  =/  vet  ?=(%lazy sem.q.sut)
  ?.  &(?=(^ a) =(%noun gol))
    =^  pro  s  (mint s &(vet ?=(%dry hud)) sut gol gen)
    [q.pro s]
  =/  hit  (hemd sut hem:(aget tab.s x.a))
  ?^  hit  [u.hit s]
  =^  pro  s  (mint s &(vet ?=(%dry hud)) sut gol gen)
  =/  r  (aget tab.s x.a)
  [q.pro s(tab (aput tab.s x.a r(hem [[sut q.pro] hem.r])))]
::
++  hemd
  |=  [sut=tip l=(list [p=tip q=nock])]
  ^-  (unit nock)
  ?~  l  ~
  ?:  =(p.i.l sut)  `q.i.l
  $(l t.l)
::                                                      ::::  mull
++  mull                                                ::  types in sut, dox
  |=  [s=sat vet=? sut=tip gol=tip dox=tip gen=hoon]
  ^-  [[p=tip q=tip] sat]
  ?:  ?=([%dbug *] gen)
    ~_((show %o p.gen) (mull s vet sut gol dox q.gen))
  ?:  =(%void sut)  ~>(%mean.'mull-none' !!)
  ?+    gen
    =/  doz  ~(open ap gen)
    ?:  =(doz gen)  ~>(%mean.'mull-open' !!)
    (mull s vet sut gol dox doz)
  ::
      [^ *]
    =^  hed  s  (mull s vet sut %noun dox p.gen)
    =^  tal  s  (mull s vet sut %noun dox q.gen)
    =^  typ  s  (nice s vet gol (cell p.hed p.tal))
    [[typ (cell q.hed q.tal)] s]
  ::
      [%brcn *]  (mile s vet sut gol dox p.gen %dry q.gen)
      [%brpt *]  (mile s vet sut gol dox p.gen %wet q.gen)
      [%cnts *]  (emul s vet sut gol dox p.gen q.gen)
      [%limb *]  (emul s vet sut gol dox [p.gen ~] ~)
      [%wing *]  (emul s vet sut gol dox p.gen ~)
      [%dtkt *]
    =^  x  s  (mull s vet sut %noun dox q.gen)
    (mull s vet sut gol dox [%kttr p.gen])
  ::
      [%dtls *]
    =^  x  s  (mull s vet sut [%atom %$ ~] dox p.gen)
    (beth s vet gol [%atom %$ ~])
  ::
      [%sand *]
    =^  t  s  (play s sut gen)
    (beth s vet gol t)
  ::
      [%rock *]  (beth s vet gol (rock p.gen q.gen))
      [%dttr *]
    =^  x  s  (mull s vet sut %noun dox p.gen)
    =^  y  s  (mull s vet sut %noun dox q.gen)
    (beth s vet gol %noun)
  ::
      [%dtts *]
    =^  x  s  (mull s vet sut %noun dox p.gen)
    =^  y  s  (mull s vet sut %noun dox q.gen)
    (beth s vet gol bool)
  ::
      [%dtwt *]
    =^  x  s  (mull s vet sut %noun dox p.gen)
    (beth s vet gol bool)
  ::
      [%hand *]
    =^  typ  s  (mpo s p.gen)
    [[typ typ] s]
  ::
      [%ktbr *]
    =^  vat  s  (mull s vet sut gol dox p.gen)
    =^  a  s  (wrap s %iron p.vat)
    =^  b  s  (wrap s %iron q.vat)
    [[a b] s]
  ::
      [%ktpm *]
    =^  vat  s  (mull s vet sut gol dox p.gen)
    =^  a  s  (wrap s %zinc p.vat)
    =^  b  s  (wrap s %zinc q.vat)
    [[a b] s]
  ::
      [%ktwt *]
    =^  vat  s  (mull s vet sut gol dox p.gen)
    =^  a  s  (wrap s %lead p.vat)
    =^  b  s  (wrap s %lead q.vat)
    [[a b] s]
  ::
      [%ktls *]
    =^  t  s  (play s sut p.gen)
    =^  a  s  (nice s vet gol t)
    =^  b  s  (play s dox p.gen)
    =^  x  s  (mull s vet sut a dox q.gen)
    [[a b] s]
  ::
      [%ktcb *]
    =^  t  s  (play s sut p.gen)
    =^  a  s  (nice s vet gol t)
    =^  b  s  (play s dox p.gen)
    (mull s vet sut a dox q.gen)
  ::
      [%tune *]  [[(face p.gen sut) (face p.gen dox)] s]
      [%note *]
    =^  vat  s  (mull s vet sut gol dox q.gen)
    [[(hint sut p.gen p.vat) (hint dox p.gen q.vat)] s]
  ::
      [%ktsg *]  (mull s vet sut gol dox p.gen)
      [%sgzp *]  (mull s vet sut gol dox q.gen)
      [%sggr *]  (mull s vet sut gol dox q.gen)
      [%tsgr *]
    =^  lem  s  (mull s vet sut %noun dox p.gen)
    (mull s vet p.lem gol q.lem q.gen)
  ::
      [%tscm *]
    (mull s vet (busk sut p.gen) gol (busk dox p.gen) q.gen)
  ::
      [%wtcl *]
    =^  nor  s  (mull s vet sut bool dox p.gen)
    =^  fex  s  (chip s vet sut & p.gen)
    =^  fox  s  (chip s vet dox & p.gen)
    =^  hiq  s
      ^-  [[p=tip q=tip] sat]
      ?:  =(%void fex)
        ?:  =(%void fox)  [[%void %void] s]
        ~>  %mean.'if-z'
        =^  x  s  (play s fox q.gen)
        [[%void x] s]
      ?:  =(%void fox)  ~>(%mean.'mull-bonk-b' !!)
      (mull s vet fex gol fox q.gen)
    =^  wux  s  (chip s vet sut | p.gen)
    =^  wox  s  (chip s vet dox | p.gen)
    =^  ran  s
      ^-  [[p=tip q=tip] sat]
      ?:  =(%void wux)
        ?:  =(%void wox)  [[%void %void] s]
        ~>  %mean.'if-a'
        =^  x  s  (play s wox r.gen)
        [[%void x] s]
      ?:  =(%void wox)  ~>(%mean.'mull-bonk-c' !!)
      (mull s vet wux gol wox r.gen)
    =^  typ  s  (nice s vet gol (fork p.hiq p.ran ~))
    [[typ (fork q.hiq q.ran ~)] s]
  ::
      [%fits *]
    =^  wap  s  (play s sut p.gen)
    =^  waq  s  (play s dox p.gen)
    =^  syp  s  (mint s vet sut %noun [%wing q.gen])
    =^  syq  s  (mint s vet dox %noun [%wing q.gen])
    =/  axp  (cove q.syp)
    =/  axq  (cove q.syq)
    =^  pop  s  (fish s wap axp)
    =^  poq  s  (fish s waq axq)
    ?.  &(=(axp axq) =(pop poq))  ~>(%mean.'mull-bonk-a' !!)
    (beth s vet gol bool)
  ::
      [%wthx *]
    ~>  %mean.'mull-bonk-x'
    =^  new  s  (fend s vet sut %read [[%& 1] q.gen])
    =^  old  s  (fend s vet dox %read [[%& 1] q.gen])
    ?>  =(q.old q.new)
    =^  ok  s  (nest s p.old & p.new)
    (beth s vet gol bool)
  ::
      [%zpcm *]
    =^  t  s  (play s sut p.gen)
    =^  a  s  (nice s vet gol t)
    =^  b  s  (play s dox p.gen)
    [[a b] s]
  ::
      [%lost *]
    ?:  vet  ~>(%mean.'mull-skip' !!)
    (beth s vet gol %void)
  ::
      [%zpts *]  (beth s vet gol %noun)
      [%zpmc *]
    =^  vos  s  (mull s vet sut %noun dox q.gen)
    =^  t  s  (play s sut p.gen)
    =^  a  s  (nice s vet gol (cell t p.vos))
    =^  u  s  (play s dox p.gen)
    [[a (cell u q.vos)] s]
  ::
      [%zpgl *]
    =^  t  s  (play s sut [%kttr p.gen])
    (beth s vet gol t)
  ::
      [%zppt *]
    =^  a  s  (feel s vet sut p.gen)
    =^  b  s  (feel s vet dox p.gen)
    ?.  =(a b)  ~>(%mean.'mull-bonk-f' !!)
    ?.(a (mull s vet sut gol dox q.gen) (mull s vet sut gol dox r.gen))
  ::
      [%zpzp *]  (beth s vet gol %void)
  ==
::
++  beth
  |=  [s=sat vet=? gol=tip typ=tip]
  ^-  [[p=tip q=tip] sat]
  =^  a  s  (nice s vet gol typ)
  [[a typ] s]
::
++  cove                                                ::  axis of [0 *]
  |=  nug=nock
  ^-  @
  ?+  nug     ~>(%mean.'cove' !!)
    [%0 *]    p.nug
    [%11 *]   $(nug q.nug)
  ==
::
++  mile                                                ::  mull a core
  |=  $:  s=sat  vet=?  sut=tip  gol=tip  dox=tip
          nym=(unit term)  hud=poly  dom=(map term tome)
      ==
  ^-  [[p=tip q=tip] sat]
  =^  b  s  (base s sut nym hud dom)
  =^  d  s  (base s dox nym hud dom)
  =/  kin  ?:(vet %lazy %lazn)
  =/  yet  (core sut %gold (deck b kin))
  =/  hum  (core dox %gold (deck d kin))
  =?  s  ?=(%dry hud)
    |-  ^-  sat
    ?~  dom  s
    =.  s
      =/  dab=(map term hoon)  q.n.dom
      |-  ^-  sat
      ?~  dab  s
      =^  x  s  (mull s vet yet %noun hum q.n.dab)
      =.  s  $(dab l.dab)
      $(dab r.dab)
    =.  s  $(dom l.dom)
    $(dom r.dom)
  =^  typ  s  (nice s vet gol yet)
  [[typ hum] s]
::                                                      ::::  redo
++  redo                                                ::  faces of ref
  |=  [s=sat sut=tip ref=tip]                           ::  onto sut
  ^-  [tip sat]
  ~|  %redo-match
  (redo-dext s ~ [~ ~] ~ sut ref)
::
++  redo-dext
  |=  $:  s=sat
          hos=(list tool)
          wec=(list (list tool))
          gil=(list tap)
          sut=tip  ref=tip
      ==
  ^-  [tip sat]
  ?:  ?|  =(sut ref)
          ?=(?(%noun %void [?(%atom %core) *]) ref)
      ==
    [(redo-done sut hos wec) s]
  ?-    sut
      ?(%noun %void [?(%atom %core) *])
    =^  x  s  (redo-sint s sut wec & ref)
    [(redo-done sut hos q.x) s]
  ::
      [%cell *]
    =^  x  s  (redo-sint s sut wec & ref)
    =^  hed  s  (peek s p.x %free 2)
    =^  a  s  (redo-dext s ~ [~ ~] gil p.sut hed)
    =^  tal  s  (peek s p.x %free 3)
    =^  b  s  (redo-dext s ~ [~ ~] gil q.sut tal)
    [(redo-done [%cell a b] hos q.x) s]
  ::
      [%face *]  (redo-dext s [p.sut hos] wec gil q.sut ref)
      [%hint *]
    =^  x  s  (redo-dext s hos wec gil q.sut ref)
    [(hint s.sut n.sut x) s]
  ::
      [%fork *]  (redo-fork s ref hos wec gil ~ p.sut)
      [%hold *]
    =^  x  s  (redo-sint s sut wec | ref)
    ?:  (fans i.q.sut p.sut fan.s)
      =^  y  s  (redo-sint s sut q.x & p.x)
      [(redo-done sut hos q.y) s]
    ?:  (hath-two sut p.x gil)
      [(redo-done sut hos q.x) s]
    =^  rep  s  (repo s sut)
    =^  out  s  (redo-dext s hos q.x [[sut p.x] gil] rep p.x)
    [?:(=(out rep) sut out) s]
  ==
::
++  redo-fork
  |=  $:  s=sat  ref=tip
          hos=(list tool)
          wec=(list (list tool))
          gil=(list tap)
          out=(list tip)
          yed=(list fom)
      ==
  ^-  [tip sat]
  ?~  yed  [(fork out) s]
  =^  x  s  (redo-dext s hos wec gil t.i.yed ref)
  (redo-fork s ref hos wec gil [x out] t.yed)
::
++  redo-done                                           ::  put the faces on
  |=  [sut=tip hos=(list tool) wec=(list (list tool))]
  ^-  tip
  =/  lov=(list tool)
    ?~  wec  ~
    ?^  t.wec  ~>(%mean.'redo-dear-many' !!)
    =/  har  i.wec
    =/  p  (lent hos)
    =/  q  (lent har)
    =/  lip
      =|  lup=(unit @ud)
      =|  lip=@ud
      |-  ^-  @ud
      ?:  |((gth lip p) (gth lip q))
        (fall lup 0)
      =/  lep  (slag (sub p lip) hos)
      =/  lap  (scag lip har)
      $(lip +(lip), lup ?.(=(lep lap) lup `lip))
    (weld hos (slag lip har))
  |-  ^-  tip
  ?~  lov  sut
  $(lov t.lov, sut (face i.lov sut))
::
++  redo-sint                                           ::  reduce the ref
  |=  [s=sat sut=tip wec=(list (list tool)) hod=? ref=tip]
  ^-  [[p=tip q=(list (list tool))] sat]
  ?+    ref  [[ref wec] s]
      [%hint *]  (redo-sint s sut wec hod q.ref)
      [%face *]  (redo-sint s sut (tool-push p.ref wec) hod q.ref)
      [%fork *]
    =/  k  (fky sut)
    (redo-sint-fork s sut c.k m.k wec hod ~ ~ p.ref)
      [%hold *]
    ?.  hod  [[ref wec] s]
    =^  x  s  (repo s ref)
    (redo-sint s sut wec hod x)
  ==
::
++  redo-sint-fork                                      ::  sc: sut has the
  |=  $:  s=sat  sut=tip  sc=?  sk=@                    ::  constant sk at
          wec=(list (list tool))  hod=?                 ::  its head
          out=(list tip)  wex=(list (list tool))  moy=(list fom)
      ==
  ^-  [[p=tip q=(list (list tool))] sat]
  ?~  moy  [[(fork out) wex] s]
  ?:  &(sc c.i.moy !=(sk m.i.moy))
    (redo-sint-fork s sut sc sk wec hod out wex t.moy)
  =^  mis  s  (miss s sut t.i.moy)
  ?:  mis  (redo-sint-fork s sut sc sk wec hod out wex t.moy)
  =^  dis  s  (redo-sint s sut wec hod t.i.moy)
  (redo-sint-fork s sut sc sk wec hod [p.dis out] (tool-uni wex q.dis) t.moy)
::
++  tool-push                                           ::  onto each stack
  |=  [t=tool wec=(list (list tool))]
  ^-  (list (list tool))
  ?~  wec  ~
  [[t i.wec] (tool-push t t.wec)]
::
++  tool-uni                                            ::  union of stacks
  |=  [wex=(list (list tool)) new=(list (list tool))]
  ^-  (list (list tool))
  ?~  new  wex
  ?:  (tool-has i.new wex)  (tool-uni wex t.new)
  (tool-uni [i.new wex] t.new)
::
++  tool-has
  |=  [a=(list tool) wex=(list (list tool))]
  ^-  ?
  ?~  wex  |
  ?:  =(a i.wex)  &
  (tool-has a t.wex)
::
++  miss                                                ::  nothing in both
  |=  [s=sat sut=tip ref=tip]
  ^-  [? sat]
  (miss-dext s ~ sut ref)
::
++  miss-dext
  |=  [s=sat gil=(list tap) sut=tip ref=tip]
  ^-  [? sat]
  ?:  =(ref sut)  (nest s %void | sut)
  ?-    sut
      %void      [& s]
      %noun      (nest s %void | ref)
      [%atom *]  (miss-sint s gil sut ref)
      [%cell *]  (miss-sint s gil sut ref)
      [%core *]  (miss-sint s gil [%cell %noun %noun] ref)
      [%fork *]
    =/  k  (fky ref)
    (miss-fork s ref c.k m.k gil p.sut)
  ::
      [%face *]  (miss-dext s gil q.sut ref)
      [%hint *]  (miss-dext s gil q.sut ref)
      [%hold *]
    ?:  |((hath-two sut ref gil) (hath-two ref sut gil))  [& s]
    ::  a pair found to have a noun in common has it whatever is assumed,
    ::  and the last sixteen of the hold's arm are kept
    ::
    =/  y  (kee ref)
    ?:  (ness y sut ref mis:(aget tab.s x.q.sut))
      [| s]
    =^  x  s  (repo s sut)
    =^  a  s  (miss-dext s [[sut ref] gil] x ref)
    ?:  a  [& s]
    =/  r  (aget tab.s x.q.sut)
    [| s(tab (aput tab.s x.q.sut r(mis [[y sut ref] (scag 15 mis.r)])))]
  ==
::
++  miss-fork                                           ::  rc: ref has the
  |=  $:  s=sat  ref=tip  rc=?  rk=@                    ::  constant rk at
          gil=(list tap)  yed=(list fom)                ::  its head
      ==
  ^-  [? sat]
  ?~  yed  [& s]
  ::  two constants that differ have nothing in common
  ::
  ?:  &(rc c.i.yed !=(rk m.i.yed))  (miss-fork s ref rc rk gil t.yed)
  =^  a  s  (miss-dext s gil t.i.yed ref)
  ?.  a  [| s]
  (miss-fork s ref rc rk gil t.yed)
::
++  miss-sint
  |=  [s=sat gil=(list tap) sut=tip ref=tip]
  ^-  [? sat]
  ?+    ref  (miss-dext s gil ref sut)
      [%atom *]
    ?.  ?=([%atom *] sut)  [& s]
    [&(?=(^ q.ref) ?=(^ q.sut) !=(q.ref q.sut)) s]
  ::
      [%cell *]
    ?.  ?=([%cell *] sut)  [& s]
    =^  a  s  (miss-dext s gil p.sut p.ref)
    ?:  a  [& s]
    (miss-dext s gil q.sut q.ref)
  ==
::                                                      ::::  static evaluation
++  mush                                                ::  run a formula on
  |=  [s=sat sut=tip fol=*]                             ::  what is known
  ^-  [(unit *) sat]
  =^  noy  s  (araw s [%type sut ~] fol)
  ?~  noy  [~ s]
  =^  bus  s  (pith s noy)
  ?.  ?=(%full -.bus)  [~ s]
  [`p.bus s]
::
++  araw                                                ::  ~: stopped
  |=  [s=sat bus=pan fol=*]
  ^-  [$@(~ pan) sat]
  ?@  fol  [~ s]
  ?:  ?=(^ -.fol)
    =^  hed  s  (araw s bus -.fol)
    ?~  hed  [~ s]
    =^  tal  s  (araw s bus +.fol)
    ?~  tal  [~ s]
    [(duo hed tal) s]
  ?+    fol  [~ s]
      [%0 b=@]
    ?:  =(0 b.fol)  [~ s]
    (frag s b.fol bus)
  ::
      [%1 b=*]  [[%full b.fol] s]
      [%2 b=* c=*]
    =^  x  s  (araw s bus c.fol)
    =^  ryf  s  (want s x)
    ?~  ryf  [~ s]
    ?.  ?=(%full -.ryf)  [ryf s]
    =^  lub  s  (araw s bus b.fol)
    ?~  lub  [~ s]
    (araw s lub p.ryf)
  ::
      [%3 b=*]
    =^  x  s  (araw s bus b.fol)
    =^  fig  s  (want s x)
    ?~  fig  [~ s]
    ?.  ?=(%full -.fig)  [fig s]
    [[%full .?(p.fig)] s]
  ::
      [%4 b=*]
    =^  x  s  (araw s bus b.fol)
    =^  fig  s  (want s x)
    ?~  fig  [~ s]
    ?.  ?=(%full -.fig)  [fig s]
    ?^  p.fig  [~ s]
    [[%full +(p.fig)] s]
  ::
      [%5 b=* c=*]
    =^  x  s  (araw s bus b.fol)
    =^  hed  s  (want s x)
    ?~  hed  [~ s]
    ?.  ?=(%full -.hed)  [hed s]
    =^  y  s  (araw s bus c.fol)
    =^  tal  s  (want s y)
    ?~  tal  [~ s]
    ?.  ?=(%full -.tal)  [tal s]
    [[%full =(p.hed p.tal)] s]
  ::
      [%6 b=* c=* d=*]
    =^  x  s  (araw s bus b.fol)
    =^  fig  s  (want s x)
    ?~  fig  [~ s]
    ?.  ?=(%full -.fig)  [fig s]
    ?:  =(& p.fig)  (araw s bus c.fol)
    ?:  =(| p.fig)  (araw s bus d.fol)
    [~ s]
  ::
      [%7 b=* c=*]
    =^  one  s  (araw s bus b.fol)
    ?~  one  [~ s]
    (araw s one c.fol)
  ::
      [%8 b=* c=*]
    =^  one  s  (araw s bus b.fol)
    ?~  one  [~ s]
    (araw s (duo one bus) c.fol)
  ::
      [%9 b=* c=*]
    ?^  b.fol  [~ s]
    =^  one  s  (araw s bus c.fol)
    ?~  one  [~ s]
    =^  kno  s  (know s one)
    ?^  kno
      =/  pro  (mack u.kno [%9 b.fol %0 1])
      ?~  pro  [~ s]
      [[%full u.pro] s]
    =^  x  s  (frag s b.fol one)
    =^  ryf  s  (want s x)
    ?~  ryf  [~ s]
    ?.  ?=(%full -.ryf)  [ryf s]
    (araw s one p.ryf)
  ::
      [%10 [b=@ c=*] d=*]
    =^  tar  s  (araw s bus d.fol)
    ?~  tar  [~ s]
    =^  inn  s  (araw s bus c.fol)
    ?~  inn  [~ s]
    (muta s b.fol inn tar)
  ::
      [%11 @ c=*]  (araw s bus c.fol)
      [%11 [b=* c=*] d=*]
    =^  noy  s  (araw s bus c.fol)
    ?~  noy  [~ s]
    (araw s bus d.fol)
  ==
::
++  duo                                                 ::  cell of two
  |=  [hed=pan tal=pan]
  ^-  pan
  ?:  &(?=(%full -.hed) ?=(%full -.tal))  [%full p.hed p.tal]
  ?:  &(?=(%bloc -.hed) ?=(%bloc -.tal))  hed
  [%half hed tal]
::
++  want                                                ::  a whole noun: ~,
  |=  [s=sat noy=$@(~ pan)]                             ::  %bloc or %full
  ^-  [$@(~ pan) sat]
  ?~  noy  [~ s]
  =^  bus  s  (pith s noy)
  ?:  ?=(%full -.bus)  [bus s]
  [[%bloc ~] s]
::
++  pith                                                ::  settle a noun:
  |=  [s=sat bus=pan]                                   ::  no %lazy, %type
  ^-  [pan sat]
  ?-    -.bus
      %full  [bus s]
      %bloc  [bus s]
      %half
    =^  a  s  $(bus p.bus)
    =^  b  s  $(bus q.bus)
    [(duo a b) s]
  ::
      %lazy
    ?:  =(1 q.bus)  [[%bloc ~] s]
    =/  gen  (dom-at dom.bas.p.bus q.bus)
    ?~  gen  [[%bloc ~] s]
    =^  fol  s
      %:  hemp
        s
        (self p.bus)
        hud.bas.p.bus
        %noun
        (arx-gen arx.bas.p.bus u.gen)
        u.gen
      ==
    [[%full fol] s]
  ::
      %type
    =^  x  s  (peer s p.bus q.bus)
    $(bus x)
  ==
::
++  peer                                                ::  a type, one step
  |=  [s=sat sut=tip gil=(list tip)]
  ^-  [pan sat]
  ?-    sut
      %noun      [[%bloc ~] s]
      %void      [[%bloc ~] s]
      [%atom *]  [?~(q.sut [%bloc ~] [%full u.q.sut]) s]
      [%cell *]  [[%half [%type p.sut gil] [%type q.sut gil]] s]
      [%core *]
    :_  s
    :+  %half
      =/  m  sem.q.sut
      ?@  m  ?:(?=(%bunt m) [%bloc ~] [%lazy q.sut 1])
      ?-  -.m
        %full  [%full dez.m]
        %sock  (sock p.m)
      ==
    [%type p.sut gil]
  ::
      [%face *]  $(sut q.sut)
      [%fork *]  [[%bloc ~] s]
      [%hint *]  $(sut q.sut)
      [%hold *]
    ?:  (hath sut gil)  [[%bloc ~] s]
    =^  x  s  (repo s sut)
    $(sut x, gil [sut gil])
  ==
::
++  sock                                                ::  foreign seminoun
  |=  sem=seminoun
  ^-  pan
  =/  mas=stencil  mask.sem
  =/  dat  data.sem
  |-  ^-  pan
  ?-  -.mas
    %lazy  [%bloc ~]
    %full  ?~(blocks.mas [%full dat] [%bloc ~])
    %half  ?@  dat  [%bloc ~]
           (duo $(mas left.mas, dat -.dat) $(mas rite.mas, dat +.dat))
  ==
::
++  know                                                ::  if all is known
  |=  [s=sat bus=pan]                                   ::  with no arm to
  ^-  [(unit *) sat]                                    ::  compile, the noun
  ?-    -.bus
      %full  [`p.bus s]
      %bloc  [~ s]
      %lazy  [~ s]
      %half
    =^  a  s  $(bus p.bus)
    ?~  a  [~ s]
    =^  b  s  $(bus q.bus)
    ?~  b  [~ s]
    [`[u.a u.b] s]
  ::
      %type
    =/  sut  p.bus
    =/  gil  q.bus
    |-  ^-  [(unit *) sat]
    ?-    sut
        %noun      [~ s]
        %void      [~ s]
        [%atom *]  [q.sut s]
        [%cell *]
      =^  a  s  $(sut p.sut)
      ?~  a  [~ s]
      =^  b  s  $(sut q.sut)
      ?~  b  [~ s]
      [`[u.a u.b] s]
    ::
        [%core *]
      =/  m  sem.q.sut
      ?.  ?=([%full *] m)  [~ s]
      =^  b  s  $(sut p.sut)
      ?~  b  [~ s]
      [`[dez.m u.b] s]
    ::
        [%face *]  $(sut q.sut)
        [%fork *]  [~ s]
        [%hint *]  $(sut q.sut)
        [%hold *]
      ?:  (hath sut gil)  [~ s]
      =^  x  s  (repo s sut)
      $(sut x, gil [sut gil])
    ==
  ==
::
++  frag                                                ::  part at an axis
  |=  [s=sat axe=axis bus=pan]
  ^-  [$@(~ pan) sat]
  ?:  =(1 axe)  [bus s]
  ?-    -.bus
      %lazy  [bus(q (peg q.bus axe)) s]
      %bloc  [bus s]
      %full
    ?@  p.bus  [~ s]
    (frag s (mas axe) [%full ?:(=(2 (cap axe)) -.p.bus +.p.bus)])
  ::
      %half
    ?:  =(2 (cap axe))  (frag s (mas axe) p.bus)
    (frag s (mas axe) q.bus)
  ::
      %type
    =^  x  s  (peer s p.bus q.bus)
    (frag s axe x)
  ==
::
++  muta                                                ::  edit at an axis
  |=  [s=sat axe=@ lit=pan big=pan]
  ^-  [$@(~ pan) sat]
  ?:  =(0 axe)  [~ s]
  ?:  =(1 axe)  [lit s]
  |-  ^-  [$@(~ pan) sat]
  ?:  =(2 axe)
    =^  tal  s  (frag s 3 big)
    ?~  tal  [~ s]
    [(duo lit tal) s]
  ?:  =(3 axe)
    =^  hed  s  (frag s 2 big)
    ?~  hed  [~ s]
    [(duo hed lit) s]
  =^  hed  s  (frag s 2 big)
  ?~  hed  [~ s]
  =^  tal  s  (frag s 3 big)
  ?~  tal  [~ s]
  ?:  =(2 (cap axe))
    =^  mut  s  $(big hed, axe (mas axe))
    ?~  mut  [~ s]
    [(duo mut tal) s]
  =^  mut  s  $(big tal, axe (mas axe))
  ?~  mut  [~ s]
  [(duo hed mut) s]
::                                                      ::::  stock types
++  xpo                                                 ::  to a $type; bur:
  |=  [s=sat sut=tip bur=?]                             ::  as burp:ut
  ^-  [type sat]
  ?-    sut
      %noun      [%noun s]
      %void      [%void s]
      [%atom *]  [sut s]
      [%cell *]
    =^  a  s  $(sut p.sut)
    =^  b  s  $(sut q.sut)
    [[%cell a b] s]
  ::
      [%core *]
    =/  key  (mug sut)
    =/  lis  (fall (~(get by xpo.s) key) ~)
    =/  hit
      =/  l  lis
      |-  ^-  (unit type)
      ?~  l  ~
      ?:  &(=(b.i.l bur) =(p.i.l sut))  `q.i.l
      $(l t.l)
    ?^  hit  [u.hit s]
    =/  bas  bas.q.sut
    =^  ctx  s  $(sut ctx.bas)
    =^  pay  s  ?:(=(p.sut ctx.bas) [ctx s] $(sut p.sut))
    =/  m  sem.q.sut
    =/  sem=seminoun
      ?@  m
        ?:  |(bur ?=(%bunt m))  [[%full ~ ~ ~] ~]
        (~(laze %*(. ut vet ?=(%lazy m)) ctx) nym.bas hud.bas dom.bas)
      ?-  -.m
        %full  [[%full ~] dez.m]
        %sock  ?.(bur p.m ?:(?=([[%full ~] *] p.m) p.m [[%full ~ ~ ~] ~]))
      ==
    =/  out=type
      [%core pay [nym.bas hud.bas r.sut] ctx sem dom.bas]
    =/  lis  (fall (~(get by xpo.s) key) ~)
    [out s(xpo (~(put by xpo.s) key [[sut bur out] lis]))]
  ::
      [%face *]
    =^  a  s  $(sut q.sut)
    [[%face p.sut a] s]
  ::
      [%fork *]
    =/  yed  p.sut
    =|  out=(list type)
    |-  ^-  [type sat]
    ?~  yed  [[%fork (~(gas in *(set type)) out)] s]
    =^  x  s  (xpo s t.i.yed bur)
    $(yed t.yed, out [x out])
  ::
      [%hint *]
    =^  a  s  $(sut s.sut)
    =^  b  s  $(sut q.sut)
    [[%hint [a n.sut] b] s]
  ::
      [%hold *]
    =^  a  s  $(sut p.sut)
    [[%hold a gen.q.sut] s]
  ==
::
++  mpo                                                 ::  from a $type
  |=  [s=sat typ=type]
  ^-  [tip sat]
  ?-    typ
      %noun      [%noun s]
      %void      [%void s]
      [%atom *]  [typ s]
      [%cell *]
    =^  a  s  $(typ p.typ)
    =^  b  s  $(typ q.typ)
    [[%cell a b] s]
  ::
      [%core *]
    =/  key  (mug typ)
    =/  lis  (fall (~(get by mpo.s) key) ~)
    =/  hit
      =/  l  lis
      |-  ^-  (unit tip)
      ?~  l  ~
      ?:  =(p.i.l typ)  `q.i.l
      $(l t.l)
    ?^  hit  [u.hit s]
    =^  ctx  s  $(typ q.q.typ)
    =^  pay  s  ?:(=(p.typ q.q.typ) [ctx s] $(typ p.typ))
    =^  b  s  (base s ctx p.p.q.typ q.p.q.typ q.r.q.typ)
    =/  mas  p.r.q.typ
    =/  m=sem
      ?:  ?=([[%full ~] *] mas)  [%full data.mas]
      ?:  =([[%full ~ ~ ~] ~] mas)  %bunt
      [%sock mas]
    =/  out=tip  [%core r.p.q.typ (deck b m) pay]
    =/  lis  (fall (~(get by mpo.s) key) ~)
    [out s(mpo (~(put by mpo.s) key [[typ out] lis]))]
  ::
      [%face *]
    =^  a  s  $(typ q.typ)
    [[%face p.typ a] s]
  ::
      [%fork *]
    =/  yed  ~(tap in p.typ)
    =|  out=(list tip)
    |-  ^-  [tip sat]
    ?~  yed  [(fork out) s]
    =^  x  s  (mpo s i.yed)
    $(yed t.yed, out [x out])
  ::
      [%hint *]
    =^  a  s  $(typ p.p.typ)
    =^  b  s  $(typ q.typ)
    [[%hint q.p.typ a b] s]
  ::
      [%hold *]
    =^  a  s  $(typ p.typ)
    =/  q=$@(~ arm)
      ?.  ?=([%core *] a)  ~
      (arx-gen arx.bas.q.a q.typ)
    [[%hold ?^(q q [0 ~ %$ 0 q.typ]) a] s]
  ==
--
