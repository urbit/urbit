::  toyhoon: xx
::
|%
+$  axis  @
+$  part  (each axis term)
+$  wing  (list part)
+$  aura-tree  $@(aura [aura-tree aura-tree])
::  naty: natural runes
::
::    each of these represents a digraph followed by its "sub runes".
::    later, we will have a type that also has synthetic runes.
::
+$  naty
  $~  [%look [&+1]~]
  $^  [naty naty]
  $%  ::  nock operations
      ::
      [%look =wing]                               ::  %0
      [%noun =type =noun]                         ::  %1
      [%dttr p=naty q=naty]                       ::  %2
      [%dtwt p=naty]                              ::  %3
      [%dtls p=naty]                              ::  %4
      [%dtts p=naty q=naty]                       ::  %5
      [%wtcl p=naty q=naty r=naty]                ::  %6
      [%tsgr p=naty q=naty]                       ::  %7
      [%tsls p=naty q=naty]                       ::  %8
      [%pull =axis =naty]                         ::  %9
      [%cnts =naty diff=(list (pair wing naty))]  ::  %10
      [%sggr tag=$@(@ (pair @ naty)) =naty]       ::  %11
    ::
      ::  hoon constructs
      ::
      [%brcn var=?(%gold %iron %lead) bat=(map term naty)]
      [%brpt bat=(map term naty)]
    ::
      ::  type operations
      ::
      [%ktls p=naty q=naty]
      ::[%bccb =naty]
      [%wtpt =wing y=naty n=naty]
      [%wtcn =wing tom=naty y=naty n=naty]
      [%wtkt =wing y=naty n=naty]
  ==
::
+$  type
  $~  %noun
  $@  $?  %noun
          %void
      ==
  $%  [%atom p=term q=(unit @)]
      [%cell p=type q=type]
      [%core p=type var=?(%wet ?(%gold %iron %lead)) pay=type bat=(map term naty)]
      [%face p=term q=type]
      [%bcpt tom=type cel=type]
      [%bccn p=(map @ [=aura type=$~(%noun type)])]  ::NOTE  strange compiler bug
      [%bckt cel=type tom=type]
      [%bcwt p=(map @ aura)]
      [%hold p=type q=naty]
  ==
::
++  nice
  |=  [gol=type pro=type]
  ^-  type
  ?>  (nest gol pro)
  pro
::
++  nest
  |=  [sut=type ref=type]
  =|  cil=(set [$>(%core type) $>(%core type)])
  =|  seg=(set type)
  =|  reg=(set type)
  =|  gil=(set [type type])
  =<  dext
  |%
  ++  deem
    |=  [mel=?(%gold %iron %lead) ram=?(%gold %iron %lead)]
    ^-  ?
    ?.  |(=(mel ram) =(%lead mel) =(%gold ram))  |
    ?-  mel
      %lead  &
      %gold  &(dext dext(sut ref, ref sut)) ::meet
      %iron  dext(sut (peek ref %rite 2), ref (peek sut %rite 2))
    ==
  ::
  ++  deep
    |=  $:  sat=(map term naty)
            rat=(map term naty)
        ==
    ^-  ?
    ?~  sat  =(~ rat)
    ?~  rat  |
    ?&  =(p.n.sat p.n.rat)
        $(sat l.sat, rat l.rat)
        $(sat r.sat, rat r.rat)
        dext(sut (play sut q.n.sat), ref (play ref q.n.rat))
    ==
  ::
  ++  dext
    ^-  ?
    ?:  =(sut ref)  &
    ?@  sut
      ?-  sut
        %noun  &
        %void  sint
      ==
    ?-  -.sut
    ::
      %atom  ?.  ?=([%atom *] ref)  sint
             ?&  (fitz p.sut p.ref)
                 |(?=(~ q.sut) =(q.sut q.ref))
             ==
    ::
      %cell  ?.  ?=([%cell *] ref)  sint
             ?&  dext(sut p.sut, ref p.ref)
                 dext(sut q.sut, ref q.ref)
             ==
    ::
      %core
             ?.  ?=([%core *] ref)  sint
             ?:  =(+>.sut +>.ref)  dext(sut p.sut, ref p.ref)
             ?:  ?=(%wet var.sut)
                 ?&(=(%wet var.ref) =(bat.sut bat.ref))
             ?:  ?=(%wet var.ref)  |
             ?&
               &(dext(sut pay.sut, ref p.sut) dext(sut p.sut, ref pay.sut)) ::meet
               dext(sut pay.ref, ref p.ref)
               (deem(sut pay.sut, ref pay.ref) var.sut var.ref)
               ?|  (~(has in cil) [sut ref])
                   %.  [bat.sut bat.ref]
                   %=  deep
                     cil  (~(put in cil) [sut ref])
                     sut  sut(p pay.sut, var %gold)
                     ref  ref(p pay.ref, var %gold)
               ==  ==
             ==
    ::
      %face  dext(sut q.sut)
    ::
      %bcpt  ?.  ?=(?([%atom *] %noun [%cell *] [%core *]) ref)  sint
             |(dext(sut tom.sut) dext(sut cel.sut))
    ::
      %bccn  ?.  ?=(?(%noun [%cell *] [%core *]) ref)  sint
             %-  ~(any in p.sut)
             |=([a=@ b=aura c=type] dext(sut [%cell [%atom b `a] c]))
    ::
      %bckt  ?.  ?=(?(%noun [%cell *] [%core *]) ref)  sint
             |(dext(sut cel.sut) dext(sut tom.sut))
    ::
      %bcwt  ?.  ?=([%atom *] ref)  sint
            (~(any in p.sut) |=([a=@ b=aura] dext(sut [%atom b `a])))
    ::
      %hold  ?:  (~(has in seg) sut)  |
             ?:  (~(has in gil) [sut ref])  &
             %=  dext
               sut  (drop sut)
               seg  (~(put in seg) sut)
               gil  (~(put in gil) [sut ref])
             ==
    ==
  ++  sint
    ^-  ?
    ?@  ref
      ?-  ref
        %noun  |
        %void  &
      ==
    ?-  -.ref
    ::
      %atom  |
    ::
      %cell  |
    ::
      %core  dext(ref [%cell %noun p.ref])
    ::
      %face  dext(ref q.ref)
    ::
      %bcpt  &(dext(ref tom.ref) dext(ref cel.ref)) :: unvalidated but so what
    ::
      %bccn  %-  ~(all in p.ref)
             |=([a=@ b=aura c=type] dext(ref [%cell [%atom b `a] c]))
    ::
      %bckt  &(dext(ref cel.ref) dext(ref tom.ref)) :: unvalidated but so what
    ::
      %bcwt  (~(all in p.ref) |=([a=@ b=aura] dext(ref [%atom b `a])))
    ::
      %hold  ?:  (~(has in reg) ref)  &
             ?:  (~(has in gil) [sut ref])  &
             %=  dext
               ref  (drop ref)
               reg  (~(put in reg) ref)
               gil  (~(put in gil) [sut ref])
             ==
    ==
  --
::
++  peek
  |=  [sut=type way=?(%read %rite %both %free) axe=axis]
  ^-  type
  ?:  =(1 axe)  sut
  =+  [now=(cap axe) lat=(mas axe)]
  =|  gil=(set type)
  |-  ^-  type
  ?@  sut  sut
  ?-  -.sut
      %atom  %void
      %cell  ?:(=(2 now) ^$(sut p.sut, axe lat) ^$(sut q.sut, axe lat))
      %face  $(sut q.sut)
      %bcpt  %noun
      %bccn  ?:(&(=(2 now) !=(2 axe)) %void %noun)
      %bckt  %noun
      %bcwt  %void
      %hold  $(sut (drop sut))
      %core  ?.  =(2 now)  %noun
             ?.  |(?=(%gold var.sut) ?=(%wet var.sut) ?=(%free way))
               %noun
             ^$(axe lat, sut pay.sut)
  ==
++  drop
  |=  $=  sut
      $~  [%hold %void *naty]
      $>(%hold type)
  ^-  type
  (play +.sut)
::
++  play
  |=  [sut=type =naty]
  ^-  type
  -:(mint sut %noun naty)
::
++  mint
  |=  [sut=type gol=type =naty]
  ^-  [type nock]
  ?^  -.naty
    =/  g
      =|  sog=(set type)
      |-  ^-  [l=type r=type]
      ?+  gol  ~|(%mint-nice !!)
        %noun      [%noun %noun]
        [%cell *]  +.gol
        [%face *]  $(gol q.gol)
        [%hold *]  ?:  (~(has in sog) gol)  !! ::xx do we crash?
                   %=  $
                     gol  (drop gol)
                     sog  (~(put in sog) gol)
                   ==
      ==
    =/  l  $(naty -.naty, gol l.g)
    =/  r  $(naty +.naty, gol r.g)
    [[%cell -.l -.r] +.l +.r]
  ?-  -.naty
      %look  !!
  ::
      %noun  [(nice gol type.naty) %1 noun.naty]
  ::
      %dttr  [%noun %2 +:$(naty p.naty) +:$(naty q.naty)]
  ::
      %dtwt  :-  (nice gol [%bcwt (my [%& %f] [%| %f] ~)])
             [%3 +:$(naty p.naty)]
  ::
      %dtls  :-  (nice gol [%atom %$ ~])
             [%4 +:$(naty p.naty)]
  ::
      %dtts  :-  (nice gol [%bcwt (my [%& %f] [%| %f] ~)])
             [%5 +:$(naty p.naty) +:$(naty q.naty)]
  ::
      %wtcl  !!
  ::
      %tsgr
    =/  sub  $(naty p.naty, gol %noun)
    =/  pro  $(naty q.naty, sut -.sub)
    :-  -.pro
    [%7 +.sub +.pro]
  ::
      %tsls
    =/  sub  $(naty p.naty, gol %noun)
    =/  pro  $(naty q.naty, sut [%cell -.sub sut])
    :-  -.pro
    [%8 +.sub +.pro]
  ::
      %pull
    !!
  ::
      %cnts
    !!
  ::
      %sggr
    =/  pro  $(naty naty.naty)
    :-  -.pro
    ?@  tag.naty
      [%11 tag.naty +.pro]
    [%11 [p.tag.naty +:$(naty q.tag.naty)] +.pro]
  ::
    %brcn  :-  (nice gol [%core sut var.naty sut bat.naty])
           !!
  ::
    %brpt  :-  (nice gol [%core sut %wet sut bat.naty])
           !!
  ::
      %ktls
    =/  sam  $(naty p.naty, gol %noun)
    ::TODO  check type from recursion against -.sam
    [-.sam +:$(naty q.naty, gol -.sam)]
  ::
    %wtpt  !!
    %wtcn  !!
    %wtkt  !!
  ==
::  +xxxx: makes a union from two types, if possible
::  ?(%foo %bar) <- type union
::  ?(@ud @ux)   <- aura union ?
::
++  make-union
  |=  [a=type b=type]
  ^-  type
  ?:  ?=(%void a)  b
  ?:  ?=(%void b)  a
  ::NOTE  two-sided nest handles %noun types
  ?:  (nest a b)   a
  ?:  (nest b a)   b
  ::  %hold expansion
  ::TODO  track these y/n?
  ::
  ?:  ?=([%hold *] a)  $(a (drop a))
  ?:  ?=([%hold *] b)  $(b (drop b))
  ::  keep faces only if they match
  ::
  ?:  ?=([%face *] a)
    ?:  ?=([%face *] b)
      ?:  =(p.a p.b)
        a(q $(a q.a, b q.b))
      $(a q.a, b q.b)
    $(a q.a)
  ?:  ?=([%face *] b)
    $(b q.b)
  ::
  ?:  (is-atomic a)
    ?:  (is-atomic b)
      ?:  &((is-cat a) (is-cat b))
        =+  c=(cat-map a)
        =+  d=(cat-map b)
        :-  %bcwt
        %-  (~(uno by c) d)
        |=  [k=@ a=aura b=aura]
        (aura-union a b)
      [%atom (aura-union (extract-aura a) (extract-aura b)) ~]
    [%bcpt a b]
  ?:  (is-atomic b)
    [%bcpt b a]
  ::
  ::NOTE  remember, a and b could be %cores...
  ::
  =+  ha=(has-atomic-head a)
  =+  hb=(has-atomic-head b)
  ?:  &(ha !hb)  [%bckt b a]
  ?:  &(!ha hb)  [%bckt a b]
  ?:  &(ha hb)
    ?:  &((has-cat-head a) (has-cat-head b))
      =+  c=(cat-head-map a)
      =+  d=(cat-head-map b)
      :-  %bccn
      %-  (~(uno by c) d)
      |=  [k=@ a=[=aura =type] b=[=aura =type]]
      [(aura-union aura.a aura.b) ^$(a type.a, b type.b)]
    ::  both cell types
    ::  both atomic heads
    ::  one of them not cat head, could be %bccn or %bckt
    :+  %cell
      $(a (peek a %free 2), b (peek b %free 2))
    $(a (peek a %free 3), b (peek b %free 3))
  ::  neither has atomic head
  ::  one could be %bckt
  :+  %cell
    $(a (peek a %free 2), b (peek b %free 2))
  $(a (peek a %free 3), b (peek b %free 3))
::
++  is-cat  ::  Constant Atomic Type
  |=  a=type
  ?=(?([%atom @ ~ @] [%bcwt *]) a)
++  is-atomic
  |=  a=type
  ?=(?(%atom %bcwt) -.a)
++  cat-map
  |=  a=type
  ^-  (map @ aura)
  ?+  a  ~
    [%atom *]  [(need q.a)^p.a ~ ~]
    [%bcwt *]  p.a
  ==
++  cat-head-map
  |=  a=type
  ^-  (map @ [aura type])
  ?+  a  ~
    ?([%atom *] [%bcwt *] [%face *] [%hold *])  !!
    [%bccn *]  p.a
  ::
      [%cell *]
    =|  gil=(set type)
    |-
    ?+  p.a  ~
      [%face *]  $(p.a q.p.a)
      [%atom *]  [[(need q.p.a) [p.p.a q.a]] ~ ~]
      [%bcwt *]  (~(run by p.p.a) (late q.a))
    ::
        [%hold *]
      ?:  (~(has in gil) p.a)  ~|(%hah-hold !!)
      $(p.a (drop p.a), gil (~(put in gil) p.a))
    ==
  ==
++  has-cat-head
  |=  a=type
  ^-  ?
  ?+  a  |
    ?([%atom *] [%bcwt *] [%face *] [%hold *])  !!
    [%bccn *]  &
  ::
      [%cell *]
    =|  gil=(set type)
    |-
    ?+  p.a  (is-cat p.a)
      [%face *]  $(p.a q.p.a)
    ::
        [%hold *]
      ?:  (~(has in gil) p.a)  ~|(%hah-hold !!)
      $(p.a (drop p.a), gil (~(put in gil) p.a))
    ==
  ==
++  has-atomic-head
  |=  a=type
  ^-  ?
  ?+  a  |
    ?([%atom *] [%bcwt *] [%face *] [%hold *])  !!
    [%bccn *]  &
  ::
      [%cell *]
    =|  gil=(set type)
    |-
    ?+  p.a  (is-atomic p.a)
      [%face *]  $(p.a q.p.a)
    ::
        [%hold *]
      ?:  (~(has in gil) p.a)  ~|(%hah-hold !!)
      $(p.a (drop p.a), gil (~(put in gil) p.a))
    ==
  ==
++  extract-aura
  |=  a=type
  ?:  ?=(%atom -.a)  p.a
  ?>  ?=(%bcwt -.a)
  =+  b=(sy ~(val by p.a))
  ?:(?=([* ~ ~] b) n.b %$)
++  aura-union
  |=  [a=aura b=aura]
  ^-  aura
  ?:  |((fitz a b) (fitz b a))
    (min a b)  ::  shortest & alphabetical
  %$
--
