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
      [%core p=type var=?(%wet ?(%gold %iron %lead)) context=type bat=(map term naty)]
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
  ::TODO  nest check
  pro
::
++  mint
  |=  [sut=type gol=type =naty]
  ^-  [type nock]
  ?^  -.naty
    =/  g
      |-  ^-  [l=type r=type]
      ?+  gol  ~|(%mint-nice !!)
        %noun      [%noun %noun]
        [%cell *]  +.gol
        [%face *]  $(gol q.gol)
        [%hold *]  !!  ::TODO  expand and recur
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
    %brcn  :-  (nice gol [%core var.naty sut bat.naty])
           !!
  ::
    %brpt  :-  (nice gol [%core %wet sut bat.naty])
           !!
  ::
      %ktls
    =/  sam  $(naty p.naty)
    ::TODO  check type from recursion against -.sam
    [-.sam +:$(naty q.naty, gol -.sam)]
  ::
    %wtpt  !!
    %wtcn  !!
    %wtkt  !!
  ==
--
