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
  $^  [naty naty]
  $%  ::  nock operations
      ::
      [%look =wing]                             ::  %0
      [%noun =type =noun]                       ::  %1
      [%dttr p=naty q=naty]                     ::  %2
      [%dtwt =naty]                             ::  %3
      [%dtls =naty]                             ::  %4
      [%dtts p=naty q=naty]                     ::  %5
      [%wtcl p=naty q=naty r=naty]              ::  %6
      [%tsgr p=naty q=naty]                     ::  %7
      [%tsls p=naty q=naty]                     ::  %8
      [%pull =axis =naty]                       ::  %9
      [%cnts p=naty q=(list (pair wing naty))]  ::  %10
      [%sggr tag=$@(@ (pair @ naty)) p=naty q=naty]                     ::  %11
    ::
      ::  hoon constructs
      ::
      [%brcn var=?(%gold %iron %lead) bat=(map term naty)]
      [%brpt bat=(map term naty)]
    ::
      ::  type operations
      ::
      [%ktls example=naty =naty]
      ::[%bccb =naty]
      [%wtpt =wing y=hoon n=hoon]
      [%wtcn =wing tom=hoon y=hoon n=hoon]
      [%wtkt =wing y=hoon n=hoon]
  ==
::
+$  type
  $~  %noun
  $@  $?  %noun
          %void
      ==
  $%  [%atom p=term q=(unit @)]
      [%cell p=type q=type]
      [%core p=type variance=?(%wet ?(%gold %iron %lead)) context=type arms=(map term naty)]
      [%face p=term q=type]
      [%bcpt tom=type cel=type]
      [%bccn p=(map @ type)]
      [%bckt cel=type tom=type]
      [%bcwt p=(map @ aura)]
      [%hold p=type q=naty]
  ==
::
++  mint
  |=  [=type =naty]
  ^-  [type nock]
  ?^  naty  
    =/  l  $(naty -.naty)
    =/  r  $(naty +.naty)
    [[%cell -.l -.r] +.l +.r]
  ?+  -.naty  !!
    ::
      %look  !!
    ::
      %noun  [type.naty %1 noun.naty]
    ::
      %dttr  [%noun %2 +:$(naty p.naty) +:$(naty q.naty)]
    ::
      %dtwt  :-  [%bcwt (my [%& %f] [%| %f] ~)] 
           [%3 +:$(naty naty.naty)]
    ::
      %dtls  :-  [%atom %$ ~] 
           [%4 +:$(naty naty.naty)]
    ::
      %dtts  :-  [%bcwt (my [%& %f] [%| %f] ~)] 
           [%5 +:$(naty p.naty) +:$(naty q.naty)]
    ::
      %wtcl  !!
    ::
      %tsgr  
    =/  sub  $(naty p.naty)
    =/  pro  $(naty q.naty, type -.sub)
    :-  -.pro  
    [%7 +.sub +.pro]
    ::
      %tsls
    =/  sub  $(naty p.naty)
    =/  pro  $(naty q.naty, type [%cell -.sub type])
    :-  -.pro  
    [%8 +.sub +.pro]
    ::
      %pull
    !!
    ::
      %cnts
    !!
    ::
  ==
--
