::  toyhoon: xx
::
|%
+$  axis  $~(1 @)
::TODO  custom $limb (and $wing) type that doesn't have a silly (unit term) case
::  naty: natural runes
::
::    each of these represents a digraph followed by its "sub runes".
::    later, we will have a type that also has synthetic runes.
::
+$  naty
  $~  [%noun %noun 0]  ::[%look [&+1]~]
  $^  [naty naty]
  $%  ::  nock operations
      ::
      :: [%look =wing]                               ::  %0
      [%noun =type =noun]                         ::  %1
      [%dttr p=naty q=naty]                       ::  %2
      [%dtwt p=naty]                              ::  %3
      [%dtls p=naty]                              ::  %4
      [%dtts p=naty q=naty]                       ::  %5
      [%wtcl p=naty q=naty r=naty]                ::  %6
      [%tsgr p=naty q=naty]                       ::  %7
      [%tsls p=naty q=naty]                       ::  %8
      :: [%pull =axis =naty]                         ::  %9
      [%cnts =wing diff=(list (pair wing naty))]  ::  %10
      [%sggr tag=$@(@ (pair @ naty)) =naty]       ::  %11
    ::
      ::  hoon constructs
      ::
      [%brcn var=?(%gold %iron %lead) lay=(unit layout) bat=(map term naty)]
      [%brpt lay=(unit layout) bat=(map term naty)]
    ::
      ::  type operations
      ::
      [%ktls p=naty q=naty]
      ::[%bccb =naty]
      [%wtpt =wing y=naty n=naty]
      [%wtcn =wing tom=@ y=naty n=naty]
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
      ::  .p: current payload type
      ::  .pay: formal payload type
      ::TODO  rename .cur and .for ?
      [%core p=type var=?(%wet ?(%gold %iron %lead)) pay=type bat=naty nam=(map term axis)]
      [%face p=term q=type]
      [%bcpt tom=type cel=type]
      [%bccn p=(map @ [=aura type=$~(%noun type)])]  ::NOTE  strange compiler bug
      [%bckt cel=type tom=type]
      [%bcwt p=(map @ aura)]
      [%hold p=type q=naty]
  ==
+$  vase  (pair type noun)
::
++  dupe  |$  [a]  (pair a a)
+$  layout  $@(term ?([~ ~] (dupe layout)))
--
