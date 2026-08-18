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
+$  naty        (naty-xtra ~)
+$  naty-sugar  (naty-xtra sugar)
::
+$  sugar
  $~  [%brcl [%noun %noun 0] [%noun %noun 0]]
  $%  [%brcl sam=naty-sugar bod=naty-sugar]
      [%atom aura=term]
  ==
::
++  naty-xtra
  |$  [xtra]
  $~  [%noun %noun 0]  ::[%look [&+1]~]
  $^  [$ $]
  $%  ::  nock operations
      ::
      :: [%look =wing]                         ::  %0
      [%noun =type =noun]                      ::  %1
      [%dttr p=$ q=$]                          ::  %2
      [%dtwt p=$]                              ::  %3
      [%dtls p=$]                              ::  %4
      [%dtts p=$ q=$]                          ::  %5
      [%wtcl p=$ q=$ r=$]                      ::  %6
      [%tsgr p=$ q=$]                          ::  %7
      [%tsls p=$ q=$]                          ::  %8
      :: [%pull =axis =$]                      ::  %9
      [%cnts =wing diff=(list (pair wing $))]  ::  %10
      [%sggr tag=$@(@ (pair @ $)) naty=$]      ::  %11
    ::
      ::  hoon constructs
      ::
      [%brcn var=?(%gold %iron %lead) lay=(unit layout) bat=(map term $)]
      [%brpt lay=(unit layout) bat=(map term $)]
    ::
      ::  type operations
      ::
      [%ktls p=$ q=$]
      ::[%bccb =$]
      [%wtpt =wing y=$ n=$]
      [%wtcn =wing tom=@ y=$ n=$]
      [%wtkt =wing y=$ n=$]
    ::
      [%xtra xtra]
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
