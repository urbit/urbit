::  toyhoon: xx
::
|%
+$  axis  @
+$  part  (each axis term)
+$  wing  (list part)
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
      [%sggr p=naty q=naty]                     ::  %11
    ::
      ::  hoon constructs
      ::
      [%brcn var=?(%gold %iron %lead) bat=(map term naty)]
    ::
      ::  type operations
      ::
      [%ktls example=naty =naty]
      [%bccb =naty]
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
      [%fork p=(set type)]
      [%hold p=type q=naty]
  ==
::
--
