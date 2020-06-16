/-  *sole
/+  sole, default-agent
/+  drum=hood-drum, helm=hood-helm, kiln=hood-kiln
::
|%
+$  state
  $:  %7
      drum=state:drum
      helm=state:helm
      kiln=state:kiln
  ==
--
::
=>  |%
    +$  any-state  $%(state hood-old)
    ++  hood-old                                        ::  unified old-state
      {?($1 $2 $3 $4 $5 $6) lac/(map @tas hood-part-old)}
    ++  hood-1                                          ::  unified state
      {$6 lac/(map @tas hood-part)}
    ++  hood-good                                       ::  extract specific
      =+  hed=$:hood-head
      |@  ++  $
            |:  paw=$:hood-part
            ?-  hed
              $drum  ?>(?=($drum -.paw) `part:hood-drum`paw)
              $helm  ?>(?=($helm -.paw) `part:hood-helm`paw)
              $kiln  ?>(?=($kiln -.paw) `part:hood-kiln`paw)
              $write  ?>(?=($write -.paw) `part:hood-write`paw)
            ==
      --
    ++  hood-part-old
      $%  [%drum part-old:drum]
          [%helm part-old:helm]
          [%kiln part-old:kiln]
          [%write part-old:write]
      ==
    ++  hood-part
      $%  {$drum $2 pith-2:drum}
          {$helm $0 pith:helm}
          {$kiln $0 pith:kiln}
          {$write $0 pith:write}
      ==
    --
::
^-  agent:gall
=|  =state
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bol)
    drum-core  (drum bowl drum.state)
    helm-core  (helm bowl helm.state)
    kiln-core  (kiln bowl kiln.state)
::
++  on-fail   on-fail:def
++  on-init   on-init:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
::
++  on-save  !>(state)
++  on-load
  |=  =old-state=vase
  =/  old-state  !<(any-state old-state-vase)
  ::  TODO rewrite
  [~ this]
::  =^  cards  lac
::    =.  lac  lac.old-state
::    ?-    -.old-state
::        %1  ((wrap on-load):from-drum:(help hid) %1)
::        %2  ((wrap on-load):from-drum:(help hid) %2)
::        %3  ((wrap on-load):from-drum:(help hid) %3)
::        %4  ((wrap on-load):from-drum:(help hid) %4)
::        %5
::      =/  start  ..$:(from-kiln)
::      =/  old-kiln-part  (~(got by lac.old-state) %kiln)
::      ?>  ?=(%kiln -.old-kiln-part)
::      %-  ably
::      (on-load:(start hid *part:hood-kiln) old-kiln-part)
::    ::
::        %6  `lac
::    ==
::  [cards ..on-init]
::
++  on-poke
  |^
  |=  [=mark =vase]
  ^-  step:agent:gall
  ::
  =/  fin  (end 3 4 mark)
  ?:  =(%drum fin)  (poke-drum mark vase)
  ?:  =(%helm fin)  (poke-helm mark vase)
  ?:  =(%kiln fin)  (poke-kiln mark vase)
  ::
  ?+  mark  (on-poke:def mark vase)
    %atom       (poke-helm %helm-atom vase)
    %dill-belt  (poke-drum %drum-dill-belt vase)
    %dill-blit  (poke-drum %drum-dill-blit vase)
    %hood-sync  (poke-kiln %kiln-sync vase)
  ==
  ++  poke-drum  |=([mark vase] =^(c drum.state (poke:drum-core +<) [c this]))
  ++  poke-helm  |=([mark vase] =^(c helm.state (poke:helm-core +<) [c this]))
  ++  poke-kiln  |=([mark vase] =^(c kiln.state (poke:kiln-core +<) [c this]))
  --
::
++  on-watch
  |=  =path
  ^-  step:agent:gall
  ?+  path  (on-watch:def +<)
    [%drum *]  =^(c drum.state (peer:drum-core +<) [c this])
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-agent:drum-core +<) [c this]))
    [%helm *]  =^(c helm.state (take-agent:helm-core +<) [c this]))
    [%kiln *]  =^(c kiln.state (take-agent:kiln-core +<) [c this]))
  ==
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-arvo:drum-core +<) [c this]))
    [%helm *]  =^(c helm.state (take-arvo:helm-core +<) [c this]))
    [%kiln *]  =^(c kiln.state (take-arvo:kiln-core +<) [c this]))
  ==
--
