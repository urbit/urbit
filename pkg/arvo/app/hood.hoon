/+  default-agent
/+  drum=hood-drum, helm=hood-helm, kiln=hood-kiln
|%
+$  state
  $~  [%26 *state:drum *state:helm *state:kiln]
  $>(%26 any-state)
::
+$  any-state
  $%  [ver=?(%1 %2 %3 %4 %5 %6) lac=(map @tas fin-any-state)]
      [%7 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%8 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%9 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%10 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%11 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%12 drum=state-2:drum helm=state-1:helm kiln=state-0:kiln]
      [%13 drum=state-2:drum helm=state-1:helm kiln=state-1:kiln]
      [%14 drum=state-2:drum helm=state-1:helm kiln=state-1:kiln]
      [%15 drum=state-2:drum helm=state-1:helm kiln=state-2:kiln]
      [%16 drum=state-4:drum helm=state-1:helm kiln=state-3:kiln]
      [%17 drum=state-4:drum helm=state-1:helm kiln=state-4:kiln]
      [%18 drum=state-4:drum helm=state-1:helm kiln=state-5:kiln]
      [%19 drum=state-4:drum helm=state-1:helm kiln=state-6:kiln]
      [%20 drum=state-4:drum helm=state-1:helm kiln=state-7:kiln]
      [%21 drum=state-4:drum helm=state-1:helm kiln=state-8:kiln]
      [%22 drum=state-4:drum helm=state-1:helm kiln=state-9:kiln]
      [%23 drum=state-4:drum helm=state-2:helm kiln=state-9:kiln]
      [%24 drum=state-4:drum helm=state-2:helm kiln=state-10:kiln]
      [%25 drum=state-5:drum helm=state-2:helm kiln=state-10:kiln]
      [%26 drum=state-6:drum helm=state-2:helm kiln=state-10:kiln]
  ==
+$  any-state-tuple
  $:  drum=any-state:drum
      helm=any-state:helm
      kiln=any-state:kiln
  ==
+$  fin-any-state
  $%  [%drum any-state:drum]
      [%helm any-state:helm]
      [%kiln any-state:kiln]
      [%write *]  ::  gets deleted
  ==
--
^-  agent:gall
=|  =state
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    drum-core  (drum bowl drum.state)
    helm-core  (helm bowl helm.state)
    kiln-core  (kiln bowl kiln.state)
::
++  on-fail   on-fail:def
++  on-init
  ^-  step:agent:gall
  =^  h  helm.state  on-init:helm-core
  =^  d  drum.state  on-init:drum-core
  =^  k  kiln.state  on-init:kiln-core
  [:(welp d k) this]
::
++  on-leave  on-leave:def
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
    [* %kiln *]  (on-peek:kiln-core path)
  ==
::
++  on-save   !>(state)
++  on-load
  |=  =old-state=vase
  ^-  step:agent:gall
  =+  !<(old=any-state old-state-vase)
  =/  tup=any-state-tuple
    ?+    -.old  +.old
        ?(%1 %2 %3 %4 %5 %6)
      :*  =-(?>(?=(%drum -<) ->) (~(got by lac.old) %drum))
          =-(?>(?=(%helm -<) ->) (~(got by lac.old) %helm))
          =-(?>(?=(%kiln -<) ->) (~(got by lac.old) %kiln))
      ==
    ==
  =^  d  drum.state  (on-load:(drum bowl *state:drum) -.old drum.tup)
  =^  h  helm.state  (on-load:(helm bowl *state:helm) -.old helm.tup)
  =^  k  kiln.state  (on-load:(kiln bowl *state:kiln) -.old kiln.tup)
  [:(welp d h k) this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  step:agent:gall
  |^
  =/  fin  (end [3 4] mark)
  ?:  =(%drum fin)  poke-drum
  ?:  =(%helm fin)  poke-helm
  ?:  =(%kiln fin)  poke-kiln
  ::
  ?+  mark  (on-poke:def mark vase)
    %atom            poke-helm(mark %helm-atom)
    %dill-poke       poke-drum
    %hood-sync       poke-kiln(mark %kiln-sync)
    %write-sec-atom  poke-helm(mark %helm-write-sec-atom)
  ==
  ++  poke-drum  =^(c drum.state (poke:drum-core mark vase) [c this])
  ++  poke-helm  =^(c helm.state (poke:helm-core mark vase) [c this])
  ++  poke-kiln  =^(c kiln.state (poke:kiln-core mark vase) [c this])
  --
::
++  on-watch
  |=  =path
  ^-  step:agent:gall
  ?+  path  (on-watch:def +<)
    [%drum *]  =^(c drum.state (peer:drum-core t.path) [c this])
    [%kiln *]  =^(c kiln.state (peer:kiln-core t.path) [c this])
    [%dill *]  =^(c drum.state (peer:drum-core +<) [c this])
  ==
::
++  on-agent
  |=  [=wire syn=sign:agent:gall]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-agent:drum-core t.wire syn) [c this])
    [%helm *]  =^(c helm.state (take-agent:helm-core t.wire syn) [c this])
    [%kiln *]  =^(c kiln.state (take-agent:kiln-core t.wire syn) [c this])
  ==
::
++  on-arvo
  |=  [=wire syn=sign-arvo]
  ^-  step:agent:gall
  ?+  wire  ~|([%hood-bad-wire wire] !!)
    [%drum *]  =^(c drum.state (take-arvo:drum-core t.wire syn) [c this])
    [%helm *]  =^(c helm.state (take-arvo:helm-core t.wire syn) [c this])
    [%kiln *]  =^(c kiln.state (take-arvo:kiln-core t.wire syn) [c this])
  ==
--
