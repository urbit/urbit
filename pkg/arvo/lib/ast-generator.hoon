/-  *hoon-parser
|%
::
++  sym-tape
  ;~(plug low (star ;~(pose nud low hep)))
::
++  all-skin
  |=  =hast
  ^-  skin
  ?+  -.hast  ~|('bad skin node' !!) :: skins don't have regular syntax
      %irregular-adjacent
    (irregular-adjacent-skin hast)
  ::
      %irregular
    (irregular-skin hast)
  ::
  ==
++  irregular-adjacent-skin
  |=  =hast
  ^-  skin
  ?>  ?=(%irregular-adjacent -.hast)
  %+  scan
    tape.hast
  ;~  pose
  ::
    %+  sear
      |=  =spec
      ^-  (unit skin)
      %+  bind  ~(autoname ax & spec)
      |=  =term
      [%name term %spec spec %base %noun]
    (cold (all-spec child.hast) (full tis))
  ::
    %+  cook
      |=  =term
      ^-  skin
      [%name term %spec (all-spec child.hast) %base %noun]
    ;~(sfix sym ;~(pose net tis))
  ::
  ==
++  irregular-skin
  |=  =hast
  ?>  ?=(%irregular -.hast)
  ^-  skin
  %+  scan
    tape.hast
  ;~  pose
    ;~  pfix  tis
      %+  sear
        |=  =spec
        ^-  (unit skin)
        %+  bind  ~(autoname ax & spec)
        |=  =term
        [%name term %spec spec %base %noun]
      scad:vast
    ==
  ::
    %+  cook
      |=  [=term =(unit tape)]
      ^-  skin
      ?~  unit
        term
      =/  =spec
        (irregular-spec [%irregular u.unit])
      [%name term %spec spec %base %noun]
    %-  full
    ;~  plug  sym
    ::  XX: net deprecated
      (punt ;~(pfix ;~(pose net tis) sym-tape))
    ==
  ::
    %+  cook
      |=  =tape
      ^-  skin
      =/  =spec
        (irregular-spec [%irregular tape])
      [%spec spec %base %noun]
    sym-tape
  ==


::
++  all-spec
  |=  =hast
  ^-  spec
  ?+  -.hast  ~|('bad node in spec' !!)
      %rune
    (rune-spec hast)
      %irregular
    (irregular-spec hast)
  ==

++  rune-spec
  |=  =hast
  ^-  spec
  ?>  ?=(%rune -.hast)
  |^
  ?+  name.hast  !!
      %cnkt
    (make 4)
      %cnls
    (make 3)
      %cnhp
    (make 2)
      %cncl
    ?~  children.hast  !!
    [%make (all i.children.hast) (turn t.children.hast all-spec)]
      ::
      %cltr
    ?~  children.hast  !!
    [%bscl (all-spec i.children.hast) (turn t.children.hast all-spec)]
  ==
  ::
  ++  make
    |=  arity=@ud
    ^-  spec
    ?>  =(arity (lent children.hast))
    ?~  children.hast  !!
    [%make (all i.children.hast) `(list spec)`(turn t.children.hast all-spec)]
  --


++  irregular
  |=  =hast
  ^-  hoon
  ?>  ?=(%irregular -.hast)
  (scan tape.hast scat:vast)

++  irregular-spec
  |=  =hast
  ^-  spec
  ?>  ?=(%irregular -.hast)
  (scan tape.hast scad:vast)
::
++  cncl  (rch-1h-lh %cncl)
++  brts  (rch-1sp-1h %brts)
++  tsnt  (rch-1sk-2h %tsnt)
++  kthp  (rch-1sp-1h %kthp)

++  rune
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?+  name.hast  ~|('bad rune' !!)
     %brts   (brts hast)
     %cncl   (cncl hast)
     %tsnt   (tsnt hast)
     %kthp   (kthp hast)
   ==

++  all
  |=  =hast
  ^-  hoon
  ?+  -.hast  !!
    %rune
      (rune hast)
    %irregular
      (irregular hast)
  ==
::  ++  rc-single
::    |*  kind=node-kind
::    ^-  $>(kind
::    ?-  kind
::  +$  to-hoon  $-(hast hoon)
::  +$  to-spec  $-(hast spec)
::  +$  to-skin  $-(hast skin)
:: Runechildren name layout:
:: rch -> runechildren returning hoon
:: 1sp -> 1 spec followed by
:: 1h ->  1 hoon
+$  rch-1sp-1h-tags  ?(%kthp %brts %mcmc %tsbr %zpld)
++  rch-1sp-1h
  |*  name=rch-1sp-1h-tags
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 2)
    !!
  =/  first=spec
    (all-spec (snag 0 children.hast))
  =/  second=hoon
    (all (snag 1 children.hast))
  [name first second]
+$  rch-1sk-2h-tags  ?(%tsnt %tsmc)
++  rch-1sk-2h
  |*  tag=rch-1sk-2h-tags
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 3)
    !!
  =/  first=skin
    (all-skin (snag 0 children.hast))
  =/  second=hoon
    (all (snag 1 children.hast))
  =/  third=hoon
    (all (snag 2 children.hast))
  [tag first second third]
::  lh - list of hoons
+$  rch-1h-lh-tags  ?(%cncl %mccl %mcsg)
++  rch-1h-lh
  |*  tag=rch-1h-lh-tags
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?~  children.hast
    !!
  [tag (all i.children.hast) (turn t.children.hast all)]

--
