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
  ^-  skin
  ?>  ?=(%irregular -.hast)
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
++  cncl
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?~  children.hast
    !!
  [%cncl (all i.children.hast) (turn t.children.hast all)]
::
++  brts
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 2)
    !!
  =/  sample=spec
    (all-spec (snag 0 children.hast))
  =/  product=hoon
    (all (snag 1 children.hast))
  [%brts sample product]
++  tsnt
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 3)
    !!
  =/  =skin
    (all-skin (snag 0 children.hast))
  =/  value=hoon
    (all (snag 1 children.hast))
  =/  rest=hoon
    (all (snag 2 children.hast))
  [%tsnt skin value rest]
::
++  rune
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?+  name.hast  !!
      %brts   (brts hast)
      %cncl   (cncl hast)
      %tsnt   (tsnt hast)
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



::  |_  [errors=(list @t) ~]
::  ++  transform
::    |=  =hast
::    ?+  -.hast  !!
::        ::
::        %rune




::  ++  check-rune-children
::    |=  [name=@ta children=(list hast)]

::  ++  get-core-arms
::    |=  arms=(list hast)
::    =|  tail=(map term hoon)
::    |-  ^-  [tail=(map term hoon)]
::    ?~  arms
::      ~
::    ?.  ?=(%core-arm -.i.arm)
::      =.  errors  ['Not arm' errors]
::      []


  

--
