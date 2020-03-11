/-  *hoon-parser
|%
::
++  sym-tape
  ;~(plug low (star ;~(pose nud low hep)))

++  transform-skin
  |=  =hast
  ^-  skin
  ?+  -.hast  ~|('bad skin node' !!)
      %irregular-adjacent
    (transform-irregular-adjacent-skin hast)
  ::
      %irregular
    (transform-irregular-skin hast)
  ::
  ==
++  transform-irregular-adjacent-skin
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
    (cold (transform-spec child.hast) (full tis))
  ::
    %+  cook
      |=  =term
      ^-  skin
      [%name term %spec (transform-spec child.hast) %base %noun]
    ;~(sfix sym ;~(pose net tis))
  ::
  ==
++  transform-irregular-skin
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
        (transform-irregular-spec [%irregular u.unit])
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
        (transform-irregular-spec [%irregular tape])
      [%spec spec %base %noun]
    sym-tape
  ==


::
++  transform-spec
  |=  =hast
  ^-  spec
  ?+  -.hast  ~|('bad node in spec' !!)
      %rune
    (transform-rune-spec hast)
      %irregular
    (transform-irregular-spec hast)
  ==

++  transform-rune-spec
  |=  =hast
  ^-  spec
  ?>  ?=(%rune -.hast)
  |^
  ?+  name.hast  ~|('bad rune in spec' !!)
      ::  %cnkt
    ::  (build-make children.hast 4)
      ::  %cnls
    ::  (build-make children.hast 3)
      ::  %cnhp
    ::  (build-make children.hast 2)
      %cncl
    ?~  children.hast  !!
    [%make (transform i.children.hast) (turn t.children.hast transform-spec)]
      ::
      %cltr
    ?~  children.hast  !!
    [%bscl (transform-spec i.children.hast) (turn t.children.hast transform-spec)]

  ==
  ++  abc  %foo
  ::
  ::  ++  build-make
  ::    |=  [children=(list hast) arity=@]
  ::    ^-  spec
  ::    ?>  =((lent children) arity)
  ::    ?~  children  !!
  ::    [%make (transform i.children) (turn t.children.hast transform-spec)]
  --


++  transform-irregular
  |=  =hast
  ^-  hoon
  ?>  ?=(%irregular -.hast)
  (scan tape.hast scat:vast)

++  transform-irregular-spec
  |=  =hast
  ^-  spec
  ?>  ?=(%irregular -.hast)
  (scan tape.hast scad:vast)
++  transform-cncl
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?~  children.hast
    !!
  [%cncl (transform i.children.hast) (turn t.children.hast transform)]

:: The following rule is used for the naming of the transform functions
:: transform-
++  transform-brts
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 2)
    !!
  =/  sample=spec
    (transform-spec (snag 0 children.hast))
  =/  product=hoon
    (transform (snag 1 children.hast))
  [%brts sample product]
++  transform-tsnt
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?.  =((lent children.hast) 3)
    !!
  =/  =skin
    (transform-skin (snag 0 children.hast))
  =/  value=hoon
    (transform (snag 1 children.hast))
  =/  rest=hoon
    (transform (snag 2 children.hast))
  [%tsnt skin value rest]
::
++  transform-rune
  |=  =hast
  ^-  hoon
  ?>  ?=(%rune -.hast)
  ?+  name.hast  !!
      %brts
    (transform-brts hast)
      %cncl
    (transform-cncl hast)
      %tsnt
    (transform-tsnt hast)
  ==

++  transform
  |=  =hast
  ^-  hoon
  ?+  -.hast  ~|(<hast> !!)
    %rune
      (transform-rune hast)
    %irregular
      (transform-irregular hast)
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
