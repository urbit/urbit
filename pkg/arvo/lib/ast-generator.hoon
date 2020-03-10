|%
+$  rune-kind
  ?(%t %w %i)
::
::  $hast: Hoon AST node
+$  hast
  $%
    [%core name=@ta head=(unit hast) arms=(list hast)]
    [%core-arm kind=@ta name=tape arm=hast]
    [%rune =rune-kind name=@ta children=(list hast)]
    [%irregular-adjacent =tape rune=hast]
    [%irregular =tape]
  ==
+$  transform-sample
  $%
    errors=(list @t)
    ~
  ==
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
    (transform-irregular-spec (snag 0 children.hast))
  =/  product=hoon
    (transform (snag 1 children.hast))
  [%brts sample product]
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
  ==

++  transform
  |=  =hast
  ^-  hoon
  ?+  -.hast  !!
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
