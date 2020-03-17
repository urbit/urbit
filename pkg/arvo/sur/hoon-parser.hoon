|%
+$  rune-kind
  ?(%t %w %i)
+$  spec-rune
  _-:*spec
+$  hoon-rune
  _-:*hoon
+$  rune-name
  ?(spec-rune hoon-rune)
::
::  $hast: Hoon AST node
+$  hast
  $%
    [%core name=@ta head=(unit hast) arms=(list hast)]
    [%core-arm kind=@ta name=tape arm=hast]
    [%rune =rune-kind name=rune-name children=(list hast)]
    [%trace =pint child=hast]
    [%irregular-adjacent =tape child=hast]
    [%irregular-suffix child=hast =tape]
    [%irregular =tape]
    [%alas p=*]
  ==
+$  hast-core
  $>(%core hast)
+$  hast-core-arm
  $>(%core-arm hast)
+$  hast-rune
  $>(%rune hast)
+$  hast-irregular-adjacent
  $>(%irregular-adjacent hast)
+$  hast-irregular
  $>(%irregular hast)
--
