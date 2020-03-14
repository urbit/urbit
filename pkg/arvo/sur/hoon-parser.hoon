|%
+$  rune-kind
  ?(%t %w %i)
+$  hoon-rune
  _-:*hoon
::
::  $hast: Hoon AST node
+$  hast
  $%
    [%core name=@ta head=(unit hast) arms=(list hast)]
    [%core-arm kind=@ta name=tape arm=hast]
    [%rune =rune-kind name=hoon-rune children=(list hast)]
    [%trace =pint child=hast]
    [%irregular-adjacent =tape child=hast]
    [%irregular =tape]
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
