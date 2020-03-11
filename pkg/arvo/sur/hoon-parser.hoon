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
    [%irregular-adjacent =tape child=hast]
    [%irregular =tape]
  ==
--
