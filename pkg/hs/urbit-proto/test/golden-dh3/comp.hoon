=/  comp
  |=  [a/$ b/$ c/$ f/$-(b c) g/$-(a b)]
  ^-  $-(a c)
  |=  x
  (f (g x))
=/  inc  |=  x/@  +(x)
=/  dup  |=  x/@   [x x]
=/  sly
  |=  [t/$ u/$]
  |=  [x/t y/u]
  [y y x]
::
:*  ((comp @ @ @ inc inc) 1)
    ((comp @ @ {@ @} dup inc) 9)
    ((comp @ {@ @} {@ @ @} (sly @ @) dup) 4)
    ((comp {^ @} {@ @ ^} @ |=(x +2.x) (sly ^ @)) [[23 24] 42])
==
