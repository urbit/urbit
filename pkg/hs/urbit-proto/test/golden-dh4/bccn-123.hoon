=/  one  $1
=/  two  ?(%1 %2)
=/  tre  ?(%1 %2 %3)
=/  ott
  $%  [%one one]
      [%two two]
      [%tre tre]
  ==
%.  [%one 1]
|=  t=ott
^-  @
?-  t
  [_ 1]     1
  [%two 2]  2
  [%tre 2]  3
  [%tre 3]  3
==
