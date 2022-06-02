=>
|%
++  list
  ^-  $-($ $)
  |=  a=$
  $%  [%nil $~]
      [%con i=a t=(list a)]
  ==
--
=/  b  `(list @)`[%con 1 %con 2 %nil ~]
^-  @
?-  b
  [%nil ~]  1
  [%con _ %con _]  i.t.b
  [%con _]  i.b
==
