=>
|%
++  list
  ^-  $-($ $)
  |=  a=$
  $%  [%nil $~]
      [%con a (list a)]
  ==
--
=/  a  `(list @)`[%nil ~]
=/  b  `(list @)`[%con 1 %con 2 %nil ~]
`(list ^)`b
