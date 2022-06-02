=>
|%
++  list
  ^-  $-($ $)
  |=  a=$
  $%  [%nil $~]
      [%con i=a t=(list a)]
  ==
--
=/  a  `(list @)`[%con 1 %con 2 %nil ~]
a
