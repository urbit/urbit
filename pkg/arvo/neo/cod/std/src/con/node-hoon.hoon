/@  hoon
/@  node
/-  manx-utils
:-  [%node %$ %hoon]
|=  nod=node
^-  hoon
=/  text  (need (~(val manx-utils nod) "text"))
?:  ?&
      (gth (lent text) 0)
      =((rear text) '\0a')
    ==
  (crip (snip (text)))
(crip text)
