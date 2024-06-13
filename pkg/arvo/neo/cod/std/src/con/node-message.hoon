/@  node
/@  message
/-  manx-utils
:-  [%node %$ %message]
|=  nod=node
^-  message
=/  text  (~(vol manx-utils nod) "text")
=/  ship  (slav %p (~(vol manx-utils nod) "ship"))
=/  date  (slav %da (~(vol manx-utils nod) "date"))
[ship date text]