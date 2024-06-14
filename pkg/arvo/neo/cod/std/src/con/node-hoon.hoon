/@  hoon
/@  node
/-  manx-utils
:-  [%node %$ %hoon]
|=  nod=node
^-  hoon
=/  raw=tape  (need (val:mu "text"))
%-  crip
?:  =(0 (lent raw))       raw
?.  =((rear raw) '\0a')   raw
::  remove newline by html encoding nonsense
(snip raw)
