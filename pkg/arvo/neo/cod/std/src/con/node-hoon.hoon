/@  hoon
/@  node
/-  manx-utils
:-  [%node %$ %hoon]
|=  nod=node
^-  hoon
=*  mu  ~(. manx-utils nod)
=/  raw=tape  (need (val:mu "text"))
%-  crip
?:  =(0 (lent raw))       raw
?.  =((rear raw) '\0a')   raw
::  remove newline added by html encoding nonsense
(snip raw)
