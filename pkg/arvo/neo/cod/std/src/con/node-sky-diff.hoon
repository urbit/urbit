/@  sky-diff
/@  node
/-  manx-utils
:-  [%node %$ %sky-diff]
|=  nod=node
^-  sky-diff
=*  mu  ~(. manx-utils nod)
=/  head  (@tas (got:mu %head))
?+  head  !!
  %menu
    =/  c  ?~((get:mu %closed) %.y %.n)
    [%menu c]
  ::
==
