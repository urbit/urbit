/@  node
/@  folder-diff
/-  manx-utils
:-  [%node %$ %folder-diff]
|=  nod=node
^-  folder-diff
=/  mu  ~(. manx-utils nod)
=/  head  (@tas (got:mu %head))
?+    head  ~|(%unknown-head !!)
    %make
  =/  stud  (vol:mu "stud")
  =/  name  (vol:mu "name")
  [%make name stud]
::
    %tomb
  [%tomb (got:mu %smeg)]
==
