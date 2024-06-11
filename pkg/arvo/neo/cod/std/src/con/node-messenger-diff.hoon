/@  node
/@  messenger-diff
/-  manx-utils
:-  [%node %$ %messenger-diff]
|=  nod=node
^-  messenger-diff
=/  mu  ~(. manx-utils nod)
=/  head  (@tas (got:mu %head))
~&  >>>  head 
%-  messenger-diff
?+    head
  ~|  [%unknown-head head]
  !!
    %new-dm
  =/  partner  `@p`(slav %p (vol:mu "partner"))
  [head partner]
::
    %new-groupchat
  =/  name  (vol:mu "name")
  [head name ~]
::
    %invite-to-groupchat
  ~&  >  (vol:mu "name")
  =/  name  (vol:mu "name")
  ::=/  ship  `@p`(slav %p (vol:mu "ship"))
  ::~&  >>>  [head name ship]
  [head name ~]
==