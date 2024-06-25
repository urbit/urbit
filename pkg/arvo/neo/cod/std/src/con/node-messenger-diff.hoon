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
  =/  partner  `@p`(slav %p (vol:mu "invites"))
  [head partner]
::
    %new-groupchat
  =/  invites=tape  (need (val:mu "invites"))
  =/  parsed-invites=(set @p)
    %-  silt
    %+  scan  (weld " " invites)
      %-  star
        ;~  pose
          ;~  pfix  (jest ' ~')
          fed:ag
          ==
          ;~  pfix  (jest ', ~')
          fed:ag
          ==
        ==
  ~&  >  parsed-invites
  =/  value  (val:mu "name")
  =/  name=cord 
  ::  TODO: if invites are longer than some amount of character
    ?~  value  (crip invites)
    (crip (need value))
  ~&  >  name
  [head name parsed-invites]
::
    %invite-to-groupchat
  ~&  >  (vol:mu "name")
  =/  name  (vol:mu "name")
  [head name ~]
==