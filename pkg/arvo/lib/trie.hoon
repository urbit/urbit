|%
++  trie
  |$  [key-t val-t]
  [val=(unit val-t) kid=(map key-t (trie key-t val-t))]
--
::
=|  a=(trie * *)
=*  val-t  ?>(?=(^ val.a) val.a)
|@
++  put
  |*  [b=(list *) c=*]
  =>  .(b (homo b))
  |-  ^+  a
  ?~  b
    a(val `c)
  =/  son  (~(gut by kid.a) i.b [~ ~])
  a(kid (~(put by kid.a) i.b $(a son, b t.b)))
::
++  get
  |*  b=(list *)
  =>  .(b (homo b))
  |-
  ?~  b
    [~ val.a]
  =/  son  (~(get by kid.a) i.b)
  ?~  son
    [b val.a]
  $(a u.son, b t.b)
--
