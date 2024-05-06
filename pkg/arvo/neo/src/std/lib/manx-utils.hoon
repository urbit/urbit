|_  a=manx
:: named
::
::  first child with name=tape, or null
::
++  named
  |=  name=tape
  ^-  (unit manx)
  =/  n
    %+  skim  pre-flatten
    |=  =manx
    =((~(gut by (malt a.g.manx)) %name "") name)
  ?:  =(0 (lent n))  ~
  `(snag 0 n)
:: get-attribute
::
::   tape at attribute, or null
::
++  get-attribute
  |=  =mane
  ^-  (unit tape)
  (~(get by (malt a.g.a)) mane)
:: value
::
::  value attribute of first named descendant, or null
++  value
  |=  name=tape
  ^-  (unit tape)
  ?~  t=(named name)  ~
  =.  a  u.t
  (get-attribute %value)
:: whitelisted
::
::   check all tags are in whitelist
::
++  whitelisted
  |=  b=(set mane)
  ^-  ?
  %-  post-fold
  |=  [g=marx w=?]
  ?.  w  w
  (~(has in b) n.g)
:: whitelisted
::
::   check whether any tags are in blacklist
::
++  blacklisted
  |=  b=(set mane)
  ^-  ?
  %-  post-fold
  |=  [g=marx w=_|]
  ?:  w  w
  (~(has in b) n.g)
:: get-max-depth
::
::   deepest node depth (root is 0)
::
++  get-max-depth
  ^-  @ud
  |-
  ?~  c.a  0
  %+  max
    .+  $(c.a c.i.c.a)
  $(c.a t.c.a)
:: apply-elem:
::
::   apply gate to tags/attrs
::
++  apply-elem
  |=  b=$-(marx marx)
  |^  ^-  manx
  [(b g.a) (cloop c.a)]
  ++  cloop
    |=  c=marl
    ?~  c  ~
    [^$(a i.c) (cloop t.c)]
  --
:: apply-elem-chain
::
::   apply gate to tags/attrs with parentage given
::
::   gate takes [a b] where a is the current marx
::   and b is the list of parent marxes in ascending
::   order (i is the direct parent)
::
++  apply-elem-chain
  =/  ch=(lest marx)  ~[g.a]
  |=  b=$-([marx (list marx)] marx)
  |^  ^-  manx
  [(b ch) (cloop c.a)]
  ++  cloop
    |=  c=marl
    ?~  c  ~
    [^$(a i.c, ch [g.i.c ch]) (cloop t.c)]
  --
:: post-apply-nodes
::
::   apply gate to nodes in postorder
::
::   (unlike apply-elem, the gate takes the
::   whole manx instead of just marx)
::
++  post-apply-nodes
  |=  b=$-(manx manx)
  |^  ^-  manx
  (b [g.a (cloop c.a)])
  ++  cloop
    |=  c=marl
    ?~  c  ~
    [^$(a i.c) (cloop t.c)]
  --
:: apply-attrs
::
::   apply a gate to all attributes
::
++  apply-attrs
  |=  b=$-([mane tape] [mane tape])
  ^-  manx
  %-  apply-elem
  |=  g=marx
  =|  y=mart
  |-
  ?~  a.g  g(a (flop y))
  $(a.g t.a.g, y [(b i.a.g) y])
:: apply-text
::
::   apply a gate to all ordinary text
::
++  apply-text
  |=  b=$-(tape tape)
  ^-  manx
  %-  apply-elem
  |=  g=marx
  ?.  ?=(%$ n.g)            g
  ?~  a.g                   g
  ?.  ?=(%$ n.i.a.g)        g
  ?^  t.a.g                 g
  =.  v.i.a.g  (b v.i.a.g)  g
:: post-fold
::
::   fold over tags/attrs in postorder
::
++  post-fold
  |*  b=_|=([marx *] +<+)
  ^+  ,.+<+.b
  |-
  ?~  c.a  (b g.a +<+.b)
  $(a a(c t.c.a), +<+.b $(a i.c.a))
:: pre-fold
::
::   fold over tags/attrs in preorder
::
++  pre-fold
  |*  b=_|=([marx *] +<+)
  ^+  ,.+<+.b
  |-
  ?~  c.a
    ?:  =(%$^%$ n.g.a)
      +<+.b
    (b g.a +<+.b)
  %=    $
    a  [[%$^%$ ~] t.c.a]
      +<+.b
    %=    $
      a  i.c.a
        +<+.b
      ?:  =(%$^%$ n.g.a)
        +<+.b
      (b g.a +<+.b)
    ==
  ==
:: lvl-fold
::
::   fold over tags/attrs in level order
::
++  lvl-fold
  |*  b=_|=([marx *] +<+)
  |^  ^+  ,.+<+.b
  =.  +<+.b  (b g.a +<+.b)
  (cloop-a c.a +<+.b)
  ++  cloop-a
    |=  [c=marl acc=_+<+.b]
    =/  l  c
    |-
    ?~  l  (cloop-b c acc)
    $(l t.l, acc (b g.i.l acc))
  ++  cloop-b
    |=  [c=marl acc=_+<+.b]
    ?~  c  acc
    $(c t.c, acc (cloop-a c.i.c acc))
  --
:: prune
::
::   delete nodes when applied gate produces %.y
::
++  prune
  |=  b=$-(manx ?)
  |^  ^-  (unit manx)
  ?:  (b a)  ~
  [~ g.a (cloop c.a)]
  ++  cloop
    =|  fro=marl
    |=  to=marl
    ?~  to  (flop fro)
    =+  u=^$(a i.to)
    ?~  u  $(to t.to)
    $(to t.to, fro [u.u fro])
  --
:: prune-tag
::
::   delete nodes by tag
::
++  prune-tag
  |=  b=mane
  ^-  (unit manx)
  (prune |=(x=manx =(b n.g.x)))
::
:: prune-tags
::
::   delete nodes by tags
::
++  prune-tags
  |=  b=(set mane)
  ^-  (unit manx)
  (prune |=(x=manx (~(has in b) n.g.x)))
:: prune-namespace
::
::   delete nodes by tag namespace
::
++  prune-namespace
  |=  b=@tas
  ^-  (unit manx)
  (prune |=(x=manx ?@(n.g.x %.n =(b -.n.g.x))))
:: prune-namespaces
::
::   delete nodes by tag namespaces
::
++  prune-namespaces
  |=  b=(set @tas)
  ^-  (unit manx)
  (prune |=(x=manx ?@(n.g.x %.n (~(has in b) -.n.g.x))))
:: prune-attr
::
::   delete nodes by attribute name
::
++  prune-attr
  |=  b=mane
  ^-  (unit manx)
  %-  prune
  |=  x=manx
  %+  roll  a.g.x
  |=  [[n=mane v=tape] w=_|]
  ?:(w w =(n b))
:: prune-attrs
::
::   delete nodes by attribute names
::
++  prune-attrs
  |=  b=(set mane)
  ^-  (unit manx)
  %-  prune
  |=  x=manx
  %+  roll  a.g.x
  |=  [[n=mane v=tape] w=_|]
  ?:  w  w
  (~(has in b) n)
:: prune-depth
::
::   delete nodes deeper than b (root is 0)
::
++  prune-depth
  |=  b=@ud
  |^  ^-  (unit manx)
  ?:  =(0 b)  ~
  [~ g.a (cloop c.a)]
  ++  cloop
    |=  to=marl
    =|  fro=marl
    |-
    ?~  to  (flop fro)
    =/  x   (prune-depth(a i.to) (dec b))
    ?~  x   $(to t.to)
    $(to t.to, fro [u.x fro])
  --
:: del-attrs
::
::   delete attributes by name
::
++  del-attrs
  |=  b=(set mane)
  ^-  manx
  %-  apply-elem
  |=  g=marx
  =|  y=mart
  |-
  ?~  a.g  g(a (flop y))
  ?:  (~(has in b) n.i.a.g)
    $(a.g t.a.g)
  $(a.g t.a.g, y [i.a.g y])
:: keep-attrs
::
::   delete all attributes except those
::   with the given names
::
++  keep-attrs
  |=  b=(set mane)
  ^-  manx
  %-  apply-elem
  |=  g=marx
  =|  y=mart
  |-
  ?~  a.g  g(a (flop y))
  ?.  (~(has in b) n.i.a.g)
    $(a.g t.a.g)
  $(a.g t.a.g, y [i.a.g y])
:: post-flatten
::
::   get a list of elements by postorder traversal
::
++  post-flatten
  ^-  marl
  (flop (post-fold |=([g=marx l=marl] [[g ~] l])))
:: pre-flatten
::
::   get a list of elements by preorder traversal
::
++  pre-flatten
  ^-  marl
  (flop (pre-fold |=([g=marx l=marl] [[g ~] l])))
:: lvl-flatten
::
::   get a list of elements by level order traversal
::
++  lvl-flatten
  ^-  marl
  (flop (lvl-fold |=([g=marx l=marl] [[g ~] l])))
:: post-get-text
::
::   get a list of plain text by postorder traversal
::
++  post-get-text
  ^-  wall
  %-  flop
  %-  post-fold
  |=  [g=marx l=wall]
  ?.  ?=(%$ n.g)      l
  ?~  a.g             l
  ?.  ?=(%$ n.i.a.g)  l
  ?^  t.a.g           l
  :-  v.i.a.g         l
:: pre-get-text
::
::   get a list of plain text by preorder traversal
::
++  pre-get-text
  ^-  wall
  %-  flop
  %-  pre-fold
  |=  [g=marx l=wall]
  ?.  ?=(%$ n.g)      l
  ?~  a.g             l
  ?.  ?=(%$ n.i.a.g)  l
  ?^  t.a.g           l
  :-  v.i.a.g         l
:: lvl-get-text
::
::   get a list of plain text by level order traversal
::
++  lvl-get-text
  ^-  wall
  %-  flop
  %-  lvl-fold
  |=  [g=marx l=wall]
  ?.  ?=(%$ n.g)      l
  ?~  a.g             l
  ?.  ?=(%$ n.i.a.g)  l
  ?^  t.a.g           l
  :-  v.i.a.g         l
:: search-text
::
:: find plain text containing the given cord
::
++  search-text
  |=  b=@t
  ^-  wall
  %-  flop
  %-  post-fold
  |=  [g=marx l=wall]
  ?.  ?=(%$ n.g)      l
  ?~  a.g             l
  ?.  ?=(%$ n.i.a.g)  l
  ?^  t.a.g           l
  =+  par=(cury (jest b) *hair)
  ?.  |-
      ?~  v.i.a.g  %.n
      ?^  (tail (par v.i.a.g))
        %.y
      $(v.i.a.g t.v.i.a.g)
    l
  [v.i.a.g l]
--
