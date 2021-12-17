/+  store=graph-store
|_  =graph:store
++  via  .
++  orm  orm:store
++  get
  |=  =index:store
  ^-  (unit node:store)
  ?~  index    ~
  =/  child   (get:orm graph i.index)
  ?~  child    ~
  ?~  t.index  `u.child
  ?.  ?=(%graph -.children.u.child)  ~
  (~(get via p.children.u.child) t.index)
::
++  get-parent
  |=  =index:store
  ^-  (unit node:store)
  =/  len  (lent index)
  ?.  (gte len 2)  ~
  (get:via (scag (dec len) index))
::
++  got
  |=  =index:store
  ^-  node:store
  (need (get index))
::
++  has
  |=  =index:store
  =(~ (get index))
::
++  pet
  |=  [=index:store =node:store]
  ^-  (unit graph:store)
  ?~  index    ~
  ?:  =(~ t.index)  `(put:orm graph i.index node)
  =/  m-child       (get:orm graph i.index)
  ?~  m-child        ~
  =/  child          u.m-child
  ?.  ?=(%graph -.children.child)  ~
  =/  inner  (~(pet via p.children.child) t.index node)
  ?~  inner  ~
  `(put:orm graph i.index child(p.children u.inner))
::
++  put
  |=  [=index:store =node:store]
  (need (pet index node))
::
++  all
  |=  f=$-(indexed-post:store ?)
  %+  all:orm  graph
  |=  [=atom =node:store]
  ?&  ?|  ?=(%| -.post.node)
          (f [atom p.post.node])
      ==
      ::
      ?-  -.children.node
        %empty  %.y
        %graph  ^$(graph p.children.node)
  ==  ==
--
