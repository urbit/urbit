|%
++  mip                                                 ::  map of maps
  |$  [kex key value]
  (map kex (map key value))
::
++  bi                                                  ::  mip engine
  =|  a=(map * (map))
  |@
  ++  del
    |*  [b=* c=*]
    =+  d=(~(gut by a) b ~)
    =+  e=(~(del by d) c)
    ?~  e
      (~(del by a) b)
    (~(put by a) b e)
  ::
  ++  get
    |*  [b=* c=*]
    =>  .(b `_?>(?=(^ a) p.n.a)`b, c `_?>(?=(^ a) ?>(?=(^ q.n.a) p.n.q.n.a))`c)
    ^-  (unit _?>(?=(^ a) ?>(?=(^ q.n.a) q.n.q.n.a)))
    (~(get by (~(gut by a) b ~)) c)
  ::
  ++  got
    |*  [b=* c=*]
    (need (get b c))
  ::
  ++  gut
    |*  [b=* c=* d=*]
    (~(gut by (~(gut by a) b ~)) c d)
  ::
  ++  has
    |*  [b=* c=*]
    !=(~ (get b c))
  ::
  ++  key
    |*  b=*
    ~(key by (~(gut by a) b ~))
  ::
  ++  put
    |*  [b=* c=* d=*]
    %+  ~(put by a)  b
    %.  [c d]
    %~  put  by
    (~(gut by a) b ~)
  ::
  ++  tap
    ::NOTE  naive turn-based implementation find-errors ):
    =<  $
    =+  b=`_?>(?=(^ a) *(list [x=_p.n.a _?>(?=(^ q.n.a) [y=p v=q]:n.q.n.a)]))`~
    |.  ^+  b
    ?~  a
      b
    $(a r.a, b (welp (turn ~(tap by q.n.a) (lead p.n.a)) $(a l.a)))
  --
--
