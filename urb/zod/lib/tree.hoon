/-  tree-include
|%
++  baff  |*([a=(unit) b=(trap)] ?^(a a *b))
++  find-in-tree
  |*  [paz=fist:jo fun=$+(* (unit))]
  |=  a=json  ^+  *fun
  %+  biff  (paz a)
  |*  [b=(list json) c=*]  ^+  *fun
  %+  baff  (fun c)
  |.
  ?~  b  ~
  (baff ^^$(a i.b) |.(^$(b t.b)))
::
++  json-front
  |=  a=json  ^-  json
  =-  (fall `(unit json)`- ~)
  %+  biff  ((ot body/some ~):jo a)
  %+  find-in-tree  (ot c/(ar some) gn/so ga/(om so) ~):jo
  |=  [nom=span atr=(map span cord)]  ^-  (unit json)
  ?.  (~(has by atr) 'urb:front')  ~
  ?>  ?=(%meta nom)
  (biff (~(get by atr) %value) poja)
::
++  read-schem  
  =<  (cook to-noun (cook to-tree apex))
  |%
  ++  data  $|(term [n=@ l=data r=data])
  ++  apex  ;~(plug sym (star ;~(plug delim sym)))
  ++  delim  ;~(pose (cold 0 dot) (cook lent (plus cab)))
  ++  to-noun  |=(a=data ?@(a a [$(a l.a) $(a r.a)]))
  ++  to-tree
    |=  [acc=data a=(list ,[p=@u q=term])]
    %+  roll  a  =<  .(acc ^acc)
    |=  [[n=@u v=term] acc=data]
    ?@  acc            [n acc v]
    ?:  (gth n n.acc)  [n acc v]
    acc(r $(acc r.acc))
  --
--
