/-  tree-include
!:
|%
++  baff  |*([a=(unit) b=(trap)] ?^(a a *b))
++  find-in-tree
  |*  [paz=fist:jo fun=$+(* (unit))]
  |=  a=(list json)
  |^  (try)
  ++  try
    |.  ^+  *fun
    ?~  a  ~
    %+  biff  (paz i.a)
    |*  [b=(list json) c=*]  ^+  *fun
    (baff (baff (fun c) try(a b)) try(a t.a))
  --
::
++  json-front
  |=  a=json  ^-  json
  =<  ?~(. [%b |] .)                  :: XX overloaded nulls
  =-  (fall `(unit json)`- ~)
  %+  biff  ((ar some):jo a)
  %+  find-in-tree  (ot c/(ar some) gn/so ga/(om so) ~):jo
  |=  [nom=span atr=(map span cord)]  ^-  (unit json)
  ?.  (~(has by atr) 'urb:front')  ~
  ?>  ?=(%meta nom)
  (biff (~(get by atr) %value) poja)
::
++  read-schem  
  =<  (cook to-noun (cook to-tree apex))
  |%
  ++  noun  $|(term [noun noun])       ::  shadow
  ++  data  $|(term [n=@ l=noun r=data])
  ++  apex  ;~(plug sym (star ;~(plug delim sym)))
  ++  delim  ;~(pose (cold 0 dot) (cook lent (plus cab)))
  ++  to-noun  |=(a=data ?@(a a [l.a $(a r.a)]))
  ++  to-tree
    |=  [acc=data a=(list ,[p=@u q=term])]
    %+  roll  a  =<  .(acc ^acc)
    |=  [[n=@u v=term] acc=data]
    ?@  acc            [n acc v]
    ?:  (gth n n.acc)  [n (to-noun acc) v]
    acc(r $(acc r.acc))
  --
--
