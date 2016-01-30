/-  tree-include
!:
|%
++  extract
  |=  a/marl  ^-  tape
  ?~  a  ~
  %+  weld  
    ?.  ?=(_;/(**) i.a)
      $(a c.i.a)
    v.i.a.g.i.a
  $(a t.a) 
::
++  getall
  |=  tag/(list mane)
  |=  ele/manx  ^-  marl
  ?:  (lien tag |=(a/mane =(a n.g.ele)))
    ~[ele]
  (zing (turn c.ele ..$))
::
++  baff  |*({a/(unit) b/(trap)} ?^(a a *b))
++  find-in-tree
  |*  {paz/fist:jo fun/$+(* (unit))}
  |=  jon/json
  =+  a=`(list json)`~[jon]
  |^  (try)
  ++  try
    |.  ^+  *fun
    ?~  a  ~
    %+  biff  (paz i.a)
    |*  {b/(list json) c/*}  ^+  *fun
    (baff (baff (fun +<+) try(a +<-)) try(a t.a))
  --
::
++  map-to-json
  |*  {a/$+(* cord) b/$+(* json)}
  |*  c/(map)  ^-  json
  ~!  c
  (jobe (turn (~(tap by c)) |*({* *} [(a +<-) (b +<+)])))
:: 
++  json-front
  |=  a/json  ^-  json
  =-  (fall `(unit json)`- ~)
  %.  a
  %+  find-in-tree  (ot c+(ar some) gn+so ga+(om so) ~):jo
  |=  {nom/knot atr/(map knot cord)}  ^-  (unit json)
  ?.  (~(has by atr) 'urb:front')  ~
  ?>  ?=($meta nom)
  (biff (~(get by atr) %value) poja)
::
++  read-schem  
  =<  (cook to-noun (cook to-tree apex))
  |%
  ++  noun  $@(term [noun noun])       ::  shadow
  ++  data  $@(term {n/@ l/noun r/data})
  ++  apex  ;~(plug sym (star ;~(plug delim sym)))
  ++  delim  ;~(pose (cold 0 dot) (cook lent (plus cab)))
  ++  to-noun  |=(a/data ?@(a a [l.a $(a r.a)]))
  ++  to-tree
    |=  {acc/data a/(list {p/@u q/term})}
    %+  roll  a  =<  .(acc ^acc)
    |=  {{n/@u v/term} acc/data}
    ^-  data  
    ?@  acc            [n acc v]
    ?:  (gth n n.acc)  
      ::  [n (to-noun acc) v]
      ~|  %this-code-was-abusing-the-type-loophole-and-is-incorrect
      !!
    acc(r $(acc r.acc))
  --
--
