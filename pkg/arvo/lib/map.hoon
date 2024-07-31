  ::  /lib/map
::::
::  Utilities to replicate ++by behavior via gates.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
::  To avoid shadowing the name `map`, import as `/+  *map`.
::
|@
:: +all: [(map) gate] -> ?
::
:: Computes the logical AND on the results of slamming
:: every element in map a with gate b.
:: Examples
:: > =mymap (make `(list [@tas *])`~[a+1 b+[2 3]])
:: > (all mymap |=(a=* ?@(a & |)))
:: %.n
:: Source
++  all
  |*  [m=(map) g=$-(* ?)]
  ^-  ?
  (~(all by m) g)
:: +and: [(map) gate] -> ?
::
:: Computes the logical AND on the results of slamming
:: every element in map a with gate b.  Alias for +all.
:: Examples
:: > =mymap (make `(list [@tas *])`~[a+1 b+[2 3]])
:: > (and mymap |=(a=* ?@(a & |)))
:: %.n
:: Source
++  and  all
:: +any: [(map) gate] -> ?
::
:: Computes the logical OR on the results of slamming
:: every element in map a with gate b.
:: Examples
:: > =mymap (make `(list [@tas *])`~[a+1 b+[2 3]])
:: > (any mymap |=(a=* ?@(a & |)))
:: %.y
:: Source
++  any
  |*  [m=(map) g=$-(* ?)]
  ^-  ?
  (~(any by m) g)
:: +apply: [(map) gate] -> (map)
::
:: Turn with key (Haskell map or apply-to-all). Produces
:: a new map with the same keys as the input map, but
:: with the values transformed by the gate.  Alias for
:: +urn.
:: Examples
:: > =mymap `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > mymap
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: 
:: > (apply mymap |=([k=@ v=@] (pow v 2)))
:: {[p=1 q=1] [p=2 q=4] [p=3 q=9]}
:: Source
++  apply  urn
:: +apt: (map) -> ?
::
:: Check correctness.  Computes whether input has a
:: correct horizontal order and a correct vertical order.
:: Examples
:: > =a (make `(list [@tas @])`~[a+1 b+2 c+3 d+4 e+5])
:: > ~(apt by a)
:: %.y
:: > =z ?~(a ~ a(p.n `@tas`%z))
:: > z
:: [n=[p=%z q=2] l={[p=%e q=5]} r={[p=%d q=4] [p=%a q=1] [p=%c q=3]}]
:: > ~(apt by z)
:: %.n
:: Source
++  apt
  |*  [m=(map)]
  ^-  ?
  ~(apt by m)
:: +bif: [(map) noun noun] -> [(map) (map)]
::
:: Splits the map into two maps, each containing the items
:: either side of the key but not including the key.
:: Examples
:: > =a (make `(list [@tas @])`~[a+1 b+2 c+3 d+4 e+5])
:: > (~(bif by a) b+2)
:: [l=[n=[p=%e q=5] l=~ r=~] r=[n=[p=%d q=4] l=~ r=[n=[p=%c q=3] l={[p=%a q=1]} r={}]]]
:: > `[(map @tas @) (map @tas @)]`(~(bif by a) b+2)
:: [{[p=%e q=5]} {[p=%d q=4] [p=%a q=1] [p=%c q=3]}]
++  bif
  |*  [m=(map) k=* v=*]
  ^+  [(map) (map)]
  (~(bif by m) k v)
:: +del: [(map) noun] -> (map)
::
:: Returns a new map that does not contain the key
:: Examples
:: > =/  mymap  (make `(list (pair @ud @ud))`~[[%1 1] [%2 2]])
::   (del mymap %2)
:: [n=[p=p=1 q=q=1] l=~ r=~]
:: Source
++  del
  |*  [m=(map) k=*]
  ^+  m
  (~(del by m) k)
:: +dif: [(map) (map)] -> (map)
::
:: Computes the difference between two maps, producing the
:: map that contains the items in the first map that are not
:: in the second map.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
:: > `(map @tas @)`(dif a b)
:: {[p=%b q=2] [p=%a q=1]}
:: Source
++  dif
  |*  [a=(map) b=(map)]
  ^+  a
  (~(dif by a) b)
:: +diff-left: [(map) (map)] -> (map)
::
:: Computes the difference between two maps, producing the
:: map that contains the items in the first map that are not
:: in the second map.  Alias for +dif.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
:: > `(map @tas @)`(diff-left a b)
:: {[p=%b q=2] [p=%a q=1]}
:: Source
++  diff-left  dif
:: +diff-right: [(map) (map)] -> (map)
::
:: Computes the difference between two maps, producing the
:: map that contains the items in the second map that are not
:: in the first map.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
:: > `(map @tas @)`(diff-right a b)
:: {[p=%e q=5] [p=%f q=6]}
:: Source
++  diff-right
  |*  [a=(map) b=(map)]
  ^+  a
  (dif b a)
:: +diff-symmetric: [(map) (map)] -> (map)
::
:: Computes the difference between two maps, producing the
:: map that contains the items that are in one map but not
:: in the other.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
:: > `(map @tas @)`(diff-symmetric a b)
:: {[p=%b q=2] [p=%a q=1] [p=%e q=5] [p=%f q=6]}
:: Source
++  diff-symmetric
  |*  [a=(map) b=(map)]
  ^+  a
  %-  %~  uni  by
    (diff-left a b)
  (diff-right a b)
:: +dig: [(map) noun] -> (unit *)
::
:: Produce the address of key within map.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > (dig mymap %b)
:: [~ 252]
:: > (dig mymap %b)
:: [~ 2]
:: > (dig by mymap %e)
:: ~
:: Source
++  dig
  |*  [m=(map) k=*]
  ^+  (unit @)
  (~(dig by m) k)
:: +filter: [(map) gate] -> (map)
::
:: Produces a new map with only the key-value pairs for
:: which the gate produces %.y against the value.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > `(map @tas @)`(filter mymap (curr gth 2))
:: {[p=%d q=4] [p=%c q=3]}
:: Source
++  filter
  |*  [a=(map) b=$-(* ?)]
  =/  kvl  ~(tap by a)
  =|  res=(list _?>(?=(^ a) n.a))
  |-  ^+  a
  ?~  kvl  (malt res)
  ?.  (b +.i.kvl)
    $(kvl t.kvl)
  $(kvl t.kvl, res [i.kvl res])
:: +gas: [(map) (list)] -> (map)
::
:: Insert list of key-value pairs into map.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > `(map @tas @)`(gas mymap ~[e+5 f+6 g+7])
:: {[p=%e q=5] [p=%b q=2] [p=%d q=4] [p=%f q=6] [p=%g q=7] [p=%a q=1] [p=%c q=3]}
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2]))
:: > a
:: {[p=%b q=2] [p=%a q=1]}
:: > `(map @tas @)`(gas mymap ~[a+100 b+200])
:: {[p=%b q=200] [p=%a q=100]}
:: > `(map @tas @)`(gas `(map @tas @)`~) ~[a+100 b+200])
:: {[p=%b q=200] [p=%a q=100]}
:: Source
++  gas
  |*  [m=(map) l=(list (pair))]
  ^+  m
  (~(gas by m) l)
:: +get: [(map) noun] -> (unit noun)
::
:: Returns the unit value at key in map.
:: Examples
:: > =/  mymap  (make `(list (pair @ud @ud))`~[[%1 1] [%2 2]])
::   (get mymap %2)
:: [~ q=2]
:: Source
++  get
  |*  [m=(map) k=*]
  (~(get by m) k)
:: +got: [(map) noun] -> noun
::
:: Returns the value at key in map; crash if nonexistent.
:: Examples
:: > =/  mymap  (make `(list (pair @ud @ud))`~[[%1 1] [%2 2]])
::   (got mymap %2)
:: q=2
:: Source
++  got
  |*  [m=(map) k=*]
  (~(got by m) k)
:: +got: [(map) noun noun] -> noun
::
:: Returns the value at key in map; default if nonexistent.
:: Examples
:: > =/  mymap  (make `(list (pair @ud @ud))`~[[%1 1] [%2 2]])
::   (gut mymap %2 q=5)
:: q=2
::   (gut mymap %3 q=5)
:: q=5
:: Source
++  gut
  |*  [m=(map) k=* d=*]
  (~(gut by m) k d)
:: +has: [(map) noun] -> ?
::
:: Returns whether map contains key.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > (has mymap %a)
:: %.y
:: > (has mymap %z)
:: %.n
:: Source
++  has
  |*  [m=(map) k=*]
  ^-  ?
  (~(has by m) k)
:: +int: [(map) (map)] -> (map)
::
:: Produces a map of the key intersection between two maps
:: of the same type.  In case of conflict, the value from
:: the first map is used.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
::
:: > `(map @tas @)`(int a b)
:: {[p=%d q=4] [p=%c q=3]}
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2]))
:: > =b `(map @tas @)`(make (limo ~[a+100 b+200]))
:: > a
:: {[p=%b q=2] [p=%a q=1]}
:: > b
:: {[p=%b q=200] [p=%a q=100]}
::
:: > `(map @tas @)`(int a b)
:: {[p=%b q=200] [p=%a q=100]}
:: Source
++  int
  |*  [a=(map) b=(map)]
  ^+  a
  (~(int by a) b)
:: +intersect: [(map) (map)] -> (map)
::
:: Produces a map of the key intersection between two maps
:: of the same type.  In case of conflict, the value from
:: the first map is used.  Alias for +int.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > =b `(map @tas @)`(make (limo ~[c+3 d+4 e+5 f+6]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > b
:: {[p=%e q=5] [p=%d q=4] [p=%f q=6] [p=%c q=3]}
::
:: > `(map @tas @)`(int a b)
:: {[p=%d q=4] [p=%c q=3]}
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2]))
:: > =b `(map @tas @)`(make (limo ~[a+100 b+200]))
:: > a
:: {[p=%b q=2] [p=%a q=1]}
:: > b
:: {[p=%b q=200] [p=%a q=100]}
::
:: > `(map @tas @)`(int a b)
:: {[p=%b q=200] [p=%a q=100]}
:: Source
++  intersect  int
:: +jab: [(map) noun gate] -> (map)
::
:: Produces a map with the value at key transformed by the
:: gate.
:: Examples
:: > =a `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > a
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: 
:: > `(map @tas @)`(jab a %d |=(x=@ (pow x 2)))
:: {[p=%b q=2] [p=%d q=16] [p=%a q=1] [p=%c q=3]}
:: 
:: > (jab a %z |=(x=@ (pow x 2)))
:: dojo: hoon expression failed
:: 
:: > (jab a %d |=(a=@ [a a]))
:: -need.?(%~ [n=[p=@tas q=@] l=nlr([p=@tas q=@]) r=nlr([p=@tas q=@])])
:: -have.[n=[p=@tas q=[@ @]] l=nlr([p=@tas q=@]) r=nlr([p=@tas q=@])]
:: nest-fail
:: dojo: hoon expression failed
++  jab
  |*  [m=(map) k=* g=gate]
  ^+  m
  (~(jab by m) k g)
:: +key: (map) -> (set)
::
:: Produces the set of all keys in the map.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > (key mymap)
:: {%b %d %a %c}
++  key
  |*  [m=(map)]
  ^-  (set _?>(?=(^ m) p.n.m))
  ~(key by m)
:: +keys: (map) -> (set)
::
:: Produces the set of all keys in the map.  Alias for +key.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > (keys mymap)
:: {%b %d %a %c}
++  keys  key
:: +mar: [(map) noun (unit)] -> (map)
::
:: Produces map with the addition of key-value pair if
:: the unit value is a nonempty.  Else delete the key.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: 
:: > `(map @tas @)`(mar mymap %e (some 5))
:: {[p=%e q=5] [p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: 
:: > `(map @tas @)`(mar mymap %a (some 10))
:: {[p=%b q=2] [p=%d q=4] [p=%a q=10] [p=%c q=3]}
:: 
:: > `(map @tas @)`(mar mymap %a ~)
:: {[p=%b q=2] [p=%d q=4] [p=%c q=3]}
++  mar
  |*  [m=(map) k=* v=(unit *)]
  ^+  m
  (~(mar by m) k v)
:: +make: (list (pair)) -> (map)
::
:: Produces a map from a list of key-value pairs.
:: Alias for +malt.
:: Examples
:: > `(map @tas @)`(make ~[[a 1] [b 2] [c 3] [d 4]])
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: Source
++  make  malt
:: +or: [(map) gate] -> ?
::
:: Computes the logical OR on the results of slamming
:: every element in map a with gate b.  Alias for +any.
:: Examples
:: > =mymap (make `(list [@tas *])`~[a+1 b+[2 3]])
:: > (or mymap |=(a=* ?@(a & |)))
:: %.y
:: Source
++  or  any
:: +pairs: (map) -> (list (pair))
::
:: Produces a list of key-value pairs in the map.
:: Alias for +tap.
:: Examples
:: > =mymap `(map @ @)`(make ~[[1 1] [2 2] [3 3] [4 4] [5 5]])
:: > (pairs mymap)
:: ~[[p=4 q=4] [p=3 q=3] [p=2 q=2] [p=1 q=1] [p=5 q=5]]
:: Source
++  pairs  tap
:: +put: [(map) noun noun] -> (map)
::
:: Returns a new map that contains the new value at key
:: Examples
:: > =/  mymap  (make `(list (pair @ud @ud))`~[[%1 1] [%2 2]])
::   (put mymap %3 3)
:: [n=[p=p=2 q=q=2] l=[n=[p=p=1 q=q=1] l=~ r=~] r=[n=[p=p=3 q=q=3] l=~ r=~]]
:: Source
++  put
  |*  [m=(map) k=* v=*]
  ^+  m
  (~(put by m) k v)
:: +reduce: [(map) gate] -> noun
::
:: Reduce; accumulate the elements of map using gate.
:: Alias for +rep.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > (rep mymap |=([p=[@tas @] q=@] ~&([p q] (add +.p q))))
:: [[%b 2] 0]
:: [[%d 4] 2]
:: [[%c 3] 6]
:: [[%a 1] 9]
:: q=10
:: Source
++  reduce  rep
:: +rep: [(map) gate] -> noun
::
:: Reduce; accumulate the elements of map using gate.
:: Examples
:: > =mymap `(map @tas @)`(make (limo ~[a+1 b+2 c+3 d+4]))
:: > mymap
:: {[p=%b q=2] [p=%d q=4] [p=%a q=1] [p=%c q=3]}
:: > (rep mymap |=([p=[@tas @] q=@] ~&([p q] (add +.p q))))
:: [[%b 2] 0]
:: [[%d 4] 2]
:: [[%c 3] 6]
:: [[%a 1] 9]
:: q=10
:: Source
++  rep
  |*  [m=(map) g=_=>(~ |=([* *] +<+))]
  ^-  *
  (~(rep by m) g)
:: +rib: [(map) noun gate] -> [noun (map)]
::
:: Transform plus product.  Accumulate over each key-value
:: pair and transform the map in place.  The input gate
:: accepts a sample of the form [[key value] accumulator]
:: and yields a product like [accumulator [key value]].
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5]])
:: > mymap
:: {[p='e' q=5] [p='b' q=2] [p='d' q=4] [p='a' q=1] [p='c' q=3]}
:: 
:: > =c |=  [[k=@t v=@] acc=(list @t)]
::      ?:  (lth v 3)
::        [[k acc] [k 0]]
::      [acc [k v]]
:: 
:: > `[(list @t) (map @t @)]`(rib mymap *(list @t) c)
:: [<|a b|> {[p='e' q=5] [p='b' q=0] [p='d' q=4] [p='a' q=0] [p='c' q=3]}]
:: Source
++  rib
  |*  [m=(map) b=* g=gate]
  ^+  [b m]
  (~(rib by m) b g)
:: +run: [(map) gate] -> (map)
::
:: Transform values in the map using the gate.
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5]])
:: > `(map @t @)`(run mymap dec)
:: {[p='e' q=4] [p='b' q=1] [p='d' q=3] [p='a' q=0] [p='c' q=2]}
:: Source
++  run
  |*  [m=(map) g=gate]
  ^+  m
  (~(run by m) g)
:: +size: (map) -> @
::
:: Produces the depth (size) of the map.  Alias for +wyt.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[1 1] [2 2] [3 3] [4 4] [5 5]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=5] [p=1 q=1] [p=2 q=2] [p=3 q=3] [p=4 q=4]}
:: 
:: > (size a)
:: 3
:: > (size b)
:: 5
:: Source
++  size  wyt
:: +tap: (map) -> (list (pair))
::
:: Produces a list of key-value pairs in the map.
:: Examples
:: > =mymap `(map @ @)`(make ~[[1 1] [2 2] [3 3] [4 4] [5 5]])
:: > (tap mymap)
:: ~[[p=4 q=4] [p=3 q=3] [p=2 q=2] [p=1 q=1] [p=5 q=5]]
:: Source
++  tap
  |*  [m=(map)]
  ^-  (list _?>(?=(^ m) n.m))
  ~(tap by m)
:: +transform-value: [(map) gate] -> (map)
::
:: Transform values in the map using the gate.
:: Alias for +run.
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5]])
:: > `(map @t @)`(transform-value mymap dec)
:: {[p='e' q=4] [p='b' q=1] [p='d' q=3] [p='a' q=0] [p='c' q=2]}
:: Source
++  transform-value  run
:: +transform-product: [(map) noun gate] -> [noun (map)]
::
:: Transform plus product.  Accumulate over each key-value
:: pair and transform the map in place.  The input gate
:: accepts a sample of the form [[key value] accumulator]
:: and yields a product like [accumulator [key value]].
:: Alias for +rib.
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5]])
:: > mymap
:: {[p='e' q=5] [p='b' q=2] [p='d' q=4] [p='a' q=1] [p='c' q=3]}
:: 
:: > =c |=  [[k=@t v=@] acc=(list @t)]
::      ?:  (lth v 3)
::        [[k acc] [k 0]]
::      [acc [k v]]
:: 
:: > `[(list @t) (map @t @)]`(transform-product mymap *(list @t) c)
:: [<|a b|> {[p='e' q=5] [p='b' q=0] [p='d' q=4] [p='a' q=0] [p='c' q=3]}]
:: Source
++  transform-product  rib
:: +uni: [(map) (map)] -> (map)
::
:: Produces a map of the union of two maps of the same type.
:: In case of conflict, the value from the first map is used.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[3 300] [4 400] [5 500]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=500] [p=3 q=300] [p=4 q=400]}
:: > `(map @ @)`(uni a b)
:: {[p=5 q=500] [p=1 q=1] [p=2 q=2] [p=3 q=300] [p=4 q=400]}
++  uni
  |*  [a=(map) b=(map)]
  (~(uni by a) b)
:: +union: [(map) (map)] -> (map)
::
:: Produces a map of the union of two maps of the same type.
:: In case of conflict, the value from the first map is used.
:: Alias for +uni.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[3 300] [4 400] [5 500]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=500] [p=3 q=300] [p=4 q=400]}
:: > `(map @ @)`(union a b)
:: {[p=5 q=500] [p=1 q=1] [p=2 q=2] [p=3 q=300] [p=4 q=400]}
:: Source
++  union  uni
:: +uno: [(map) (map)] -> (map)
::
:: General union: Produces a map of the union between the
:: keys of a and b. If b shares a key with a, gate meg
:: is applied to both and its product is used as the new
:: value of the key in question.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[3 3] [4 4] [5 5]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=5] [p=3 q=3] [p=4 q=4]}
:: 
:: > `(map @ @)`(uno a b |=([k=@ v=@ w=@] (add v w)))
:: {[p=5 q=5] [p=1 q=1] [p=2 q=2] [p=3 q=6] [p=4 q=4]}
:: Source
++  uno
  |*  [a=(map) b=(map) g=gate]
  ((~(uno by a) b) g)
:: +unify: [(map) (map)] -> (map)
::
:: General union: Produces a map of the union between the
:: keys of a and b. If b shares a key with a, gate meg
:: is applied to both and its product is used as the new
:: value of the key in question.  Alias for +uno.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[3 3] [4 4] [5 5]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=5] [p=3 q=3] [p=4 q=4]}
:: 
:: > `(map @ @)`(uno a b |=([k=@ v=@ w=@] (add v w)))
:: {[p=5 q=5] [p=1 q=1] [p=2 q=2] [p=3 q=6] [p=4 q=4]}
:: Source
++  unify  uno
:: +urn: [(map) gate] -> (map)
::
:: Turn with key (Haskell map or apply-to-all). Produces
:: a new map with the same keys as the input map, but
:: with the values transformed by the gate.
:: Examples
:: > =mymap `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > mymap
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: 
:: > (urn mymap |=([k=@ v=@] (pow v 2)))
:: {[p=1 q=1] [p=2 q=4] [p=3 q=9]}
:: Source
++  urn
  |*  [m=(map) g=gate]
  (~(urn by m) g)
:: +val: (map) -> (list)
::
:: Produces the list of values in map.
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3]])
:: > mymap
:: {[p='b' q=2] [p='a' q=1] [p='c' q=3]}
:: > (val mymap)
:: ~[3 1 2]
:: Source
++  val
  |*  [m=(map)]
  ~(val by m)
:: +values: (map) -> (list)
::
:: Produces the list of values in map.  Alias for +val.
:: Examples
:: > =mymap `(map @t @)`(make ~[['a' 1] ['b' 2] ['c' 3]])
:: > mymap
:: {[p='b' q=2] [p='a' q=1] [p='c' q=3]}
:: > (values mymap)
:: ~[3 1 2]
:: Source
++  values  val
:: +wyt: (map) -> @
::
:: Produces the depth (size) of the map.
:: Examples
:: > =a `(map @ @)`(make ~[[1 1] [2 2] [3 3]])
:: > =b `(map @ @)`(make ~[[1 1] [2 2] [3 3] [4 4] [5 5]])
:: > a
:: {[p=1 q=1] [p=2 q=2] [p=3 q=3]}
:: > b
:: {[p=5 q=5] [p=1 q=1] [p=2 q=2] [p=3 q=3] [p=4 q=4]}
:: 
:: > (wyt a)
:: 3
:: > (wyt b)
:: 5
:: Source
++  wyt
  |*  [m=(map)]
  ^-  @
  ~(wyt by m)
--