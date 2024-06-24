  ::  /lib/maplist
::::  Utilities to replicate ++ja behavior via gates.
::
::  A jar is a (map (list)).  Because it is a map, the
::  ++by functions can also be utilized with it directly.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
/+  libmap=map
|@
:: +add: [(jar) noun noun] -> (jar)
::
:: Adds a value to the head of the list at key in jar.
:: Examples
:: > =j `(jar @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: > j
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
:: 
:: > `(jar @t @ud)`(add j 'b' 7)
:: {[p='b' q=~[7 4 5 6]] [p='a' q=~[1 2 3]]}
:: 
:: > `(jar @t @ud)`(add j 'c' 8)
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]] [p='c' q=~[8]]}
:: Source
++  add
  |*  [j=(jar) k=* v=*]
  (~(add ja j) k v)
:: +del: [(jar) noun] -> (jar)
::
:: Deletes the key from jar.
:: Examples
:: > =j `(jar @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: > j
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
::
:: > `(jar @t @ud)`(del j 'a')
:: {[p='b' q=~[4 5 6]]}
:: Source
++  del
  |*  [j=(jar) k=*]
  (~(del by j) k)
:: +get: [(jar) noun] -> (unit noun)
::
:: Returns the unit value at key in jar.
:: Examples
:: > =j `(jar @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: > j
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
:: 
:: > `(list @ud)`(get j 'a')
:: ~[1 2 3]
:: 
:: > `(list @ud)`(get j 'b')
:: ~[4 5 6]
:: 
:: > `(list @ud)`(get j 'c')
:: ~
:: Source
++  get
  |*  [j=(jar) k=*]
  (~(get ja j) k)
:: +has: [(jar) noun] -> ?
::
:: Check whether the key is in the jar.
:: Examples
:: > =j `(jar @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: > j
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
::
:: > (has j 'a')
:: %.y
:: Source
++  has
  |*  [j=(jar) k=*]
  (~(has by j) k)
:: +make:  (list (pair * (list))) -> (jar)
::
:: Creates a jar from a list of pairs of key and list.
:: Examples
:: > `(jar @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
:: Source
++  make
  |*  [l=(list (pair * (list)))]
  (malt l)
::
:: Other arms do not need special-case handling for jars.
::
++  all  all:libmap
++  and  and:libmap
++  any  any:libmap
++  apply  apply:libmap
++  apt  apt:libmap
++  bif  bif:libmap
:: ++  del  del:libmap
++  dif  dif:libmap
++  diff-left  diff-left:libmap
++  diff-right  diff-right:libmap
++  diff-symmetric  diff-symmetric:libmap
++  dig  dig:libmap
++  filter  filter:libmap
++  gas  gas:libmap
:: ++  get  get:libmap
++  got  got:libmap
++  gut  gut:libmap
:: ++  has  has:libmap
++  int  int:libmap
++  intersect  intersect:libmap
++  jab  jab:libmap
++  key  key:libmap
++  keys  keys:libmap
++  mar  mar:libmap
:: ++  make  make:libmap
++  or  or:libmap
++  pairs  pairs:libmap
++  put  put:libmap
++  reduce  reduce:libmap
++  rep  rep:libmap
++  rib  rib:libmap
++  run  run:libmap
++  size  size:libmap
++  tap  tap:libmap
++  transform-value  transform-value:libmap
++  transform-product  transform-product:libmap
++  uni  uni:libmap
++  union  union:libmap
++  uno  uno:libmap
++  unify  unify:libmap
++  urn  urn:libmap
++  val  val:libmap
++  values  values:libmap
++  wyt  wyt:libmap
--