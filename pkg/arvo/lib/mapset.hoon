  ::  /lib/mapset
::::
::  Utilities to replicate ++ju behavior via gates.
::
::  A jug is a (map (set)).  Because it is a map, the
::  ++by functions can also be utilized with it directly.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
/+  libmap=map
|@
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
:: +gas: [(jug) (list (pair))] -> (jug)
::
:: Add each of the pairs in the list to the jug.  Append
:: to the end of a set if the key already exists.
:: Examples
:: > =j `(jug @t @ud)`(make ~[['a' (silt ~[1 2 3])] ['b' (silt ~[4 5 6])]])
:: > j
:: {[p='b' q={5 6 4}] [p='a' q={1 2 3}]}
:: 
:: > `(jug @t @ud)`(gas j ~[['a' 10] ['a' 42] ['b' 999] ['c' 7]])
:: {[p='b' q={5 6 4 999}] [p='a' q={10 42 1 2 3}] [p='c' q={7}]}
:: Source
++  gas
  |*  [j=(jug) l=(list (pair * *))]
  (~(gas ju j) l)
:: +get: [(jug) noun] -> (unit noun)
::
:: Returns the unit value at key in jug.
:: Examples
:: > =j `(jug @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: > j
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
:: 
:: > `(set @ud)`(get j 'a')
:: ~[1 2 3]
:: 
:: > `(set @ud)`(get j 'b')
:: ~[4 5 6]
:: 
:: > `(set @ud)`(get j 'c')
:: ~
:: Source
++  get
  |*  [m=(map) k=*]
  (~(get by m) k)
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
:: +make:  [(jug) noun noun] -> (jug)
::
:: Adds a key-set pairs to a jug.
:: Examples
:: > `(jug @t @ud)`(make ~[['a' ~[1 2 3]] ['b' ~[4 5 6]]])
:: {[p='b' q=~[4 5 6]] [p='a' q=~[1 2 3]]}
:: Source
++  make
  |*  [l=(set (pair * (set)))]
  (malt l)
:: +put: [(jug) noun noun] -> (jug)
::
:: Produces jug with element added to the set located
:: at key. If key isn't in jug, it will be added and
:: a new set created containing element.
:: Examples
:: > j
:: {[p='b' q={5 6 4}] [p='a' q={1 2 3}]}
:: 
:: > `(jug @t @ud)`(put j 'c' 5)
:: {[p='b' q={5 6 4}] [p='a' q={1 2 3}] [p='c' q={5}]}
:: 
:: > `(jug @t @ud)`(put j 'a' 4)
:: {[p='b' q={5 6 4}] [p='a' q={1 2 3 4}]}
:: 
:: > `(jug @t @ud)`(put j 'a' 1)
:: {[p='b' q={5 6 4}] [p='a' q={1 2 3}]}
:: Source
++  put
  |*  [j=(jug) k=* v=*]
  (~(put ju j) k v)
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
:: ++  gas  gas:libmap
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
:: ++  put  put:libmap
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
