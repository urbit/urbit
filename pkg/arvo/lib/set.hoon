  ::  /lib/set
::::
::  Utilities to replicate ++in behavior via gates.
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
::  To avoid shadowing the name `set`, import as `/+  *set`.
::
|@
:: +all: [(set) gate] -> ?
::
:: Computes the logical AND on the results of slamming
:: every element in set a with gate b.
:: Examples
:: > (all (make ~[1 2 3 4]) |=(a=@ (lth a 5)))
:: %.y
:: > (all (make ~[1 2 3 4 5]) |=(a=@ (lth a 5)))
:: %.n
:: Source
++  all
  |*  [s=(set) g=$-(* ?)]
  ^-  ?
  (~(all in s) g)
:: +all: [(set) gate] -> ?
::
:: Computes the logical AND on the results of slamming
:: every element in set a with gate b.  Alias for +all.
:: Examples
:: > (all (make ~[1 2 3 4]) |=(a=@ (lth a 5)))
:: %.y
:: > (all (make ~[1 2 3 4 5]) |=(a=@ (lth a 5)))
:: %.n
:: Source
++  and  all
:: +any: [(set) gate] -> ?
::
:: Computes the logical OR on the results of slamming
:: every element in set a with gate b.
:: Examples
:: > (any (make ~[2 3 4 5]) |=(a=@ (lth a 3)))
:: %.y
:: > (any (make ~[3 4 5]) |=(a=@ (lth a 3)))
:: %.n
:: Source
++  any
  |*  [s=(set) g=$-(* ?)]
  ^-  ?
  (~(any in s) g)
:: +apply: [(set) gate] -> (set)
::
:: Turn with key (Haskell map or apply-to-all). Produces
:: a new set transformed by the gate.  Alias for +run.
:: Examples
:: > =s (make ~["a" "A" "b" "c"])
:: > `(set tape)`s
:: {"A" "a" "c" "b"}
:: > (apply s cuss)
:: {"A" "C" "B"}
:: Source
++  apply  run
:: +apt: (set) -> ?
::
:: Check correctness.  Computes whether input has a
:: correct horizontal order and a correct vertical order.
:: Examples
:: > (apt ~)
:: %.y
:: > =a (make ~[1 2 3])
:: > a
:: [n=2 l={1} r={3}]
:: > (apt a)
:: %.y
:: 
:: > =z ?~(a ~ a(n 10))
:: > z
:: [n=10 l={1} r={3}]
:: > (apt z)
:: %.n
:: Source
++  apt
  |*  [s=(set)]
  ^-  ?
  ~(apt in s)
:: +bif: [(set) noun noun] -> [(set) (set)]
::
:: Splits the set into two sets, each containing the items
:: either side of the value but not including the value.
:: Examples
:: > =a `(set @)`(make (gulf 1 20))
:: > a
:: {17 8 20 13 11 5 19 7 15 10 18 14 6 12 9 1 2 3 16 4}
:: 
:: > (bif a 10)
:: [l=[n=11 l={17 8 20 13} r={5 19 7 15}] r=[n=12 l={18 14 6} r={9 1 2 3 16 4}]]
:: 
:: > `[(set @) (set @)]`(bif a 10)
:: [{17 8 20 13 11 5 19 7 15} {18 14 6 12 9 1 2 3 16 4}]
++  bif
  |*  [s=(set) k=*]
  ^+  [(set) (set)]
  (~(bif in s) k)
:: +del: [(set) noun] -> (set)
::
:: Returns a new set that does not contain the element
:: Examples
:: > `(set @)`(del (make ~[1 2 3 4 5]) 3)
:: {5 1 2 4}
:: > `(set @t)`(del (make ~['foo' 'bar' 'baz']) 'bar')
:: {'baz' 'foo'}
:: > `(set @)`(del (make ~[1 2 3 4 5]) 10)
:: {5 1 2 3 4}
:: > `(set @)`(del ~ 10)
:: {}
:: Source
++  del
  |*  [s=(set) k=*]
  ^+  s
  (~(del in s) k)
:: +dif: [(set) (set)] -> (set)
::
:: Computes the difference between two sets, producing the
:: set that contains the items in the first set that are not
:: in the second set.
:: Examples
:: > =a (make ~[1 2 3 4 5])
:: > =b (make ~[3 4])

:: > `(set @)`(dif a b)
:: {5 1 2}
:: Source
++  dif
  |*  [a=(set) b=(set)]
  ^+  a
  (~(dif in a) b)
:: +diff-left: [(set) (set)] -> (set)
::
:: Computes the difference between two sets, producing the
:: set that contains the items in the first set that are not
:: in the second set.  Alias for +dif.
:: Examples
:: > =a (make ~[1 2 3 4 5])
:: > =b (make ~[3 4])
:: > `(set @)`(diff=left a b)
:: {5 1 2}
:: Source
++  diff-left  dif
:: +diff-right: [(set) (set)] -> (set)
::
:: Computes the difference between two sets, producing the
:: set that contains the items in the second set that are not
:: in the first set.
:: Examples
:: > =a (make ~[1 2 3 4 5])
:: > =b (make ~[3 4 6])
:: > `(set @)`(diff-right a b)
:: {6}
:: Source
++  diff-right
  |*  [a=(set) b=(set)]
  ^+  a
  (dif b a)
:: +diff-symmetric: [(set) (set)] -> (set)
::
:: Computes the difference between two sets, producing the
:: set that contains the items that are in one set but not
:: in the other.
:: Examples
:: > =a (make ~[1 2 3 4 5])
:: > =b (make ~[3 4 6])
:: > `(set @)`(diff-symmetric a b)
:: {1 2 5 6}
:: Source
++  diff-symmetric
  |*  [a=(set) b=(set)]
  ^+  a
  %-  %~  uni  in
    (diff-left a b)
  (diff-right a b)
:: +dig: [(set) noun] -> (unit *)
::
:: Produces the tree address of element within set.
:: Examples
:: > =a (make ~[1 2 3 4 5 6 7])
:: > -.a
:: n=6
:: > (dig a 7)
:: [~ 12]
:: > (dig a 2)
:: [~ 60]
:: > (dig a 6)
:: [~ 2]
:: > (dig a 10)
:: ~
:: Source
++  dig
  |*  [s=(set) k=*]
  ^+  (unit @)
  (~(dig in s) k)
:: +filter: [(set) gate] -> (set)
::
:: Produces a new set with only the elements for
:: which the gate produces %.y against the element.
:: Examples
:: > =myset `(set @)`(make (limo ~[1 2 3 4]))
:: > myset
:: {1 2 3 4}
:: > `(set @)`(filter myset (curr gth 2))
:: {3 4}
:: Source
++  filter
  |*  [a=(set) b=$-(* ?)]
  =/  vl  ~(tap in a)
  =|  res=(list _?>(?=(^ a) n.a))
  |-  ^+  a
  ?~  vl  (silt res)
  ?.  (b i.vl)
    $(vl t.vl)
  $(vl t.vl, res [i.vl res])
:: +gas: [(set) (list)] -> (set)
::
:: Insert list of elements into set.
:: Examples
:: > =a (make ~['foo' 'bar' 'baz'])
:: > `(set @t)`a
:: {'bar' 'baz' 'foo'}

:: > `(set @t)`(gas a ~['foo' 'foo' 'foo' 'foo'])
:: {'bar' 'baz' 'foo'}

:: > `(set @t)`(gas a ~['abc' 'xyz' '123'])
:: {'xyz' 'bar' 'baz' 'foo' 'abc' '123'}
:: Source
++  gas
  |*  [s=(set) l=(list (pair))]
  ^+  s
  (~(gas in s) l)
:: +has: [(set) noun] -> ?
::
:: Returns whether set contains element.
:: Examples
:: > =myset `(set @)`(make (limo ~[1 2 3 4]))
:: > myset
:: {1 2 3 4}
:: > (has myset 1)
:: %.y
:: > (has myset 5)
:: %.n
:: Source
++  has
  |*  [s=(set) k=*]
  ^-  ?
  (~(has in s) k)
:: +int: [(set) (set)] -> (set)
::
:: Produces a set of the intersection between two sets
:: of the same type.  In case of conflict, the value from
:: the first set is used.
:: Examples
:: > `(set @tD)`(int (silt "foobar") (silt "bar"))
:: {'r' 'b' 'a'}

:: > `(set @tD)`(int (silt "foobar") ~)
:: {}

:: > `(set @tD)`(int (silt "foobar") (silt "baz"))
:: {'b' 'a'}
:: Source
++  int
  |*  [a=(set) b=(set)]
  ^+  a
  (~(int in a) b)
:: +int: [(set) (set)] -> (set)
::
:: Produces a set of the intersection between two sets
:: of the same type.  In case of conflict, the value from
:: the first set is used.  Alias for +int.
:: Examples
:: > `(set @tD)`(intersect (silt "foobar") (silt "bar"))
:: {'r' 'b' 'a'}

:: > `(set @tD)`(intersect (silt "foobar") ~)
:: {}

:: > `(set @tD)`(intersect (silt "foobar") (silt "baz"))
:: {'b' 'a'}
:: Source
++  intersect  int
:: +make: (list) -> (set)
::
:: Produces a set from a list of elements.
:: Alias for +silt.
:: Examples
:: > `(set @tas)`(make `(list @tas)`~[%a %b %c %d])
:: {%b %d %a %c}
:: Source
++  make  silt
:: +or: [(set) gate] -> ?
::
:: Computes the logical OR on the results of slamming
:: every element in set a with gate b.  Alias for +any.
:: Examples
:: > (or (make ~[2 3 4 5]) |=(a=@ (lth a 3)))
:: %.y
:: > (or (make ~[3 4 5]) |=(a=@ (lth a 3)))
:: %.n
:: Source
++  or  any
:: +put: [(set) noun] -> (set)
::
:: Returns a new set that contains the new element.
:: Examples
:: > `(set @)`(put (make ~[1 2 3]) 4)
:: {1 2 3 4}
:: > `(set @)`(put `(set @)`~ 42)
:: {42}
:: Source
++  put
  |*  [s=(set) v=*]
  ^+  s
  (~(put in s) v)
:: +reduce: [(set) gate] -> noun
::
:: Reduce; accumulate the elements of set using gate.
:: Alias for +rep.
:: Examples
:: > (reduce (make ~[1 2 3 4 5]) add)
:: b=15
:: > `@t`(reduce (silt ~['foo' 'bar' 'baz']) |=(a=[@ @] (cat 3 a)))
:: 'foobarbaz'
:: Source
++  reduce  rep
:: +rep: [(set) gate] -> noun
::
:: Reduce; accumulate the elements of set using gate.
:: Examples
:: > (rep (make ~[1 2 3 4 5]) add)
:: b=15
:: > `@t`(rep (silt ~['foo' 'bar' 'baz']) |=(a=[@ @] (cat 3 a)))
:: 'foobarbaz'
:: Source
++  rep
  |*  [s=(set) g=_=>(~ |=([* *] +<+))]
  ^-  *
  (~(rep in s) g)
:: +run: [(set) gate] -> (set)
::
:: Turn with key (Haskell map or apply-to-all). Produces
:: a new set transformed by the gate.  Alias for +run.
:: Examples
:: > =s (make ~["a" "A" "b" "c"])
:: > `(set tape)`s
:: {"A" "a" "c" "b"}
:: > (run s cuss)
:: {"A" "C" "B"}
:: Source
++  run
  |*  [s=(set) g=gate]
  ^+  s
  (~(run in s) g)
:: +size: (set) -> @
::
:: Produces the depth (size) of the set.  Alias for +wyt.
:: Examples
:: > =a `(set @)`(make ~[1 2 3])
:: > =b `(set @)`(make ~[1 2 3 4 5])
:: > a
:: {1 2 3}
:: > b
:: {5 1 2 3 4}
:: 
:: > (size a)
:: 3
:: > (size b)
:: 5
:: Source
++  size  wyt
:: +tap: (set) -> (list)
::
:: Produces the list of values in set.
:: Examples
:: > =myset `(set @t)`(make ~['a' 'b' 'c'])
:: > myset
:: {'b' 'a' 'c'}
:: > (tap myset)
:: ~['b' 'a' 'c']
:: Source
:: Source
++  tap
  |*  [s=(set)]
  ^-  (list _?>(?=(^ s) n.s))
  ~(tap in s)
:: +uni: [(set) (set)] -> (set)
::
:: Produces a set of the union of two sets of the same type.
:: Examples
:: > =a (silt ~[1 2 3 4 5])
:: > =b (silt ~[4 5 6 7 8])
:: 
:: > `(set @)`(uni a b)
:: {8 5 7 6 1 2 3 4}
:: > `(set @)`(uni a ~)
:: {5 1 2 3 4}
:: > `(set @)`(uni `(set @)`~ b)
:: {8 5 7 6 4}
:: Source
++  uni
  |*  [a=(set) b=(set)]
  (~(uni in a) b)
:: +union: [(set) (set)] -> (set)
::
:: Produces a set of the union of two sets of the same type.
:: Alias for +uni.
:: Examples
:: > =a (silt ~[1 2 3 4 5])
:: > =b (silt ~[4 5 6 7 8])
:: 
:: > `(set @)`(uni a b)
:: {8 5 7 6 1 2 3 4}
:: > `(set @)`(uni a ~)
:: {5 1 2 3 4}
:: > `(set @)`(uni `(set @)`~ b)
:: {8 5 7 6 4}
:: Source
++  union  uni
:: +values: (set) -> (list)
::
:: Produces the list of values in set.  Alias for +tap.
:: Examples
:: > =myset `(set @t)`(make ~['a' 'b' 'c'])
:: > myset
:: {'b' 'a' 'c'}
:: > (values myset)
:: ~['b' 'a' 'c']
:: Source
++  values  tap
:: +wyt: (set) -> @
::
:: Produces the depth (size) of the set.
:: Examples
:: > =a `(set @)`(make ~[1 2 3])
:: > =b `(set @)`(make ~[1 2 3 4 5])
:: > a
:: {1 2 3}
:: > b
:: {5 1 2 3 4}
:: 
:: > (wyt a)
:: 3
:: > (wyt b)
:: 5
:: Source
++  wyt
  |*  [s=(set)]
  ^-  @
  ~(wyt in s)
--