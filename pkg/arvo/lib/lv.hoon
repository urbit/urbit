::
::::  Vector type in single-precision floating-point @rs
  ::
::  Conventions:
::
::  m,n,p   always dimensions
::  i,j,k   always indices (also ii,jj,kk for coder-spec not user-spec)
::  a,b,c   always lists
::  u,v,w   always vector/matrix atoms
::  s,t     always real/floats
|%
++  lvs
  ^|
  ~%  %lvs  ..part  ~
  |_  r=$?(%n %u %d %z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::
  ::    Zeroes
  ++  zeros
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lace-fail"
    `@lvs`(lsh [5 n] 1)   :: pin at head for leading zeros
  ::
  ::    Fill value
  ++  fill
    |=  [n=@ud s=@rs]  ^-  @lvs
    `@lvs`(mix (zeros n) (fil 5 n s))
  ::
  ::    Ones
  ++  ones
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lace-fail"
    (fill n .1)
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvs  ^-  @ud
    ~_  leaf+"lace-fail"
    =/  ell  (met 5 u)
    ?:  (gth ell 1)  (dec ell)  0
  ::
  ::    Produce a vector from `(list @u)` (of natural numbers)
  ++  make-nat
    |=  a=(list @u)  ^-  @lvs
    (make (turn a sun:rs))
  ::
  ::    APL-style index list
  ++  iota
    |=  n=@u  ^-  @lvs
    (make-nat (gulf 1 n))
  ++  make
    |=  [a=(list @rs)]  ^-  @lvs
    ~_  leaf+"lace-fail"
    `@lvs`(mix (rep [5 1] a) (zeros (lent a)))
  ++  unmake
    |=  [u=@lvs]  ^-  (list @rs)
    ~_  leaf+"lace-fail"
    (flop `(list @rs)`+:(flop (rip 5 u)))
  ++  append
    |=  [u=@lvs s=@rs]  ^-  @lvs
    (make (snoc (unmake u) s))
    ::  XX could be done faster with a mix/lsh
  ::
  ::  Yield the substring [lhs:rhs] inclusive
  ++  subvector
    |=  [u=@lvs lhs=@ud rhs=@ud]
    (mix (zeros +((sub rhs lhs))) (cut 5 [(dec lhs) +((sub rhs lhs))] u))
  ::
  ::  |x|
  ++  abs
    |=  [s=@rs]
    ?:  (gth:rs s .0)  s  (sub:rs .0 s)
  ::
  ::  |x-y| <= tol
  ++  isclose
    |=  [s=@rs t=@rs tol=@rs]
    (lth:rs (abs (sub:rs s t)) tol)
  ++  near0
    |=  s=@rs
    (isclose s .0 .1e-6)
  ++  all-close
    |=  [u=@lvs v=@lvs tol=@rs]
    =/  n  `@ud`(length u)
    =/  count  1
    =/  off  0
    |-  ^-  ?
      ?:  (gth off 0)  %.n
      ?:  (gth count n)  %.y
    $(count +(count), off (add off ?:((isclose (get u count) (get v count) tol) 0 1)))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    |=  [u=@lvs i=@ud]  ^-  @rs
    ~_  leaf+"lace-fail"
    (cut 5 [(dec i) 1] u)
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint
    |=  u=@lvs  ^-  tape
    `tape`(zing (join " " (turn (unmake u) |=(s=@rs <s>))))
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    |=  [u=@lvs i=@ud s=@rs]  ^-  @lvs
    ~_  leaf+"lace-fail"
    ?:  (gth i (length u))  !!
    =/  full  0xffff.ffff
    =/  n  (length u)
    =/  mask  (mix (fil 5 +(n) full) (lsh [5 (dec i)] full))
    =/  cleared  (dis mask u)
    =/  value  (lsh [5 (dec i)] s)
    (con cleared value)
  ::
  ::    Return larger of two single-precision floats.
  ++  max-rs
    |=  [s=@rs t=@rs]  ^-  @rs
    ?:  (gth:rs s t)  s  t
  ::
  ::    Find maximum value in array.
  ++  max
    |=  [u=@lvs]  ^-  @rs
    ~_  leaf+"lace-fail"
    `@rs`(reel (unmake u) max-rs)
  ::
  ::    Return index of maximum value in array, 1-indexed
  ::    DOES NOT handle repeated values, returns first match
  ++  argmax
    |=  [u=@lvs]
    ~_  leaf+"lace-fail"
    +(+:(find ~[(max u)] (unmake u)))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    |=  [u=@lvs s=@rs]  ^-  @lvs
    =/  ss  (fill (length u) s)
    (addv u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    |=  [u=@lvs s=@rs]  ^-  @lvs
    =/  ss  (fill (length u) s)
    (subv u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    |=  [u=@lvs s=@rs]  ^-  @lvs
    =/  ss  (fill (length u) s)
    (mulv u ss)
  ::
  ::    Scalar division
  ++  divs
    |=  [u=@lvs s=@rs]  ^-  @lvs
    =/  ss  (fill (length u) s)
    (divv u ss)
  ::
  ::    Turn on a gate of two variables.
  ++  turn2  :: I guess not in hoon.hoon
    |=  [[a=(list @rs) b=(list @rs)] f=$-([@rs @rs] @rs)]
    ^-  (list @rs)
    ?+  +<-  ~|(%turn2-length !!)
      [~ ~]  ~
      [^ ^]  [(f i.a i.b) $(a t.a, b t.b)]
    ==
  ::
  ::    Apply a two-variable function across a vector input.
  ++  funv
    |=  f=$-([@rs @rs] @rs)
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lace-fail"
    (make (turn2 [(unmake u) (unmake v)] f))
  ::
  ::    Vector addition
  ++  addv  ~/  %addv  |=([@lvs @lvs] %.(+< (funv add:rs)))
  ::
  ::    Vector subtraction
  ++  subv  ~/  %subv  |=([@lvs @lvs] %.(+< (funv sub:rs)))
  ::
  ::    Vector multiplication
  ++  mulv  ~/  %mulv  |=([@lvs @lvs] %.(+< (funv mul:rs)))
  ::
  ::    Vector division
  ++  divv  ~/  %divv  |=([@lvs @lvs] %.(+< (funv div:rs)))
  ::
  ::    Sum of elements
  ++  sum
    |=  [u=@lvs]  ^-  @rs
    (roll (unmake u) add:rs)
  ::
  ::    Cumulative sum of elements
  ++  cumsum
    |=  [u=@lvs]  ^-  @lvs
    =/  n  (length u)
    =/  uu  (unmake u)
    =/  v  (zeros n)
    =/  index  1
    |-  ^-  @lvs
      ?:  (gth index n)  v
    $(index +(index), v (set v index (sum (subvector u 1 index))))
  ::
  ::    Product of elements
  ++  product
    |=  [u=@lvs]  ^-  @rs
    (roll (unmake u) |:([a=.1 b=.1] (mul:rs a b)))
  ::
  ::  Linear algebraic operators
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    |=  [u=@lvs v=@lvs]  ^-  @rs
    ~_  leaf+"lace-fail"
    (sum (mulv u v))
  ++  outer  !!  :: unimplemented pending @lm type
  ++  catenate
    |=  [u=@lvs v=@lvs]
    ~_  leaf+"lace-fail"
    (make (weld (unmake u) (unmake v)))
    :: XX slow way, do in bits
  --
::
::::  Vector type in double-precision floating-point @rd
  ::
++  lvd
  ^|
  ~%  %lvd  ..part  ~
  |_  r=$?(%n %u %d %z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::
  ::    Zeroes
  ++  zeros
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lace-fail"
    `@lvd`(lsh [6 n] 1)   :: pin at head for leading zeros
  ::
  ::    Fill value
  ++  fill
    |=  [n=@ud s=@rd]  ^-  @lvd
    `@lvd`(mix (zeros n) (fil 6 n s))
  ::
  ::    Ones
  ++  ones
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lace-fail"
    (fill n .~1)
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvd  ^-  @ud
    ~_  leaf+"lace-fail"
    =/  ell  (met 6 u)
    ?:  (gth ell 1)  (dec ell)  0
  ::
  ::    Produce a vector from `(list @u)` (of natural numbers)
  ++  make-nat
    |=  a=(list @u)  ^-  @lvd
    (make (turn a sun:rd))
  ::
  ::    APL-style index list
  ++  iota
    |=  n=@u  ^-  @lvd
    (make-nat (gulf 1 n))
  ++  make
    |=  [a=(list @rd)]  ^-  @lvd
    ~_  leaf+"lace-fail"
    `@lvd`(mix (rep [6 1] a) (zeros (lent a)))
  ++  unmake
    |=  [u=@lvd]  ^-  (list @rd)
    ~_  leaf+"lace-fail"
    (flop `(list @rd)`+:(flop (rip 6 u)))
  ++  append
    |=  [u=@lvd s=@rd]  ^-  @lvd
    (make (snoc (unmake u) s))
    ::  XX could be done faster with a mix/lsh
  ::
  ::  Yield the substring [lhs:rhs] inclusive
  ++  subvector
    |=  [u=@lvd lhs=@ud rhs=@ud]
    (mix (zeros +((sub rhs lhs))) (cut 6 [(dec lhs) +((sub rhs lhs))] u))
  ::
  ::  |x|
  ++  abs
    |=  [s=@rd]
    ?:  (gth:rd s .~0)  s  (sub:rd .~0 s)
  ::
  ::  |x-y| <= tol
  ++  isclose
    |=  [s=@rd t=@rd tol=@rd]
    (lth:rd (abs (sub:rd s t)) tol)
  ++  near0
    |=  s=@rd
    (isclose s .~0 .~1e-6)
  ++  all-close
    |=  [u=@lvd v=@lvd tol=@rd]
    =/  n  `@ud`(length u)
    =/  count  1
    =/  off  0
    |-   ^-  ?
      ?:  (gth off 0)  %.n
      ?:  (gth count n)  %.y
    $(count +(count), off (add off ?:((isclose (get u count) (get v count) tol) 0 1)))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    |=  [u=@lvd i=@ud]  ^-  @rd
    ~_  leaf+"lace-fail"
    (cut 6 [(dec i) 1] u)
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint
    |=  u=@lvd  ^-  tape
    `tape`(zing (join " " (turn (unmake u) |=(s=@rd <s>))))
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    |=  [u=@lvd i=@ud s=@rd]  ^-  @lvd
    ~_  leaf+"lace-fail"
    ?:  (gth i (length u))  !!
    =/  full  0xffff.ffff.ffff.ffff
    =/  n  (length u)
    =/  mask  (mix (fil 6 +(n) full) (lsh [6 (dec i)] full))
    =/  cleared  (dis mask u)
    =/  value  (lsh [6 (dec i)] s)
    (con cleared value)
  ::
  ::    Return larger of two single-precision floats.
  ++  max-rd
    |=  [s=@rd t=@rd]  ^-  @rd
    ?:  (gth:rd s t)  s  t
  ::
  ::    Find maximum value in array.
  ++  max
    |=  [u=@lvd]  ^-  @rd
    ~_  leaf+"lace-fail"
    `@rd`(reel (unmake u) max-rd)
  ::
  ::    Return index of maximum value in array, 1-indexed
  ::    DOES NOT handle repeated values, returns first match
  ++  argmax
    |=  [u=@lvd]
    ~_  leaf+"lace-fail"
    +(+:(find ~[(max u)] (unmake u)))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    |=  [u=@lvd s=@rd]  ^-  @lvd
    =/  ss  (fill (length u) s)
    (addv u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    |=  [u=@lvd s=@rd]  ^-  @lvd
    =/  ss  (fill (length u) s)
    (subv u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    |=  [u=@lvd s=@rd]  ^-  @lvd
    =/  ss  (fill (length u) s)
    (mulv u ss)
  ::
  ::    Scalar division
  ++  divs
    |=  [u=@lvd s=@rd]  ^-  @lvd
    =/  ss  (fill (length u) s)
    (divv u ss)
  ::
  ::    Turn on a gate of two variables.
  ++  turn2  :: I guess not in hoon.hoon
    |=  [[a=(list @rd) b=(list @rd)] f=$-([@rd @rd] @rd)]
    ^-  (list @rd)
    ?+  +<-  ~|(%turn2-length !!)
      [~ ~]  ~
      [^ ^]  [(f i.a i.b) $(a t.a, b t.b)]
    ==
  ::
  ::    Apply a two-variable function across a vector input.
  ++  funv
    |=  f=$-([@rd @rd] @rd)
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lace-fail"
    (make (turn2 [(unmake u) (unmake v)] f))
  ::
  ::    Vector addition
  ++  addv  ~/  %addv  |=([@lvd @lvd] %.(+< (funv add:rd)))
  ::
  ::    Vector subtraction
  ++  subv  ~/  %subv  |=([@lvd @lvd] %.(+< (funv sub:rd)))
  ::
  ::    Vector multiplication
  ++  mulv  ~/  %mulv  |=([@lvd @lvd] %.(+< (funv mul:rd)))
  ::
  ::    Vector division
  ++  divv  ~/  %divv  |=([@lvd @lvd] %.(+< (funv div:rd)))
  ::
  ::    Sum of elements
  ++  sum
    |=  [u=@lvd]  ^-  @rd
    (roll (unmake u) add:rd)
  ::
  ::    Cumulative sum of elements
  ++  cumsum
    |=  [u=@lvd]  ^-  @lvd
    =/  n  (length u)
    =/  uu  (unmake u)
    =/  v  (zeros n)
    =/  index  1
    |-  ^-  @lvd
      ?:  (gth index n)  v
    $(index +(index), v (set v index (sum (subvector u 1 index))))
  ::
  ::    Product of elements
  ++  product
    |=  [u=@lvd]  ^-  @rd
    (roll (unmake u) |:([a=.~1 b=.~1] (mul:rd a b)))
  ::
  ::  Linear algebraic operators
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    |=  [u=@lvd v=@lvd]  ^-  @rd
    ~_  leaf+"lace-fail"
    (sum (mulv u v))
  ++  outer  !!  :: unimplemented pending @lm type
  ++  catenate
    |=  [u=@lvd v=@lvd]
    ~_  leaf+"lace-fail"
    (make (weld (unmake u) (unmake v)))
    :: XX slow way, do in bits
  --
--
