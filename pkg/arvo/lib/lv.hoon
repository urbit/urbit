::
::::  Vector type in single-precision floating-point @rs
  ::
|%
++  lvs
  ^|
  ~%  %lvs  ..is  ~
  |_  r/$?($n $u $d $z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    `@lvs`(lsh 5 n n)
  ::
  ::    Fill value
  ++  fill
    ~/  %fill
    |=  [n=@ud v=@rs]  ^-  @lvs
    `@lvs`(mix (zeros n) (fil 5 n v))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    (fill n .1)
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvs  ^-  @ud
    ~_  leaf+"lagoon-fail"
    (end 5 1 (swp 5 u))
  ::
  ::    Produce a vector from `(list @u)` (of natural numbers)
  ++  make-nat
    |=  a=(list @u)  ^-  @lvs
    (make (turn a sun:rs))
  ::
  ::    APL-style index list
  ++  iota
    |=  a=@u  ^-  @lvs
    (make-nat (gulf 1 a))
  ++  make
    |=  [u=(list @rs)]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    `@lvs`(mix (rep 5 u) (zeros (lent u)))
  ++  unmake
    |=  [u=@lvs]  ^-  (list @rs)
    ~_  leaf+"lagoon-fail"
    `(list @rs)`(scag 5 (rip 5 u))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lvs i=@ud]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    (cut 5 [(dec i) 1] u)
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint
    |=  u=@lvs  ^-  tape
    `tape`(zing (join " " (turn (unmake u) |=(a=@rs <a>))))
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lvs i=@ud x=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    ?:  (gth i (length u))  !!
    `@lvs`(cat 5 (cat 5 (cut 5 [0 i] u) x) (cut 5 [+(i) (length u)] u))
  ::
  ::    Return larger of two single-precision floats.
  ++  max-rs
    |=  [a=@rs b=@rs]  ^-  @rs
    ?:  (gth:rs a b)  a  b
  ::
  ::    Find maximum value in array.
  ++  max
    |=  [u=@lvs]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    `@rs`(reel (unmake u) max-rs)
  ::
  ::    Return index of maximum value in array, 1-indexed
  ++  argmax
    |=  [u=@lvs]
    ~_  leaf+"lagoon-fail"
    (find ~[(max u)] (unmake u))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    |=  [u=@lvs v=@rs]  ^-  @lvs
    =/  ss  (fill (length u) v)
    (addv u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    |=  [u=@lvs v=@rs]  ^-  @lvs
    =/  ss  (fill (length u) v)
    (subv u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    |=  [u=@lvs v=@rs]  ^-  @lvs
    =/  ss  (fill (length u) v)
    (mulv u ss)
  ::
  ::    Scalar division
  ++  divs
    |=  [u=@lvs v=@rs]  ^-  @lvs
    =/  ss  (fill (length u) v)
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
    ~/  %funv
    |=  f=$-([@rs @rs] @rs)
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    (make (turn2 [(unmake u) (unmake v)] f))
  ::
  ::    Vector addition
  ++  addv  (funv add:rs)
  ::
  ::    Vector subtraction
  ++  subv  (funv sub:rs)
  ::
  ::    Vector multiplication
  ++  mulv  (funv mul:rs)
  ::
  ::    Vector division
  ++  divv  (funv div:rs)
  ::
  ::    Sum of elements
  ++  sum
    |=  [u=@lvs]  ^-  @rs
    (roll (unmake u) add:rs)
  ::
  ::    Product of elements
  ++  product
    |=  [u=@lvs]  ^-  @rs
    (roll (unmake u) mul:rs)
  ::
  ::  Linear algebraic operators
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    ~/  %inner
    |=  [u=@lvs v=@lvs]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    (sum (mulv u v))
  ++  outer  !!
  --
::
::::  Vector type in single-precision floating-point @rs
  ::
++  lvd
  ^|
  ~%  %lvd  ..is  ~
  |_  r/$?($n $u $d $z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    `@lvd`(lsh 6 n n)
  ::
  ::    Fill value
  ++  fill
    ~/  %fill
    |=  [n=@ud v=@rd]  ^-  @lvd
    `@lvd`(mix (zeros n) (fil 6 n v))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    (fill n .~1)
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvd  ^-  @ud
    ~_  leaf+"lagoon-fail"
    (end 6 1 (swp 6 u))
  ::
  ::    Produce a vector from `(list @u)` (of natural numbers)
  ++  make-nat
    |=  a=(list @u)  ^-  @lvd
    (make (turn a sun:rd))
  ::
  ::    APL-style index list
  ++  iota
    |=  a=@u  ^-  @lvd
    (make-nat (gulf 1 a))
  ++  make
    |=  [u=(list @rd)]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    `@lvd`(mix (rep 6 u) (zeros (lent u)))
  ++  unmake
    |=  [u=@lvd]  ^-  (list @rd)
    ~_  leaf+"lagoon-fail"
    `(list @rd)`(scag 6 (rip 6 u))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lvd i=@ud]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    (cut 6 [(dec i) 1] u)
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint
    |=  u=@lvd  ^-  tape
    `tape`(zing (join " " (turn (unmake u) |=(a=@rd <a>))))
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lvd i=@ud x=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    ?:  (gth i (length u))  !!
    `@lvd`(cat 6 (cat 6 (cut 6 [0 i] u) x) (cut 6 [+(i) (length u)] u))
  ::
  ::    Return larger of two single-precision floats.
  ++  max-rd
    |=  [a=@rd b=@rd]  ^-  @rd
    ?:  (gth:rd a b)  a  b
  ::
  ::    Find maximum value in array.
  ++  max-v
    |=  [u=@lvd]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    `@rd`(reel (unmake u) max-rd)
  ::
  ::    Return index of maximum value in array, 1-indexed
  ++  argmax
    |=  [u=@lvd]
    ~_  leaf+"lagoon-fail"
    (find ~[(max-v u)] (unmake u))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    |=  [u=@lvd v=@rd]  ^-  @lvd
    =/  ss  (fill (length u) v)
    (addv u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    |=  [u=@lvd v=@rd]  ^-  @lvd
    =/  ss  (fill (length u) v)
    (subv u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    |=  [u=@lvd v=@rd]  ^-  @lvd
    =/  ss  (fill (length u) v)
    (mulv u ss)
  ::
  ::    Scalar division
  ++  divs
    |=  [u=@lvd v=@rd]  ^-  @lvd
    =/  ss  (fill (length u) v)
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
    ~/  %funv
    |=  f=$-([@rd @rd] @rd)
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    (make (turn2 [(unmake u) (unmake v)] f))
  ::
  ::    Vector addition
  ++  addv  (funv add:rd)
  ::
  ::    Vector subtraction
  ++  subv  (funv sub:rd)
  ::
  ::    Vector multiplication
  ++  mulv  (funv mul:rd)
  ::
  ::    Vector division
  ++  divv  (funv div:rd)
  ::
  ::    Sum of elements
  ++  sum
    |=  [u=@lvd]  ^-  @rd
    (roll (unmake u) add:rd)
  ::
  ::    Product of elements
  ++  product
    |=  [u=@lvd]  ^-  @rd
    (roll (unmake u) mul:rd)
  ::
  ::  Linear algebraic operators
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    ~/  %inner
    |=  [u=@lvd v=@lvd]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    (sum (mulv u v))
  ++  outer  !!
  --
--
