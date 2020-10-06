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
    :: There is a vanishingly small chance of getting a NaN in a vector which
    :: collides with this, but it's like getting a hash table collision, very
    :: very unlikely.  Also the Urbit NaN is hard-coded, `SINGNAN`.
    =/  nan  0xffff.ffff
    `@lvs`(not 5 +(n) (rap 5 ~[(fil 5 n nan) (not 5 1 n)]))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  one  (bit:rs [%f s=%.y e=-0 a=1])
    =/  not-one  (not 5 1 one)
    `@lvs`(not 5 +(n) (rap 5 ~[(fil 5 n not-one) (not 5 1 n)]))
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvs  ^-  @ud
    ~_  leaf+"lagoon-fail"
    (snag 0 (flop (rip 5 u)))
  ::
  ::    Produce a vector from `(list @rs)`
  ++  not-rs
    |=  a=@rs
    ~_  leaf+"lagoon-fail"
    (not 5 1 a)
  ++  make
    |=  [u=(list @rs)]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(lent u)
    =/  not-u  (turn u not-rs)
    `@lvs`(not 5 +(n) (rep 5 (snoc not-u (not 5 1 n))))
  ++  unmake
    |=  [u=@lvs]  ^-  (list @rs)
    ~_  leaf+"lagoon-fail"
    %-  flop
    =/  n  `@ud`(length u)
    ::  Leading and trailing zeros get cut off (because we flop or swp):
    ::  we have to pin values on both ends to compensate, then trim.
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    (oust [n 1] (oust [0 2] (rip 5 ut)))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lvs i=@ud]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    (snag (dec i) (unmake u))
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint  !!
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lvs i=@ud x=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    (make (weld (scag (dec i) (unmake u)) [x (slag i (unmake u))]))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    ~/  %adds
    |=  [u=@lvs v=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rs`(add:rs ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar subtraction
  ++  subs
    ~/  %subs
    |=  [u=@lvs v=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rs`(sub:rs ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar multiplication
  ++  muls
    ~/  %muls
    |=  [u=@lvs v=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rs`(mul:rs ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar division
  ++  divs
    ~/  %divs
    |=  [u=@lvs v=@rs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rs`(div:rs ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector addition
  ++  addv
    ~/  %addv
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  vt  (swp 5 (cat 5 nan (cat 5 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 5 vt)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rs`(add:rs ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector subtraction
  ++  subv
    ~/  %subv
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  vt  (swp 5 (cat 5 nan (cat 5 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 5 vt)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rs`(sub:rs ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector multiplication
  ++  mulv
    ~/  %mulv
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  vt  (swp 5 (cat 5 nan (cat 5 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 5 vt)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rs`(mul:rs ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector division
  ++  divv
    ~/  %divv
    |=  [u=@lvs v=@lvs]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  vt  (swp 5 (cat 5 nan (cat 5 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 5 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 5 vt)))
    =/  w  `(list @rs)`~
    =/  count  0
    |-  ^-  @lvs
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rs`(div:rs ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Sum of elements
  ++  sum
    ~/  %sum
    |=  [u=@lvs]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .0
    |-  ^-  @rs
      ?:  =(count n)  `@rs`sum
    $(count +(count), sum (add:rs sum (get u +(count))))
  ::
  ::    Product of elements
  ++  product
    ~/  %product
    |=  [u=@lvs]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .0
    |-  ^-  @rs
      ?:  =(count n)  `@rs`sum
    $(count +(count), sum (mul:rs sum (get u +(count))))
  ::
  ::  Linear algebraic operators
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    ~/  %inner
    |=  [u=@lvs v=@lvs]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .0
    |-  ^-  @rs
      ?:  =(count n)  `@rs`sum
    $(count +(count), sum (add:rs sum (mul:rs (get u +(count)) (get v +(count)))))
  ++  outer  !!
  --
::
::::  Vector type in double-precision floating-point @rd
  ::
++  lvd
  ^|
  ~%  %lvd  ..is  ~
  |_  r/$?($n $u $d $z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulatord
  ::
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    :: There is a vanishingly small chance of getting a NaN in a vector which
    :: collides with this, but it's like getting a hash table collision, very
    :: very unlikely.  Also the Urbit NaN is hard-coded, `DOUBNAN`.
    =/  nan  0xffff.ffff.ffff.ffff
    `@lvd`(not 6 +(n) (rap 6 ~[(fil 6 n nan) (not 6 1 n)]))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  n=@ud  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  one  (bit:rd [%f s=%.y e=-0 a=1])
    =/  not-one  (not 6 1 one)
    `@lvd`(not 6 +(n) (rap 6 ~[(fil 6 n not-one) (not 6 1 n)]))
  ::
  ::    Length of vector
  ++  length
    |=  u=@lvd  ^-  @ud
    ~_  leaf+"lagoon-fail"
    (snag 0 (flop (rip 6 u)))
  ::
  ::    Produce a vector from `(list @rd)`
  ++  not-rd
    |=  a=@rd
    ~_  leaf+"lagoon-fail"
    (not 6 1 a)
  ++  make
    |=  [u=(list @rd)]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(lent u)
    =/  not-u  (turn u not-rd)
    `@lvd`(not 6 +(n) (rep 6 (snoc not-u (not 6 1 n))))
  ++  unmake
    |=  [u=@lvd]  ^-  (list @rd)
    ~_  leaf+"lagoon-fail"
    %-  flop
    =/  n  `@ud`(length u)
    ::  Leading and trailing zeros get cut off (because we flop or swp):
    ::  we have to pin values on both ends to compensate, then trim.
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    (oust [n 1] (oust [0 2] (rip 6 ut)))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lvd i=@ud]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    (snag (dec i) (unmake u))
  ::
  ::    Pretty-print the contents of the vector.
  ++  pprint  !!
  ::
  ::    Set the value of an element within a vector, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lvd i=@ud x=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    (make (weld (scag (dec i) (unmake u)) [x (slag i (unmake u))]))
  ::
  ::    Return argument of maximum value in array, 1-indexed
  ++  argmax
    |=  [u=@lvd]  ^-  @ud
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  max-val  `@rs`0xfeff.ffff
    =/  max-arg  0
    |-  ^-  @ud
      ?:  =(count n)  +(max-arg)
      =/  cur-val  `@rs`(get u +(count))
      ?:  (gth:rs cur-val max-val)  $(count +(count), max-arg count)
    $(count +(count), max-arg max-arg)
  ::
  ::  Arithmetic operatord
  ::
  ::    Scalar addition
  ++  adds
    ~/  %adds
    |=  [u=@lvd v=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rd`(add:rd ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar subtraction
  ++  subs
    ~/  %subs
    |=  [u=@lvd v=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rd`(sub:rd ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar multiplication
  ++  muls
    ~/  %muls
    |=  [u=@lvd v=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rd`(mul:rd ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Scalar division
  ++  divs
    ~/  %divs
    |=  [u=@lvd v=@rd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  wl  `@rd`(div:rd ul v)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector addition
  ++  addv
    ~/  %addv
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  vt  (swp 6 (cat 6 nan (cat 6 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 6 vt)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rd`(add:rd ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector subtraction
  ++  subv
    ~/  %subv
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  vt  (swp 6 (cat 6 nan (cat 6 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 6 vt)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rd`(sub:rd ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector multiplication
  ++  mulv
    ~/  %mulv
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  vt  (swp 6 (cat 6 nan (cat 6 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 6 vt)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rd`(mul:rd ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Vector division
  ++  divv
    ~/  %divv
    |=  [u=@lvd v=@lvd]  ^-  @lvd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  nan  0xffff.ffff.ffff.ffff
    =/  ut  (swp 6 (cat 6 nan (cat 6 u nan)))
    =/  vt  (swp 6 (cat 6 nan (cat 6 v nan)))
    =/  uu  (oust [n 1] (oust [0 2] (rip 6 ut)))
    =/  vv  (oust [n 1] (oust [0 2] (rip 6 vt)))
    =/  w  `(list @rd)`~
    =/  count  0
    |-  ^-  @lvd
      ?:  =(count n)  (make w)
      =/  ul  (snag count uu)
      =/  vl  (snag count vv)
      =/  wl  `@rd`(div:rd ul vl)
    $(count +(count), w (weld ~[wl] w))
  ::
  ::    Sum of elements
  ++  sum
    ~/  %sum
    |=  [u=@lvd]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .~0
    |-  ^-  @rd
      ?:  =(count n)  `@rd`sum
    $(count +(count), sum (add:rd sum (get u +(count))))
  ::
  ::    Product of elements
  ++  product
    ~/  %product
    |=  [u=@lvd]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .~0
    |-  ^-  @rd
      ?:  =(count n)  `@rd`sum
    $(count +(count), sum (mul:rd sum (get u +(count))))
  ::
  ::  Linear algebraic operatord
  ::
  ::    Inner or Euclidean dot product, a · b
  ++  inner
    ~/  %inner
    |=  [u=@lvd v=@lvd]  ^-  @rd
    ~_  leaf+"lagoon-fail"
    =/  n  `@ud`(length u)
    =/  count  0
    =/  sum  .~0
    |-  ^-  @rd
      ?:  =(count n)  `@rd`sum
    $(count +(count), sum (add:rd sum (mul:rd (get u +(count)) (get v +(count)))))
  ++  outer  !!
  --
--
