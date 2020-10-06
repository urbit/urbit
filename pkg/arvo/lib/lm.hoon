::
::::  Matrix type in single-precision floating-point @rs
  ::
/+  *lv
|%
++  lms
  ^|
  ~%  %lms  ..is  ~
  |_  r/$?($n $u $d $z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::
  ::    Cantor pairing function
  ::      This is used so that the dimensions can be stored as a unique number.
  ++  cantor
    ~/  %cantor
    |=  [m=@ud n=@ud]  ^-  @ud
    (add (div (mul (add m n) (add m (add n 1))) 2) n)
  ++  decantor
    ~/  %decantor
    |=  [z=@ud]  ^-  (list @ud)
    =/  w  (div (sub p:(sqt (add (mul 8 z) 1)) 1) 2)
    =/  t  (div (add (mul w w) w) 2)
    =/  y  (sub z t)
    =/  x  (sub w y)
    ~[x y]
  ::
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    :: There is a vanishingly small chance of getting a NaN in a matrix which
    :: collides with this, but it's like getting a hash table collision, very
    :: very unlikely.  Also the Urbit NaN is hard-coded, `SINGNAN`.
    =/  nan  0xffff.ffff
    =/  p  (cantor m n)
    `@lms`(not 5 +((mul m n)) (rap 5 ~[(fil 5 (mul m n) nan) (not 5 1 p)]))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  one  (bit:rs [%f s=%.y e=-0 a=1])
    =/  not-one  (not 5 1 one)
    =/  p  (cantor m n)
    `@lms`(not 5 +((mul m n)) (rap 5 ~[(fil 5 (mul m n) not-one) (not 5 1 p)]))
  ::
  ::    Identity
  ++  id
    ~/  %id
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  u  (zeros m n)
    =/  i  1
    |-
      ?:  |(=(i m) =(i n))  u
    $(i +(i), u (set u i i .1))  :: TODO test Hoon version (jet works)
  ::
  ::    Shape of matrix
  ++  shape
    |=  u=@lms  ^-  (list @ud)
    (decantor (snag 0 (flop (rip 5 u))))
  ::
  ::    Produce a matrix from `(list (list @rs))`
  ::    Rows across, columns "down" (meaning modulus m)
  ++  not-rs
    |=  a=@rs
    ~_  leaf+"lagoon-fail"
    (not 5 1 a)
  ++  make
    |=  [u=(list (list @rs))]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  m  `@ud`(lent u)
    =/  n  `@ud`(lent (snag 0 u))
    =/  size  (mul m n)
    =/  p  (cantor m n)
    ~&  [m n size p]
    =/  i  0  :: index over rows
    =/  not-w  `(list @rs)`~
    |-  ^-  @lms
      ?:  =(i m)  `@lms`(not 5 +(size) (rep 5 (weld `(list @rs)`not-w ~[(not 5 1 p)])))
      =/  not-u  `(list @rs)`(flop (turn (snag i u) not-rs))
    $(i +(i), not-w `(list @rs)`(weld `(list @rs)`not-u `(list @rs)`not-w))
  ++  unmake
    |=  [u=@lms]  ^-  (list (list @rs))
    ~_  leaf+"lagoon-fail"
    %-  flop
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  size  (mul m n)
    =/  p  (cantor m n)
    =/  nan  0xffff.ffff
    =/  ut  (swp 5 (cat 5 nan (cat 5 u nan)))
    =/  rc  `(list @rs)`(oust [size 1] (oust [0 2] (rip 5 ut)))
    =/  q  `(list (list @rs))`~
    =/  i  0  :: index over rows
    |-  ^-  (list (list @rs))
      ?:  =(i m)  `(list (list @rs))`q
      =/  r  (mul i n)
      =/  v  `(list @rs)`(swag [r n] `(list @rs)`rc)
    $(i +(i), q `(list (list @rs))`(weld `(list (list @rs))`~[v] `(list (list @rs))`q))
  ::
  ::    Pretty-print the contents of the matrix.
  ++  pprint  !!
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lms i=@ud j=@ud]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  row  (snag (dec i) (unmake u))
    (snag (dec j) row)
  ::
  ::    Set the value of an element within a matrix, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lms i=@ud j=@ud x=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    `@lms`(setr u i (set:lvs (getr u i) j x))
  ::
  ::    Get the value of a column as @lvs in 1..n
  ++  getc
    ~/  %getc
    |=  [u=@lms i=@ud]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  w  `(list @rs)`~
    =/  uu  `(list (list @rs))`(unmake u)
    =/  count  1
    |-  ^-  @lvs
      ?:  (gth count m)  `@lvs`(make:lvs w)
      =/  row  `(list @rs)`(snag (dec count) `(list (list @rs))`uu)
      =/  elem  `@rs`(snag (dec i) row)
    $(count +(count), w `(list @rs)`(weld w `(list @rs)`~[elem]))
  ::
  ::    Set the value of a column to incoming @lvs in 1..n
  ++  setc
    ~/  %setc
    |=  [u=@lms j=@ud c=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  w  `(list (list @rs))`~
    =/  uu  `(list (list @rs))`(unmake u)
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count m)  `@lms`(make w)
      =/  src  (snag (dec count) `(list (list @rs))`uu)
      =/  ell  `@rs`(get:lvs c count)
      =/  trg  (set:lvs (make:lvs src) j ell)
      =/  row  (unmake:lvs trg)
    $(count +(count), w `(list (list @rs))`(weld w `(list (list @rs))`~[row]))
  ::
  ::
  ::    Get the value of a row as @lvs in 1..m
  ++  getr
    ~/  %getr
    |=  [u=@lms i=@ud]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    (make:lvs (snag (dec i) `(list (list @rs))`(unmake u)))
  ::
  ::    Set the value of a row to incoming @lvs in 1..m
  ++  setr
    ~/  %setr
    |=  [u=@lms i=@ud r=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  w  `(list (list @rs))`~
    =/  uu  `(list (list @rs))`(unmake u)
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count m)  `@lms`(make w)
      ?:  =(count i)
      $(count +(count), w `(list (list @rs))`(weld w `(list (list @rs))`~[(unmake:lvs r)]))
      =/  row  (snag (dec count) `(list (list @rs))`uu)
    $(count +(count), w `(list (list @rs))`(weld w `(list (list @rs))`~[row]))
  ::
  ::    Swap the value of two columns
  ++  swapc
    ~/  %swapc
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  t  (getc u j)
    (setc (setc u j (getc u i)) i t)  :: TODO I think this Hoon may be broken
  ::
  ::    Swap the value of two rows
  ++  swapr
    ~/  %swapc
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  t  (getr u j)
    (setr (setr u j (getr u i)) i t)
  ::
  ::    Transpose the entire matrix, essentially a flopped unmake
  ++  trans
    ~/  %trans
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  count  1
    =/  w  `@lms`(zeros n m)
    |-  ^-  @lms
      ?:  (gth count n)  `@lms`w
    $(count +(count), w (setr w count (getc u count)))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    ~/  %adds
    |=  [u=@lms v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  size  (mul m n)
    =/  p  (cantor m n)
    =/  w  `@lms`p
    =/  count  0
    =/  u  (end 5 size u)  :: strip off length bits
    |-  ^-  @lms
      ?:  =(count size)  `@lms`w
      =/  ul  `@rs`(end 5 1 (swp 5 u))  :: grab the high bytes
      =/  wl  `@rs`(add:rs ul v)
    $(count +(count), u (end 5 (sub (dec size) count) u), w (add (lsh 5 1 w) wl))
  ::
  ::    Scalar subtraction
  ++  subs
    ~/  %subs
    |=  [u=@lms v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  size  (mul m n)
    =/  p  (cantor m n)
    =/  w  `@lms`p
    =/  count  0
    =/  u  (end 5 size u)  :: strip off length bits
    |-  ^-  @lms
      ?:  =(count size)  `@lms`w
      =/  ul  `@rs`(end 5 1 (swp 5 u))  :: grab the high bytes
      =/  wl  `@rs`(sub:rs ul v)
    $(count +(count), u (end 5 (sub (dec size) count) u), w (add (lsh 5 1 w) wl))
  ::
  ::    Scalar multiplication
  ++  muls
    ~/  %muls
    |=  [u=@lms v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  size  (mul m n)
    =/  p  (cantor m n)
    =/  w  `@lms`p
    =/  count  0
    =/  u  (end 5 size u)  :: strip off length bits
    |-  ^-  @lms
      ?:  =(count size)  `@lms`w
      =/  ul  `@rs`(end 5 1 (swp 5 u))  :: grab the high bytes
      =/  wl  `@rs`(mul:rs ul v)
    $(count +(count), u (end 5 (sub (dec size) count) u), w (add (lsh 5 1 w) wl))
  ::
  ::    Scalar division
  ++  divs
    ~/  %divs
    |=  [u=@lms v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  m   `@ud`-:mn
    =/  n   `@ud`+<:mn
    =/  size  (mul m n)
    =/  p  (cantor m n)
    =/  w  `@lms`p
    =/  count  0
    =/  u  (end 5 size u)  :: strip off length bits
    |-  ^-  @lms
      ?:  =(count size)  `@lms`w
      =/  ul  `@rs`(end 5 1 (swp 5 u))  :: grab the high bytes
      =/  wl  `@rs`(div:rs ul v)
    $(count +(count), u (end 5 (sub (dec size) count) u), w (add (lsh 5 1 w) wl))
  ::
  ::    Columnar addition of @rs
  ++  addsc
    ~/  %addsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (adds:lvs (getc u i) v))
  ::
  ::    Columnar subtraction of @rs
  ++  subsc
    ~/  %subsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (subs:lvs (getc u i) v))
  ::
  ::    Columnar multiplication by @rs
  ++  mulsc
    ~/  %mulsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (muls:lvs (getc u i) v))
  ::
  ::    Columnar division by @rs
  ++  divsc
    ~/  %divsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (divs:lvs (getc u i) v))
  ::
  ::    Row-wise addition of @rs
  ++  addsr
    ~/  %addsr
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (adds:lvs (getr u i) v))
  ::
  ::    Row-wise subtraction of @rs
  ++  subsr
    ~/  %subsr
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (subs:lvs (getr u i) v))
  ::
  ::    Row-wise multiplication by @rs
  ++  mulsr
    ~/  %mulsr
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (muls:lvs (getr u i) v))
  ::
  ::    Row-wise division by @rs
  ++  divsr
    ~/  %divsr
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (divs:lvs (getr u i) v))
  ::
  ::    Columnar addition of @lvs
  ++  addvc
    ~/  %addvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (addv:lvs (getc u i) v))
  ::
  ::    Columnar subtraction of @lvs
  ++  subvc
    ~/  %subvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (subv:lvs (getc u i) v))
  ::
  ::    Columnar multiplication by @lvs
  ++  mulvc
    ~/  %mulvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (mulv:lvs (getc u i) v))
  ::
  ::    Columnar division by @lvs
  ++  divvc
    ~/  %divvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (divv:lvs (getc u i) v))
  ::
  ::    Row-wise addition of @lvs
  ++  addvr
    ~/  %addvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (addv:lvs (getr u i) v))
  ::
  ::    Row-wise subtraction of @lvs
  ++  subvr
    ~/  %subvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (subv:lvs (getr u i) v))
  ::
  ::    Row-wise multiplication by @lvs
  ++  mulvr
    ~/  %mulvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (mulv:lvs (getr u i) v))
  ::
  ::    Row-wise division by @lvs
  ++  divvr
    ~/  %divvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setr u i (divv:lvs (getr u i) v))
  ::
  ::    Elementwise addition of @lms
  ++  addm
    ~/  %addm
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  mnv  `(list @ud)`(shape v)
    =/  mv  `@ud`-:mnv
    =/  nv  `@ud`+<:mnv
    ?.  =(mu mv)  !!  :: make sure this is a valid operation
    ?.  =(nu nv)  !!  :: make sure this is a valid operation
    =/  w  `@lms`u
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
    $(count +(count), w (addvr w count (getr v count)))
  ::
  ::    Elementwise subtraction of @lms
  ++  subm
    ~/  %subm
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  mnv  `(list @ud)`(shape v)
    =/  mv  `@ud`-:mnv
    =/  nv  `@ud`+<:mnv
    ?.  =(mu mv)  !!  :: make sure this is a valid operation
    ?.  =(nu nv)  !!  :: make sure this is a valid operation
    =/  w  `@lms`u
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
    $(count +(count), w (subvr w count (getr v count)))
  ::
  ::    Elementwise multiplication by @lms
  ++  mulm
    ~/  %mulm
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  mnv  `(list @ud)`(shape v)
    =/  mv  `@ud`-:mnv
    =/  nv  `@ud`+<:mnv
    ?.  =(mu mv)  !!  :: make sure this is a valid operation
    ?.  =(nu nv)  !!  :: make sure this is a valid operation
    =/  w  `@lms`u
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
    $(count +(count), w (mulvr w count (getr v count)))
  ::
  ::    Elementwise division by @lms
  ++  divm
    ~/  %divm
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  mnv  `(list @ud)`(shape v)
    =/  mv  `@ud`-:mnv
    =/  nv  `@ud`+<:mnv
    ?.  =(mu mv)  !!  :: make sure this is a valid operation
    ?.  =(nu nv)  !!  :: make sure this is a valid operation
    =/  w  `@lms`u
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
    $(count +(count), w (divvr w count (getr v count)))
  ::
  ::    Matrix--matrix multiplication
  ::    Note:  We opt here for clarity NOT efficiency.  Leave that to the jets.
  ++  mmul
    ~/  %mmul
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mn  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mn
    =/  nu  `@ud`+<:mn
    =/  np  `(list @ud)`(shape v)
    =/  nv  `@ud`-:np
    =/  pv  `@ud`+<:np
    ?.  =(nu nv)  !!  :: make sure this is a valid operation
    =/  w  `@lms`(zeros mu pv)
    =/  i  1
    =/  j  1
    |-  ^-  @lms
      ?:  (gth i mu)  `@lms`w
      ?:  (gth j pv)
        $(i +(i), j 1, w w)
      $(i i, j +(j), w `@lms`(set w i j (inner:lvs (getr u i) (getc v j))))
  ::
  ::    Matrix exponentiation (A**N, not e(A))
  ++  mpow
    ~/  %mpow
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  w  `@lms`u
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
    $(count +(count), w (mmul u w))  :: TODO check this in Hoon
  ++  norm  !!
  ::
  ::    Matrix trace (sum of main diagonal elements); square matrices only
  ++  trace
    ~/  %trace
    |=  [u=@lms]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    ?.  =(mu nu)  !!  :: make sure this is a valid operation (square matrix)
    =/  sum  .0
    =/  count  1
    |-  ^-  @rs
      ?:  (gth count mu)  sum
    $(count +(count), sum (add:rs sum (get u count count)))
  ::
  ::    Operations related to matrix inversion
  ::    As with matrix multiplication, we're opting for clarity, not efficiency.
  ++  submatrix
    ~/  %submatrix
    |=  [u=@lms ia=@ud ib=@ud ja=@ud jb=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    ?:  (lth ia 1)  !!
    ?:  (lth ja 1)  !!
    ?:  (gth ia mu)  !!
    ?:  (gth ia nu)  !!
    ?:  (gth ia ib)  !!
    ?:  (gth ja jb)  !!
    =/  mw  +((sub ib ia))
    =/  nw  +((sub jb ja))
    =/  w  `@lms`(zeros mw nw)
    =/  ii  ia
    =/  jj  ja
    =/  mi  1
    =/  mj  1
    |-  ^-  @lms
      ?:  =(ii +(ib))  w
      ?:  =(jj +(jb))  $(ii +(ii), jj ja, mi +(mi), mj 1)
    $(ii ii, jj +(jj), mi mi, mj +(mj), w (set w mi mj (get u ii jj)))
  ++  augment
    ~/  %augment
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    ?.  =(mu nu)  !!  :: make sure this is a valid operation (square matrix)
    =/  w  `@lms`(zeros mu (mul mu 2))
    =/  count  1
    |-  ^-  @lms
      ?:  (gth count mu)  `@lms`w
      =/  ir  (snap (reap mu .0) (dec count) .1)
      =/  wl  (make:lvs (weld (unmake:lvs (getr u count)) ir))
    $(count +(count), w (setr w count wl))
  ::
  ::    Inverse of positive definite symmetric matrix, per Bauer & Reinsch 1971.
  ++  invert
    ~/  %invert
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (gauss-elim u)
  ++  abs
    |=  [x=@rs]  ^-  @rs
    ?:  (gth x .0)  x
    (sub:rs .0 x)
  ::
  ::  |x-y| <= tol
  ++  isclose
    |=  [x=@rs y=@rs tol=@rs]
    (lth:rs (abs (sub:rs x y)) tol)
  ++  gauss-find-next-row
    |=  [u=@lms i=@ud]  ^-  @ud
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  ii  i
    |-  ^-  @ud
      ?:  (gth ii mu)  i
      ?.  (isclose (get u ii i) .0 .1e-6)  ii
    $(ii +(ii))
  ++  gauss-scale
    |=  [u=@lms i=@ud]  ^-  @lms
    ::~&  ["gs" i (unmake:lvs (getr u i)) (get u i i) (unmake:lvs (divs:lvs (getr u i) (get u i i)))]
    (setr u i (divs:lvs (getr u i) (get u i i)))
  ++  gauss-replace-down
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  j  +(i)
    |-  ^-  @lms
      ?:  (gth j mu)  `@lms`u
      ?:  (isclose (get u j i) .0 .1e-6)  $(j +(j), u u)
      ::need to divide through by first element then subtract row i
    $(j +(j), u (setr u j (subv:lvs (getr u j) (muls:lvs (getr u i) (get u j i)))))
  ::
  ::  Row reduction has two phases:  check for zero in ith column, if so swap.
  ::  Then replace down and rescale.
  ++  gauss-row-reduce
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  i  1
    |-  ^-  @lms
      ?:  (gth i mu)  `@lms`u
      ?.  (isclose (get u i i) .0 .1e-6)  $(i +(i), u (gauss-replace-down (gauss-scale u i) i))
      =/  ii  (gauss-find-next-row u i)
    $(i +(i), u (gauss-scale (gauss-replace-down (swapr u i ii) i) i))
  ++  gauss-replace-up
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  j  (dec i)
    |-  ^-  @lms
      ?:  =(j 0)  `@lms`u
    $(j (dec j), u (setr u j (subv:lvs (getr u j) (muls:lvs (getr u i) (get u j i)))))
  ++  gauss-row-replace
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  i  1
    |-  ^-  @lms
      ?:  (gth i mu)  `@lms`u
    $(i +(i), u (gauss-replace-up u i))
  ++  gauss-elim
    ::~/  %gauss-elim
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    ?.  =(mu nu)  !!  :: make sure this is a valid operation (square matrix)
    =/  i  1
    =/  u  (augment u)
    |-  ^-  @lms
      ?:  (gth i mu)  (gauss-row-replace u)
    $(i +(i), u (gauss-row-reduce u i))
  ++  minor
    ~/  %minor
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  mnu  `(list @ud)`(shape u)
    =/  mu  `@ud`-:mnu
    =/  nu  `@ud`+<:mnu
    =/  mw  `@ud`(dec mu)
    =/  nw  `@ud`(dec nu)
    =/  w  (zeros mw nw)
    =/  ii  1
    =/  jj  1
    |-  ^-  @lms
      ?:  (gth ii mu)  w
      ?:  (gth jj nu)  $(ii +(ii), jj 1)
      ?:  =(ii i)      $(ii +(ii))
      ?:  =(jj j)      $(jj +(jj))
      =/  iii  ?:((gth ii i) (dec ii) ii)
      =/  jjj  ?:((gth jj j) (dec jj) jj)
      $(jj +(jj), w (set w iii jjj (get u ii jj)))
  ::
  ::    Calculate the determinant.
  ++  det  !!
  ::  Note to future numericists:  det isn't used much, so it's not a priority
  ::  XXX
  ::
  ::    Calculate eigenvalues and eigenvectors.
  ++  eigen  !!
  ::  Note to future numericists:  the best way is to wrap your method `eigen`
  ::  XXX
  ::  Note to future numericists:  use the QR algorithm, not set up as of today
  ::  XXX
  --
--
