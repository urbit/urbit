::
::::  Matrix type in single-precision floating-point @rs
  ::
::  Conventions:
::
::  m,n,p   always dimensions (or dimension composites in the case of p)
::  i,j,k   always indices (also ii,jj,kk for coder-spec not user-spec)
::  a,b,c   always lists
::  u,v,w   always vector/matrix atoms
::  s,t     always real/floats
/+  *lv
|%
++  lvs  ~(. ^lvs r)  :: transmit zeroing mode
++  lvd  ~(. ^lvd r)  :: transmit zeroing mode
++  lms
  ^|
  ~%  %lms  ..is  ~
  |_  r/$?($n $u $d $z)   :: round nearest, round up, round down, round to zero
  ::
  ::  Manipulators
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  p  (rep [4 1] ~[m n])
    `@lms`(lsh [5 (mul m n)] p)
  ::
  ::    Fill value
  ++  fill
    ~/  %fill
    |=  [m=@ud n=@ud s=@rs]  ^-  @lms
    `@lms`(mix (zeros m n) (fil 5 (mul m n) s))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  n=@ud  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    (fill m n .1)
  ::
  ::    Identity
  ++  id
    ~/  %id
    |=  [m=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  u  (zeros m n)
    =/  ii  1  :: index over rows
    |-
      ?:  =(ii m)  u
    $(ii +(ii), u (set u ii ii .1))
    ::  XX redo this with ++rep
  ::
  ::    Shape of matrix
  ++  shape
    |=  u=@lms  ^-  (list @ud)
    (rip [4 1] (end [5 1] (swp 5 u)))
  ::
  ::    Produce a matrix from `(list (list @rs))`
  ::    Rows across, columns "down" (meaning modulus m)
  ++  not-rs
    |=  a=@rs
    ~_  leaf+"lagoon-fail"
    (not 5 1 a)
  ++  make :: TODO
    |=  [a=(list (list @rs))]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  m  `@ud`(lent a)
    =/  n  `@ud`(lent (snag 0 a))
    =/  i  0  :: index over rows
    =/  w  (zeros m n)
    |-  ^-  @lms
      ?:  =(i m)  w
    $(i +(i), w (setr w i (make:lvs (snag i a))))
  ++  unmake :: TODO
    |=  [u=@lms]  ^-  (list (list @rs))
    ~_  leaf+"lagoon-fail"
    %-  flop  :: TODO XXX check this
    =+  [m n]=[&1 &2]:(shape u)
    =/  size  (mul m n)
    =/  i  0  :: index over rows
    =/  a  `(list @rs)`(oust [0 1] (flop (rip 5 u)))
    =/  b  `(list (list @rs))`~
    |-  ^-  (list (list @rs))
      ?:  =(i m)  `(list (list @rs))`q
      =/  b  `(list @rs)`(scag n (slag (mul i n) a))
    $(i +(i), q `(list (list @rs))`(weld ~[b] a))
  ::
  ::    Pretty-print the contents of the matrix.  XX deal with rows better
  ++  pprint  !!
  ::  |=  u=@lms  ^-  tape
  ::  `tape`(zing (join " " (turn (unmake u) |=(s=@rs <s>))))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lms i=@ud j=@ud]  ^-  @rs
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    (cut 5 [(add (mul n (dec i)) j) 1] u)
  ::
  ::    Set the value of an element within a matrix, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lms i=@ud j=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    `@lms`(setr u i (set:lvs (getr u i) j s))
  ::
  ::    Get the value of a column as @lvs in 1..n
  ++  getc
    ~/  %getc
    |=  [u=@lms j=@ud]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  ii  1  :: index over rows
    =/  v  (zeros:lvs m)
    |-  ^-  @lvs
      ?:  (gth i m)  v
    $(ii +(ii), v (set:lvs v ii (get:lms u ii j)))
  ::
  ::    Set the value of a column to incoming @lvs in 1..n
  ++  setc
    ~/  %setc
    |=  [u=@lms j=@ud w=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  ii  1  :: index over rows
    =/  v  u
    |-  ^-  @lms
      ?:  (gth i m)  v
    $(ii +(ii), v (set v ii j (get:lvs w ii)))
  ::
  ::
  ::    Get the value of a row as @lvs in 1..m
  ++  getr
    ~/  %getr
    |=  [u=@lms i=@ud]  ^-  @lvs
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    ?>  (lth (dec i) m)
    (cut 5 [(mul i n) n] u)
    ::(mix (zeros:lvs n) (cut 5 [(mul m i) n] u))
  ::
  ::    Set the value of a row to incoming @lvs in 1..m
  ++  setr
    ~/  %setr
    |=  [u=@lms i=@ud w=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  jj  1  :: index over columns
    =/  v  u
    |-  ^-  @lms
      ?:  (gth i m)  v
    $(jj +(jj), v (sew 5 [(add i jj) 1 (get:lvs w jj)] v))
    ::  XX check positioning with better tests
  ::
  ::    Swap the value of two columns
  ++  swapc
    ~/  %swapc
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  v  (getc u j)
    =/  w  (setc u j (getc u i))
    (setc w i v)
  ::
  ::    Swap the value of two rows
  ++  swapr
    ~/  %swapc
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =/  v  (getr u j)
    =/  w  (setr u j (getr u i))
    (setr w i v)
  ::
  ::    Transpose the entire matrix, essentially a flopped unmake
  ++  trans
    ~/  %trans
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  ii  1  :: index over rows/columns
    =/  w  (zeros n m)
    |-  ^-  @lms
      ?:  (gth ii n)  w
    $(ii +(ii), w (setr w ii (getc u ii)))
  ::
  ::  Arithmetic operators
  ::
  ::    Scalar addition
  ++  adds
    ~/  %adds
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (addm u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    ~/  %subs
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (subm u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    ~/  %muls
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (mulm u ss)
  ::
  ::    Scalar division
  ++  divs
    ~/  %divs
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (divm u ss)
  ::
  ::    Column-wise addition of @rs
  ++  addsc
    ~/  %addsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (adds:lvs (getc u i) v))
  ::
  ::    Column-wise subtraction of @rs
  ++  subsc
    ~/  %subsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (subs:lvs (getc u i) v))
  ::
  ::    Column-wise multiplication by @rs
  ++  mulsc
    ~/  %mulsc
    |=  [u=@lms i=@ud v=@rs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (muls:lvs (getc u i) v))
  ::
  ::    Column-wise division by @rs
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
  ::    Column-wise addition of @lvs
  ++  addvc
    ~/  %addvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (addv:lvs (getc u i) v))
  ::
  ::    Column-wise subtraction of @lvs
  ++  subvc
    ~/  %subvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (subv:lvs (getc u i) v))
  ::
  ::    Column-wise multiplication by @lvs
  ++  mulvc
    ~/  %mulvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (setc u i (mulv:lvs (getc u i) v))
  ::
  ::    Column-wise division by @lvs
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
  ::    Turn on a gate of two variables.
  ++  turn2  :: I guess not in hoon.hoon
    |=  [[a=(list @rs) b=(list @rs)] f=$-([@rs @rs] @rs)]
    ^-  (list @rs)
    ?+  +<-  ~|(%turn2-length !!)
      [~ ~]  ~
      [^ ^]  [(f i.a i.b) $(a t.a, b t.b)]
    ==
  ::
  ::    Apply a two-variable function across a matrix input.
  ++  funm
    ~/  %funm
    |=  f=$-([@rs @rs] @rs)
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    (make (turn2 [(unmake u) (unmake v)] f))
  ::
  ::    Elementwise addition of @lms
  ++  addm
    (funm add:rs)
  ::
  ::    Elementwise subtraction of @lms
  ++  subm
    (funm sub:rs)
  ::
  ::    Elementwise multiplication by @lms
  ++  mulm
    (funm mul:rs)
  ::
  ::    Elementwise division by @lms
  ++  divm
    (funm div:rs)
  ::
  ::    Matrix--matrix multiplication
  ::    Note:  We opt here for clarity NOT efficiency.  Leave that to the jets.
  ++  mmul
    ~/  %mmul
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =+  [mv nv]=[&1 &2]:(shape v)
    ?>  =(nu mv)  :: make sure this is a valid operation
    =/  w  (zeros mu nv)
    =/  ii  1  :: index over rows
    =/  jj  1  :: index over columns
    |-  ^-  @lms
      ?:  (gth ii mu)  w
      ?:  (gth jj nv)  $(ii +(ii), jj 1, w w)
      $(ii ii, jj +(jj), w (set w ii jj (inner:lvs (getr u ii) (getc v jj))))
  ::
  ::    Matrix exponentiation (A**N, not e(A))
  ++  mpow
    ~/  %mpow
    |=  [u=@lms n=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    ?~  i  !!
    =/  w  u
    |-(?:(=(1 i) w $(w (mmul u w), i (dec i))))
  ::
  ::    Matrix trace (sum of main diagonal elements); square matrices only
  ++  trace  |=(u=@lms (sum:lvs (diag u)))
  ++  diag
    |=  u=@lms  ^-  @lvs
    =/  n  +:(mate [`&1 `&2]:(shape u))
    (make:lvs (turn (gulf 1 n) |=(i=@u (get u i i))))
  ::
  ::    Operations related to matrix inversion
  ::    As with matrix multiplication, we're opting for clarity, not efficiency.
  ++  submatrix
    |=  [u=@lms [i=@ud ie=@ud] [j=@ud je=@ud]]  ^-  @lms
    =+  [is js]=[(dec i)^(sub ie (dec i)) (dec j)^(sub jb (dec j))]
    (make (turn (swag is (unmake u)) |=(a=(list @rs) (swag js a))))
  ::
  ++  catenate
    |=  [u=@lms w=@lms]  ^-  @lms
    (make (turn (paired (unmake u) (unmake w) catenate:lvs)))
  ++  augment
    |=  u=@lms  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    (catenate u (id m n))
  ::
  ::    Inverse of positive definite symmetric matrix, per Bauer & Reinsch 1971.
  ++  invert
    ~/  %invert
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    (submatrix [1 m] [+(n) (mul 2 n)] (gauss-elim u))
  ++  abs
    |=  [s=@rs]  ^-  @rs
    ?:  (gth s .0)  s  (sub:rs .0 s)
  ::
  ::  |x-y| <= tol
  ++  isclose
    |=  [s=@rs t=@rs tol=@rs]
    (lth:rs (abs (sub:rs s t)) tol)
  ++  near0
    |=  x=@rs
    (isclose .0 .1e-6)
  ++  gauss-find-next-row
    |=  [u=@lms i=@ud]  ^-  @ud
    ~_  leaf+"lagoon-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  ii  i  :: index over rows
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
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  jj  +(i)
    |-  ^-  @lms
      ?:  (gth jj mu)  u
      ?:  (isclose (get u jj i) .0 .1e-6)  $(jj +(jj), u u)
      ::need to divide through by first element then subtract row i
    $(jj +(jj), u (subvr u jj (getr u jj) (muls:lvs (getr u i) (get u jj i))))
  ::
  ::  Row reduction has two phases:  check for zero in ith column, if so swap.
  ::  Then replace down and rescale.
  ++  gauss-row-reduce
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  i  1  :: XX TODO wut?
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
    ?>  =(mu nu)  :: make sure this is a valid operation (square matrix)
    =/  i  1
    =/  u  (augment u)
    |-  ^-  @lms
      ?:  (gth i mu)  (gauss-row-replace u)
    $(i +(i), u (gauss-row-reduce u i))
  ++  minor
    ~/  %minor
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lagoon-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  w  (zeros (dec m) (dec n))
    =/  ii  1  :: index over rows
    =/  jj  1  :: index over columns
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
