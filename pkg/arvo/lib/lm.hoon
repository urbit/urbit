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
::
::  Caveats:
::
::  - The size of matrices is limited to the integer maximum of the current
::    floating-point bitwidth.
::
/+  *lv
|%
++  lms
  ^|
  ~%  %lms  ..part  ~
  |_  r=$?(%n %u %d %z)   :: round nearest, round up, round down, round to zero
  ++  lvs  ~(. ^lvs r)  :: transmit zeroing mode
  ++  lvd  ~(. ^lvd r)  :: transmit zeroing mode
  ::
  ::  Manipulators
  ::    Zeroes
  ++  zeros
    ~/  %zeros
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =/  mn  (mul m n)
    `@lms`(mix (lsh [5 +(mn)] 2) (lsh [5 mn] m))
  ::
  ::    Fill value
  ++  fill
    |=  [m=@ud n=@ud s=@rs]  ^-  @lms
    `@lms`(mix (zeros m n) (fil 5 (mul m n) s))
  ::
  ::    Ones
  ++  ones
    ~/  %ones
    |=  [m=@ud n=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    (fill m n .1)
  ::
  ::    Identity
  ++  id
    ~/  %id
    |=  [m=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =/  u  (zeros m m)
    =/  ii  1  :: index over rows
    |-
      ?:  (gth ii m)  u
    $(ii +(ii), u (set u ii ii .1))
    ::  XX redo this with ++rep
  ::
  ::    Length of matrix (rows x columns), utility function
  ++  length
    |=  u=@lms  ^-  @ud
    ~_  leaf+"lace-fail"
    (dec (dec (met 5 u)))
  ::
  ::    Shape of matrix
  ++  shape
    |=  u=@lms  ^-  (list @ud)
    =/  m  (end [5 1] (rsh [5 (length u)] u))
    =/  n  (div (length u) m)
    ~[m n]
  ::
  ::    Produce a matrix from `(list (list @rs))`
  ::    Rows across, columns "down" (meaning modulus m)
  ++  make
    |=  [a=(list (list @rs))]  ^-  @lms
    ~_  leaf+"lace-fail"
    =/  m  `@ud`(lent a)
    =/  n  `@ud`(lent (snag 0 a))
    =/  ii  1  :: index over rows
    =/  w  (zeros m n)
    |-  ^-  @lms
      ?:  (gth ii m)  w
    $(ii +(ii), w (setr w ii (make:lvs (snag (dec ii) a))))
  ++  unmake
    |=  [u=@lms]  ^-  (list (list @rs))
    ~_  leaf+"lace-fail"
    ::%-  flop
    =+  [m n]=[&1 &2]:(shape u)
    =/  size  (mul m n)
    =/  i  0  :: index over rows
    =/  a  `(list @rs)`(oust [size (add size 2)] (rip 5 u))
    =/  b  `(list (list @rs))`~
    |-  ^-  (list (list @rs))
      ?:  =(i m)  `(list (list @rs))`b
      =/  c  `(list @rs)`(scag n (slag (mul i n) a))
    $(i +(i), b `(list (list @rs))`(weld b ~[c]))
    ::$(i +(i), b `(list (list @rs))`(weld ~[c] b))
  ::
  ::    Pretty-print the contents of the matrix.  XX deal with rows better
  ++  pprint
    |=  u=@lms  ^-  tape
    `tape`(zing (join " " (turn `(list @rs)`(zing (unmake u)) |=(s=@rs <s>))))
  ::
  ::    Get the value at an index, using mathematical indices 1..n.
  ++  get
    ~/  %get
    |=  [u=@lms i=@ud j=@ud]  ^-  @rs
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    (cut 5 [(sub (mul m n) (add (mul n (dec i)) j)) 1] u)
  ::
  ::    Set the value of an element within a matrix, using math indices 1..n.
  ++  set
    ~/  %set
    |=  [u=@lms i=@ud j=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    `@lms`(setr u i (set:lvs (getr u i) j s))
  ::
  ::    Get the value of a column as @lvs in 1..n
  ++  getc
    ~/  %getc
    |=  [u=@lms j=@ud]  ^-  @lvs
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  ii  1  :: index over rows
    =/  v  (zeros:lvs m)
    |-  ^-  @lvs
      ?:  (gth ii m)  v
    $(ii +(ii), v (set:lvs v ii (get u ii j)))
  ::
  ::    Set the value of a column to incoming @lvs in 1..n
  ++  setc
    ~/  %setc
    |=  [u=@lms j=@ud w=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  ii  1  :: index over rows
    =/  v  u
    |-  ^-  @lms
      ?:  (gth ii m)  v
    $(ii +(ii), v (set v ii j (get:lvs w ii)))
  ::
  ::
  ::    Get the value of a row as @lvs in 1..m
  ++  getr
    ~/  %getr
    |=  [u=@lms i=@ud]  ^-  @lvs
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  jj  1  :: index over columns
    =/  v  (zeros:lvs n)
    |-  ^-  @lvs
      ?:  (gth jj n)  v
    $(jj +(jj), v (set:lvs v jj (get:lms u i jj)))
  ::
  ::    Set the value of a row to incoming @lvs in 1..m
  ++  setr
    ~/  %setr
    |=  [u=@lms i=@ud w=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  i  (dec i)
    =/  jj  1  :: index over columns
    =/  v  u
    |-  ^-  @lms
      ?:  (gth jj n)  v
    $(jj +(jj), v (sew 5 [(dec (add (mul n i) jj)) 1 (get:lvs w jj)] v))
  ::
  ::    Swap the value of two columns
  ++  swapc
    ~/  %swapc
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =/  v  (getc u j)
    =/  w  (setc u j (getc u i))
    (setc w i v)
  ::
  ::    Swap the value of two rows
  ++  swapr
    ~/  %swapr
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =/  v  (getr u j)
    =/  w  (setr u j (getr u i))
    (setr w i v)
  ::
  ::    Transpose the entire matrix, essentially a flopped unmake
  ++  trans
    ~/  %trans
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
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
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (addm u ss)
  ::
  ::    Scalar subtraction
  ++  subs
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (subm u ss)
  ::
  ::    Scalar multiplication
  ++  muls
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (mulm u ss)
  ::
  ::    Scalar division
  ++  divs
    |=  [u=@lms s=@rs]  ^-  @lms
    =+  [m n]=[&1 &2]:(shape u)
    =/  ss  (fill m n s)
    (divm u ss)
  ::
  ::    Column-wise addition of @rs
  ++  addsc
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (adds:lvs (getc u i) s))
  ::
  ::    Column-wise subtraction of @rs
  ++  subsc
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (subs:lvs (getc u i) s))
  ::
  ::    Column-wise multiplication by @rs
  ++  mulsc
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (muls:lvs (getc u i) s))
  ::
  ::    Column-wise division by @rs
  ++  divsc
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (divs:lvs (getc u i) s))
  ::
  ::    Row-wise addition of @rs
  ++  addsr
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (adds:lvs (getr u i) s))
  ::
  ::    Row-wise subtraction of @rs
  ++  subsr
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (subs:lvs (getr u i) s))
  ::
  ::    Row-wise multiplication by @rs
  ++  mulsr
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (muls:lvs (getr u i) s))
  ::
  ::    Row-wise division by @rs
  ++  divsr
    |=  [u=@lms i=@ud s=@rs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (divs:lvs (getr u i) s))
  ::
  ::    Column-wise addition of @lvs
  ++  addvc
    ~/  %addvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (addv:lvs (getc u i) v))
  ::
  ::    Column-wise subtraction of @lvs
  ++  subvc
    ~/  %subvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (subv:lvs (getc u i) v))
  ::
  ::    Column-wise multiplication by @lvs
  ++  mulvc
    ~/  %mulvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (mulv:lvs (getc u i) v))
  ::
  ::    Column-wise division by @lvs
  ++  divvc
    ~/  %divvc
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setc u i (divv:lvs (getc u i) v))
  ::
  ::    Row-wise addition of @lvs
  ++  addvr
    ~/  %addvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (addv:lvs (getr u i) v))
  ::
  ::    Row-wise subtraction of @lvs
  ++  subvr
    ~/  %subvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (subv:lvs (getr u i) v))
  ::
  ::    Row-wise multiplication by @lvs
  ++  mulvr
    ~/  %mulvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (mulv:lvs (getr u i) v))
  ::
  ::    Row-wise division by @lvs
  ++  divvr
    ~/  %divvr
    |=  [u=@lms i=@ud v=@lvs]  ^-  @lms
    ~_  leaf+"lace-fail"
    (setr u i (divv:lvs (getr u i) v))
  ::
  ++  process
    |=  [[a=(list (list @rs)) b=(list (list @rs))] f=$-([@rs @rs] @rs)]
    ^-  (list (list @rs))
    =/  ma  (lent a)
    =/  na  (lent (snag 0 a))
    =/  mb  (lent b)
    =/  nb  (lent (snag 0 b))
    ?>  =(ma mb)  :: make sure this is a valid operation
    ?>  =(na nb)  :: make sure this is a valid operation
    =/  ii  0
    =/  jj  0
    =/  c  `(list (list @rs))`~
    =/  cc  `(list @rs)`~
    :: for each row, apply turn2
    |-
      ?:  =(ii ma)  c
      ?:  =(jj na)  $(ii +(ii), jj 0, c (snoc c cc), cc `(list @rs)`~)
      =/  aa  `@rs`(snag jj (snag ii a))
      =/  bb  `@rs`(snag jj (snag ii b))
    $(ii ii, jj +(jj), cc (snoc cc (f aa bb)))
  ::
  ::    Apply a two-variable function across a matrix input.
  ++  funm
    |=  f=$-([@rs @rs] @rs)
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
    (make (process [(unmake u) (unmake v)] f))
  ::
  ::    Elementwise addition of @lms
  ++  addm  ~/  %addm  |=([@lms @lms] %.(+< (funm add:rs)))
  ::
  ::    Elementwise subtraction of @lms
  ++  subm  ~/  %subm  |=([@lms @lms] %.(+< (funm sub:rs)))
  ::
  ::    Elementwise multiplication by @lms
  ++  mulm  ~/  %mulm  |=([@lms @lms] %.(+< (funm mul:rs)))
  ::
  ::    Elementwise division by @lms
  ++  divm  ~/  %divm  |=([@lms @lms] %.(+< (funm div:rs)))
  ::
  ::    Matrix--matrix multiplication
  ::    Note:  We opt here for clarity NOT efficiency.  Leave that to the jets.
  ++  mmul
    ~/  %mmul
    |=  [u=@lms v=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
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
    |=  [u=@lms n=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    ?~  n  (id -:(shape u))
    =/  w  u
    |-(?:(=(1 n) w $(w (mmul u w), n (dec n))))
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
    ~/  %submatrix
    |=  [u=@lms [ia=@ud ib=@ud] [ja=@ud jb=@ud]]  ^-  @lms
    =+  [is js]=[(dec ia)^(sub ib (dec ia)) (dec ja)^(sub jb (dec ja))]
    (make (turn (swag is (unmake u)) |=(a=(list @rs) (swag js a))))
  ::
  ::++  catenate
  ::  |=  [u=@lms w=@lms]  ^-  @lms
  ::  (make (turn (paired (unmake u) (unmake w) catenate:lvs)))
  ::++  ravel  !!
  ::++  augment
  ::  |=  u=@lms  ^-  @lms
  ::  =+  [m n]=[&1 &2]:(shape u)
  ::  ?.  =(mu nu)  !!  :: make sure this is a valid operation (square matrix)
  ::  (catenate u (id m))
  ++  augment  :: TODO XX replace once ++catenate written
    ~/  %augment
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
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
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    (submatrix (gauss-elim u) [1 m] [+(n) (mul 2 n)])
  ++  abs
    |=  [s=@rs]  ^-  @rs
    ?:  (gth:rs s .0)  s  (sub:rs .0 s)
  ::
  ::  |x-y| <= tol
  ++  isclose
    |=  [s=@rs t=@rs tol=@rs]
    (lth:rs (abs (sub:rs s t)) tol)
  ++  near0
    |=  x=@rs
    (isclose x .0 .1e-6)
  ++  all-close
    |=  [u=@lms v=@lms tol=@rs]
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  mn  (mul mu nu)
    =/  count  1
    =/  i  1
    =/  j  1
    =/  off  0
    |-  ^-  ?
      ?:  (gth off 0)  %.n
      ?:  (gth count mn)  %.y
      ?:  (gth i nu)  $(count +(count), i +(i), j 1, off off)
    $(count +(count), i i, j +(j), off (add off ?:((isclose (get u i j) (get v i j) tol) 0 1)))
  ++  gauss-find-next-row
    |=  [u=@lms i=@ud]  ^-  @ud
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  ii  i  :: index over rows
    |-  ^-  @ud
      ?:  (gth ii mu)  i
      ?.  (isclose (get u ii i) .0 .1e-6)  ii
    $(ii +(ii))
  ++  gauss-normalize-row
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    (divsr u i (get u i i))
  ++  gauss-replace-down
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  ii  +(i)
    =/  u  (gauss-normalize-row u i)
    |-  ^-  @lms
      ?:  (gth ii mu)  u
      ?:  (isclose (get u ii i) .0 .1e-6)  $(ii +(ii), u u)
      =/  r1  (muls:lvs (getr u i) (get u ii i))
      =/  r2  (subv:lvs (getr u ii) r1)
      =/  r3  (divs:lvs r2 (get:lvs r2 +(i)))
    $(ii +(ii), u (setr u ii r3))
  ::
  ::  Row reduction has two phases:  check for zero in ith column, if so swap.
  ::  Then replace down and rescale.
  ++  gauss-row-reduce
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  i  1
    |-  ^-  @lms
      ?:  (gth i mu)  `@lms`u
      ?.  (isclose (get u i i) .0 .1e-6)
        $(i +(i), u (gauss-replace-down u i))
      =/  ii  (gauss-find-next-row u i)
    $(i +(i), u (gauss-replace-down (swapr u i ii) i))
  ++  gauss-replace-up
    |=  [u=@lms i=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  j  (dec i)
    |-  ^-  @lms
      ?:  =(j 0)  `@lms`u
    $(j (dec j), u (setr u j (subv:lvs (getr u j) (muls:lvs (getr u i) (get u j i)))))
  ++  gauss-row-replace
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [mu nu]=[&1 &2]:(shape u)
    =/  i  1
    |-  ^-  @lms
      ?:  (gth i mu)  `@lms`u
    $(i +(i), u (gauss-replace-up u i))
  ++  gauss-elim
    ::~/  %gauss-elim
    |=  [u=@lms]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    ?>  =(m n)  :: make sure this is a valid operation (square matrix)
    =/  i  1
    =/  u  (augment u)
    |-  ^-  @lms
      ?:  (gth i m)  (gauss-row-replace u)
    $(i +(i), u (gauss-row-reduce u i))
  ++  minor
    ~/  %minor
    |=  [u=@lms i=@ud j=@ud]  ^-  @lms
    ~_  leaf+"lace-fail"
    =+  [m n]=[&1 &2]:(shape u)
    =/  w  (zeros (dec m) (dec n))
    =/  ii  1  :: index over rows
    =/  jj  1  :: index over columns
    |-  ^-  @lms
      ?:  (gth ii m)  w
      ?:  (gth jj n)  $(ii +(ii), jj 1)
      ?:  =(ii i)     $(ii +(ii))
      ?:  =(jj j)     $(jj +(jj))
      =/  iii  ?:((gth ii i) (dec ii) ii)
      =/  jjj  ?:((gth jj j) (dec jj) jj)
      $(jj +(jj), w (set w iii jjj (get u ii jj)))
  --
--
