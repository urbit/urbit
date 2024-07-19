/-  *lagoon
/+  *test
/+  *lagoon
  ::  /tests/lib/lagoon-double-args-elementwise
::::
::
::  The basic scheme for testing is to test across each kind of ray, and
::  then to test each operation on each kind of ray.
::
::  +$  kind              ::  $kind:  type of array scalars
::    $?  %i754           ::  IEEE 754 float
::        %uint           ::  unsigned integer
::        %int2           ::  2s-complement integer
::        %cplx           ::  BLAS-compatible packed floats
::        %unum           ::  unum/posit
::        %fixp           ::  fixed-precision
::    ==
::
::  Each testing arm will be named after the pattern ++test-<kind>-<op>.
::  A `canon` is a reference result, and an `assay` is the result of the
::  operation being tested.
::
^|
|_  $:  atol=_.1e-3          :: absolute tolerance for precision of operations
        rtol=_.1e-5          :: relative tolerance for precision of operations
    ==
::  Auxiliary tools
++  is-equal
  |=  [a=ray b=ray]  ^-  tang
  ?:  =(a b)  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<`ray`a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<`ray`b>}"]]
  ==
::
++  is-close
  |=  [a=ray b=ray]  ^-  tang
  ?:  (all:la (is-close:la a b [atol rtol]))  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<b>}"]]
  ==
::
::  Utilities
::
++  test-add-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-add-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0]]])
  =/  assay-add-1x1-4r  (add:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-add-1x1-4r
  assay-add-1x1-4r
::
++  test-add-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-add-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0]]])
  =/  assay-add-1x2-4r  (add:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-add-1x2-4r
  assay-add-1x2-4r
::
++  test-add-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-add-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0 .~~2.0]]])
  =/  assay-add-1x3-4r  (add:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-add-1x3-4r
  assay-add-1x3-4r
::
++  test-add-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-add-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0] ~[.~~2.0]]])
  =/  assay-add-2x1-4r  (add:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-add-2x1-4r
  assay-add-2x1-4r
::
++  test-add-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-add-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0] ~[.~~2.0 .~~2.0]]])
  =/  assay-add-2x2-4r  (add:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-add-2x2-4r
  assay-add-2x2-4r
::
++  test-add-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-add-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0 .~~2.0] ~[.~~2.0 .~~2.0 .~~2.0]]])
  =/  assay-add-2x3-4r  (add:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-add-2x3-4r
  assay-add-2x3-4r
::
++  test-add-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-add-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0] ~[.~~2.0] ~[.~~2.0]]])
  =/  assay-add-3x1-4r  (add:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-add-3x1-4r
  assay-add-3x1-4r
::
++  test-add-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-add-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0] ~[.~~2.0 .~~2.0] ~[.~~2.0 .~~2.0]]])
  =/  assay-add-3x2-4r  (add:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-add-3x2-4r
  assay-add-3x2-4r
::
++  test-add-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-add-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~2.0 .~~2.0 .~~2.0] ~[.~~2.0 .~~2.0 .~~2.0] ~[.~~2.0 .~~2.0 .~~2.0]]])
  =/  assay-add-3x3-4r  (add:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-add-3x3-4r
  assay-add-3x3-4r
::
++  test-add-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-add-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0]]])
  =/  assay-add-1x1-5r  (add:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-add-1x1-5r
  assay-add-1x1-5r
::
++  test-add-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-add-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0]]])
  =/  assay-add-1x2-5r  (add:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-add-1x2-5r
  assay-add-1x2-5r
::
++  test-add-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-add-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0 .2.0]]])
  =/  assay-add-1x3-5r  (add:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-add-1x3-5r
  assay-add-1x3-5r
::
++  test-add-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-add-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0] ~[.2.0]]])
  =/  assay-add-2x1-5r  (add:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-add-2x1-5r
  assay-add-2x1-5r
::
++  test-add-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-add-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0] ~[.2.0 .2.0]]])
  =/  assay-add-2x2-5r  (add:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-add-2x2-5r
  assay-add-2x2-5r
::
++  test-add-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-add-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0 .2.0] ~[.2.0 .2.0 .2.0]]])
  =/  assay-add-2x3-5r  (add:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-add-2x3-5r
  assay-add-2x3-5r
::
++  test-add-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-add-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0] ~[.2.0] ~[.2.0]]])
  =/  assay-add-3x1-5r  (add:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-add-3x1-5r
  assay-add-3x1-5r
::
++  test-add-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-add-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0] ~[.2.0 .2.0] ~[.2.0 .2.0]]])
  =/  assay-add-3x2-5r  (add:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-add-3x2-5r
  assay-add-3x2-5r
::
++  test-add-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-add-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.2.0 .2.0 .2.0] ~[.2.0 .2.0 .2.0] ~[.2.0 .2.0 .2.0]]])
  =/  assay-add-3x3-5r  (add:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-add-3x3-5r
  assay-add-3x3-5r
::
++  test-add-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-add-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0]]])
  =/  assay-add-1x1-6r  (add:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-add-1x1-6r
  assay-add-1x1-6r
::
++  test-add-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-add-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0]]])
  =/  assay-add-1x2-6r  (add:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-add-1x2-6r
  assay-add-1x2-6r
::
++  test-add-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-add-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0 .~2.0]]])
  =/  assay-add-1x3-6r  (add:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-add-1x3-6r
  assay-add-1x3-6r
::
++  test-add-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-add-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0] ~[.~2.0]]])
  =/  assay-add-2x1-6r  (add:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-add-2x1-6r
  assay-add-2x1-6r
::
++  test-add-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-add-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0] ~[.~2.0 .~2.0]]])
  =/  assay-add-2x2-6r  (add:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-add-2x2-6r
  assay-add-2x2-6r
::
++  test-add-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-add-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0 .~2.0] ~[.~2.0 .~2.0 .~2.0]]])
  =/  assay-add-2x3-6r  (add:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-add-2x3-6r
  assay-add-2x3-6r
::
++  test-add-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-add-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0] ~[.~2.0] ~[.~2.0]]])
  =/  assay-add-3x1-6r  (add:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-add-3x1-6r
  assay-add-3x1-6r
::
++  test-add-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-add-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0] ~[.~2.0 .~2.0] ~[.~2.0 .~2.0]]])
  =/  assay-add-3x2-6r  (add:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-add-3x2-6r
  assay-add-3x2-6r
::
++  test-add-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-add-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~2.0 .~2.0 .~2.0] ~[.~2.0 .~2.0 .~2.0] ~[.~2.0 .~2.0 .~2.0]]])
  =/  assay-add-3x3-6r  (add:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-add-3x3-6r
  assay-add-3x3-6r
::
++  test-add-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-add-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0]]])
  =/  assay-add-1x1-7r  (add:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-add-1x1-7r
  assay-add-1x1-7r
::
++  test-add-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-add-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0]]])
  =/  assay-add-1x2-7r  (add:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-add-1x2-7r
  assay-add-1x2-7r
::
++  test-add-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-add-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0 .~~~2.0]]])
  =/  assay-add-1x3-7r  (add:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-add-1x3-7r
  assay-add-1x3-7r
::
++  test-add-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-add-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0] ~[.~~~2.0]]])
  =/  assay-add-2x1-7r  (add:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-add-2x1-7r
  assay-add-2x1-7r
::
++  test-add-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-add-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0]]])
  =/  assay-add-2x2-7r  (add:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-add-2x2-7r
  assay-add-2x2-7r
::
++  test-add-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-add-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0 .~~~2.0]]])
  =/  assay-add-2x3-7r  (add:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-add-2x3-7r
  assay-add-2x3-7r
::
++  test-add-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-add-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0] ~[.~~~2.0] ~[.~~~2.0]]])
  =/  assay-add-3x1-7r  (add:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-add-3x1-7r
  assay-add-3x1-7r
::
++  test-add-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-add-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0]]])
  =/  assay-add-3x2-7r  (add:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-add-3x2-7r
  assay-add-3x2-7r
::
++  test-add-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-add-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~2.0 .~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0 .~~~2.0] ~[.~~~2.0 .~~~2.0 .~~~2.0]]])
  =/  assay-add-3x3-7r  (add:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-add-3x3-7r
  assay-add-3x3-7r
::
++  test-add-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-add-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[2]]])
  =/  assay-add-1x1-3u  (add:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-add-1x1-3u
  assay-add-1x1-3u
::
++  test-add-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-add-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[2 2]]])
  =/  assay-add-1x2-3u  (add:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-add-1x2-3u
  assay-add-1x2-3u
::
++  test-add-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-add-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[2 2 2]]])
  =/  assay-add-1x3-3u  (add:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-add-1x3-3u
  assay-add-1x3-3u
::
++  test-add-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-add-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[2] ~[2]]])
  =/  assay-add-2x1-3u  (add:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-add-2x1-3u
  assay-add-2x1-3u
::
++  test-add-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-add-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2]]])
  =/  assay-add-2x2-3u  (add:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-add-2x2-3u
  assay-add-2x2-3u
::
++  test-add-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-add-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2]]])
  =/  assay-add-2x3-3u  (add:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-add-2x3-3u
  assay-add-2x3-3u
::
++  test-add-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-add-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[2] ~[2] ~[2]]])
  =/  assay-add-3x1-3u  (add:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-add-3x1-3u
  assay-add-3x1-3u
::
++  test-add-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-add-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2] ~[2 2]]])
  =/  assay-add-3x2-3u  (add:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-add-3x2-3u
  assay-add-3x2-3u
::
++  test-add-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-add-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2] ~[2 2 2]]])
  =/  assay-add-3x3-3u  (add:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-add-3x3-3u
  assay-add-3x3-3u
::
++  test-add-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-add-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[2]]])
  =/  assay-add-1x1-4u  (add:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-add-1x1-4u
  assay-add-1x1-4u
::
++  test-add-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-add-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[2 2]]])
  =/  assay-add-1x2-4u  (add:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-add-1x2-4u
  assay-add-1x2-4u
::
++  test-add-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-add-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[2 2 2]]])
  =/  assay-add-1x3-4u  (add:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-add-1x3-4u
  assay-add-1x3-4u
::
++  test-add-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-add-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[2] ~[2]]])
  =/  assay-add-2x1-4u  (add:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-add-2x1-4u
  assay-add-2x1-4u
::
++  test-add-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-add-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2]]])
  =/  assay-add-2x2-4u  (add:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-add-2x2-4u
  assay-add-2x2-4u
::
++  test-add-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-add-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2]]])
  =/  assay-add-2x3-4u  (add:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-add-2x3-4u
  assay-add-2x3-4u
::
++  test-add-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-add-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[2] ~[2] ~[2]]])
  =/  assay-add-3x1-4u  (add:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-add-3x1-4u
  assay-add-3x1-4u
::
++  test-add-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-add-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2] ~[2 2]]])
  =/  assay-add-3x2-4u  (add:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-add-3x2-4u
  assay-add-3x2-4u
::
++  test-add-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-add-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2] ~[2 2 2]]])
  =/  assay-add-3x3-4u  (add:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-add-3x3-4u
  assay-add-3x3-4u
::
++  test-add-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-add-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[2]]])
  =/  assay-add-1x1-5u  (add:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-add-1x1-5u
  assay-add-1x1-5u
::
++  test-add-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-add-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[2 2]]])
  =/  assay-add-1x2-5u  (add:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-add-1x2-5u
  assay-add-1x2-5u
::
++  test-add-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-add-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[2 2 2]]])
  =/  assay-add-1x3-5u  (add:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-add-1x3-5u
  assay-add-1x3-5u
::
++  test-add-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-add-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[2] ~[2]]])
  =/  assay-add-2x1-5u  (add:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-add-2x1-5u
  assay-add-2x1-5u
::
++  test-add-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-add-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2]]])
  =/  assay-add-2x2-5u  (add:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-add-2x2-5u
  assay-add-2x2-5u
::
++  test-add-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-add-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2]]])
  =/  assay-add-2x3-5u  (add:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-add-2x3-5u
  assay-add-2x3-5u
::
++  test-add-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-add-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[2] ~[2] ~[2]]])
  =/  assay-add-3x1-5u  (add:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-add-3x1-5u
  assay-add-3x1-5u
::
++  test-add-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-add-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2] ~[2 2]]])
  =/  assay-add-3x2-5u  (add:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-add-3x2-5u
  assay-add-3x2-5u
::
++  test-add-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-add-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2] ~[2 2 2]]])
  =/  assay-add-3x3-5u  (add:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-add-3x3-5u
  assay-add-3x3-5u
::
++  test-add-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-add-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[2]]])
  =/  assay-add-1x1-6u  (add:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-add-1x1-6u
  assay-add-1x1-6u
::
++  test-add-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-add-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[2 2]]])
  =/  assay-add-1x2-6u  (add:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-add-1x2-6u
  assay-add-1x2-6u
::
++  test-add-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-add-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[2 2 2]]])
  =/  assay-add-1x3-6u  (add:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-add-1x3-6u
  assay-add-1x3-6u
::
++  test-add-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-add-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[2] ~[2]]])
  =/  assay-add-2x1-6u  (add:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-add-2x1-6u
  assay-add-2x1-6u
::
++  test-add-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-add-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2]]])
  =/  assay-add-2x2-6u  (add:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-add-2x2-6u
  assay-add-2x2-6u
::
++  test-add-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-add-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2]]])
  =/  assay-add-2x3-6u  (add:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-add-2x3-6u
  assay-add-2x3-6u
::
++  test-add-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-add-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[2] ~[2] ~[2]]])
  =/  assay-add-3x1-6u  (add:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-add-3x1-6u
  assay-add-3x1-6u
::
++  test-add-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-add-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[2 2] ~[2 2] ~[2 2]]])
  =/  assay-add-3x2-6u  (add:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-add-3x2-6u
  assay-add-3x2-6u
::
++  test-add-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-add-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[2 2 2] ~[2 2 2] ~[2 2 2]]])
  =/  assay-add-3x3-6u  (add:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-add-3x3-6u
  assay-add-3x3-6u
::
++  test-sub-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-sub-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0]]])
  =/  assay-sub-1x1-4r  (sub:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-sub-1x1-4r
  assay-sub-1x1-4r
::
++  test-sub-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-sub-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0]]])
  =/  assay-sub-1x2-4r  (sub:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-sub-1x2-4r
  assay-sub-1x2-4r
::
++  test-sub-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-sub-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-sub-1x3-4r  (sub:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-sub-1x3-4r
  assay-sub-1x3-4r
::
++  test-sub-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-sub-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0]]])
  =/  assay-sub-2x1-4r  (sub:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-sub-2x1-4r
  assay-sub-2x1-4r
::
++  test-sub-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-sub-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-sub-2x2-4r  (sub:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-sub-2x2-4r
  assay-sub-2x2-4r
::
++  test-sub-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-sub-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-sub-2x3-4r  (sub:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-sub-2x3-4r
  assay-sub-2x3-4r
::
++  test-sub-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-sub-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0] ~[.~~0.0]]])
  =/  assay-sub-3x1-4r  (sub:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-sub-3x1-4r
  assay-sub-3x1-4r
::
++  test-sub-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-sub-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-sub-3x2-4r  (sub:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-sub-3x2-4r
  assay-sub-3x2-4r
::
++  test-sub-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-sub-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-sub-3x3-4r  (sub:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-sub-3x3-4r
  assay-sub-3x3-4r
::
++  test-sub-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-sub-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0]]])
  =/  assay-sub-1x1-5r  (sub:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-sub-1x1-5r
  assay-sub-1x1-5r
::
++  test-sub-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-sub-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0]]])
  =/  assay-sub-1x2-5r  (sub:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-sub-1x2-5r
  assay-sub-1x2-5r
::
++  test-sub-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-sub-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0]]])
  =/  assay-sub-1x3-5r  (sub:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-sub-1x3-5r
  assay-sub-1x3-5r
::
++  test-sub-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-sub-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0]]])
  =/  assay-sub-2x1-5r  (sub:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-sub-2x1-5r
  assay-sub-2x1-5r
::
++  test-sub-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-sub-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-sub-2x2-5r  (sub:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-sub-2x2-5r
  assay-sub-2x2-5r
::
++  test-sub-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-sub-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-sub-2x3-5r  (sub:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-sub-2x3-5r
  assay-sub-2x3-5r
::
++  test-sub-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-sub-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0] ~[.0.0]]])
  =/  assay-sub-3x1-5r  (sub:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-sub-3x1-5r
  assay-sub-3x1-5r
::
++  test-sub-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-sub-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-sub-3x2-5r  (sub:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-sub-3x2-5r
  assay-sub-3x2-5r
::
++  test-sub-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-sub-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-sub-3x3-5r  (sub:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-sub-3x3-5r
  assay-sub-3x3-5r
::
++  test-sub-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-sub-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0]]])
  =/  assay-sub-1x1-6r  (sub:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-sub-1x1-6r
  assay-sub-1x1-6r
::
++  test-sub-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-sub-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0]]])
  =/  assay-sub-1x2-6r  (sub:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-sub-1x2-6r
  assay-sub-1x2-6r
::
++  test-sub-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-sub-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-sub-1x3-6r  (sub:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-sub-1x3-6r
  assay-sub-1x3-6r
::
++  test-sub-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-sub-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0]]])
  =/  assay-sub-2x1-6r  (sub:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-sub-2x1-6r
  assay-sub-2x1-6r
::
++  test-sub-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-sub-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-sub-2x2-6r  (sub:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-sub-2x2-6r
  assay-sub-2x2-6r
::
++  test-sub-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-sub-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-sub-2x3-6r  (sub:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-sub-2x3-6r
  assay-sub-2x3-6r
::
++  test-sub-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-sub-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0] ~[.~0.0]]])
  =/  assay-sub-3x1-6r  (sub:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-sub-3x1-6r
  assay-sub-3x1-6r
::
++  test-sub-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-sub-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-sub-3x2-6r  (sub:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-sub-3x2-6r
  assay-sub-3x2-6r
::
++  test-sub-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-sub-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-sub-3x3-6r  (sub:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-sub-3x3-6r
  assay-sub-3x3-6r
::
++  test-sub-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-sub-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0]]])
  =/  assay-sub-1x1-7r  (sub:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-sub-1x1-7r
  assay-sub-1x1-7r
::
++  test-sub-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-sub-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0]]])
  =/  assay-sub-1x2-7r  (sub:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-sub-1x2-7r
  assay-sub-1x2-7r
::
++  test-sub-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-sub-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-sub-1x3-7r  (sub:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-sub-1x3-7r
  assay-sub-1x3-7r
::
++  test-sub-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-sub-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-sub-2x1-7r  (sub:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-sub-2x1-7r
  assay-sub-2x1-7r
::
++  test-sub-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-sub-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-sub-2x2-7r  (sub:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-sub-2x2-7r
  assay-sub-2x2-7r
::
++  test-sub-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-sub-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-sub-2x3-7r  (sub:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-sub-2x3-7r
  assay-sub-2x3-7r
::
++  test-sub-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-sub-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-sub-3x1-7r  (sub:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-sub-3x1-7r
  assay-sub-3x1-7r
::
++  test-sub-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-sub-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-sub-3x2-7r  (sub:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-sub-3x2-7r
  assay-sub-3x2-7r
::
++  test-sub-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-sub-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-sub-3x3-7r  (sub:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-sub-3x3-7r
  assay-sub-3x3-7r
::
++  test-sub-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-sub-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-sub-1x1-3u  (sub:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-sub-1x1-3u
  assay-sub-1x1-3u
::
++  test-sub-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-sub-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-sub-1x2-3u  (sub:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-sub-1x2-3u
  assay-sub-1x2-3u
::
++  test-sub-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-sub-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-sub-1x3-3u  (sub:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-sub-1x3-3u
  assay-sub-1x3-3u
::
++  test-sub-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-sub-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-sub-2x1-3u  (sub:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-sub-2x1-3u
  assay-sub-2x1-3u
::
++  test-sub-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-sub-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-sub-2x2-3u  (sub:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-sub-2x2-3u
  assay-sub-2x2-3u
::
++  test-sub-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-2x3-3u  (sub:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-sub-2x3-3u
  assay-sub-2x3-3u
::
++  test-sub-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-sub-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-sub-3x1-3u  (sub:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-sub-3x1-3u
  assay-sub-3x1-3u
::
++  test-sub-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-sub-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-sub-3x2-3u  (sub:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-sub-3x2-3u
  assay-sub-3x2-3u
::
++  test-sub-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-3x3-3u  (sub:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-sub-3x3-3u
  assay-sub-3x3-3u
::
++  test-sub-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-sub-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-sub-1x1-4u  (sub:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-sub-1x1-4u
  assay-sub-1x1-4u
::
++  test-sub-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-sub-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-sub-1x2-4u  (sub:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-sub-1x2-4u
  assay-sub-1x2-4u
::
++  test-sub-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-sub-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-sub-1x3-4u  (sub:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-sub-1x3-4u
  assay-sub-1x3-4u
::
++  test-sub-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-sub-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-sub-2x1-4u  (sub:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-sub-2x1-4u
  assay-sub-2x1-4u
::
++  test-sub-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-sub-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-sub-2x2-4u  (sub:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-sub-2x2-4u
  assay-sub-2x2-4u
::
++  test-sub-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-2x3-4u  (sub:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-sub-2x3-4u
  assay-sub-2x3-4u
::
++  test-sub-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-sub-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-sub-3x1-4u  (sub:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-sub-3x1-4u
  assay-sub-3x1-4u
::
++  test-sub-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-sub-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-sub-3x2-4u  (sub:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-sub-3x2-4u
  assay-sub-3x2-4u
::
++  test-sub-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-3x3-4u  (sub:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-sub-3x3-4u
  assay-sub-3x3-4u
::
++  test-sub-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-sub-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-sub-1x1-5u  (sub:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-sub-1x1-5u
  assay-sub-1x1-5u
::
++  test-sub-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-sub-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-sub-1x2-5u  (sub:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-sub-1x2-5u
  assay-sub-1x2-5u
::
++  test-sub-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-sub-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-sub-1x3-5u  (sub:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-sub-1x3-5u
  assay-sub-1x3-5u
::
++  test-sub-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-sub-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-sub-2x1-5u  (sub:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-sub-2x1-5u
  assay-sub-2x1-5u
::
++  test-sub-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-sub-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-sub-2x2-5u  (sub:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-sub-2x2-5u
  assay-sub-2x2-5u
::
++  test-sub-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-2x3-5u  (sub:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-sub-2x3-5u
  assay-sub-2x3-5u
::
++  test-sub-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-sub-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-sub-3x1-5u  (sub:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-sub-3x1-5u
  assay-sub-3x1-5u
::
++  test-sub-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-sub-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-sub-3x2-5u  (sub:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-sub-3x2-5u
  assay-sub-3x2-5u
::
++  test-sub-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-3x3-5u  (sub:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-sub-3x3-5u
  assay-sub-3x3-5u
::
++  test-sub-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-sub-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-sub-1x1-6u  (sub:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-sub-1x1-6u
  assay-sub-1x1-6u
::
++  test-sub-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-sub-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-sub-1x2-6u  (sub:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-sub-1x2-6u
  assay-sub-1x2-6u
::
++  test-sub-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-sub-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-sub-1x3-6u  (sub:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-sub-1x3-6u
  assay-sub-1x3-6u
::
++  test-sub-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-sub-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-sub-2x1-6u  (sub:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-sub-2x1-6u
  assay-sub-2x1-6u
::
++  test-sub-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-sub-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-sub-2x2-6u  (sub:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-sub-2x2-6u
  assay-sub-2x2-6u
::
++  test-sub-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-2x3-6u  (sub:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-sub-2x3-6u
  assay-sub-2x3-6u
::
++  test-sub-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-sub-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-sub-3x1-6u  (sub:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-sub-3x1-6u
  assay-sub-3x1-6u
::
++  test-sub-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-sub-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-sub-3x2-6u  (sub:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-sub-3x2-6u
  assay-sub-3x2-6u
::
++  test-sub-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-sub-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-sub-3x3-6u  (sub:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-sub-3x3-6u
  assay-sub-3x3-6u
::
++  test-mul-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-mul-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  assay-mul-1x1-4r  (mul:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-mul-1x1-4r
  assay-mul-1x1-4r
::
++  test-mul-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-mul-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  assay-mul-1x2-4r  (mul:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-mul-1x2-4r
  assay-mul-1x2-4r
::
++  test-mul-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-mul-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-mul-1x3-4r  (mul:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-mul-1x3-4r
  assay-mul-1x3-4r
::
++  test-mul-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-mul-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  assay-mul-2x1-4r  (mul:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-mul-2x1-4r
  assay-mul-2x1-4r
::
++  test-mul-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-mul-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-mul-2x2-4r  (mul:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-mul-2x2-4r
  assay-mul-2x2-4r
::
++  test-mul-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-mul-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-mul-2x3-4r  (mul:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-mul-2x3-4r
  assay-mul-2x3-4r
::
++  test-mul-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-mul-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  assay-mul-3x1-4r  (mul:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-mul-3x1-4r
  assay-mul-3x1-4r
::
++  test-mul-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-mul-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-mul-3x2-4r  (mul:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-mul-3x2-4r
  assay-mul-3x2-4r
::
++  test-mul-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-mul-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-mul-3x3-4r  (mul:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-mul-3x3-4r
  assay-mul-3x3-4r
::
++  test-mul-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-mul-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  assay-mul-1x1-5r  (mul:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-mul-1x1-5r
  assay-mul-1x1-5r
::
++  test-mul-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-mul-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  assay-mul-1x2-5r  (mul:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-mul-1x2-5r
  assay-mul-1x2-5r
::
++  test-mul-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-mul-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  assay-mul-1x3-5r  (mul:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-mul-1x3-5r
  assay-mul-1x3-5r
::
++  test-mul-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-mul-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  assay-mul-2x1-5r  (mul:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-mul-2x1-5r
  assay-mul-2x1-5r
::
++  test-mul-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-mul-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-mul-2x2-5r  (mul:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-mul-2x2-5r
  assay-mul-2x2-5r
::
++  test-mul-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-mul-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-mul-2x3-5r  (mul:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-mul-2x3-5r
  assay-mul-2x3-5r
::
++  test-mul-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-mul-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  assay-mul-3x1-5r  (mul:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-mul-3x1-5r
  assay-mul-3x1-5r
::
++  test-mul-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-mul-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-mul-3x2-5r  (mul:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-mul-3x2-5r
  assay-mul-3x2-5r
::
++  test-mul-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-mul-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-mul-3x3-5r  (mul:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-mul-3x3-5r
  assay-mul-3x3-5r
::
++  test-mul-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-mul-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  assay-mul-1x1-6r  (mul:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-mul-1x1-6r
  assay-mul-1x1-6r
::
++  test-mul-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-mul-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  assay-mul-1x2-6r  (mul:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-mul-1x2-6r
  assay-mul-1x2-6r
::
++  test-mul-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-mul-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-mul-1x3-6r  (mul:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-mul-1x3-6r
  assay-mul-1x3-6r
::
++  test-mul-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-mul-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  assay-mul-2x1-6r  (mul:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-mul-2x1-6r
  assay-mul-2x1-6r
::
++  test-mul-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-mul-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-mul-2x2-6r  (mul:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-mul-2x2-6r
  assay-mul-2x2-6r
::
++  test-mul-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-mul-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-mul-2x3-6r  (mul:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-mul-2x3-6r
  assay-mul-2x3-6r
::
++  test-mul-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-mul-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  assay-mul-3x1-6r  (mul:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-mul-3x1-6r
  assay-mul-3x1-6r
::
++  test-mul-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-mul-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-mul-3x2-6r  (mul:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-mul-3x2-6r
  assay-mul-3x2-6r
::
++  test-mul-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-mul-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-mul-3x3-6r  (mul:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-mul-3x3-6r
  assay-mul-3x3-6r
::
++  test-mul-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-mul-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  assay-mul-1x1-7r  (mul:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-mul-1x1-7r
  assay-mul-1x1-7r
::
++  test-mul-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-mul-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  assay-mul-1x2-7r  (mul:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-mul-1x2-7r
  assay-mul-1x2-7r
::
++  test-mul-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-mul-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-mul-1x3-7r  (mul:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-mul-1x3-7r
  assay-mul-1x3-7r
::
++  test-mul-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-mul-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-mul-2x1-7r  (mul:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-mul-2x1-7r
  assay-mul-2x1-7r
::
++  test-mul-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-mul-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-mul-2x2-7r  (mul:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-mul-2x2-7r
  assay-mul-2x2-7r
::
++  test-mul-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-mul-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-mul-2x3-7r  (mul:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-mul-2x3-7r
  assay-mul-2x3-7r
::
++  test-mul-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-mul-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-mul-3x1-7r  (mul:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-mul-3x1-7r
  assay-mul-3x1-7r
::
++  test-mul-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-mul-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-mul-3x2-7r  (mul:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-mul-3x2-7r
  assay-mul-3x2-7r
::
++  test-mul-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-mul-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-mul-3x3-7r  (mul:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-mul-3x3-7r
  assay-mul-3x3-7r
::
++  test-mul-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mul-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-mul-1x1-3u  (mul:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-mul-1x1-3u
  assay-mul-1x1-3u
::
++  test-mul-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mul-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-mul-1x2-3u  (mul:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-mul-1x2-3u
  assay-mul-1x2-3u
::
++  test-mul-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mul-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-mul-1x3-3u  (mul:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-mul-1x3-3u
  assay-mul-1x3-3u
::
++  test-mul-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mul-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-mul-2x1-3u  (mul:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-mul-2x1-3u
  assay-mul-2x1-3u
::
++  test-mul-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mul-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-mul-2x2-3u  (mul:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-mul-2x2-3u
  assay-mul-2x2-3u
::
++  test-mul-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-2x3-3u  (mul:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-mul-2x3-3u
  assay-mul-2x3-3u
::
++  test-mul-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mul-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-mul-3x1-3u  (mul:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-mul-3x1-3u
  assay-mul-3x1-3u
::
++  test-mul-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mul-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-mul-3x2-3u  (mul:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-mul-3x2-3u
  assay-mul-3x2-3u
::
++  test-mul-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-3x3-3u  (mul:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-mul-3x3-3u
  assay-mul-3x3-3u
::
++  test-mul-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mul-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-mul-1x1-4u  (mul:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-mul-1x1-4u
  assay-mul-1x1-4u
::
++  test-mul-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mul-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-mul-1x2-4u  (mul:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-mul-1x2-4u
  assay-mul-1x2-4u
::
++  test-mul-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mul-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-mul-1x3-4u  (mul:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-mul-1x3-4u
  assay-mul-1x3-4u
::
++  test-mul-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mul-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-mul-2x1-4u  (mul:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-mul-2x1-4u
  assay-mul-2x1-4u
::
++  test-mul-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mul-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-mul-2x2-4u  (mul:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-mul-2x2-4u
  assay-mul-2x2-4u
::
++  test-mul-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-2x3-4u  (mul:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-mul-2x3-4u
  assay-mul-2x3-4u
::
++  test-mul-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mul-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-mul-3x1-4u  (mul:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-mul-3x1-4u
  assay-mul-3x1-4u
::
++  test-mul-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mul-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-mul-3x2-4u  (mul:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-mul-3x2-4u
  assay-mul-3x2-4u
::
++  test-mul-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-3x3-4u  (mul:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-mul-3x3-4u
  assay-mul-3x3-4u
::
++  test-mul-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mul-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-mul-1x1-5u  (mul:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-mul-1x1-5u
  assay-mul-1x1-5u
::
++  test-mul-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mul-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-mul-1x2-5u  (mul:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-mul-1x2-5u
  assay-mul-1x2-5u
::
++  test-mul-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mul-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-mul-1x3-5u  (mul:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-mul-1x3-5u
  assay-mul-1x3-5u
::
++  test-mul-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mul-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-mul-2x1-5u  (mul:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-mul-2x1-5u
  assay-mul-2x1-5u
::
++  test-mul-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mul-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-mul-2x2-5u  (mul:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-mul-2x2-5u
  assay-mul-2x2-5u
::
++  test-mul-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-2x3-5u  (mul:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-mul-2x3-5u
  assay-mul-2x3-5u
::
++  test-mul-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mul-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-mul-3x1-5u  (mul:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-mul-3x1-5u
  assay-mul-3x1-5u
::
++  test-mul-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mul-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-mul-3x2-5u  (mul:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-mul-3x2-5u
  assay-mul-3x2-5u
::
++  test-mul-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-3x3-5u  (mul:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-mul-3x3-5u
  assay-mul-3x3-5u
::
++  test-mul-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mul-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-mul-1x1-6u  (mul:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-mul-1x1-6u
  assay-mul-1x1-6u
::
++  test-mul-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mul-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-mul-1x2-6u  (mul:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-mul-1x2-6u
  assay-mul-1x2-6u
::
++  test-mul-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mul-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-mul-1x3-6u  (mul:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-mul-1x3-6u
  assay-mul-1x3-6u
::
++  test-mul-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mul-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-mul-2x1-6u  (mul:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-mul-2x1-6u
  assay-mul-2x1-6u
::
++  test-mul-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mul-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-mul-2x2-6u  (mul:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-mul-2x2-6u
  assay-mul-2x2-6u
::
++  test-mul-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-2x3-6u  (mul:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-mul-2x3-6u
  assay-mul-2x3-6u
::
++  test-mul-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mul-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-mul-3x1-6u  (mul:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-mul-3x1-6u
  assay-mul-3x1-6u
::
++  test-mul-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mul-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-mul-3x2-6u  (mul:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-mul-3x2-6u
  assay-mul-3x2-6u
::
++  test-mul-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mul-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-mul-3x3-6u  (mul:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-mul-3x3-6u
  assay-mul-3x3-6u
::
++  test-div-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-div-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  assay-div-1x1-4r  (div:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-div-1x1-4r
  assay-div-1x1-4r
::
++  test-div-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-div-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  assay-div-1x2-4r  (div:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-div-1x2-4r
  assay-div-1x2-4r
::
++  test-div-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-div-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-div-1x3-4r  (div:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-div-1x3-4r
  assay-div-1x3-4r
::
++  test-div-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-div-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  assay-div-2x1-4r  (div:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-div-2x1-4r
  assay-div-2x1-4r
::
++  test-div-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-div-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-div-2x2-4r  (div:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-div-2x2-4r
  assay-div-2x2-4r
::
++  test-div-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-div-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-div-2x3-4r  (div:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-div-2x3-4r
  assay-div-2x3-4r
::
++  test-div-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-div-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  assay-div-3x1-4r  (div:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-div-3x1-4r
  assay-div-3x1-4r
::
++  test-div-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-div-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-div-3x2-4r  (div:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-div-3x2-4r
  assay-div-3x2-4r
::
++  test-div-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-div-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-div-3x3-4r  (div:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-div-3x3-4r
  assay-div-3x3-4r
::
++  test-div-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-div-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  assay-div-1x1-5r  (div:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-div-1x1-5r
  assay-div-1x1-5r
::
++  test-div-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-div-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  assay-div-1x2-5r  (div:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-div-1x2-5r
  assay-div-1x2-5r
::
++  test-div-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-div-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  assay-div-1x3-5r  (div:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-div-1x3-5r
  assay-div-1x3-5r
::
++  test-div-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-div-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  assay-div-2x1-5r  (div:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-div-2x1-5r
  assay-div-2x1-5r
::
++  test-div-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-div-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-div-2x2-5r  (div:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-div-2x2-5r
  assay-div-2x2-5r
::
++  test-div-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-div-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-div-2x3-5r  (div:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-div-2x3-5r
  assay-div-2x3-5r
::
++  test-div-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-div-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  assay-div-3x1-5r  (div:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-div-3x1-5r
  assay-div-3x1-5r
::
++  test-div-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-div-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-div-3x2-5r  (div:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-div-3x2-5r
  assay-div-3x2-5r
::
++  test-div-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-div-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-div-3x3-5r  (div:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-div-3x3-5r
  assay-div-3x3-5r
::
++  test-div-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-div-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  assay-div-1x1-6r  (div:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-div-1x1-6r
  assay-div-1x1-6r
::
++  test-div-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-div-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  assay-div-1x2-6r  (div:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-div-1x2-6r
  assay-div-1x2-6r
::
++  test-div-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-div-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-div-1x3-6r  (div:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-div-1x3-6r
  assay-div-1x3-6r
::
++  test-div-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-div-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  assay-div-2x1-6r  (div:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-div-2x1-6r
  assay-div-2x1-6r
::
++  test-div-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-div-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-div-2x2-6r  (div:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-div-2x2-6r
  assay-div-2x2-6r
::
++  test-div-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-div-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-div-2x3-6r  (div:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-div-2x3-6r
  assay-div-2x3-6r
::
++  test-div-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-div-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  assay-div-3x1-6r  (div:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-div-3x1-6r
  assay-div-3x1-6r
::
++  test-div-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-div-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-div-3x2-6r  (div:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-div-3x2-6r
  assay-div-3x2-6r
::
++  test-div-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-div-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-div-3x3-6r  (div:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-div-3x3-6r
  assay-div-3x3-6r
::
++  test-div-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-div-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  assay-div-1x1-7r  (div:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-div-1x1-7r
  assay-div-1x1-7r
::
++  test-div-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-div-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  assay-div-1x2-7r  (div:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-div-1x2-7r
  assay-div-1x2-7r
::
++  test-div-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-div-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-div-1x3-7r  (div:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-div-1x3-7r
  assay-div-1x3-7r
::
++  test-div-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-div-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-div-2x1-7r  (div:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-div-2x1-7r
  assay-div-2x1-7r
::
++  test-div-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-div-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-div-2x2-7r  (div:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-div-2x2-7r
  assay-div-2x2-7r
::
++  test-div-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-div-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-div-2x3-7r  (div:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-div-2x3-7r
  assay-div-2x3-7r
::
++  test-div-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-div-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-div-3x1-7r  (div:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-div-3x1-7r
  assay-div-3x1-7r
::
++  test-div-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-div-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-div-3x2-7r  (div:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-div-3x2-7r
  assay-div-3x2-7r
::
++  test-div-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-div-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-div-3x3-7r  (div:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-div-3x3-7r
  assay-div-3x3-7r
::
++  test-div-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-div-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-div-1x1-3u  (div:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-div-1x1-3u
  assay-div-1x1-3u
::
++  test-div-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-div-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-div-1x2-3u  (div:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-div-1x2-3u
  assay-div-1x2-3u
::
++  test-div-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-div-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-div-1x3-3u  (div:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-div-1x3-3u
  assay-div-1x3-3u
::
++  test-div-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-div-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-div-2x1-3u  (div:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-div-2x1-3u
  assay-div-2x1-3u
::
++  test-div-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-div-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-div-2x2-3u  (div:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-div-2x2-3u
  assay-div-2x2-3u
::
++  test-div-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-div-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-div-2x3-3u  (div:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-div-2x3-3u
  assay-div-2x3-3u
::
++  test-div-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-div-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-div-3x1-3u  (div:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-div-3x1-3u
  assay-div-3x1-3u
::
++  test-div-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-div-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-div-3x2-3u  (div:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-div-3x2-3u
  assay-div-3x2-3u
::
++  test-div-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-div-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-div-3x3-3u  (div:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-div-3x3-3u
  assay-div-3x3-3u
::
++  test-div-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-div-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-div-1x1-4u  (div:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-div-1x1-4u
  assay-div-1x1-4u
::
++  test-div-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-div-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-div-1x2-4u  (div:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-div-1x2-4u
  assay-div-1x2-4u
::
++  test-div-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-div-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-div-1x3-4u  (div:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-div-1x3-4u
  assay-div-1x3-4u
::
++  test-div-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-div-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-div-2x1-4u  (div:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-div-2x1-4u
  assay-div-2x1-4u
::
++  test-div-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-div-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-div-2x2-4u  (div:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-div-2x2-4u
  assay-div-2x2-4u
::
++  test-div-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-div-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-div-2x3-4u  (div:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-div-2x3-4u
  assay-div-2x3-4u
::
++  test-div-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-div-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-div-3x1-4u  (div:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-div-3x1-4u
  assay-div-3x1-4u
::
++  test-div-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-div-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-div-3x2-4u  (div:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-div-3x2-4u
  assay-div-3x2-4u
::
++  test-div-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-div-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-div-3x3-4u  (div:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-div-3x3-4u
  assay-div-3x3-4u
::
++  test-div-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-div-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-div-1x1-5u  (div:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-div-1x1-5u
  assay-div-1x1-5u
::
++  test-div-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-div-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-div-1x2-5u  (div:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-div-1x2-5u
  assay-div-1x2-5u
::
++  test-div-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-div-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-div-1x3-5u  (div:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-div-1x3-5u
  assay-div-1x3-5u
::
++  test-div-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-div-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-div-2x1-5u  (div:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-div-2x1-5u
  assay-div-2x1-5u
::
++  test-div-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-div-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-div-2x2-5u  (div:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-div-2x2-5u
  assay-div-2x2-5u
::
++  test-div-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-div-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-div-2x3-5u  (div:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-div-2x3-5u
  assay-div-2x3-5u
::
++  test-div-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-div-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-div-3x1-5u  (div:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-div-3x1-5u
  assay-div-3x1-5u
::
++  test-div-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-div-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-div-3x2-5u  (div:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-div-3x2-5u
  assay-div-3x2-5u
::
++  test-div-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-div-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-div-3x3-5u  (div:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-div-3x3-5u
  assay-div-3x3-5u
::
++  test-div-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-div-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-div-1x1-6u  (div:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-div-1x1-6u
  assay-div-1x1-6u
::
++  test-div-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-div-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-div-1x2-6u  (div:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-div-1x2-6u
  assay-div-1x2-6u
::
++  test-div-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-div-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-div-1x3-6u  (div:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-div-1x3-6u
  assay-div-1x3-6u
::
++  test-div-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-div-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-div-2x1-6u  (div:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-div-2x1-6u
  assay-div-2x1-6u
::
++  test-div-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-div-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-div-2x2-6u  (div:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-div-2x2-6u
  assay-div-2x2-6u
::
++  test-div-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-div-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-div-2x3-6u  (div:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-div-2x3-6u
  assay-div-2x3-6u
::
++  test-div-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-div-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-div-3x1-6u  (div:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-div-3x1-6u
  assay-div-3x1-6u
::
++  test-div-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-div-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-div-3x2-6u  (div:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-div-3x2-6u
  assay-div-3x2-6u
::
++  test-div-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-div-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-div-3x3-6u  (div:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-div-3x3-6u
  assay-div-3x3-6u
::
++  test-mod-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mod-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-mod-1x1-3u  (mod:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-mod-1x1-3u
  assay-mod-1x1-3u
::
++  test-mod-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mod-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-mod-1x2-3u  (mod:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-mod-1x2-3u
  assay-mod-1x2-3u
::
++  test-mod-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mod-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-mod-1x3-3u  (mod:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-mod-1x3-3u
  assay-mod-1x3-3u
::
++  test-mod-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mod-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-mod-2x1-3u  (mod:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-mod-2x1-3u
  assay-mod-2x1-3u
::
++  test-mod-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mod-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-mod-2x2-3u  (mod:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-mod-2x2-3u
  assay-mod-2x2-3u
::
++  test-mod-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-2x3-3u  (mod:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-mod-2x3-3u
  assay-mod-2x3-3u
::
++  test-mod-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mod-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-mod-3x1-3u  (mod:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-mod-3x1-3u
  assay-mod-3x1-3u
::
++  test-mod-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mod-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-mod-3x2-3u  (mod:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-mod-3x2-3u
  assay-mod-3x2-3u
::
++  test-mod-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-3x3-3u  (mod:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-mod-3x3-3u
  assay-mod-3x3-3u
::
++  test-mod-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mod-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-mod-1x1-4u  (mod:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-mod-1x1-4u
  assay-mod-1x1-4u
::
++  test-mod-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mod-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-mod-1x2-4u  (mod:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-mod-1x2-4u
  assay-mod-1x2-4u
::
++  test-mod-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mod-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-mod-1x3-4u  (mod:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-mod-1x3-4u
  assay-mod-1x3-4u
::
++  test-mod-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mod-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-mod-2x1-4u  (mod:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-mod-2x1-4u
  assay-mod-2x1-4u
::
++  test-mod-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mod-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-mod-2x2-4u  (mod:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-mod-2x2-4u
  assay-mod-2x2-4u
::
++  test-mod-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-2x3-4u  (mod:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-mod-2x3-4u
  assay-mod-2x3-4u
::
++  test-mod-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mod-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-mod-3x1-4u  (mod:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-mod-3x1-4u
  assay-mod-3x1-4u
::
++  test-mod-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mod-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-mod-3x2-4u  (mod:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-mod-3x2-4u
  assay-mod-3x2-4u
::
++  test-mod-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-3x3-4u  (mod:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-mod-3x3-4u
  assay-mod-3x3-4u
::
++  test-mod-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mod-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-mod-1x1-5u  (mod:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-mod-1x1-5u
  assay-mod-1x1-5u
::
++  test-mod-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mod-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-mod-1x2-5u  (mod:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-mod-1x2-5u
  assay-mod-1x2-5u
::
++  test-mod-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mod-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-mod-1x3-5u  (mod:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-mod-1x3-5u
  assay-mod-1x3-5u
::
++  test-mod-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mod-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-mod-2x1-5u  (mod:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-mod-2x1-5u
  assay-mod-2x1-5u
::
++  test-mod-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mod-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-mod-2x2-5u  (mod:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-mod-2x2-5u
  assay-mod-2x2-5u
::
++  test-mod-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-2x3-5u  (mod:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-mod-2x3-5u
  assay-mod-2x3-5u
::
++  test-mod-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mod-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-mod-3x1-5u  (mod:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-mod-3x1-5u
  assay-mod-3x1-5u
::
++  test-mod-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mod-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-mod-3x2-5u  (mod:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-mod-3x2-5u
  assay-mod-3x2-5u
::
++  test-mod-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-3x3-5u  (mod:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-mod-3x3-5u
  assay-mod-3x3-5u
::
++  test-mod-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-mod-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-mod-1x1-6u  (mod:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-mod-1x1-6u
  assay-mod-1x1-6u
::
++  test-mod-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-mod-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-mod-1x2-6u  (mod:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-mod-1x2-6u
  assay-mod-1x2-6u
::
++  test-mod-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-mod-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-mod-1x3-6u  (mod:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-mod-1x3-6u
  assay-mod-1x3-6u
::
++  test-mod-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-mod-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-mod-2x1-6u  (mod:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-mod-2x1-6u
  assay-mod-2x1-6u
::
++  test-mod-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-mod-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-mod-2x2-6u  (mod:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-mod-2x2-6u
  assay-mod-2x2-6u
::
++  test-mod-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-2x3-6u  (mod:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-mod-2x3-6u
  assay-mod-2x3-6u
::
++  test-mod-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-mod-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-mod-3x1-6u  (mod:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-mod-3x1-6u
  assay-mod-3x1-6u
::
++  test-mod-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-mod-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-mod-3x2-6u  (mod:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-mod-3x2-6u
  assay-mod-3x2-6u
::
++  test-mod-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-mod-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-mod-3x3-6u  (mod:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-mod-3x3-6u
  assay-mod-3x3-6u
::
++  test-lte-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-lte-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  assay-lte-1x1-4r  (lte:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-lte-1x1-4r
  assay-lte-1x1-4r
::
++  test-lte-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-lte-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  assay-lte-1x2-4r  (lte:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-lte-1x2-4r
  assay-lte-1x2-4r
::
++  test-lte-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lte-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-lte-1x3-4r  (lte:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-lte-1x3-4r
  assay-lte-1x3-4r
::
++  test-lte-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-lte-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  assay-lte-2x1-4r  (lte:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-lte-2x1-4r
  assay-lte-2x1-4r
::
++  test-lte-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-lte-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-lte-2x2-4r  (lte:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-lte-2x2-4r
  assay-lte-2x2-4r
::
++  test-lte-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lte-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-lte-2x3-4r  (lte:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-lte-2x3-4r
  assay-lte-2x3-4r
::
++  test-lte-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-lte-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  assay-lte-3x1-4r  (lte:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-lte-3x1-4r
  assay-lte-3x1-4r
::
++  test-lte-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-lte-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-lte-3x2-4r  (lte:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-lte-3x2-4r
  assay-lte-3x2-4r
::
++  test-lte-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lte-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-lte-3x3-4r  (lte:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-lte-3x3-4r
  assay-lte-3x3-4r
::
++  test-lte-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-lte-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  assay-lte-1x1-5r  (lte:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-lte-1x1-5r
  assay-lte-1x1-5r
::
++  test-lte-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-lte-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  assay-lte-1x2-5r  (lte:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-lte-1x2-5r
  assay-lte-1x2-5r
::
++  test-lte-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-lte-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  assay-lte-1x3-5r  (lte:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-lte-1x3-5r
  assay-lte-1x3-5r
::
++  test-lte-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-lte-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  assay-lte-2x1-5r  (lte:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-lte-2x1-5r
  assay-lte-2x1-5r
::
++  test-lte-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-lte-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-lte-2x2-5r  (lte:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-lte-2x2-5r
  assay-lte-2x2-5r
::
++  test-lte-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-lte-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-lte-2x3-5r  (lte:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-lte-2x3-5r
  assay-lte-2x3-5r
::
++  test-lte-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-lte-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  assay-lte-3x1-5r  (lte:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-lte-3x1-5r
  assay-lte-3x1-5r
::
++  test-lte-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-lte-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-lte-3x2-5r  (lte:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-lte-3x2-5r
  assay-lte-3x2-5r
::
++  test-lte-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-lte-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-lte-3x3-5r  (lte:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-lte-3x3-5r
  assay-lte-3x3-5r
::
++  test-lte-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-lte-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  assay-lte-1x1-6r  (lte:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-lte-1x1-6r
  assay-lte-1x1-6r
::
++  test-lte-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-lte-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  assay-lte-1x2-6r  (lte:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-lte-1x2-6r
  assay-lte-1x2-6r
::
++  test-lte-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lte-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-lte-1x3-6r  (lte:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-lte-1x3-6r
  assay-lte-1x3-6r
::
++  test-lte-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-lte-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  assay-lte-2x1-6r  (lte:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-lte-2x1-6r
  assay-lte-2x1-6r
::
++  test-lte-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-lte-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-lte-2x2-6r  (lte:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-lte-2x2-6r
  assay-lte-2x2-6r
::
++  test-lte-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lte-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-lte-2x3-6r  (lte:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-lte-2x3-6r
  assay-lte-2x3-6r
::
++  test-lte-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-lte-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  assay-lte-3x1-6r  (lte:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-lte-3x1-6r
  assay-lte-3x1-6r
::
++  test-lte-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-lte-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-lte-3x2-6r  (lte:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-lte-3x2-6r
  assay-lte-3x2-6r
::
++  test-lte-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lte-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-lte-3x3-6r  (lte:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-lte-3x3-6r
  assay-lte-3x3-6r
::
++  test-lte-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-lte-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  assay-lte-1x1-7r  (lte:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-lte-1x1-7r
  assay-lte-1x1-7r
::
++  test-lte-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-lte-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  assay-lte-1x2-7r  (lte:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-lte-1x2-7r
  assay-lte-1x2-7r
::
++  test-lte-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lte-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-lte-1x3-7r  (lte:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-lte-1x3-7r
  assay-lte-1x3-7r
::
++  test-lte-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-lte-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-lte-2x1-7r  (lte:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-lte-2x1-7r
  assay-lte-2x1-7r
::
++  test-lte-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-lte-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-lte-2x2-7r  (lte:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-lte-2x2-7r
  assay-lte-2x2-7r
::
++  test-lte-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lte-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-lte-2x3-7r  (lte:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-lte-2x3-7r
  assay-lte-2x3-7r
::
++  test-lte-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-lte-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-lte-3x1-7r  (lte:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-lte-3x1-7r
  assay-lte-3x1-7r
::
++  test-lte-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-lte-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-lte-3x2-7r  (lte:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-lte-3x2-7r
  assay-lte-3x2-7r
::
++  test-lte-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lte-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-lte-3x3-7r  (lte:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-lte-3x3-7r
  assay-lte-3x3-7r
::
++  test-lte-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lte-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-lte-1x1-3u  (lte:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-lte-1x1-3u
  assay-lte-1x1-3u
::
++  test-lte-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lte-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-lte-1x2-3u  (lte:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-lte-1x2-3u
  assay-lte-1x2-3u
::
++  test-lte-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lte-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-lte-1x3-3u  (lte:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-lte-1x3-3u
  assay-lte-1x3-3u
::
++  test-lte-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lte-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-lte-2x1-3u  (lte:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-lte-2x1-3u
  assay-lte-2x1-3u
::
++  test-lte-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lte-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-lte-2x2-3u  (lte:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-lte-2x2-3u
  assay-lte-2x2-3u
::
++  test-lte-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-2x3-3u  (lte:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-lte-2x3-3u
  assay-lte-2x3-3u
::
++  test-lte-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lte-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-lte-3x1-3u  (lte:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-lte-3x1-3u
  assay-lte-3x1-3u
::
++  test-lte-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lte-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-lte-3x2-3u  (lte:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-lte-3x2-3u
  assay-lte-3x2-3u
::
++  test-lte-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-3x3-3u  (lte:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-lte-3x3-3u
  assay-lte-3x3-3u
::
++  test-lte-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lte-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-lte-1x1-4u  (lte:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-lte-1x1-4u
  assay-lte-1x1-4u
::
++  test-lte-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lte-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-lte-1x2-4u  (lte:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-lte-1x2-4u
  assay-lte-1x2-4u
::
++  test-lte-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lte-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-lte-1x3-4u  (lte:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-lte-1x3-4u
  assay-lte-1x3-4u
::
++  test-lte-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lte-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-lte-2x1-4u  (lte:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-lte-2x1-4u
  assay-lte-2x1-4u
::
++  test-lte-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lte-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-lte-2x2-4u  (lte:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-lte-2x2-4u
  assay-lte-2x2-4u
::
++  test-lte-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-2x3-4u  (lte:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-lte-2x3-4u
  assay-lte-2x3-4u
::
++  test-lte-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lte-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-lte-3x1-4u  (lte:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-lte-3x1-4u
  assay-lte-3x1-4u
::
++  test-lte-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lte-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-lte-3x2-4u  (lte:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-lte-3x2-4u
  assay-lte-3x2-4u
::
++  test-lte-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-3x3-4u  (lte:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-lte-3x3-4u
  assay-lte-3x3-4u
::
++  test-lte-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lte-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-lte-1x1-5u  (lte:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-lte-1x1-5u
  assay-lte-1x1-5u
::
++  test-lte-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lte-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-lte-1x2-5u  (lte:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-lte-1x2-5u
  assay-lte-1x2-5u
::
++  test-lte-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lte-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-lte-1x3-5u  (lte:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-lte-1x3-5u
  assay-lte-1x3-5u
::
++  test-lte-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lte-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-lte-2x1-5u  (lte:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-lte-2x1-5u
  assay-lte-2x1-5u
::
++  test-lte-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lte-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-lte-2x2-5u  (lte:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-lte-2x2-5u
  assay-lte-2x2-5u
::
++  test-lte-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-2x3-5u  (lte:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-lte-2x3-5u
  assay-lte-2x3-5u
::
++  test-lte-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lte-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-lte-3x1-5u  (lte:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-lte-3x1-5u
  assay-lte-3x1-5u
::
++  test-lte-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lte-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-lte-3x2-5u  (lte:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-lte-3x2-5u
  assay-lte-3x2-5u
::
++  test-lte-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-3x3-5u  (lte:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-lte-3x3-5u
  assay-lte-3x3-5u
::
++  test-lte-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lte-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-lte-1x1-6u  (lte:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-lte-1x1-6u
  assay-lte-1x1-6u
::
++  test-lte-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lte-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-lte-1x2-6u  (lte:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-lte-1x2-6u
  assay-lte-1x2-6u
::
++  test-lte-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lte-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-lte-1x3-6u  (lte:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-lte-1x3-6u
  assay-lte-1x3-6u
::
++  test-lte-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lte-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-lte-2x1-6u  (lte:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-lte-2x1-6u
  assay-lte-2x1-6u
::
++  test-lte-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lte-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-lte-2x2-6u  (lte:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-lte-2x2-6u
  assay-lte-2x2-6u
::
++  test-lte-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-2x3-6u  (lte:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-lte-2x3-6u
  assay-lte-2x3-6u
::
++  test-lte-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lte-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-lte-3x1-6u  (lte:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-lte-3x1-6u
  assay-lte-3x1-6u
::
++  test-lte-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lte-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-lte-3x2-6u  (lte:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-lte-3x2-6u
  assay-lte-3x2-6u
::
++  test-lte-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lte-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-lte-3x3-6u  (lte:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-lte-3x3-6u
  assay-lte-3x3-6u
::
++  test-lth-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-lth-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0]]])
  =/  assay-lth-1x1-4r  (lth:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-lth-1x1-4r
  assay-lth-1x1-4r
::
++  test-lth-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-lth-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0]]])
  =/  assay-lth-1x2-4r  (lth:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-lth-1x2-4r
  assay-lth-1x2-4r
::
++  test-lth-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lth-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-lth-1x3-4r  (lth:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-lth-1x3-4r
  assay-lth-1x3-4r
::
++  test-lth-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-lth-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0]]])
  =/  assay-lth-2x1-4r  (lth:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-lth-2x1-4r
  assay-lth-2x1-4r
::
++  test-lth-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-lth-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-lth-2x2-4r  (lth:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-lth-2x2-4r
  assay-lth-2x2-4r
::
++  test-lth-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lth-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-lth-2x3-4r  (lth:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-lth-2x3-4r
  assay-lth-2x3-4r
::
++  test-lth-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-lth-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0] ~[.~~0.0]]])
  =/  assay-lth-3x1-4r  (lth:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-lth-3x1-4r
  assay-lth-3x1-4r
::
++  test-lth-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-lth-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-lth-3x2-4r  (lth:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-lth-3x2-4r
  assay-lth-3x2-4r
::
++  test-lth-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-lth-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-lth-3x3-4r  (lth:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-lth-3x3-4r
  assay-lth-3x3-4r
::
++  test-lth-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-lth-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0]]])
  =/  assay-lth-1x1-5r  (lth:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-lth-1x1-5r
  assay-lth-1x1-5r
::
++  test-lth-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-lth-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0]]])
  =/  assay-lth-1x2-5r  (lth:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-lth-1x2-5r
  assay-lth-1x2-5r
::
++  test-lth-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-lth-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0]]])
  =/  assay-lth-1x3-5r  (lth:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-lth-1x3-5r
  assay-lth-1x3-5r
::
++  test-lth-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-lth-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0]]])
  =/  assay-lth-2x1-5r  (lth:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-lth-2x1-5r
  assay-lth-2x1-5r
::
++  test-lth-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-lth-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-lth-2x2-5r  (lth:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-lth-2x2-5r
  assay-lth-2x2-5r
::
++  test-lth-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-lth-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-lth-2x3-5r  (lth:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-lth-2x3-5r
  assay-lth-2x3-5r
::
++  test-lth-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-lth-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0] ~[.0.0]]])
  =/  assay-lth-3x1-5r  (lth:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-lth-3x1-5r
  assay-lth-3x1-5r
::
++  test-lth-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-lth-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-lth-3x2-5r  (lth:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-lth-3x2-5r
  assay-lth-3x2-5r
::
++  test-lth-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-lth-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-lth-3x3-5r  (lth:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-lth-3x3-5r
  assay-lth-3x3-5r
::
++  test-lth-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-lth-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0]]])
  =/  assay-lth-1x1-6r  (lth:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-lth-1x1-6r
  assay-lth-1x1-6r
::
++  test-lth-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-lth-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0]]])
  =/  assay-lth-1x2-6r  (lth:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-lth-1x2-6r
  assay-lth-1x2-6r
::
++  test-lth-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lth-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-lth-1x3-6r  (lth:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-lth-1x3-6r
  assay-lth-1x3-6r
::
++  test-lth-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-lth-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0]]])
  =/  assay-lth-2x1-6r  (lth:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-lth-2x1-6r
  assay-lth-2x1-6r
::
++  test-lth-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-lth-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-lth-2x2-6r  (lth:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-lth-2x2-6r
  assay-lth-2x2-6r
::
++  test-lth-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lth-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-lth-2x3-6r  (lth:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-lth-2x3-6r
  assay-lth-2x3-6r
::
++  test-lth-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-lth-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0] ~[.~0.0]]])
  =/  assay-lth-3x1-6r  (lth:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-lth-3x1-6r
  assay-lth-3x1-6r
::
++  test-lth-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-lth-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-lth-3x2-6r  (lth:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-lth-3x2-6r
  assay-lth-3x2-6r
::
++  test-lth-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-lth-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-lth-3x3-6r  (lth:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-lth-3x3-6r
  assay-lth-3x3-6r
::
++  test-lth-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-lth-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0]]])
  =/  assay-lth-1x1-7r  (lth:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-lth-1x1-7r
  assay-lth-1x1-7r
::
++  test-lth-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-lth-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0]]])
  =/  assay-lth-1x2-7r  (lth:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-lth-1x2-7r
  assay-lth-1x2-7r
::
++  test-lth-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lth-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-lth-1x3-7r  (lth:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-lth-1x3-7r
  assay-lth-1x3-7r
::
++  test-lth-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-lth-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-lth-2x1-7r  (lth:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-lth-2x1-7r
  assay-lth-2x1-7r
::
++  test-lth-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-lth-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-lth-2x2-7r  (lth:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-lth-2x2-7r
  assay-lth-2x2-7r
::
++  test-lth-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lth-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-lth-2x3-7r  (lth:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-lth-2x3-7r
  assay-lth-2x3-7r
::
++  test-lth-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-lth-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-lth-3x1-7r  (lth:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-lth-3x1-7r
  assay-lth-3x1-7r
::
++  test-lth-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-lth-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-lth-3x2-7r  (lth:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-lth-3x2-7r
  assay-lth-3x2-7r
::
++  test-lth-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-lth-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-lth-3x3-7r  (lth:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-lth-3x3-7r
  assay-lth-3x3-7r
::
++  test-lth-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lth-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-lth-1x1-3u  (lth:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-lth-1x1-3u
  assay-lth-1x1-3u
::
++  test-lth-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lth-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-lth-1x2-3u  (lth:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-lth-1x2-3u
  assay-lth-1x2-3u
::
++  test-lth-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lth-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-lth-1x3-3u  (lth:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-lth-1x3-3u
  assay-lth-1x3-3u
::
++  test-lth-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lth-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-lth-2x1-3u  (lth:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-lth-2x1-3u
  assay-lth-2x1-3u
::
++  test-lth-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lth-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-lth-2x2-3u  (lth:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-lth-2x2-3u
  assay-lth-2x2-3u
::
++  test-lth-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-2x3-3u  (lth:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-lth-2x3-3u
  assay-lth-2x3-3u
::
++  test-lth-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lth-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-lth-3x1-3u  (lth:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-lth-3x1-3u
  assay-lth-3x1-3u
::
++  test-lth-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lth-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-lth-3x2-3u  (lth:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-lth-3x2-3u
  assay-lth-3x2-3u
::
++  test-lth-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-3x3-3u  (lth:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-lth-3x3-3u
  assay-lth-3x3-3u
::
++  test-lth-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lth-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-lth-1x1-4u  (lth:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-lth-1x1-4u
  assay-lth-1x1-4u
::
++  test-lth-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lth-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-lth-1x2-4u  (lth:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-lth-1x2-4u
  assay-lth-1x2-4u
::
++  test-lth-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lth-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-lth-1x3-4u  (lth:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-lth-1x3-4u
  assay-lth-1x3-4u
::
++  test-lth-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lth-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-lth-2x1-4u  (lth:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-lth-2x1-4u
  assay-lth-2x1-4u
::
++  test-lth-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lth-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-lth-2x2-4u  (lth:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-lth-2x2-4u
  assay-lth-2x2-4u
::
++  test-lth-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-2x3-4u  (lth:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-lth-2x3-4u
  assay-lth-2x3-4u
::
++  test-lth-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lth-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-lth-3x1-4u  (lth:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-lth-3x1-4u
  assay-lth-3x1-4u
::
++  test-lth-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lth-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-lth-3x2-4u  (lth:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-lth-3x2-4u
  assay-lth-3x2-4u
::
++  test-lth-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-3x3-4u  (lth:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-lth-3x3-4u
  assay-lth-3x3-4u
::
++  test-lth-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lth-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-lth-1x1-5u  (lth:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-lth-1x1-5u
  assay-lth-1x1-5u
::
++  test-lth-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lth-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-lth-1x2-5u  (lth:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-lth-1x2-5u
  assay-lth-1x2-5u
::
++  test-lth-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lth-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-lth-1x3-5u  (lth:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-lth-1x3-5u
  assay-lth-1x3-5u
::
++  test-lth-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lth-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-lth-2x1-5u  (lth:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-lth-2x1-5u
  assay-lth-2x1-5u
::
++  test-lth-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lth-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-lth-2x2-5u  (lth:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-lth-2x2-5u
  assay-lth-2x2-5u
::
++  test-lth-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-2x3-5u  (lth:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-lth-2x3-5u
  assay-lth-2x3-5u
::
++  test-lth-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lth-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-lth-3x1-5u  (lth:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-lth-3x1-5u
  assay-lth-3x1-5u
::
++  test-lth-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lth-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-lth-3x2-5u  (lth:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-lth-3x2-5u
  assay-lth-3x2-5u
::
++  test-lth-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-3x3-5u  (lth:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-lth-3x3-5u
  assay-lth-3x3-5u
::
++  test-lth-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-lth-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-lth-1x1-6u  (lth:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-lth-1x1-6u
  assay-lth-1x1-6u
::
++  test-lth-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-lth-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-lth-1x2-6u  (lth:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-lth-1x2-6u
  assay-lth-1x2-6u
::
++  test-lth-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-lth-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-lth-1x3-6u  (lth:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-lth-1x3-6u
  assay-lth-1x3-6u
::
++  test-lth-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-lth-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-lth-2x1-6u  (lth:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-lth-2x1-6u
  assay-lth-2x1-6u
::
++  test-lth-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-lth-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-lth-2x2-6u  (lth:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-lth-2x2-6u
  assay-lth-2x2-6u
::
++  test-lth-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-2x3-6u  (lth:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-lth-2x3-6u
  assay-lth-2x3-6u
::
++  test-lth-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-lth-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-lth-3x1-6u  (lth:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-lth-3x1-6u
  assay-lth-3x1-6u
::
++  test-lth-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-lth-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-lth-3x2-6u  (lth:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-lth-3x2-6u
  assay-lth-3x2-6u
::
++  test-lth-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-lth-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-lth-3x3-6u  (lth:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-lth-3x3-6u
  assay-lth-3x3-6u
::
++  test-gte-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-gte-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  assay-gte-1x1-4r  (gte:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-gte-1x1-4r
  assay-gte-1x1-4r
::
++  test-gte-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-gte-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  assay-gte-1x2-4r  (gte:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-gte-1x2-4r
  assay-gte-1x2-4r
::
++  test-gte-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gte-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-gte-1x3-4r  (gte:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-gte-1x3-4r
  assay-gte-1x3-4r
::
++  test-gte-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-gte-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  assay-gte-2x1-4r  (gte:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-gte-2x1-4r
  assay-gte-2x1-4r
::
++  test-gte-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-gte-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-gte-2x2-4r  (gte:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-gte-2x2-4r
  assay-gte-2x2-4r
::
++  test-gte-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gte-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-gte-2x3-4r  (gte:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-gte-2x3-4r
  assay-gte-2x3-4r
::
++  test-gte-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-gte-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  assay-gte-3x1-4r  (gte:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-gte-3x1-4r
  assay-gte-3x1-4r
::
++  test-gte-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-gte-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-gte-3x2-4r  (gte:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-gte-3x2-4r
  assay-gte-3x2-4r
::
++  test-gte-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gte-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-gte-3x3-4r  (gte:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-gte-3x3-4r
  assay-gte-3x3-4r
::
++  test-gte-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-gte-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  assay-gte-1x1-5r  (gte:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-gte-1x1-5r
  assay-gte-1x1-5r
::
++  test-gte-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-gte-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  assay-gte-1x2-5r  (gte:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-gte-1x2-5r
  assay-gte-1x2-5r
::
++  test-gte-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-gte-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  assay-gte-1x3-5r  (gte:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-gte-1x3-5r
  assay-gte-1x3-5r
::
++  test-gte-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-gte-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  assay-gte-2x1-5r  (gte:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-gte-2x1-5r
  assay-gte-2x1-5r
::
++  test-gte-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-gte-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-gte-2x2-5r  (gte:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-gte-2x2-5r
  assay-gte-2x2-5r
::
++  test-gte-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-gte-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-gte-2x3-5r  (gte:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-gte-2x3-5r
  assay-gte-2x3-5r
::
++  test-gte-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-gte-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  assay-gte-3x1-5r  (gte:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-gte-3x1-5r
  assay-gte-3x1-5r
::
++  test-gte-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-gte-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-gte-3x2-5r  (gte:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-gte-3x2-5r
  assay-gte-3x2-5r
::
++  test-gte-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-gte-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-gte-3x3-5r  (gte:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-gte-3x3-5r
  assay-gte-3x3-5r
::
++  test-gte-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-gte-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  assay-gte-1x1-6r  (gte:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-gte-1x1-6r
  assay-gte-1x1-6r
::
++  test-gte-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-gte-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  assay-gte-1x2-6r  (gte:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-gte-1x2-6r
  assay-gte-1x2-6r
::
++  test-gte-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gte-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-gte-1x3-6r  (gte:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-gte-1x3-6r
  assay-gte-1x3-6r
::
++  test-gte-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-gte-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  assay-gte-2x1-6r  (gte:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-gte-2x1-6r
  assay-gte-2x1-6r
::
++  test-gte-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-gte-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-gte-2x2-6r  (gte:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-gte-2x2-6r
  assay-gte-2x2-6r
::
++  test-gte-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gte-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-gte-2x3-6r  (gte:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-gte-2x3-6r
  assay-gte-2x3-6r
::
++  test-gte-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-gte-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  assay-gte-3x1-6r  (gte:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-gte-3x1-6r
  assay-gte-3x1-6r
::
++  test-gte-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-gte-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-gte-3x2-6r  (gte:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-gte-3x2-6r
  assay-gte-3x2-6r
::
++  test-gte-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gte-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-gte-3x3-6r  (gte:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-gte-3x3-6r
  assay-gte-3x3-6r
::
++  test-gte-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-gte-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  assay-gte-1x1-7r  (gte:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-gte-1x1-7r
  assay-gte-1x1-7r
::
++  test-gte-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-gte-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  assay-gte-1x2-7r  (gte:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-gte-1x2-7r
  assay-gte-1x2-7r
::
++  test-gte-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gte-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-gte-1x3-7r  (gte:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-gte-1x3-7r
  assay-gte-1x3-7r
::
++  test-gte-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-gte-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-gte-2x1-7r  (gte:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-gte-2x1-7r
  assay-gte-2x1-7r
::
++  test-gte-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-gte-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-gte-2x2-7r  (gte:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-gte-2x2-7r
  assay-gte-2x2-7r
::
++  test-gte-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gte-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-gte-2x3-7r  (gte:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-gte-2x3-7r
  assay-gte-2x3-7r
::
++  test-gte-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-gte-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-gte-3x1-7r  (gte:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-gte-3x1-7r
  assay-gte-3x1-7r
::
++  test-gte-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-gte-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-gte-3x2-7r  (gte:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-gte-3x2-7r
  assay-gte-3x2-7r
::
++  test-gte-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gte-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-gte-3x3-7r  (gte:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-gte-3x3-7r
  assay-gte-3x3-7r
::
++  test-gte-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gte-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-gte-1x1-3u  (gte:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-gte-1x1-3u
  assay-gte-1x1-3u
::
++  test-gte-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gte-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-gte-1x2-3u  (gte:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-gte-1x2-3u
  assay-gte-1x2-3u
::
++  test-gte-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gte-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-gte-1x3-3u  (gte:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-gte-1x3-3u
  assay-gte-1x3-3u
::
++  test-gte-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gte-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-gte-2x1-3u  (gte:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-gte-2x1-3u
  assay-gte-2x1-3u
::
++  test-gte-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gte-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-gte-2x2-3u  (gte:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-gte-2x2-3u
  assay-gte-2x2-3u
::
++  test-gte-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-2x3-3u  (gte:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-gte-2x3-3u
  assay-gte-2x3-3u
::
++  test-gte-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gte-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-gte-3x1-3u  (gte:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-gte-3x1-3u
  assay-gte-3x1-3u
::
++  test-gte-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gte-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-gte-3x2-3u  (gte:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-gte-3x2-3u
  assay-gte-3x2-3u
::
++  test-gte-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-3x3-3u  (gte:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-gte-3x3-3u
  assay-gte-3x3-3u
::
++  test-gte-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gte-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-gte-1x1-4u  (gte:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-gte-1x1-4u
  assay-gte-1x1-4u
::
++  test-gte-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gte-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-gte-1x2-4u  (gte:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-gte-1x2-4u
  assay-gte-1x2-4u
::
++  test-gte-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gte-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-gte-1x3-4u  (gte:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-gte-1x3-4u
  assay-gte-1x3-4u
::
++  test-gte-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gte-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-gte-2x1-4u  (gte:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-gte-2x1-4u
  assay-gte-2x1-4u
::
++  test-gte-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gte-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-gte-2x2-4u  (gte:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-gte-2x2-4u
  assay-gte-2x2-4u
::
++  test-gte-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-2x3-4u  (gte:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-gte-2x3-4u
  assay-gte-2x3-4u
::
++  test-gte-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gte-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-gte-3x1-4u  (gte:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-gte-3x1-4u
  assay-gte-3x1-4u
::
++  test-gte-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gte-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-gte-3x2-4u  (gte:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-gte-3x2-4u
  assay-gte-3x2-4u
::
++  test-gte-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-3x3-4u  (gte:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-gte-3x3-4u
  assay-gte-3x3-4u
::
++  test-gte-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gte-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-gte-1x1-5u  (gte:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-gte-1x1-5u
  assay-gte-1x1-5u
::
++  test-gte-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gte-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-gte-1x2-5u  (gte:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-gte-1x2-5u
  assay-gte-1x2-5u
::
++  test-gte-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gte-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-gte-1x3-5u  (gte:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-gte-1x3-5u
  assay-gte-1x3-5u
::
++  test-gte-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gte-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-gte-2x1-5u  (gte:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-gte-2x1-5u
  assay-gte-2x1-5u
::
++  test-gte-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gte-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-gte-2x2-5u  (gte:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-gte-2x2-5u
  assay-gte-2x2-5u
::
++  test-gte-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-2x3-5u  (gte:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-gte-2x3-5u
  assay-gte-2x3-5u
::
++  test-gte-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gte-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-gte-3x1-5u  (gte:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-gte-3x1-5u
  assay-gte-3x1-5u
::
++  test-gte-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gte-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-gte-3x2-5u  (gte:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-gte-3x2-5u
  assay-gte-3x2-5u
::
++  test-gte-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-3x3-5u  (gte:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-gte-3x3-5u
  assay-gte-3x3-5u
::
++  test-gte-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gte-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  assay-gte-1x1-6u  (gte:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-gte-1x1-6u
  assay-gte-1x1-6u
::
++  test-gte-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gte-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  assay-gte-1x2-6u  (gte:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-gte-1x2-6u
  assay-gte-1x2-6u
::
++  test-gte-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gte-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  assay-gte-1x3-6u  (gte:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-gte-1x3-6u
  assay-gte-1x3-6u
::
++  test-gte-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gte-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  assay-gte-2x1-6u  (gte:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-gte-2x1-6u
  assay-gte-2x1-6u
::
++  test-gte-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gte-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-gte-2x2-6u  (gte:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-gte-2x2-6u
  assay-gte-2x2-6u
::
++  test-gte-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-2x3-6u  (gte:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-gte-2x3-6u
  assay-gte-2x3-6u
::
++  test-gte-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gte-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-gte-3x1-6u  (gte:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-gte-3x1-6u
  assay-gte-3x1-6u
::
++  test-gte-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gte-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-gte-3x2-6u  (gte:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-gte-3x2-6u
  assay-gte-3x2-6u
::
++  test-gte-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gte-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-gte-3x3-6u  (gte:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-gte-3x3-6u
  assay-gte-3x3-6u
::
++  test-gth-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  jnput-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0]]])
  =/  canon-gth-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0]]])
  =/  assay-gth-1x1-4r  (gth:la input-ones-1x1-4r jnput-ones-1x1-4r)
  %+  is-equal
    canon-gth-1x1-4r
  assay-gth-1x1-4r
::
++  test-gth-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-gth-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0]]])
  =/  assay-gth-1x2-4r  (gth:la input-ones-1x2-4r jnput-ones-1x2-4r)
  %+  is-equal
    canon-gth-1x2-4r
  assay-gth-1x2-4r
::
++  test-gth-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gth-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-gth-1x3-4r  (gth:la input-ones-1x3-4r jnput-ones-1x3-4r)
  %+  is-equal
    canon-gth-1x3-4r
  assay-gth-1x3-4r
::
++  test-gth-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-gth-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0]]])
  =/  assay-gth-2x1-4r  (gth:la input-ones-2x1-4r jnput-ones-2x1-4r)
  %+  is-equal
    canon-gth-2x1-4r
  assay-gth-2x1-4r
::
++  test-gth-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-gth-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-gth-2x2-4r  (gth:la input-ones-2x2-4r jnput-ones-2x2-4r)
  %+  is-equal
    canon-gth-2x2-4r
  assay-gth-2x2-4r
::
++  test-gth-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gth-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-gth-2x3-4r  (gth:la input-ones-2x3-4r jnput-ones-2x3-4r)
  %+  is-equal
    canon-gth-2x3-4r
  assay-gth-2x3-4r
::
++  test-gth-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  jnput-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-gth-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0] ~[.~~0.0] ~[.~~0.0]]])
  =/  assay-gth-3x1-4r  (gth:la input-ones-3x1-4r jnput-ones-3x1-4r)
  %+  is-equal
    canon-gth-3x1-4r
  assay-gth-3x1-4r
::
++  test-gth-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  jnput-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-gth-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-gth-3x2-4r  (gth:la input-ones-3x2-4r jnput-ones-3x2-4r)
  %+  is-equal
    canon-gth-3x2-4r
  assay-gth-3x2-4r
::
++  test-gth-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  jnput-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-gth-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 fxp=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-gth-3x3-4r  (gth:la input-ones-3x3-4r jnput-ones-3x3-4r)
  %+  is-equal
    canon-gth-3x3-4r
  assay-gth-3x3-4r
::
++  test-gth-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  jnput-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0]]])
  =/  canon-gth-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0]]])
  =/  assay-gth-1x1-5r  (gth:la input-ones-1x1-5r jnput-ones-1x1-5r)
  %+  is-equal
    canon-gth-1x1-5r
  assay-gth-1x1-5r
::
++  test-gth-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  jnput-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-gth-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0]]])
  =/  assay-gth-1x2-5r  (gth:la input-ones-1x2-5r jnput-ones-1x2-5r)
  %+  is-equal
    canon-gth-1x2-5r
  assay-gth-1x2-5r
::
++  test-gth-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-gth-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0]]])
  =/  assay-gth-1x3-5r  (gth:la input-ones-1x3-5r jnput-ones-1x3-5r)
  %+  is-equal
    canon-gth-1x3-5r
  assay-gth-1x3-5r
::
++  test-gth-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  jnput-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-gth-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0]]])
  =/  assay-gth-2x1-5r  (gth:la input-ones-2x1-5r jnput-ones-2x1-5r)
  %+  is-equal
    canon-gth-2x1-5r
  assay-gth-2x1-5r
::
++  test-gth-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-gth-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-gth-2x2-5r  (gth:la input-ones-2x2-5r jnput-ones-2x2-5r)
  %+  is-equal
    canon-gth-2x2-5r
  assay-gth-2x2-5r
::
++  test-gth-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-gth-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-gth-2x3-5r  (gth:la input-ones-2x3-5r jnput-ones-2x3-5r)
  %+  is-equal
    canon-gth-2x3-5r
  assay-gth-2x3-5r
::
++  test-gth-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  jnput-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-gth-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0] ~[.0.0] ~[.0.0]]])
  =/  assay-gth-3x1-5r  (gth:la input-ones-3x1-5r jnput-ones-3x1-5r)
  %+  is-equal
    canon-gth-3x1-5r
  assay-gth-3x1-5r
::
++  test-gth-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  jnput-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-gth-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-gth-3x2-5r  (gth:la input-ones-3x2-5r jnput-ones-3x2-5r)
  %+  is-equal
    canon-gth-3x2-5r
  assay-gth-3x2-5r
::
++  test-gth-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  jnput-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-gth-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 fxp=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-gth-3x3-5r  (gth:la input-ones-3x3-5r jnput-ones-3x3-5r)
  %+  is-equal
    canon-gth-3x3-5r
  assay-gth-3x3-5r
::
++  test-gth-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  jnput-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0]]])
  =/  canon-gth-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0]]])
  =/  assay-gth-1x1-6r  (gth:la input-ones-1x1-6r jnput-ones-1x1-6r)
  %+  is-equal
    canon-gth-1x1-6r
  assay-gth-1x1-6r
::
++  test-gth-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  jnput-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-gth-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0]]])
  =/  assay-gth-1x2-6r  (gth:la input-ones-1x2-6r jnput-ones-1x2-6r)
  %+  is-equal
    canon-gth-1x2-6r
  assay-gth-1x2-6r
::
++  test-gth-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gth-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-gth-1x3-6r  (gth:la input-ones-1x3-6r jnput-ones-1x3-6r)
  %+  is-equal
    canon-gth-1x3-6r
  assay-gth-1x3-6r
::
++  test-gth-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-gth-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0]]])
  =/  assay-gth-2x1-6r  (gth:la input-ones-2x1-6r jnput-ones-2x1-6r)
  %+  is-equal
    canon-gth-2x1-6r
  assay-gth-2x1-6r
::
++  test-gth-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-gth-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-gth-2x2-6r  (gth:la input-ones-2x2-6r jnput-ones-2x2-6r)
  %+  is-equal
    canon-gth-2x2-6r
  assay-gth-2x2-6r
::
++  test-gth-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gth-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-gth-2x3-6r  (gth:la input-ones-2x3-6r jnput-ones-2x3-6r)
  %+  is-equal
    canon-gth-2x3-6r
  assay-gth-2x3-6r
::
++  test-gth-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  jnput-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-gth-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0] ~[.~0.0] ~[.~0.0]]])
  =/  assay-gth-3x1-6r  (gth:la input-ones-3x1-6r jnput-ones-3x1-6r)
  %+  is-equal
    canon-gth-3x1-6r
  assay-gth-3x1-6r
::
++  test-gth-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  jnput-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-gth-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-gth-3x2-6r  (gth:la input-ones-3x2-6r jnput-ones-3x2-6r)
  %+  is-equal
    canon-gth-3x2-6r
  assay-gth-3x2-6r
::
++  test-gth-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  jnput-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-gth-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 fxp=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-gth-3x3-6r  (gth:la input-ones-3x3-6r jnput-ones-3x3-6r)
  %+  is-equal
    canon-gth-3x3-6r
  assay-gth-3x3-6r
::
++  test-gth-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  jnput-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0]]])
  =/  canon-gth-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0]]])
  =/  assay-gth-1x1-7r  (gth:la input-ones-1x1-7r jnput-ones-1x1-7r)
  %+  is-equal
    canon-gth-1x1-7r
  assay-gth-1x1-7r
::
++  test-gth-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-gth-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0]]])
  =/  assay-gth-1x2-7r  (gth:la input-ones-1x2-7r jnput-ones-1x2-7r)
  %+  is-equal
    canon-gth-1x2-7r
  assay-gth-1x2-7r
::
++  test-gth-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gth-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-gth-1x3-7r  (gth:la input-ones-1x3-7r jnput-ones-1x3-7r)
  %+  is-equal
    canon-gth-1x3-7r
  assay-gth-1x3-7r
::
++  test-gth-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-gth-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-gth-2x1-7r  (gth:la input-ones-2x1-7r jnput-ones-2x1-7r)
  %+  is-equal
    canon-gth-2x1-7r
  assay-gth-2x1-7r
::
++  test-gth-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-gth-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-gth-2x2-7r  (gth:la input-ones-2x2-7r jnput-ones-2x2-7r)
  %+  is-equal
    canon-gth-2x2-7r
  assay-gth-2x2-7r
::
++  test-gth-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gth-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-gth-2x3-7r  (gth:la input-ones-2x3-7r jnput-ones-2x3-7r)
  %+  is-equal
    canon-gth-2x3-7r
  assay-gth-2x3-7r
::
++  test-gth-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  jnput-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-gth-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0] ~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-gth-3x1-7r  (gth:la input-ones-3x1-7r jnput-ones-3x1-7r)
  %+  is-equal
    canon-gth-3x1-7r
  assay-gth-3x1-7r
::
++  test-gth-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-gth-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-gth-3x2-7r  (gth:la input-ones-3x2-7r jnput-ones-3x2-7r)
  %+  is-equal
    canon-gth-3x2-7r
  assay-gth-3x2-7r
::
++  test-gth-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  jnput-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-gth-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 fxp=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-gth-3x3-7r  (gth:la input-ones-3x3-7r jnput-ones-3x3-7r)
  %+  is-equal
    canon-gth-3x3-7r
  assay-gth-3x3-7r
::
++  test-gth-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gth-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-gth-1x1-3u  (gth:la input-ones-1x1-3u jnput-ones-1x1-3u)
  %+  is-equal
    canon-gth-1x1-3u
  assay-gth-1x1-3u
::
++  test-gth-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gth-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-gth-1x2-3u  (gth:la input-ones-1x2-3u jnput-ones-1x2-3u)
  %+  is-equal
    canon-gth-1x2-3u
  assay-gth-1x2-3u
::
++  test-gth-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gth-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-gth-1x3-3u  (gth:la input-ones-1x3-3u jnput-ones-1x3-3u)
  %+  is-equal
    canon-gth-1x3-3u
  assay-gth-1x3-3u
::
++  test-gth-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gth-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-gth-2x1-3u  (gth:la input-ones-2x1-3u jnput-ones-2x1-3u)
  %+  is-equal
    canon-gth-2x1-3u
  assay-gth-2x1-3u
::
++  test-gth-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gth-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-gth-2x2-3u  (gth:la input-ones-2x2-3u jnput-ones-2x2-3u)
  %+  is-equal
    canon-gth-2x2-3u
  assay-gth-2x2-3u
::
++  test-gth-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-2x3-3u  (gth:la input-ones-2x3-3u jnput-ones-2x3-3u)
  %+  is-equal
    canon-gth-2x3-3u
  assay-gth-2x3-3u
::
++  test-gth-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gth-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-gth-3x1-3u  (gth:la input-ones-3x1-3u jnput-ones-3x1-3u)
  %+  is-equal
    canon-gth-3x1-3u
  assay-gth-3x1-3u
::
++  test-gth-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gth-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-gth-3x2-3u  (gth:la input-ones-3x2-3u jnput-ones-3x2-3u)
  %+  is-equal
    canon-gth-3x2-3u
  assay-gth-3x2-3u
::
++  test-gth-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-3x3-3u  (gth:la input-ones-3x3-3u jnput-ones-3x3-3u)
  %+  is-equal
    canon-gth-3x3-3u
  assay-gth-3x3-3u
::
++  test-gth-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gth-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-gth-1x1-4u  (gth:la input-ones-1x1-4u jnput-ones-1x1-4u)
  %+  is-equal
    canon-gth-1x1-4u
  assay-gth-1x1-4u
::
++  test-gth-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gth-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-gth-1x2-4u  (gth:la input-ones-1x2-4u jnput-ones-1x2-4u)
  %+  is-equal
    canon-gth-1x2-4u
  assay-gth-1x2-4u
::
++  test-gth-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gth-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-gth-1x3-4u  (gth:la input-ones-1x3-4u jnput-ones-1x3-4u)
  %+  is-equal
    canon-gth-1x3-4u
  assay-gth-1x3-4u
::
++  test-gth-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gth-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-gth-2x1-4u  (gth:la input-ones-2x1-4u jnput-ones-2x1-4u)
  %+  is-equal
    canon-gth-2x1-4u
  assay-gth-2x1-4u
::
++  test-gth-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gth-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-gth-2x2-4u  (gth:la input-ones-2x2-4u jnput-ones-2x2-4u)
  %+  is-equal
    canon-gth-2x2-4u
  assay-gth-2x2-4u
::
++  test-gth-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-2x3-4u  (gth:la input-ones-2x3-4u jnput-ones-2x3-4u)
  %+  is-equal
    canon-gth-2x3-4u
  assay-gth-2x3-4u
::
++  test-gth-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gth-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-gth-3x1-4u  (gth:la input-ones-3x1-4u jnput-ones-3x1-4u)
  %+  is-equal
    canon-gth-3x1-4u
  assay-gth-3x1-4u
::
++  test-gth-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gth-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-gth-3x2-4u  (gth:la input-ones-3x2-4u jnput-ones-3x2-4u)
  %+  is-equal
    canon-gth-3x2-4u
  assay-gth-3x2-4u
::
++  test-gth-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-3x3-4u  (gth:la input-ones-3x3-4u jnput-ones-3x3-4u)
  %+  is-equal
    canon-gth-3x3-4u
  assay-gth-3x3-4u
::
++  test-gth-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gth-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-gth-1x1-5u  (gth:la input-ones-1x1-5u jnput-ones-1x1-5u)
  %+  is-equal
    canon-gth-1x1-5u
  assay-gth-1x1-5u
::
++  test-gth-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gth-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-gth-1x2-5u  (gth:la input-ones-1x2-5u jnput-ones-1x2-5u)
  %+  is-equal
    canon-gth-1x2-5u
  assay-gth-1x2-5u
::
++  test-gth-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gth-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-gth-1x3-5u  (gth:la input-ones-1x3-5u jnput-ones-1x3-5u)
  %+  is-equal
    canon-gth-1x3-5u
  assay-gth-1x3-5u
::
++  test-gth-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gth-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-gth-2x1-5u  (gth:la input-ones-2x1-5u jnput-ones-2x1-5u)
  %+  is-equal
    canon-gth-2x1-5u
  assay-gth-2x1-5u
::
++  test-gth-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gth-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-gth-2x2-5u  (gth:la input-ones-2x2-5u jnput-ones-2x2-5u)
  %+  is-equal
    canon-gth-2x2-5u
  assay-gth-2x2-5u
::
++  test-gth-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-2x3-5u  (gth:la input-ones-2x3-5u jnput-ones-2x3-5u)
  %+  is-equal
    canon-gth-2x3-5u
  assay-gth-2x3-5u
::
++  test-gth-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gth-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-gth-3x1-5u  (gth:la input-ones-3x1-5u jnput-ones-3x1-5u)
  %+  is-equal
    canon-gth-3x1-5u
  assay-gth-3x1-5u
::
++  test-gth-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gth-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-gth-3x2-5u  (gth:la input-ones-3x2-5u jnput-ones-3x2-5u)
  %+  is-equal
    canon-gth-3x2-5u
  assay-gth-3x2-5u
::
++  test-gth-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-3x3-5u  (gth:la input-ones-3x3-5u jnput-ones-3x3-5u)
  %+  is-equal
    canon-gth-3x3-5u
  assay-gth-3x3-5u
::
++  test-gth-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  jnput-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[1]]])
  =/  canon-gth-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint fxp=~] baum=~[~[0]]])
  =/  assay-gth-1x1-6u  (gth:la input-ones-1x1-6u jnput-ones-1x1-6u)
  %+  is-equal
    canon-gth-1x1-6u
  assay-gth-1x1-6u
::
++  test-gth-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  jnput-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1]]])
  =/  canon-gth-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0]]])
  =/  assay-gth-1x2-6u  (gth:la input-ones-1x2-6u jnput-ones-1x2-6u)
  %+  is-equal
    canon-gth-1x2-6u
  assay-gth-1x2-6u
::
++  test-gth-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  jnput-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1]]])
  =/  canon-gth-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0]]])
  =/  assay-gth-1x3-6u  (gth:la input-ones-1x3-6u jnput-ones-1x3-6u)
  %+  is-equal
    canon-gth-1x3-6u
  assay-gth-1x3-6u
::
++  test-gth-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  jnput-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1]]])
  =/  canon-gth-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0]]])
  =/  assay-gth-2x1-6u  (gth:la input-ones-2x1-6u jnput-ones-2x1-6u)
  %+  is-equal
    canon-gth-2x1-6u
  assay-gth-2x1-6u
::
++  test-gth-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  jnput-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-gth-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-gth-2x2-6u  (gth:la input-ones-2x2-6u jnput-ones-2x2-6u)
  %+  is-equal
    canon-gth-2x2-6u
  assay-gth-2x2-6u
::
++  test-gth-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-2x3-6u  (gth:la input-ones-2x3-6u jnput-ones-2x3-6u)
  %+  is-equal
    canon-gth-2x3-6u
  assay-gth-2x3-6u
::
++  test-gth-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  jnput-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-gth-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint fxp=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-gth-3x1-6u  (gth:la input-ones-3x1-6u jnput-ones-3x1-6u)
  %+  is-equal
    canon-gth-3x1-6u
  assay-gth-3x1-6u
::
++  test-gth-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  jnput-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-gth-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint fxp=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-gth-3x2-6u  (gth:la input-ones-3x2-6u jnput-ones-3x2-6u)
  %+  is-equal
    canon-gth-3x2-6u
  assay-gth-3x2-6u
::
++  test-gth-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  jnput-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-gth-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint fxp=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-gth-3x3-6u  (gth:la input-ones-3x3-6u jnput-ones-3x3-6u)
  %+  is-equal
    canon-gth-3x3-6u
  assay-gth-3x3-6u
--
