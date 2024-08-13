/-  *lagoon
/+  *test
/+  *lagoon
  ::
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
::
^|
|_  $:  atol=_.1e-3          :: absolute tolerance for precision of operations
        rtol=_.1e-5          :: relative tolerance for precision of operations
    ==
::  Auxiliary tools
++  is-equal
  |=  [a=ray b=ray]  ^-  tang
  ?:  =(a b)  ~
  :~  [%palm [": " ~ ~ ~] [leaf+"expected" "{<a>}"]]
      [%palm [": " ~ ~ ~] [leaf+"actual  " "{<b>}"]]
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

++  test-eye-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  canon-eye-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  assay-eye-1x1-4r  (eye:la meta.input-ones-1x1-4r)
  %+  is-equal
    canon-eye-1x1-4r
  assay-eye-1x1-4r

++  test-eye-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-eye-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~0.0] ~[.~~0.0 .~~1.0]]])
  =/  assay-eye-2x2-4r  (eye:la meta.input-ones-2x2-4r)
  %+  is-equal
    canon-eye-2x2-4r
  assay-eye-2x2-4r

++  test-eye-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-eye-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~1.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~1.0]]])
  =/  assay-eye-3x3-4r  (eye:la meta.input-ones-3x3-4r)
  %+  is-equal
    canon-eye-3x3-4r
  assay-eye-3x3-4r

++  test-eye-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  canon-eye-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  assay-eye-1x1-5r  (eye:la meta.input-ones-1x1-5r)
  %+  is-equal
    canon-eye-1x1-5r
  assay-eye-1x1-5r

++  test-eye-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-eye-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .0.0] ~[.0.0 .1.0]]])
  =/  assay-eye-2x2-5r  (eye:la meta.input-ones-2x2-5r)
  %+  is-equal
    canon-eye-2x2-5r
  assay-eye-2x2-5r

++  test-eye-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-eye-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .0.0 .0.0] ~[.0.0 .1.0 .0.0] ~[.0.0 .0.0 .1.0]]])
  =/  assay-eye-3x3-5r  (eye:la meta.input-ones-3x3-5r)
  %+  is-equal
    canon-eye-3x3-5r
  assay-eye-3x3-5r

++  test-eye-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  canon-eye-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  assay-eye-1x1-6r  (eye:la meta.input-ones-1x1-6r)
  %+  is-equal
    canon-eye-1x1-6r
  assay-eye-1x1-6r

++  test-eye-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-eye-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~0.0] ~[.~0.0 .~1.0]]])
  =/  assay-eye-2x2-6r  (eye:la meta.input-ones-2x2-6r)
  %+  is-equal
    canon-eye-2x2-6r
  assay-eye-2x2-6r

++  test-eye-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-eye-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~0.0 .~0.0] ~[.~0.0 .~1.0 .~0.0] ~[.~0.0 .~0.0 .~1.0]]])
  =/  assay-eye-3x3-6r  (eye:la meta.input-ones-3x3-6r)
  %+  is-equal
    canon-eye-3x3-6r
  assay-eye-3x3-6r

++  test-eye-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  canon-eye-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  assay-eye-1x1-7r  (eye:la meta.input-ones-1x1-7r)
  %+  is-equal
    canon-eye-1x1-7r
  assay-eye-1x1-7r

++  test-eye-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-eye-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~0.0] ~[.~~~0.0 .~~~1.0]]])
  =/  assay-eye-2x2-7r  (eye:la meta.input-ones-2x2-7r)
  %+  is-equal
    canon-eye-2x2-7r
  assay-eye-2x2-7r

++  test-eye-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-eye-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~1.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~1.0]]])
  =/  assay-eye-3x3-7r  (eye:la meta.input-ones-3x3-7r)
  %+  is-equal
    canon-eye-3x3-7r
  assay-eye-3x3-7r

++  test-eye-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-eye-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-eye-1x1-3u  (eye:la meta.input-ones-1x1-3u)
  %+  is-equal
    canon-eye-1x1-3u
  assay-eye-1x1-3u

++  test-eye-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-eye-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[1 0] ~[0 1]]])
  =/  assay-eye-2x2-3u  (eye:la meta.input-ones-2x2-3u)
  %+  is-equal
    canon-eye-2x2-3u
  assay-eye-2x2-3u

++  test-eye-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-eye-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[1 0 0] ~[0 1 0] ~[0 0 1]]])
  =/  assay-eye-3x3-3u  (eye:la meta.input-ones-3x3-3u)
  %+  is-equal
    canon-eye-3x3-3u
  assay-eye-3x3-3u

++  test-eye-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-eye-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-eye-1x1-4u  (eye:la meta.input-ones-1x1-4u)
  %+  is-equal
    canon-eye-1x1-4u
  assay-eye-1x1-4u

++  test-eye-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-eye-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[1 0] ~[0 1]]])
  =/  assay-eye-2x2-4u  (eye:la meta.input-ones-2x2-4u)
  %+  is-equal
    canon-eye-2x2-4u
  assay-eye-2x2-4u

++  test-eye-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-eye-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[1 0 0] ~[0 1 0] ~[0 0 1]]])
  =/  assay-eye-3x3-4u  (eye:la meta.input-ones-3x3-4u)
  %+  is-equal
    canon-eye-3x3-4u
  assay-eye-3x3-4u

++  test-eye-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-eye-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-eye-1x1-5u  (eye:la meta.input-ones-1x1-5u)
  %+  is-equal
    canon-eye-1x1-5u
  assay-eye-1x1-5u

++  test-eye-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-eye-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[1 0] ~[0 1]]])
  =/  assay-eye-2x2-5u  (eye:la meta.input-ones-2x2-5u)
  %+  is-equal
    canon-eye-2x2-5u
  assay-eye-2x2-5u

++  test-eye-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-eye-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[1 0 0] ~[0 1 0] ~[0 0 1]]])
  =/  assay-eye-3x3-5u  (eye:la meta.input-ones-3x3-5u)
  %+  is-equal
    canon-eye-3x3-5u
  assay-eye-3x3-5u

++  test-eye-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-eye-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-eye-1x1-6u  (eye:la meta.input-ones-1x1-6u)
  %+  is-equal
    canon-eye-1x1-6u
  assay-eye-1x1-6u

++  test-eye-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-eye-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[1 0] ~[0 1]]])
  =/  assay-eye-2x2-6u  (eye:la meta.input-ones-2x2-6u)
  %+  is-equal
    canon-eye-2x2-6u
  assay-eye-2x2-6u

++  test-eye-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-eye-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[1 0 0] ~[0 1 0] ~[0 0 1]]])
  =/  assay-eye-3x3-6u  (eye:la meta.input-ones-3x3-6u)
  %+  is-equal
    canon-eye-3x3-6u
  assay-eye-3x3-6u

++  test-zeros-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  canon-zeros-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0]]])
  =/  assay-zeros-1x1-4r  (zeros:la meta.input-ones-1x1-4r)
  %+  is-equal
    canon-zeros-1x1-4r
  assay-zeros-1x1-4r

++  test-zeros-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-zeros-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0]]])
  =/  assay-zeros-1x2-4r  (zeros:la meta.input-ones-1x2-4r)
  %+  is-equal
    canon-zeros-1x2-4r
  assay-zeros-1x2-4r

++  test-zeros-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-zeros-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-zeros-1x3-4r  (zeros:la meta.input-ones-1x3-4r)
  %+  is-equal
    canon-zeros-1x3-4r
  assay-zeros-1x3-4r

++  test-zeros-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-zeros-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0] ~[.~~0.0]]])
  =/  assay-zeros-2x1-4r  (zeros:la meta.input-ones-2x1-4r)
  %+  is-equal
    canon-zeros-2x1-4r
  assay-zeros-2x1-4r

++  test-zeros-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-zeros-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-zeros-2x2-4r  (zeros:la meta.input-ones-2x2-4r)
  %+  is-equal
    canon-zeros-2x2-4r
  assay-zeros-2x2-4r

++  test-zeros-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-zeros-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-zeros-2x3-4r  (zeros:la meta.input-ones-2x3-4r)
  %+  is-equal
    canon-zeros-2x3-4r
  assay-zeros-2x3-4r

++  test-zeros-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-zeros-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0] ~[.~~0.0] ~[.~~0.0]]])
  =/  assay-zeros-3x1-4r  (zeros:la meta.input-ones-3x1-4r)
  %+  is-equal
    canon-zeros-3x1-4r
  assay-zeros-3x1-4r

++  test-zeros-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-zeros-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0] ~[.~~0.0 .~~0.0]]])
  =/  assay-zeros-3x2-4r  (zeros:la meta.input-ones-3x2-4r)
  %+  is-equal
    canon-zeros-3x2-4r
  assay-zeros-3x2-4r

++  test-zeros-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-zeros-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0] ~[.~~0.0 .~~0.0 .~~0.0]]])
  =/  assay-zeros-3x3-4r  (zeros:la meta.input-ones-3x3-4r)
  %+  is-equal
    canon-zeros-3x3-4r
  assay-zeros-3x3-4r

++  test-zeros-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  canon-zeros-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0]]])
  =/  assay-zeros-1x1-5r  (zeros:la meta.input-ones-1x1-5r)
  %+  is-equal
    canon-zeros-1x1-5r
  assay-zeros-1x1-5r

++  test-zeros-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-zeros-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0]]])
  =/  assay-zeros-1x2-5r  (zeros:la meta.input-ones-1x2-5r)
  %+  is-equal
    canon-zeros-1x2-5r
  assay-zeros-1x2-5r

++  test-zeros-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-zeros-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0 .0.0]]])
  =/  assay-zeros-1x3-5r  (zeros:la meta.input-ones-1x3-5r)
  %+  is-equal
    canon-zeros-1x3-5r
  assay-zeros-1x3-5r

++  test-zeros-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-zeros-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0] ~[.0.0]]])
  =/  assay-zeros-2x1-5r  (zeros:la meta.input-ones-2x1-5r)
  %+  is-equal
    canon-zeros-2x1-5r
  assay-zeros-2x1-5r

++  test-zeros-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-zeros-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-zeros-2x2-5r  (zeros:la meta.input-ones-2x2-5r)
  %+  is-equal
    canon-zeros-2x2-5r
  assay-zeros-2x2-5r

++  test-zeros-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-zeros-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-zeros-2x3-5r  (zeros:la meta.input-ones-2x3-5r)
  %+  is-equal
    canon-zeros-2x3-5r
  assay-zeros-2x3-5r

++  test-zeros-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-zeros-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0] ~[.0.0] ~[.0.0]]])
  =/  assay-zeros-3x1-5r  (zeros:la meta.input-ones-3x1-5r)
  %+  is-equal
    canon-zeros-3x1-5r
  assay-zeros-3x1-5r

++  test-zeros-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-zeros-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0] ~[.0.0 .0.0] ~[.0.0 .0.0]]])
  =/  assay-zeros-3x2-5r  (zeros:la meta.input-ones-3x2-5r)
  %+  is-equal
    canon-zeros-3x2-5r
  assay-zeros-3x2-5r

++  test-zeros-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-zeros-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0] ~[.0.0 .0.0 .0.0]]])
  =/  assay-zeros-3x3-5r  (zeros:la meta.input-ones-3x3-5r)
  %+  is-equal
    canon-zeros-3x3-5r
  assay-zeros-3x3-5r

++  test-zeros-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  canon-zeros-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0]]])
  =/  assay-zeros-1x1-6r  (zeros:la meta.input-ones-1x1-6r)
  %+  is-equal
    canon-zeros-1x1-6r
  assay-zeros-1x1-6r

++  test-zeros-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-zeros-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0]]])
  =/  assay-zeros-1x2-6r  (zeros:la meta.input-ones-1x2-6r)
  %+  is-equal
    canon-zeros-1x2-6r
  assay-zeros-1x2-6r

++  test-zeros-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-zeros-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-zeros-1x3-6r  (zeros:la meta.input-ones-1x3-6r)
  %+  is-equal
    canon-zeros-1x3-6r
  assay-zeros-1x3-6r

++  test-zeros-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-zeros-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0] ~[.~0.0]]])
  =/  assay-zeros-2x1-6r  (zeros:la meta.input-ones-2x1-6r)
  %+  is-equal
    canon-zeros-2x1-6r
  assay-zeros-2x1-6r

++  test-zeros-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-zeros-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-zeros-2x2-6r  (zeros:la meta.input-ones-2x2-6r)
  %+  is-equal
    canon-zeros-2x2-6r
  assay-zeros-2x2-6r

++  test-zeros-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-zeros-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-zeros-2x3-6r  (zeros:la meta.input-ones-2x3-6r)
  %+  is-equal
    canon-zeros-2x3-6r
  assay-zeros-2x3-6r

++  test-zeros-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-zeros-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0] ~[.~0.0] ~[.~0.0]]])
  =/  assay-zeros-3x1-6r  (zeros:la meta.input-ones-3x1-6r)
  %+  is-equal
    canon-zeros-3x1-6r
  assay-zeros-3x1-6r

++  test-zeros-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-zeros-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0] ~[.~0.0 .~0.0] ~[.~0.0 .~0.0]]])
  =/  assay-zeros-3x2-6r  (zeros:la meta.input-ones-3x2-6r)
  %+  is-equal
    canon-zeros-3x2-6r
  assay-zeros-3x2-6r

++  test-zeros-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-zeros-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0] ~[.~0.0 .~0.0 .~0.0]]])
  =/  assay-zeros-3x3-6r  (zeros:la meta.input-ones-3x3-6r)
  %+  is-equal
    canon-zeros-3x3-6r
  assay-zeros-3x3-6r

++  test-zeros-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  canon-zeros-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0]]])
  =/  assay-zeros-1x1-7r  (zeros:la meta.input-ones-1x1-7r)
  %+  is-equal
    canon-zeros-1x1-7r
  assay-zeros-1x1-7r

++  test-zeros-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-zeros-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0]]])
  =/  assay-zeros-1x2-7r  (zeros:la meta.input-ones-1x2-7r)
  %+  is-equal
    canon-zeros-1x2-7r
  assay-zeros-1x2-7r

++  test-zeros-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-zeros-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-zeros-1x3-7r  (zeros:la meta.input-ones-1x3-7r)
  %+  is-equal
    canon-zeros-1x3-7r
  assay-zeros-1x3-7r

++  test-zeros-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-zeros-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-zeros-2x1-7r  (zeros:la meta.input-ones-2x1-7r)
  %+  is-equal
    canon-zeros-2x1-7r
  assay-zeros-2x1-7r

++  test-zeros-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-zeros-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-zeros-2x2-7r  (zeros:la meta.input-ones-2x2-7r)
  %+  is-equal
    canon-zeros-2x2-7r
  assay-zeros-2x2-7r

++  test-zeros-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-zeros-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-zeros-2x3-7r  (zeros:la meta.input-ones-2x3-7r)
  %+  is-equal
    canon-zeros-2x3-7r
  assay-zeros-2x3-7r

++  test-zeros-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-zeros-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0] ~[.~~~0.0] ~[.~~~0.0]]])
  =/  assay-zeros-3x1-7r  (zeros:la meta.input-ones-3x1-7r)
  %+  is-equal
    canon-zeros-3x1-7r
  assay-zeros-3x1-7r

++  test-zeros-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-zeros-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0]]])
  =/  assay-zeros-3x2-7r  (zeros:la meta.input-ones-3x2-7r)
  %+  is-equal
    canon-zeros-3x2-7r
  assay-zeros-3x2-7r

++  test-zeros-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-zeros-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0] ~[.~~~0.0 .~~~0.0 .~~~0.0]]])
  =/  assay-zeros-3x3-7r  (zeros:la meta.input-ones-3x3-7r)
  %+  is-equal
    canon-zeros-3x3-7r
  assay-zeros-3x3-7r

++  test-zeros-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-zeros-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-zeros-1x1-3u  (zeros:la meta.input-ones-1x1-3u)
  %+  is-equal
    canon-zeros-1x1-3u
  assay-zeros-1x1-3u

++  test-zeros-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-zeros-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[0 0]]])
  =/  assay-zeros-1x2-3u  (zeros:la meta.input-ones-1x2-3u)
  %+  is-equal
    canon-zeros-1x2-3u
  assay-zeros-1x2-3u

++  test-zeros-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-zeros-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[0 0 0]]])
  =/  assay-zeros-1x3-3u  (zeros:la meta.input-ones-1x3-3u)
  %+  is-equal
    canon-zeros-1x3-3u
  assay-zeros-1x3-3u

++  test-zeros-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-zeros-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  assay-zeros-2x1-3u  (zeros:la meta.input-ones-2x1-3u)
  %+  is-equal
    canon-zeros-2x1-3u
  assay-zeros-2x1-3u

++  test-zeros-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-zeros-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-zeros-2x2-3u  (zeros:la meta.input-ones-2x2-3u)
  %+  is-equal
    canon-zeros-2x2-3u
  assay-zeros-2x2-3u

++  test-zeros-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-2x3-3u  (zeros:la meta.input-ones-2x3-3u)
  %+  is-equal
    canon-zeros-2x3-3u
  assay-zeros-2x3-3u

++  test-zeros-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-zeros-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-zeros-3x1-3u  (zeros:la meta.input-ones-3x1-3u)
  %+  is-equal
    canon-zeros-3x1-3u
  assay-zeros-3x1-3u

++  test-zeros-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-zeros-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-zeros-3x2-3u  (zeros:la meta.input-ones-3x2-3u)
  %+  is-equal
    canon-zeros-3x2-3u
  assay-zeros-3x2-3u

++  test-zeros-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-3x3-3u  (zeros:la meta.input-ones-3x3-3u)
  %+  is-equal
    canon-zeros-3x3-3u
  assay-zeros-3x3-3u

++  test-zeros-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-zeros-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-zeros-1x1-4u  (zeros:la meta.input-ones-1x1-4u)
  %+  is-equal
    canon-zeros-1x1-4u
  assay-zeros-1x1-4u

++  test-zeros-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-zeros-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[0 0]]])
  =/  assay-zeros-1x2-4u  (zeros:la meta.input-ones-1x2-4u)
  %+  is-equal
    canon-zeros-1x2-4u
  assay-zeros-1x2-4u

++  test-zeros-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-zeros-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[0 0 0]]])
  =/  assay-zeros-1x3-4u  (zeros:la meta.input-ones-1x3-4u)
  %+  is-equal
    canon-zeros-1x3-4u
  assay-zeros-1x3-4u

++  test-zeros-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-zeros-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  assay-zeros-2x1-4u  (zeros:la meta.input-ones-2x1-4u)
  %+  is-equal
    canon-zeros-2x1-4u
  assay-zeros-2x1-4u

++  test-zeros-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-zeros-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-zeros-2x2-4u  (zeros:la meta.input-ones-2x2-4u)
  %+  is-equal
    canon-zeros-2x2-4u
  assay-zeros-2x2-4u

++  test-zeros-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-2x3-4u  (zeros:la meta.input-ones-2x3-4u)
  %+  is-equal
    canon-zeros-2x3-4u
  assay-zeros-2x3-4u

++  test-zeros-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-zeros-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-zeros-3x1-4u  (zeros:la meta.input-ones-3x1-4u)
  %+  is-equal
    canon-zeros-3x1-4u
  assay-zeros-3x1-4u

++  test-zeros-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-zeros-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-zeros-3x2-4u  (zeros:la meta.input-ones-3x2-4u)
  %+  is-equal
    canon-zeros-3x2-4u
  assay-zeros-3x2-4u

++  test-zeros-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-3x3-4u  (zeros:la meta.input-ones-3x3-4u)
  %+  is-equal
    canon-zeros-3x3-4u
  assay-zeros-3x3-4u

++  test-zeros-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-zeros-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-zeros-1x1-5u  (zeros:la meta.input-ones-1x1-5u)
  %+  is-equal
    canon-zeros-1x1-5u
  assay-zeros-1x1-5u

++  test-zeros-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-zeros-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[0 0]]])
  =/  assay-zeros-1x2-5u  (zeros:la meta.input-ones-1x2-5u)
  %+  is-equal
    canon-zeros-1x2-5u
  assay-zeros-1x2-5u

++  test-zeros-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-zeros-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[0 0 0]]])
  =/  assay-zeros-1x3-5u  (zeros:la meta.input-ones-1x3-5u)
  %+  is-equal
    canon-zeros-1x3-5u
  assay-zeros-1x3-5u

++  test-zeros-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-zeros-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  assay-zeros-2x1-5u  (zeros:la meta.input-ones-2x1-5u)
  %+  is-equal
    canon-zeros-2x1-5u
  assay-zeros-2x1-5u

++  test-zeros-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-zeros-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-zeros-2x2-5u  (zeros:la meta.input-ones-2x2-5u)
  %+  is-equal
    canon-zeros-2x2-5u
  assay-zeros-2x2-5u

++  test-zeros-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-2x3-5u  (zeros:la meta.input-ones-2x3-5u)
  %+  is-equal
    canon-zeros-2x3-5u
  assay-zeros-2x3-5u

++  test-zeros-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-zeros-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-zeros-3x1-5u  (zeros:la meta.input-ones-3x1-5u)
  %+  is-equal
    canon-zeros-3x1-5u
  assay-zeros-3x1-5u

++  test-zeros-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-zeros-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-zeros-3x2-5u  (zeros:la meta.input-ones-3x2-5u)
  %+  is-equal
    canon-zeros-3x2-5u
  assay-zeros-3x2-5u

++  test-zeros-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-3x3-5u  (zeros:la meta.input-ones-3x3-5u)
  %+  is-equal
    canon-zeros-3x3-5u
  assay-zeros-3x3-5u

++  test-zeros-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-zeros-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-zeros-1x1-6u  (zeros:la meta.input-ones-1x1-6u)
  %+  is-equal
    canon-zeros-1x1-6u
  assay-zeros-1x1-6u

++  test-zeros-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-zeros-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[0 0]]])
  =/  assay-zeros-1x2-6u  (zeros:la meta.input-ones-1x2-6u)
  %+  is-equal
    canon-zeros-1x2-6u
  assay-zeros-1x2-6u

++  test-zeros-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-zeros-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[0 0 0]]])
  =/  assay-zeros-1x3-6u  (zeros:la meta.input-ones-1x3-6u)
  %+  is-equal
    canon-zeros-1x3-6u
  assay-zeros-1x3-6u

++  test-zeros-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-zeros-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  assay-zeros-2x1-6u  (zeros:la meta.input-ones-2x1-6u)
  %+  is-equal
    canon-zeros-2x1-6u
  assay-zeros-2x1-6u

++  test-zeros-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-zeros-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[0 0] ~[0 0]]])
  =/  assay-zeros-2x2-6u  (zeros:la meta.input-ones-2x2-6u)
  %+  is-equal
    canon-zeros-2x2-6u
  assay-zeros-2x2-6u

++  test-zeros-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-2x3-6u  (zeros:la meta.input-ones-2x3-6u)
  %+  is-equal
    canon-zeros-2x3-6u
  assay-zeros-2x3-6u

++  test-zeros-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-zeros-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0] ~[0]]])
  =/  assay-zeros-3x1-6u  (zeros:la meta.input-ones-3x1-6u)
  %+  is-equal
    canon-zeros-3x1-6u
  assay-zeros-3x1-6u

++  test-zeros-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-zeros-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[0 0] ~[0 0] ~[0 0]]])
  =/  assay-zeros-3x2-6u  (zeros:la meta.input-ones-3x2-6u)
  %+  is-equal
    canon-zeros-3x2-6u
  assay-zeros-3x2-6u

++  test-zeros-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-zeros-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[0 0 0] ~[0 0 0] ~[0 0 0]]])
  =/  assay-zeros-3x3-6u  (zeros:la meta.input-ones-3x3-6u)
  %+  is-equal
    canon-zeros-3x3-6u
  assay-zeros-3x3-6u

++  test-ones-1x1-4r  ^-  tang
  =/  input-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  canon-ones-1x1-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  assay-ones-1x1-4r  (ones:la meta.input-ones-1x1-4r)
  %+  is-equal
    canon-ones-1x1-4r
  assay-ones-1x1-4r

++  test-ones-1x2-4r  ^-  tang
  =/  input-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  canon-ones-1x2-4r  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0]]])
  =/  assay-ones-1x2-4r  (ones:la meta.input-ones-1x2-4r)
  %+  is-equal
    canon-ones-1x2-4r
  assay-ones-1x2-4r

++  test-ones-1x3-4r  ^-  tang
  =/  input-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-ones-1x3-4r  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-ones-1x3-4r  (ones:la meta.input-ones-1x3-4r)
  %+  is-equal
    canon-ones-1x3-4r
  assay-ones-1x3-4r

++  test-ones-2x1-4r  ^-  tang
  =/  input-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  canon-ones-2x1-4r  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0]]])
  =/  assay-ones-2x1-4r  (ones:la meta.input-ones-2x1-4r)
  %+  is-equal
    canon-ones-2x1-4r
  assay-ones-2x1-4r

++  test-ones-2x2-4r  ^-  tang
  =/  input-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-ones-2x2-4r  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-ones-2x2-4r  (ones:la meta.input-ones-2x2-4r)
  %+  is-equal
    canon-ones-2x2-4r
  assay-ones-2x2-4r

++  test-ones-2x3-4r  ^-  tang
  =/  input-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-ones-2x3-4r  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-ones-2x3-4r  (ones:la meta.input-ones-2x3-4r)
  %+  is-equal
    canon-ones-2x3-4r
  assay-ones-2x3-4r

++  test-ones-3x1-4r  ^-  tang
  =/  input-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  canon-ones-3x1-4r  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0] ~[.~~1.0] ~[.~~1.0]]])
  =/  assay-ones-3x1-4r  (ones:la meta.input-ones-3x1-4r)
  %+  is-equal
    canon-ones-3x1-4r
  assay-ones-3x1-4r

++  test-ones-3x2-4r  ^-  tang
  =/  input-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  canon-ones-3x2-4r  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0] ~[.~~1.0 .~~1.0]]])
  =/  assay-ones-3x2-4r  (ones:la meta.input-ones-3x2-4r)
  %+  is-equal
    canon-ones-3x2-4r
  assay-ones-3x2-4r

++  test-ones-3x3-4r  ^-  tang
  =/  input-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  canon-ones-3x3-4r  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0] ~[.~~1.0 .~~1.0 .~~1.0]]])
  =/  assay-ones-3x3-4r  (ones:la meta.input-ones-3x3-4r)
  %+  is-equal
    canon-ones-3x3-4r
  assay-ones-3x3-4r

++  test-ones-1x1-5r  ^-  tang
  =/  input-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  canon-ones-1x1-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  assay-ones-1x1-5r  (ones:la meta.input-ones-1x1-5r)
  %+  is-equal
    canon-ones-1x1-5r
  assay-ones-1x1-5r

++  test-ones-1x2-5r  ^-  tang
  =/  input-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0]]])
  =/  canon-ones-1x2-5r  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0]]])
  =/  assay-ones-1x2-5r  (ones:la meta.input-ones-1x2-5r)
  %+  is-equal
    canon-ones-1x2-5r
  assay-ones-1x2-5r

++  test-ones-1x3-5r  ^-  tang
  =/  input-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  canon-ones-1x3-5r  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0]]])
  =/  assay-ones-1x3-5r  (ones:la meta.input-ones-1x3-5r)
  %+  is-equal
    canon-ones-1x3-5r
  assay-ones-1x3-5r

++  test-ones-2x1-5r  ^-  tang
  =/  input-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  canon-ones-2x1-5r  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0]]])
  =/  assay-ones-2x1-5r  (ones:la meta.input-ones-2x1-5r)
  %+  is-equal
    canon-ones-2x1-5r
  assay-ones-2x1-5r

++  test-ones-2x2-5r  ^-  tang
  =/  input-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-ones-2x2-5r  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-ones-2x2-5r  (ones:la meta.input-ones-2x2-5r)
  %+  is-equal
    canon-ones-2x2-5r
  assay-ones-2x2-5r

++  test-ones-2x3-5r  ^-  tang
  =/  input-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-ones-2x3-5r  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-ones-2x3-5r  (ones:la meta.input-ones-2x3-5r)
  %+  is-equal
    canon-ones-2x3-5r
  assay-ones-2x3-5r

++  test-ones-3x1-5r  ^-  tang
  =/  input-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  canon-ones-3x1-5r  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0] ~[.1.0] ~[.1.0]]])
  =/  assay-ones-3x1-5r  (ones:la meta.input-ones-3x1-5r)
  %+  is-equal
    canon-ones-3x1-5r
  assay-ones-3x1-5r

++  test-ones-3x2-5r  ^-  tang
  =/  input-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  canon-ones-3x2-5r  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0] ~[.1.0 .1.0] ~[.1.0 .1.0]]])
  =/  assay-ones-3x2-5r  (ones:la meta.input-ones-3x2-5r)
  %+  is-equal
    canon-ones-3x2-5r
  assay-ones-3x2-5r

++  test-ones-3x3-5r  ^-  tang
  =/  input-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  canon-ones-3x3-5r  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0] ~[.1.0 .1.0 .1.0]]])
  =/  assay-ones-3x3-5r  (ones:la meta.input-ones-3x3-5r)
  %+  is-equal
    canon-ones-3x3-5r
  assay-ones-3x3-5r

++  test-ones-1x1-6r  ^-  tang
  =/  input-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  canon-ones-1x1-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  assay-ones-1x1-6r  (ones:la meta.input-ones-1x1-6r)
  %+  is-equal
    canon-ones-1x1-6r
  assay-ones-1x1-6r

++  test-ones-1x2-6r  ^-  tang
  =/  input-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0]]])
  =/  canon-ones-1x2-6r  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0]]])
  =/  assay-ones-1x2-6r  (ones:la meta.input-ones-1x2-6r)
  %+  is-equal
    canon-ones-1x2-6r
  assay-ones-1x2-6r

++  test-ones-1x3-6r  ^-  tang
  =/  input-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-ones-1x3-6r  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-ones-1x3-6r  (ones:la meta.input-ones-1x3-6r)
  %+  is-equal
    canon-ones-1x3-6r
  assay-ones-1x3-6r

++  test-ones-2x1-6r  ^-  tang
  =/  input-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  canon-ones-2x1-6r  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0]]])
  =/  assay-ones-2x1-6r  (ones:la meta.input-ones-2x1-6r)
  %+  is-equal
    canon-ones-2x1-6r
  assay-ones-2x1-6r

++  test-ones-2x2-6r  ^-  tang
  =/  input-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-ones-2x2-6r  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-ones-2x2-6r  (ones:la meta.input-ones-2x2-6r)
  %+  is-equal
    canon-ones-2x2-6r
  assay-ones-2x2-6r

++  test-ones-2x3-6r  ^-  tang
  =/  input-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-ones-2x3-6r  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-ones-2x3-6r  (ones:la meta.input-ones-2x3-6r)
  %+  is-equal
    canon-ones-2x3-6r
  assay-ones-2x3-6r

++  test-ones-3x1-6r  ^-  tang
  =/  input-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  canon-ones-3x1-6r  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0] ~[.~1.0] ~[.~1.0]]])
  =/  assay-ones-3x1-6r  (ones:la meta.input-ones-3x1-6r)
  %+  is-equal
    canon-ones-3x1-6r
  assay-ones-3x1-6r

++  test-ones-3x2-6r  ^-  tang
  =/  input-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  canon-ones-3x2-6r  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0] ~[.~1.0 .~1.0] ~[.~1.0 .~1.0]]])
  =/  assay-ones-3x2-6r  (ones:la meta.input-ones-3x2-6r)
  %+  is-equal
    canon-ones-3x2-6r
  assay-ones-3x2-6r

++  test-ones-3x3-6r  ^-  tang
  =/  input-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  canon-ones-3x3-6r  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0] ~[.~1.0 .~1.0 .~1.0]]])
  =/  assay-ones-3x3-6r  (ones:la meta.input-ones-3x3-6r)
  %+  is-equal
    canon-ones-3x3-6r
  assay-ones-3x3-6r

++  test-ones-1x1-7r  ^-  tang
  =/  input-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  canon-ones-1x1-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  assay-ones-1x1-7r  (ones:la meta.input-ones-1x1-7r)
  %+  is-equal
    canon-ones-1x1-7r
  assay-ones-1x1-7r

++  test-ones-1x2-7r  ^-  tang
  =/  input-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  canon-ones-1x2-7r  (en-ray:la [meta=[shape=~[1 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0]]])
  =/  assay-ones-1x2-7r  (ones:la meta.input-ones-1x2-7r)
  %+  is-equal
    canon-ones-1x2-7r
  assay-ones-1x2-7r

++  test-ones-1x3-7r  ^-  tang
  =/  input-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-ones-1x3-7r  (en-ray:la [meta=[shape=~[1 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-ones-1x3-7r  (ones:la meta.input-ones-1x3-7r)
  %+  is-equal
    canon-ones-1x3-7r
  assay-ones-1x3-7r

++  test-ones-2x1-7r  ^-  tang
  =/  input-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-ones-2x1-7r  (en-ray:la [meta=[shape=~[2 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-ones-2x1-7r  (ones:la meta.input-ones-2x1-7r)
  %+  is-equal
    canon-ones-2x1-7r
  assay-ones-2x1-7r

++  test-ones-2x2-7r  ^-  tang
  =/  input-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-ones-2x2-7r  (en-ray:la [meta=[shape=~[2 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-ones-2x2-7r  (ones:la meta.input-ones-2x2-7r)
  %+  is-equal
    canon-ones-2x2-7r
  assay-ones-2x2-7r

++  test-ones-2x3-7r  ^-  tang
  =/  input-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-ones-2x3-7r  (en-ray:la [meta=[shape=~[2 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-ones-2x3-7r  (ones:la meta.input-ones-2x3-7r)
  %+  is-equal
    canon-ones-2x3-7r
  assay-ones-2x3-7r

++  test-ones-3x1-7r  ^-  tang
  =/  input-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  canon-ones-3x1-7r  (en-ray:la [meta=[shape=~[3 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0] ~[.~~~1.0] ~[.~~~1.0]]])
  =/  assay-ones-3x1-7r  (ones:la meta.input-ones-3x1-7r)
  %+  is-equal
    canon-ones-3x1-7r
  assay-ones-3x1-7r

++  test-ones-3x2-7r  ^-  tang
  =/  input-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  canon-ones-3x2-7r  (en-ray:la [meta=[shape=~[3 2] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0]]])
  =/  assay-ones-3x2-7r  (ones:la meta.input-ones-3x2-7r)
  %+  is-equal
    canon-ones-3x2-7r
  assay-ones-3x2-7r

++  test-ones-3x3-7r  ^-  tang
  =/  input-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  canon-ones-3x3-7r  (en-ray:la [meta=[shape=~[3 3] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0] ~[.~~~1.0 .~~~1.0 .~~~1.0]]])
  =/  assay-ones-3x3-7r  (ones:la meta.input-ones-3x3-7r)
  %+  is-equal
    canon-ones-3x3-7r
  assay-ones-3x3-7r

++  test-ones-1x1-3u  ^-  tang
  =/  input-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-ones-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-ones-1x1-3u  (ones:la meta.input-ones-1x1-3u)
  %+  is-equal
    canon-ones-1x1-3u
  assay-ones-1x1-3u

++  test-ones-1x2-3u  ^-  tang
  =/  input-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-ones-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  assay-ones-1x2-3u  (ones:la meta.input-ones-1x2-3u)
  %+  is-equal
    canon-ones-1x2-3u
  assay-ones-1x2-3u

++  test-ones-1x3-3u  ^-  tang
  =/  input-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-ones-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  assay-ones-1x3-3u  (ones:la meta.input-ones-1x3-3u)
  %+  is-equal
    canon-ones-1x3-3u
  assay-ones-1x3-3u

++  test-ones-2x1-3u  ^-  tang
  =/  input-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-ones-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  assay-ones-2x1-3u  (ones:la meta.input-ones-2x1-3u)
  %+  is-equal
    canon-ones-2x1-3u
  assay-ones-2x1-3u

++  test-ones-2x2-3u  ^-  tang
  =/  input-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-ones-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-ones-2x2-3u  (ones:la meta.input-ones-2x2-3u)
  %+  is-equal
    canon-ones-2x2-3u
  assay-ones-2x2-3u

++  test-ones-2x3-3u  ^-  tang
  =/  input-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-2x3-3u  (ones:la meta.input-ones-2x3-3u)
  %+  is-equal
    canon-ones-2x3-3u
  assay-ones-2x3-3u

++  test-ones-3x1-3u  ^-  tang
  =/  input-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-ones-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-ones-3x1-3u  (ones:la meta.input-ones-3x1-3u)
  %+  is-equal
    canon-ones-3x1-3u
  assay-ones-3x1-3u

++  test-ones-3x2-3u  ^-  tang
  =/  input-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-ones-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-ones-3x2-3u  (ones:la meta.input-ones-3x2-3u)
  %+  is-equal
    canon-ones-3x2-3u
  assay-ones-3x2-3u

++  test-ones-3x3-3u  ^-  tang
  =/  input-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-3x3-3u  (ones:la meta.input-ones-3x3-3u)
  %+  is-equal
    canon-ones-3x3-3u
  assay-ones-3x3-3u

++  test-ones-1x1-4u  ^-  tang
  =/  input-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-ones-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-ones-1x1-4u  (ones:la meta.input-ones-1x1-4u)
  %+  is-equal
    canon-ones-1x1-4u
  assay-ones-1x1-4u

++  test-ones-1x2-4u  ^-  tang
  =/  input-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-ones-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  assay-ones-1x2-4u  (ones:la meta.input-ones-1x2-4u)
  %+  is-equal
    canon-ones-1x2-4u
  assay-ones-1x2-4u

++  test-ones-1x3-4u  ^-  tang
  =/  input-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-ones-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  assay-ones-1x3-4u  (ones:la meta.input-ones-1x3-4u)
  %+  is-equal
    canon-ones-1x3-4u
  assay-ones-1x3-4u

++  test-ones-2x1-4u  ^-  tang
  =/  input-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-ones-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  assay-ones-2x1-4u  (ones:la meta.input-ones-2x1-4u)
  %+  is-equal
    canon-ones-2x1-4u
  assay-ones-2x1-4u

++  test-ones-2x2-4u  ^-  tang
  =/  input-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-ones-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-ones-2x2-4u  (ones:la meta.input-ones-2x2-4u)
  %+  is-equal
    canon-ones-2x2-4u
  assay-ones-2x2-4u

++  test-ones-2x3-4u  ^-  tang
  =/  input-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-2x3-4u  (ones:la meta.input-ones-2x3-4u)
  %+  is-equal
    canon-ones-2x3-4u
  assay-ones-2x3-4u

++  test-ones-3x1-4u  ^-  tang
  =/  input-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-ones-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-ones-3x1-4u  (ones:la meta.input-ones-3x1-4u)
  %+  is-equal
    canon-ones-3x1-4u
  assay-ones-3x1-4u

++  test-ones-3x2-4u  ^-  tang
  =/  input-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-ones-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-ones-3x2-4u  (ones:la meta.input-ones-3x2-4u)
  %+  is-equal
    canon-ones-3x2-4u
  assay-ones-3x2-4u

++  test-ones-3x3-4u  ^-  tang
  =/  input-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-3x3-4u  (ones:la meta.input-ones-3x3-4u)
  %+  is-equal
    canon-ones-3x3-4u
  assay-ones-3x3-4u

++  test-ones-1x1-5u  ^-  tang
  =/  input-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-ones-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-ones-1x1-5u  (ones:la meta.input-ones-1x1-5u)
  %+  is-equal
    canon-ones-1x1-5u
  assay-ones-1x1-5u

++  test-ones-1x2-5u  ^-  tang
  =/  input-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-ones-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  assay-ones-1x2-5u  (ones:la meta.input-ones-1x2-5u)
  %+  is-equal
    canon-ones-1x2-5u
  assay-ones-1x2-5u

++  test-ones-1x3-5u  ^-  tang
  =/  input-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-ones-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  assay-ones-1x3-5u  (ones:la meta.input-ones-1x3-5u)
  %+  is-equal
    canon-ones-1x3-5u
  assay-ones-1x3-5u

++  test-ones-2x1-5u  ^-  tang
  =/  input-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-ones-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  assay-ones-2x1-5u  (ones:la meta.input-ones-2x1-5u)
  %+  is-equal
    canon-ones-2x1-5u
  assay-ones-2x1-5u

++  test-ones-2x2-5u  ^-  tang
  =/  input-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-ones-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-ones-2x2-5u  (ones:la meta.input-ones-2x2-5u)
  %+  is-equal
    canon-ones-2x2-5u
  assay-ones-2x2-5u

++  test-ones-2x3-5u  ^-  tang
  =/  input-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-2x3-5u  (ones:la meta.input-ones-2x3-5u)
  %+  is-equal
    canon-ones-2x3-5u
  assay-ones-2x3-5u

++  test-ones-3x1-5u  ^-  tang
  =/  input-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-ones-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-ones-3x1-5u  (ones:la meta.input-ones-3x1-5u)
  %+  is-equal
    canon-ones-3x1-5u
  assay-ones-3x1-5u

++  test-ones-3x2-5u  ^-  tang
  =/  input-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-ones-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-ones-3x2-5u  (ones:la meta.input-ones-3x2-5u)
  %+  is-equal
    canon-ones-3x2-5u
  assay-ones-3x2-5u

++  test-ones-3x3-5u  ^-  tang
  =/  input-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-3x3-5u  (ones:la meta.input-ones-3x3-5u)
  %+  is-equal
    canon-ones-3x3-5u
  assay-ones-3x3-5u

++  test-ones-1x1-6u  ^-  tang
  =/  input-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[1]]])
  =/  canon-ones-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[1]]])
  =/  assay-ones-1x1-6u  (ones:la meta.input-ones-1x1-6u)
  %+  is-equal
    canon-ones-1x1-6u
  assay-ones-1x1-6u

++  test-ones-1x2-6u  ^-  tang
  =/  input-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-ones-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  assay-ones-1x2-6u  (ones:la meta.input-ones-1x2-6u)
  %+  is-equal
    canon-ones-1x2-6u
  assay-ones-1x2-6u

++  test-ones-1x3-6u  ^-  tang
  =/  input-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  canon-ones-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1]]])
  =/  assay-ones-1x3-6u  (ones:la meta.input-ones-1x3-6u)
  %+  is-equal
    canon-ones-1x3-6u
  assay-ones-1x3-6u

++  test-ones-2x1-6u  ^-  tang
  =/  input-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  canon-ones-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1]]])
  =/  assay-ones-2x1-6u  (ones:la meta.input-ones-2x1-6u)
  %+  is-equal
    canon-ones-2x1-6u
  assay-ones-2x1-6u

++  test-ones-2x2-6u  ^-  tang
  =/  input-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  canon-ones-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1]]])
  =/  assay-ones-2x2-6u  (ones:la meta.input-ones-2x2-6u)
  %+  is-equal
    canon-ones-2x2-6u
  assay-ones-2x2-6u

++  test-ones-2x3-6u  ^-  tang
  =/  input-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-2x3-6u  (ones:la meta.input-ones-2x3-6u)
  %+  is-equal
    canon-ones-2x3-6u
  assay-ones-2x3-6u

++  test-ones-3x1-6u  ^-  tang
  =/  input-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  canon-ones-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[1] ~[1] ~[1]]])
  =/  assay-ones-3x1-6u  (ones:la meta.input-ones-3x1-6u)
  %+  is-equal
    canon-ones-3x1-6u
  assay-ones-3x1-6u

++  test-ones-3x2-6u  ^-  tang
  =/  input-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  canon-ones-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1] ~[1 1] ~[1 1]]])
  =/  assay-ones-3x2-6u  (ones:la meta.input-ones-3x2-6u)
  %+  is-equal
    canon-ones-3x2-6u
  assay-ones-3x2-6u

++  test-ones-3x3-6u  ^-  tang
  =/  input-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  canon-ones-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[1 1 1] ~[1 1 1] ~[1 1 1]]])
  =/  assay-ones-3x3-6u  (ones:la meta.input-ones-3x3-6u)
  %+  is-equal
    canon-ones-3x3-6u
  assay-ones-3x3-6u

++  test-linspace-asc  ^-  tang
  =/  canon-linspace-11  (en-ray:la [meta=[shape=~[11] bloq=5 kind=%i754 prec=~] baum=~[0x0 0x3dcc.cccc 0x3e4c.cccc 0x3e99.9999 0x3ecc.cccc 0x3eff.ffff 0x3f19.9999 0x3f33.3332 0x3f4c.cccc 0x3f66.6665 0x3f80.0000]])
  =/  assay-linspace-11  (linspace:la [~[11] 5 %i754 ~] [.0 .1] 11)
  %+  is-equal
    canon-linspace-11
  assay-linspace-11

++  test-linspace-des  ^-  tang
  =/  canon-linspace-11  (en-ray:la [meta=[shape=~[11] bloq=5 kind=%i754 prec=~] baum=~[0x3f80.0000 0x3f66.6666 0x3f4c.cccd 0x3f33.3333 0x3f19.999a 0x3f00.0000 0x3ecc.ccce 0x3e99.999c 0x3e4c.ccd0 0x3dcc.ccd8 0x0]])
  =/  assay-linspace-11  (linspace:la [~[11] 5 %i754 ~] [.1 .0] 11)
  %+  is-equal
    canon-linspace-11
  assay-linspace-11

++  test-linspace-1-4r  ^-  tang
  =/  canon-linspace-1-4r  (en-ray:la [meta=[shape=~[1] bloq=4 kind=%i754 prec=~] baum=~[.~~0.0]])
  =/  assay-linspace-1-4r  (linspace:la [~[1] 4 %i754 ~] [.~~0.0 .~~1.0] 1)
  %+  is-equal
    canon-linspace-1-4r
  assay-linspace-1-4r

++  test-linspace-1-5r  ^-  tang
  =/  canon-linspace-1-5r  (en-ray:la [meta=[shape=~[1] bloq=5 kind=%i754 prec=~] baum=~[.0.0]])
  =/  assay-linspace-1-5r  (linspace:la [~[1] 5 %i754 ~] [.0.0 .1.0] 1)
  %+  is-equal
    canon-linspace-1-5r
  assay-linspace-1-5r

++  test-linspace-1-6r  ^-  tang
  =/  canon-linspace-1-6r  (en-ray:la [meta=[shape=~[1] bloq=6 kind=%i754 prec=~] baum=~[.~0.0]])
  =/  assay-linspace-1-6r  (linspace:la [~[1] 6 %i754 ~] [.~0.0 .~1.0] 1)
  %+  is-equal
    canon-linspace-1-6r
  assay-linspace-1-6r

++  test-linspace-1-7r  ^-  tang
  =/  canon-linspace-1-7r  (en-ray:la [meta=[shape=~[1] bloq=7 kind=%i754 prec=~] baum=~[.~~~0.0]])
  =/  assay-linspace-1-7r  (linspace:la [~[1] 7 %i754 ~] [.~~~0.0 .~~~1.0] 1)
  %+  is-equal
    canon-linspace-1-7r
  assay-linspace-1-7r

++  test-rounding-4r-asc-z  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457f]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %z) [~[11] 4 %i754 ~] [.~~1 .~~0] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-asc-d  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457e]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %d) [~[11] 4 %i754 ~] [.~~1 .~~0] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-asc-u  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457f]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %u) [~[11] 4 %i754 ~] [.~~1 .~~0] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-asc-n  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457f]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %n) [~[11] 4 %i754 ~] [.~~1 .~~0] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-des-z  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457d]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %z) [~[11] 4 %i754 ~] [.~~0 .~~1] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-des-d  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457d]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %d) [~[11] 4 %i754 ~] [.~~0 .~~1] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-des-u  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457d]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %u) [~[11] 4 %i754 ~] [.~~0 .~~1] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-4r-des-n  ^-  tang
  =/  canon-cumsum-4r  `ray`[meta=[shape=~[1] bloq=4 kind=%i754 prec=~] data=0x1.457d]
  =/  assay-cumsum-4r  (cumsum:la (linspace:(lake %n) [~[11] 4 %i754 ~] [.~~0 .~~1] 11))
  %+  is-equal
    canon-cumsum-4r
  assay-cumsum-4r

++  test-rounding-5r-asc-z  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.ffff]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %z) [~[11] 5 %i754 ~] [.1 .0] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-asc-d  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.fffe]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %d) [~[11] 5 %i754 ~] [.1 .0] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-asc-u  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40b0.0000]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %u) [~[11] 5 %i754 ~] [.1 .0] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-asc-n  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.ffff]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %n) [~[11] 5 %i754 ~] [.1 .0] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-des-z  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.fffc]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %z) [~[11] 5 %i754 ~] [.0 .1] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-des-d  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.fffc]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %d) [~[11] 5 %i754 ~] [.0 .1] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-des-u  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.fffd]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %u) [~[11] 5 %i754 ~] [.0 .1] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-5r-des-n  ^-  tang
  =/  canon-cumsum-5r  `ray`[meta=[shape=~[1] bloq=5 kind=%i754 prec=~] data=0x1.40af.fffd]
  =/  assay-cumsum-5r  (cumsum:la (linspace:(lake %n) [~[11] 5 %i754 ~] [.0 .1] 11))
  %+  is-equal
    canon-cumsum-5r
  assay-cumsum-5r

++  test-rounding-6r-asc-z  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.ffff]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %z) [~[11] 6 %i754 ~] [.~1 .~0] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-asc-d  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.fffe]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %d) [~[11] 6 %i754 ~] [.~1 .~0] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-asc-u  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4016.0000.0000.0000]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %u) [~[11] 6 %i754 ~] [.~1 .~0] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-asc-n  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.ffff]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %n) [~[11] 6 %i754 ~] [.~1 .~0] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-des-z  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.fffc]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %z) [~[11] 6 %i754 ~] [.~0 .~1] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-des-d  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.fffc]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %d) [~[11] 6 %i754 ~] [.~0 .~1] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-des-u  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.fffe]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %u) [~[11] 6 %i754 ~] [.~0 .~1] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-6r-des-n  ^-  tang
  =/  canon-cumsum-6r  `ray`[meta=[shape=~[1] bloq=6 kind=%i754 prec=~] data=0x1.4015.ffff.ffff.fffd]
  =/  assay-cumsum-6r  (cumsum:la (linspace:(lake %n) [~[11] 6 %i754 ~] [.~0 .~1] 11))
  %+  is-equal
    canon-cumsum-6r
  assay-cumsum-6r

++  test-rounding-7r-asc-z  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.ffff]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %z) [~[11] 7 %i754 ~] [.~~~1 .~~~0] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-asc-d  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.fffe]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %d) [~[11] 7 %i754 ~] [.~~~1 .~~~0] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-asc-u  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.6000.0000.0000.0000.0000.0000.0000]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %u) [~[11] 7 %i754 ~] [.~~~1 .~~~0] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-asc-n  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.ffff]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %n) [~[11] 7 %i754 ~] [.~~~1 .~~~0] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-des-z  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.fffc]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %z) [~[11] 7 %i754 ~] [.~~~0 .~~~1] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-des-d  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.fffc]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %d) [~[11] 7 %i754 ~] [.~~~0 .~~~1] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-des-u  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.fffe]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %u) [~[11] 7 %i754 ~] [.~~~0 .~~~1] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-rounding-7r-des-n  ^-  tang
  =/  canon-cumsum-7r  `ray`[meta=[shape=~[1] bloq=7 kind=%i754 prec=~] data=0x1.4001.5fff.ffff.ffff.ffff.ffff.ffff.fffd]
  =/  assay-cumsum-7r  (cumsum:la (linspace:(lake %n) [~[11] 7 %i754 ~] [.~~~0 .~~~1] 11))
  %+  is-equal
    canon-cumsum-7r
  assay-cumsum-7r

++  test-max-2-4r  ^-  tang
  =/  input-max-2-4r  (en-ray:la [meta=[shape=~[2] bloq=4 kind=%i754 prec=~] baum=(reap 2 .~~0.0)])
  =/  canon-max-2-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  assay-max-2-4r  (max:la (reshape:la (linspace:la meta.input-max-2-4r [.~~0.0 .~~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-max-2-4r
  assay-max-2-4r

++  test-max-9-4r  ^-  tang
  =/  input-max-9-4r  (en-ray:la [meta=[shape=~[9] bloq=4 kind=%i754 prec=~] baum=(reap 9 .~~0.0)])
  =/  canon-max-9-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~1.0]]])
  =/  assay-max-9-4r  (max:la (reshape:la (linspace:la meta.input-max-9-4r [.~~0.0 .~~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-max-9-4r
  assay-max-9-4r

++  test-max-2-5r  ^-  tang
  =/  input-max-2-5r  (en-ray:la [meta=[shape=~[2] bloq=5 kind=%i754 prec=~] baum=(reap 2 .0.0)])
  =/  canon-max-2-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  assay-max-2-5r  (max:la (reshape:la (linspace:la meta.input-max-2-5r [.0.0 .1.0] 2) ~[1 2]))
  %+  is-equal
    canon-max-2-5r
  assay-max-2-5r

++  test-max-9-5r  ^-  tang
  =/  input-max-9-5r  (en-ray:la [meta=[shape=~[9] bloq=5 kind=%i754 prec=~] baum=(reap 9 .0.0)])
  =/  canon-max-9-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0]]])
  =/  assay-max-9-5r  (max:la (reshape:la (linspace:la meta.input-max-9-5r [.0.0 .1.0] 9) ~[1 9]))
  %+  is-equal
    canon-max-9-5r
  assay-max-9-5r

++  test-max-2-6r  ^-  tang
  =/  input-max-2-6r  (en-ray:la [meta=[shape=~[2] bloq=6 kind=%i754 prec=~] baum=(reap 2 .~0.0)])
  =/  canon-max-2-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  assay-max-2-6r  (max:la (reshape:la (linspace:la meta.input-max-2-6r [.~0.0 .~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-max-2-6r
  assay-max-2-6r

++  test-max-9-6r  ^-  tang
  =/  input-max-9-6r  (en-ray:la [meta=[shape=~[9] bloq=6 kind=%i754 prec=~] baum=(reap 9 .~0.0)])
  =/  canon-max-9-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~1.0]]])
  =/  assay-max-9-6r  (max:la (reshape:la (linspace:la meta.input-max-9-6r [.~0.0 .~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-max-9-6r
  assay-max-9-6r

++  test-max-2-7r  ^-  tang
  =/  input-max-2-7r  (en-ray:la [meta=[shape=~[2] bloq=7 kind=%i754 prec=~] baum=(reap 2 .~~~0.0)])
  =/  canon-max-2-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  assay-max-2-7r  (max:la (reshape:la (linspace:la meta.input-max-2-7r [.~~~0.0 .~~~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-max-2-7r
  assay-max-2-7r

++  test-max-9-7r  ^-  tang
  =/  input-max-9-7r  (en-ray:la [meta=[shape=~[9] bloq=7 kind=%i754 prec=~] baum=(reap 9 .~~~0.0)])
  =/  canon-max-9-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~1.0]]])
  =/  assay-max-9-7r  (max:la (reshape:la (linspace:la meta.input-max-9-7r [.~~~0.0 .~~~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-max-9-7r
  assay-max-9-7r

++  test-max-1x1-3u  ^-  tang
  =/  input-max-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-max-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-max-1x1-3u  (max:la (magic:la meta.input-max-1x1-3u))
  %+  is-equal
    canon-max-1x1-3u
  assay-max-1x1-3u

++  test-max-1x2-3u  ^-  tang
  =/  input-max-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-max-1x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-1x2-3u  (max:la (magic:la meta.input-max-1x2-3u))
  %+  is-equal
    canon-max-1x2-3u
  assay-max-1x2-3u

++  test-max-1x3-3u  ^-  tang
  =/  input-max-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-max-1x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-1x3-3u  (max:la (magic:la meta.input-max-1x3-3u))
  %+  is-equal
    canon-max-1x3-3u
  assay-max-1x3-3u

++  test-max-2x1-3u  ^-  tang
  =/  input-max-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-max-2x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-2x1-3u  (max:la (magic:la meta.input-max-2x1-3u))
  %+  is-equal
    canon-max-2x1-3u
  assay-max-2x1-3u

++  test-max-2x2-3u  ^-  tang
  =/  input-max-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-max-2x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[3]])
  =/  assay-max-2x2-3u  (max:la (magic:la meta.input-max-2x2-3u))
  %+  is-equal
    canon-max-2x2-3u
  assay-max-2x2-3u

++  test-max-2x3-3u  ^-  tang
  =/  input-max-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-max-2x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-2x3-3u  (max:la (magic:la meta.input-max-2x3-3u))
  %+  is-equal
    canon-max-2x3-3u
  assay-max-2x3-3u

++  test-max-3x1-3u  ^-  tang
  =/  input-max-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-max-3x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-3x1-3u  (max:la (magic:la meta.input-max-3x1-3u))
  %+  is-equal
    canon-max-3x1-3u
  assay-max-3x1-3u

++  test-max-3x2-3u  ^-  tang
  =/  input-max-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-max-3x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-3x2-3u  (max:la (magic:la meta.input-max-3x2-3u))
  %+  is-equal
    canon-max-3x2-3u
  assay-max-3x2-3u

++  test-max-3x3-3u  ^-  tang
  =/  input-max-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-max-3x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[8]])
  =/  assay-max-3x3-3u  (max:la (magic:la meta.input-max-3x3-3u))
  %+  is-equal
    canon-max-3x3-3u
  assay-max-3x3-3u

++  test-max-1x1-4u  ^-  tang
  =/  input-max-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-max-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-max-1x1-4u  (max:la (magic:la meta.input-max-1x1-4u))
  %+  is-equal
    canon-max-1x1-4u
  assay-max-1x1-4u

++  test-max-1x2-4u  ^-  tang
  =/  input-max-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-max-1x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-1x2-4u  (max:la (magic:la meta.input-max-1x2-4u))
  %+  is-equal
    canon-max-1x2-4u
  assay-max-1x2-4u

++  test-max-1x3-4u  ^-  tang
  =/  input-max-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-max-1x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-1x3-4u  (max:la (magic:la meta.input-max-1x3-4u))
  %+  is-equal
    canon-max-1x3-4u
  assay-max-1x3-4u

++  test-max-2x1-4u  ^-  tang
  =/  input-max-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-max-2x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-2x1-4u  (max:la (magic:la meta.input-max-2x1-4u))
  %+  is-equal
    canon-max-2x1-4u
  assay-max-2x1-4u

++  test-max-2x2-4u  ^-  tang
  =/  input-max-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-max-2x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[3]])
  =/  assay-max-2x2-4u  (max:la (magic:la meta.input-max-2x2-4u))
  %+  is-equal
    canon-max-2x2-4u
  assay-max-2x2-4u

++  test-max-2x3-4u  ^-  tang
  =/  input-max-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-max-2x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-2x3-4u  (max:la (magic:la meta.input-max-2x3-4u))
  %+  is-equal
    canon-max-2x3-4u
  assay-max-2x3-4u

++  test-max-3x1-4u  ^-  tang
  =/  input-max-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-max-3x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-3x1-4u  (max:la (magic:la meta.input-max-3x1-4u))
  %+  is-equal
    canon-max-3x1-4u
  assay-max-3x1-4u

++  test-max-3x2-4u  ^-  tang
  =/  input-max-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-max-3x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-3x2-4u  (max:la (magic:la meta.input-max-3x2-4u))
  %+  is-equal
    canon-max-3x2-4u
  assay-max-3x2-4u

++  test-max-3x3-4u  ^-  tang
  =/  input-max-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-max-3x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[8]])
  =/  assay-max-3x3-4u  (max:la (magic:la meta.input-max-3x3-4u))
  %+  is-equal
    canon-max-3x3-4u
  assay-max-3x3-4u

++  test-max-1x1-5u  ^-  tang
  =/  input-max-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-max-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-max-1x1-5u  (max:la (magic:la meta.input-max-1x1-5u))
  %+  is-equal
    canon-max-1x1-5u
  assay-max-1x1-5u

++  test-max-1x2-5u  ^-  tang
  =/  input-max-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-max-1x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-1x2-5u  (max:la (magic:la meta.input-max-1x2-5u))
  %+  is-equal
    canon-max-1x2-5u
  assay-max-1x2-5u

++  test-max-1x3-5u  ^-  tang
  =/  input-max-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-max-1x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-1x3-5u  (max:la (magic:la meta.input-max-1x3-5u))
  %+  is-equal
    canon-max-1x3-5u
  assay-max-1x3-5u

++  test-max-2x1-5u  ^-  tang
  =/  input-max-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-max-2x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-2x1-5u  (max:la (magic:la meta.input-max-2x1-5u))
  %+  is-equal
    canon-max-2x1-5u
  assay-max-2x1-5u

++  test-max-2x2-5u  ^-  tang
  =/  input-max-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-max-2x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[3]])
  =/  assay-max-2x2-5u  (max:la (magic:la meta.input-max-2x2-5u))
  %+  is-equal
    canon-max-2x2-5u
  assay-max-2x2-5u

++  test-max-2x3-5u  ^-  tang
  =/  input-max-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-max-2x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-2x3-5u  (max:la (magic:la meta.input-max-2x3-5u))
  %+  is-equal
    canon-max-2x3-5u
  assay-max-2x3-5u

++  test-max-3x1-5u  ^-  tang
  =/  input-max-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-max-3x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-3x1-5u  (max:la (magic:la meta.input-max-3x1-5u))
  %+  is-equal
    canon-max-3x1-5u
  assay-max-3x1-5u

++  test-max-3x2-5u  ^-  tang
  =/  input-max-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-max-3x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-3x2-5u  (max:la (magic:la meta.input-max-3x2-5u))
  %+  is-equal
    canon-max-3x2-5u
  assay-max-3x2-5u

++  test-max-3x3-5u  ^-  tang
  =/  input-max-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-max-3x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[8]])
  =/  assay-max-3x3-5u  (max:la (magic:la meta.input-max-3x3-5u))
  %+  is-equal
    canon-max-3x3-5u
  assay-max-3x3-5u

++  test-max-1x1-6u  ^-  tang
  =/  input-max-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-max-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-max-1x1-6u  (max:la (magic:la meta.input-max-1x1-6u))
  %+  is-equal
    canon-max-1x1-6u
  assay-max-1x1-6u

++  test-max-1x2-6u  ^-  tang
  =/  input-max-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-max-1x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-1x2-6u  (max:la (magic:la meta.input-max-1x2-6u))
  %+  is-equal
    canon-max-1x2-6u
  assay-max-1x2-6u

++  test-max-1x3-6u  ^-  tang
  =/  input-max-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-max-1x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-1x3-6u  (max:la (magic:la meta.input-max-1x3-6u))
  %+  is-equal
    canon-max-1x3-6u
  assay-max-1x3-6u

++  test-max-2x1-6u  ^-  tang
  =/  input-max-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-max-2x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[1]])
  =/  assay-max-2x1-6u  (max:la (magic:la meta.input-max-2x1-6u))
  %+  is-equal
    canon-max-2x1-6u
  assay-max-2x1-6u

++  test-max-2x2-6u  ^-  tang
  =/  input-max-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-max-2x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[3]])
  =/  assay-max-2x2-6u  (max:la (magic:la meta.input-max-2x2-6u))
  %+  is-equal
    canon-max-2x2-6u
  assay-max-2x2-6u

++  test-max-2x3-6u  ^-  tang
  =/  input-max-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-max-2x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-2x3-6u  (max:la (magic:la meta.input-max-2x3-6u))
  %+  is-equal
    canon-max-2x3-6u
  assay-max-2x3-6u

++  test-max-3x1-6u  ^-  tang
  =/  input-max-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-max-3x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[2]])
  =/  assay-max-3x1-6u  (max:la (magic:la meta.input-max-3x1-6u))
  %+  is-equal
    canon-max-3x1-6u
  assay-max-3x1-6u

++  test-max-3x2-6u  ^-  tang
  =/  input-max-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-max-3x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[5]])
  =/  assay-max-3x2-6u  (max:la (magic:la meta.input-max-3x2-6u))
  %+  is-equal
    canon-max-3x2-6u
  assay-max-3x2-6u

++  test-max-3x3-6u  ^-  tang
  =/  input-max-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-max-3x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[8]])
  =/  assay-max-3x3-6u  (max:la (magic:la meta.input-max-3x3-6u))
  %+  is-equal
    canon-max-3x3-6u
  assay-max-3x3-6u

++  test-min-2-4r  ^-  tang
  =/  input-min-2-4r  (en-ray:la [meta=[shape=~[2] bloq=4 kind=%i754 prec=~] baum=(reap 2 .~~0.0)])
  =/  canon-min-2-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0]]])
  =/  assay-min-2-4r  (min:la (reshape:la (linspace:la meta.input-min-2-4r [.~~0.0 .~~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-min-2-4r
  assay-min-2-4r

++  test-min-9-4r  ^-  tang
  =/  input-min-9-4r  (en-ray:la [meta=[shape=~[9] bloq=4 kind=%i754 prec=~] baum=(reap 9 .~~0.0)])
  =/  canon-min-9-4r  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%i754 prec=~] baum=~[~[.~~0.0]]])
  =/  assay-min-9-4r  (min:la (reshape:la (linspace:la meta.input-min-9-4r [.~~0.0 .~~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-min-9-4r
  assay-min-9-4r

++  test-min-2-5r  ^-  tang
  =/  input-min-2-5r  (en-ray:la [meta=[shape=~[2] bloq=5 kind=%i754 prec=~] baum=(reap 2 .0.0)])
  =/  canon-min-2-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0]]])
  =/  assay-min-2-5r  (min:la (reshape:la (linspace:la meta.input-min-2-5r [.0.0 .1.0] 2) ~[1 2]))
  %+  is-equal
    canon-min-2-5r
  assay-min-2-5r

++  test-min-9-5r  ^-  tang
  =/  input-min-9-5r  (en-ray:la [meta=[shape=~[9] bloq=5 kind=%i754 prec=~] baum=(reap 9 .0.0)])
  =/  canon-min-9-5r  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0]]])
  =/  assay-min-9-5r  (min:la (reshape:la (linspace:la meta.input-min-9-5r [.0.0 .1.0] 9) ~[1 9]))
  %+  is-equal
    canon-min-9-5r
  assay-min-9-5r

++  test-min-2-6r  ^-  tang
  =/  input-min-2-6r  (en-ray:la [meta=[shape=~[2] bloq=6 kind=%i754 prec=~] baum=(reap 2 .~0.0)])
  =/  canon-min-2-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0]]])
  =/  assay-min-2-6r  (min:la (reshape:la (linspace:la meta.input-min-2-6r [.~0.0 .~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-min-2-6r
  assay-min-2-6r

++  test-min-9-6r  ^-  tang
  =/  input-min-9-6r  (en-ray:la [meta=[shape=~[9] bloq=6 kind=%i754 prec=~] baum=(reap 9 .~0.0)])
  =/  canon-min-9-6r  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%i754 prec=~] baum=~[~[.~0.0]]])
  =/  assay-min-9-6r  (min:la (reshape:la (linspace:la meta.input-min-9-6r [.~0.0 .~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-min-9-6r
  assay-min-9-6r

++  test-min-2-7r  ^-  tang
  =/  input-min-2-7r  (en-ray:la [meta=[shape=~[2] bloq=7 kind=%i754 prec=~] baum=(reap 2 .~~~0.0)])
  =/  canon-min-2-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0]]])
  =/  assay-min-2-7r  (min:la (reshape:la (linspace:la meta.input-min-2-7r [.~~~0.0 .~~~1.0] 2) ~[1 2]))
  %+  is-equal
    canon-min-2-7r
  assay-min-2-7r

++  test-min-9-7r  ^-  tang
  =/  input-min-9-7r  (en-ray:la [meta=[shape=~[9] bloq=7 kind=%i754 prec=~] baum=(reap 9 .~~~0.0)])
  =/  canon-min-9-7r  (en-ray:la [meta=[shape=~[1 1] bloq=7 kind=%i754 prec=~] baum=~[~[.~~~0.0]]])
  =/  assay-min-9-7r  (min:la (reshape:la (linspace:la meta.input-min-9-7r [.~~~0.0 .~~~1.0] 9) ~[1 9]))
  %+  is-equal
    canon-min-9-7r
  assay-min-9-7r

++  test-min-1x1-3u  ^-  tang
  =/  input-min-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-min-1x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x1-3u  (min:la (magic:la meta.input-min-1x1-3u))
  %+  is-equal
    canon-min-1x1-3u
  assay-min-1x1-3u

++  test-min-1x2-3u  ^-  tang
  =/  input-min-1x2-3u  (en-ray:la [meta=[shape=~[1 2] bloq=3 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-min-1x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x2-3u  (min:la (magic:la meta.input-min-1x2-3u))
  %+  is-equal
    canon-min-1x2-3u
  assay-min-1x2-3u

++  test-min-1x3-3u  ^-  tang
  =/  input-min-1x3-3u  (en-ray:la [meta=[shape=~[1 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-min-1x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x3-3u  (min:la (magic:la meta.input-min-1x3-3u))
  %+  is-equal
    canon-min-1x3-3u
  assay-min-1x3-3u

++  test-min-2x1-3u  ^-  tang
  =/  input-min-2x1-3u  (en-ray:la [meta=[shape=~[2 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-min-2x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x1-3u  (min:la (magic:la meta.input-min-2x1-3u))
  %+  is-equal
    canon-min-2x1-3u
  assay-min-2x1-3u

++  test-min-2x2-3u  ^-  tang
  =/  input-min-2x2-3u  (en-ray:la [meta=[shape=~[2 2] bloq=3 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-min-2x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x2-3u  (min:la (magic:la meta.input-min-2x2-3u))
  %+  is-equal
    canon-min-2x2-3u
  assay-min-2x2-3u

++  test-min-2x3-3u  ^-  tang
  =/  input-min-2x3-3u  (en-ray:la [meta=[shape=~[2 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-min-2x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x3-3u  (min:la (magic:la meta.input-min-2x3-3u))
  %+  is-equal
    canon-min-2x3-3u
  assay-min-2x3-3u

++  test-min-3x1-3u  ^-  tang
  =/  input-min-3x1-3u  (en-ray:la [meta=[shape=~[3 1] bloq=3 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-min-3x1-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x1-3u  (min:la (magic:la meta.input-min-3x1-3u))
  %+  is-equal
    canon-min-3x1-3u
  assay-min-3x1-3u

++  test-min-3x2-3u  ^-  tang
  =/  input-min-3x2-3u  (en-ray:la [meta=[shape=~[3 2] bloq=3 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-min-3x2-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x2-3u  (min:la (magic:la meta.input-min-3x2-3u))
  %+  is-equal
    canon-min-3x2-3u
  assay-min-3x2-3u

++  test-min-3x3-3u  ^-  tang
  =/  input-min-3x3-3u  (en-ray:la [meta=[shape=~[3 3] bloq=3 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-min-3x3-3u  (en-ray:la [meta=[shape=~[1 1] bloq=3 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x3-3u  (min:la (magic:la meta.input-min-3x3-3u))
  %+  is-equal
    canon-min-3x3-3u
  assay-min-3x3-3u

++  test-min-1x1-4u  ^-  tang
  =/  input-min-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-min-1x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x1-4u  (min:la (magic:la meta.input-min-1x1-4u))
  %+  is-equal
    canon-min-1x1-4u
  assay-min-1x1-4u

++  test-min-1x2-4u  ^-  tang
  =/  input-min-1x2-4u  (en-ray:la [meta=[shape=~[1 2] bloq=4 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-min-1x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x2-4u  (min:la (magic:la meta.input-min-1x2-4u))
  %+  is-equal
    canon-min-1x2-4u
  assay-min-1x2-4u

++  test-min-1x3-4u  ^-  tang
  =/  input-min-1x3-4u  (en-ray:la [meta=[shape=~[1 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-min-1x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x3-4u  (min:la (magic:la meta.input-min-1x3-4u))
  %+  is-equal
    canon-min-1x3-4u
  assay-min-1x3-4u

++  test-min-2x1-4u  ^-  tang
  =/  input-min-2x1-4u  (en-ray:la [meta=[shape=~[2 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-min-2x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x1-4u  (min:la (magic:la meta.input-min-2x1-4u))
  %+  is-equal
    canon-min-2x1-4u
  assay-min-2x1-4u

++  test-min-2x2-4u  ^-  tang
  =/  input-min-2x2-4u  (en-ray:la [meta=[shape=~[2 2] bloq=4 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-min-2x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x2-4u  (min:la (magic:la meta.input-min-2x2-4u))
  %+  is-equal
    canon-min-2x2-4u
  assay-min-2x2-4u

++  test-min-2x3-4u  ^-  tang
  =/  input-min-2x3-4u  (en-ray:la [meta=[shape=~[2 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-min-2x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x3-4u  (min:la (magic:la meta.input-min-2x3-4u))
  %+  is-equal
    canon-min-2x3-4u
  assay-min-2x3-4u

++  test-min-3x1-4u  ^-  tang
  =/  input-min-3x1-4u  (en-ray:la [meta=[shape=~[3 1] bloq=4 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-min-3x1-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x1-4u  (min:la (magic:la meta.input-min-3x1-4u))
  %+  is-equal
    canon-min-3x1-4u
  assay-min-3x1-4u

++  test-min-3x2-4u  ^-  tang
  =/  input-min-3x2-4u  (en-ray:la [meta=[shape=~[3 2] bloq=4 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-min-3x2-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x2-4u  (min:la (magic:la meta.input-min-3x2-4u))
  %+  is-equal
    canon-min-3x2-4u
  assay-min-3x2-4u

++  test-min-3x3-4u  ^-  tang
  =/  input-min-3x3-4u  (en-ray:la [meta=[shape=~[3 3] bloq=4 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-min-3x3-4u  (en-ray:la [meta=[shape=~[1 1] bloq=4 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x3-4u  (min:la (magic:la meta.input-min-3x3-4u))
  %+  is-equal
    canon-min-3x3-4u
  assay-min-3x3-4u

++  test-min-1x1-5u  ^-  tang
  =/  input-min-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-min-1x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x1-5u  (min:la (magic:la meta.input-min-1x1-5u))
  %+  is-equal
    canon-min-1x1-5u
  assay-min-1x1-5u

++  test-min-1x2-5u  ^-  tang
  =/  input-min-1x2-5u  (en-ray:la [meta=[shape=~[1 2] bloq=5 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-min-1x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x2-5u  (min:la (magic:la meta.input-min-1x2-5u))
  %+  is-equal
    canon-min-1x2-5u
  assay-min-1x2-5u

++  test-min-1x3-5u  ^-  tang
  =/  input-min-1x3-5u  (en-ray:la [meta=[shape=~[1 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-min-1x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x3-5u  (min:la (magic:la meta.input-min-1x3-5u))
  %+  is-equal
    canon-min-1x3-5u
  assay-min-1x3-5u

++  test-min-2x1-5u  ^-  tang
  =/  input-min-2x1-5u  (en-ray:la [meta=[shape=~[2 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-min-2x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x1-5u  (min:la (magic:la meta.input-min-2x1-5u))
  %+  is-equal
    canon-min-2x1-5u
  assay-min-2x1-5u

++  test-min-2x2-5u  ^-  tang
  =/  input-min-2x2-5u  (en-ray:la [meta=[shape=~[2 2] bloq=5 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-min-2x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x2-5u  (min:la (magic:la meta.input-min-2x2-5u))
  %+  is-equal
    canon-min-2x2-5u
  assay-min-2x2-5u

++  test-min-2x3-5u  ^-  tang
  =/  input-min-2x3-5u  (en-ray:la [meta=[shape=~[2 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-min-2x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x3-5u  (min:la (magic:la meta.input-min-2x3-5u))
  %+  is-equal
    canon-min-2x3-5u
  assay-min-2x3-5u

++  test-min-3x1-5u  ^-  tang
  =/  input-min-3x1-5u  (en-ray:la [meta=[shape=~[3 1] bloq=5 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-min-3x1-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x1-5u  (min:la (magic:la meta.input-min-3x1-5u))
  %+  is-equal
    canon-min-3x1-5u
  assay-min-3x1-5u

++  test-min-3x2-5u  ^-  tang
  =/  input-min-3x2-5u  (en-ray:la [meta=[shape=~[3 2] bloq=5 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-min-3x2-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x2-5u  (min:la (magic:la meta.input-min-3x2-5u))
  %+  is-equal
    canon-min-3x2-5u
  assay-min-3x2-5u

++  test-min-3x3-5u  ^-  tang
  =/  input-min-3x3-5u  (en-ray:la [meta=[shape=~[3 3] bloq=5 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-min-3x3-5u  (en-ray:la [meta=[shape=~[1 1] bloq=5 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x3-5u  (min:la (magic:la meta.input-min-3x3-5u))
  %+  is-equal
    canon-min-3x3-5u
  assay-min-3x3-5u

++  test-min-1x1-6u  ^-  tang
  =/  input-min-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  canon-min-1x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x1-6u  (min:la (magic:la meta.input-min-1x1-6u))
  %+  is-equal
    canon-min-1x1-6u
  assay-min-1x1-6u

++  test-min-1x2-6u  ^-  tang
  =/  input-min-1x2-6u  (en-ray:la [meta=[shape=~[1 2] bloq=6 kind=%uint prec=~] baum=~[~[1 1]]])
  =/  canon-min-1x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x2-6u  (min:la (magic:la meta.input-min-1x2-6u))
  %+  is-equal
    canon-min-1x2-6u
  assay-min-1x2-6u

++  test-min-1x3-6u  ^-  tang
  =/  input-min-1x3-6u  (en-ray:la [meta=[shape=~[1 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2]]])
  =/  canon-min-1x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-1x3-6u  (min:la (magic:la meta.input-min-1x3-6u))
  %+  is-equal
    canon-min-1x3-6u
  assay-min-1x3-6u

++  test-min-2x1-6u  ^-  tang
  =/  input-min-2x1-6u  (en-ray:la [meta=[shape=~[2 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0]]])
  =/  canon-min-2x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x1-6u  (min:la (magic:la meta.input-min-2x1-6u))
  %+  is-equal
    canon-min-2x1-6u
  assay-min-2x1-6u

++  test-min-2x2-6u  ^-  tang
  =/  input-min-2x2-6u  (en-ray:la [meta=[shape=~[2 2] bloq=6 kind=%uint prec=~] baum=~[~[0 1] ~[2 3]]])
  =/  canon-min-2x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x2-6u  (min:la (magic:la meta.input-min-2x2-6u))
  %+  is-equal
    canon-min-2x2-6u
  assay-min-2x2-6u

++  test-min-2x3-6u  ^-  tang
  =/  input-min-2x3-6u  (en-ray:la [meta=[shape=~[2 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5]]])
  =/  canon-min-2x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-2x3-6u  (min:la (magic:la meta.input-min-2x3-6u))
  %+  is-equal
    canon-min-2x3-6u
  assay-min-2x3-6u

++  test-min-3x1-6u  ^-  tang
  =/  input-min-3x1-6u  (en-ray:la [meta=[shape=~[3 1] bloq=6 kind=%uint prec=~] baum=~[~[0] ~[0] ~[2]]])
  =/  canon-min-3x1-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x1-6u  (min:la (magic:la meta.input-min-3x1-6u))
  %+  is-equal
    canon-min-3x1-6u
  assay-min-3x1-6u

++  test-min-3x2-6u  ^-  tang
  =/  input-min-3x2-6u  (en-ray:la [meta=[shape=~[3 2] bloq=6 kind=%uint prec=~] baum=~[~[0 1] ~[2 3] ~[4 5]]])
  =/  canon-min-3x2-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x2-6u  (min:la (magic:la meta.input-min-3x2-6u))
  %+  is-equal
    canon-min-3x2-6u
  assay-min-3x2-6u

++  test-min-3x3-6u  ^-  tang
  =/  input-min-3x3-6u  (en-ray:la [meta=[shape=~[3 3] bloq=6 kind=%uint prec=~] baum=~[~[0 1 2] ~[3 4 5] ~[6 7 8]]])
  =/  canon-min-3x3-6u  (en-ray:la [meta=[shape=~[1 1] bloq=6 kind=%uint prec=~] baum=~[~[0]]])
  =/  assay-min-3x3-6u  (min:la (magic:la meta.input-min-3x3-6u))
  %+  is-equal
    canon-min-3x3-6u
  assay-min-3x3-6u

++  test-mmul-3x3-3u  ^-  tang
  =/  meta-3x3-3  [~[3 3] 3 %uint ~]
  =/  assay-3x3-3-i  (eye:la meta-3x3-3)
  =/  assay-3x3-3  (en-ray:la [meta-3x3-3 ~[~[1 2 3] ~[4 5 6] ~[7 8 9]]])
  =/  canon-3x3-3  (en-ray:la [meta-3x3-3 ~[~[30 36 42] ~[66 81 96] ~[102 126 150]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x3-3)
      !>((mmul:la assay-3x3-3 assay-3x3-3))
    %+  expect-eq
      !>(assay-3x3-3)
      !>((mmul:la assay-3x3-3 assay-3x3-3-i))
    %+  expect-eq
      !>((en-ray:la [[~[3 1] 3 %uint ~] ~[~[6] ~[15] ~[24]]]))
      !>((mmul:la assay-3x3-3 (ones:la [~[3 1] 3 %uint ~])))
  ==

++  test-mmul-3x3-4u  ^-  tang
  =/  meta-3x3-4  [~[3 3] 4 %uint ~]
  =/  assay-3x3-4-i  (eye:la meta-3x3-4)
  =/  assay-3x3-4  (en-ray:la [meta-3x3-4 ~[~[1 2 3] ~[4 5 6] ~[7 8 9]]])
  =/  canon-3x3-4  (en-ray:la [meta-3x3-4 ~[~[30 36 42] ~[66 81 96] ~[102 126 150]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x3-4)
      !>((mmul:la assay-3x3-4 assay-3x3-4))
    %+  expect-eq
      !>(assay-3x3-4)
      !>((mmul:la assay-3x3-4 assay-3x3-4-i))
    %+  expect-eq
      !>((en-ray:la [[~[3 1] 4 %uint ~] ~[~[6] ~[15] ~[24]]]))
      !>((mmul:la assay-3x3-4 (ones:la [~[3 1] 4 %uint ~])))
  ==

++  test-mmul-3x3-5u  ^-  tang
  =/  meta-3x3-5  [~[3 3] 5 %uint ~]
  =/  assay-3x3-5-i  (eye:la meta-3x3-5)
  =/  assay-3x3-5  (en-ray:la [meta-3x3-5 ~[~[1 2 3] ~[4 5 6] ~[7 8 9]]])
  =/  canon-3x3-5  (en-ray:la [meta-3x3-5 ~[~[30 36 42] ~[66 81 96] ~[102 126 150]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x3-5)
      !>((mmul:la assay-3x3-5 assay-3x3-5))
    %+  expect-eq
      !>(assay-3x3-5)
      !>((mmul:la assay-3x3-5 assay-3x3-5-i))
    %+  expect-eq
      !>((en-ray:la [[~[3 1] 5 %uint ~] ~[~[6] ~[15] ~[24]]]))
      !>((mmul:la assay-3x3-5 (ones:la [~[3 1] 5 %uint ~])))
  ==

++  test-mmul-3x4x5-4r  ^-  tang
  =/  meta-3x4-4  [~[3 4] 4 %i754 ~]
  =/  meta-4x5-4  [~[4 5] 4 %i754 ~]
  =/  meta-3x5-4  [~[3 5] 4 %i754 ~]
  =/  assay-3x4-4  (en-ray:la [meta-3x4-4 ~[~[.~~1 .~~2 .~~3 .~~4] ~[.~~5 .~~6 .~~7 .~~8] ~[.~~9 .~~10 .~~11 .~~12]]])
  =/  assay-4x5-4  (en-ray:la [meta-4x5-4 ~[~[.~~1 .~~2 .~~3 .~~4 .~~5] ~[.~~4 .~~5 .~~6 .~~7 .~~8] ~[.~~7 .~~8 .~~9 .~~10 .~~11] ~[.~~10 .~~11 .~~12 .~~13 .~~14]]])
  =/  canon-3x5-4  (en-ray:la [meta-3x5-4 ~[~[.~~70 .~~80 .~~90 .~~100 .~~110] ~[.~~158 .~~184 .~~210 .~~236 .~~262] ~[.~~246 .~~288 .~~330 .~~372 .~~414]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x5-4)
      !>((mmul:la assay-3x4-4 assay-4x5-4))
  ==

++  test-mmul-3x4x5-5r  ^-  tang
  =/  meta-3x4-5  [~[3 4] 5 %i754 ~]
  =/  meta-4x5-5  [~[4 5] 5 %i754 ~]
  =/  meta-3x5-5  [~[3 5] 5 %i754 ~]
  =/  assay-3x4-5  (en-ray:la [meta-3x4-5 ~[~[.1 .2 .3 .4] ~[.5 .6 .7 .8] ~[.9 .10 .11 .12]]])
  =/  assay-4x5-5  (en-ray:la [meta-4x5-5 ~[~[.1 .2 .3 .4 .5] ~[.4 .5 .6 .7 .8] ~[.7 .8 .9 .10 .11] ~[.10 .11 .12 .13 .14]]])
  =/  canon-3x5-5  (en-ray:la [meta-3x5-5 ~[~[.70 .80 .90 .100 .110] ~[.158 .184 .210 .236 .262] ~[.246 .288 .330 .372 .414]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x5-5)
      !>((mmul:la assay-3x4-5 assay-4x5-5))
  ==

++  test-mmul-3x4x5-6r  ^-  tang
  =/  meta-3x4-6  [~[3 4] 6 %i754 ~]
  =/  meta-4x5-6  [~[4 5] 6 %i754 ~]
  =/  meta-3x5-6  [~[3 5] 6 %i754 ~]
  =/  assay-3x4-6  (en-ray:la [meta-3x4-6 ~[~[.~1 .~2 .~3 .~4] ~[.~5 .~6 .~7 .~8] ~[.~9 .~10 .~11 .~12]]])
  =/  assay-4x5-6  (en-ray:la [meta-4x5-6 ~[~[.~1 .~2 .~3 .~4 .~5] ~[.~4 .~5 .~6 .~7 .~8] ~[.~7 .~8 .~9 .~10 .~11] ~[.~10 .~11 .~12 .~13 .~14]]])
  =/  canon-3x5-6  (en-ray:la [meta-3x5-6 ~[~[.~70 .~80 .~90 .~100 .~110] ~[.~158 .~184 .~210 .~236 .~262] ~[.~246 .~288 .~330 .~372 .~414]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x5-6)
      !>((mmul:la assay-3x4-6 assay-4x5-6))
  ==

++  test-mmul-3x4x5-7r  ^-  tang
  =/  meta-3x4-7  [~[3 4] 7 %i754 ~]
  =/  meta-4x5-7  [~[4 5] 7 %i754 ~]
  =/  meta-3x5-7  [~[3 5] 7 %i754 ~]
  =/  assay-3x4-7  (en-ray:la [meta-3x4-7 ~[~[.~~~1 .~~~2 .~~~3 .~~~4] ~[.~~~5 .~~~6 .~~~7 .~~~8] ~[.~~~9 .~~~10 .~~~11 .~~~12]]])
  =/  assay-4x5-7  (en-ray:la [meta-4x5-7 ~[~[.~~~1 .~~~2 .~~~3 .~~~4 .~~~5] ~[.~~~4 .~~~5 .~~~6 .~~~7 .~~~8] ~[.~~~7 .~~~8 .~~~9 .~~~10 .~~~11] ~[.~~~10 .~~~11 .~~~12 .~~~13 .~~~14]]])
  =/  canon-3x5-7  (en-ray:la [meta-3x5-7 ~[~[.~~~70 .~~~80 .~~~90 .~~~100 .~~~110] ~[.~~~158 .~~~184 .~~~210 .~~~236 .~~~262] ~[.~~~246 .~~~288 .~~~330 .~~~372 .~~~414]]])
  ;:  weld
    %+  expect-eq
      !>(canon-3x5-7)
      !>((mmul:la assay-3x4-7 assay-4x5-7))
  ==

++  test-dot-1-4r  ^-  tang
  =/  meta-1x1-4  [~[1 1] 4 %i754 ~]
  =/  assay-1x1-4  (en-ray:la [meta-1x1-4 ~[~[.~~10]]])
  =/  canon-1x1-4  (en-ray:la [meta-1x1-4 ~[~[.~~100]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-4)
      !>((dot:la assay-1x1-4 assay-1x1-4))
  ==

++  test-dot-4-4r  ^-  tang
  =/  meta-1x1-4  [~[1 1] 4 %i754 ~]
  =/  meta-1x4-4  [~[1 4] 4 %i754 ~]
  =/  assay-1x4-a-4  (en-ray:la [meta-1x4-4 ~[~[.~~1 .~~2 .~~3 .~~4]]])
  =/  assay-1x4-b-4  (en-ray:la [meta-1x4-4 ~[~[.~~0.5 .~~0.25 .~~0.125 .~~0.0625]]])
  =/  canon-1x1-4  (en-ray:la [meta-1x1-4 ~[~[.~~1.625]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-4)
      !>((dot:la assay-1x4-a-4 assay-1x4-b-4))
  ==

++  test-dot-1-5r  ^-  tang
  =/  meta-1x1-5  [~[1 1] 5 %i754 ~]
  =/  assay-1x1-5  (en-ray:la [meta-1x1-5 ~[~[.10]]])
  =/  canon-1x1-5  (en-ray:la [meta-1x1-5 ~[~[.100]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-5)
      !>((dot:la assay-1x1-5 assay-1x1-5))
  ==

++  test-dot-4-5r  ^-  tang
  =/  meta-1x1-5  [~[1 1] 5 %i754 ~]
  =/  meta-1x4-5  [~[1 4] 5 %i754 ~]
  =/  assay-1x4-a-5  (en-ray:la [meta-1x4-5 ~[~[.1 .2 .3 .4 .5]]])
  =/  assay-1x4-b-5  (en-ray:la [meta-1x4-5 ~[~[.0.5 .0.25 .0.125 .0.0625 .0.03125]]])
  =/  canon-1x1-5  (en-ray:la [meta-1x1-5 ~[~[.1.625]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-5)
      !>((dot:la assay-1x4-a-5 assay-1x4-b-5))
  ==

++  test-dot-1-6r  ^-  tang
  =/  meta-1x1-6  [~[1 1] 6 %i754 ~]
  =/  assay-1x1-6  (en-ray:la [meta-1x1-6 ~[~[.~10]]])
  =/  canon-1x1-6  (en-ray:la [meta-1x1-6 ~[~[.~100]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-6)
      !>((dot:la assay-1x1-6 assay-1x1-6))
  ==

++  test-dot-4-6r  ^-  tang
  =/  meta-1x1-6  [~[1 1] 6 %i754 ~]
  =/  meta-1x4-6  [~[1 4] 6 %i754 ~]
  =/  assay-1x4-a-6  (en-ray:la [meta-1x4-6 ~[~[.~1 .~2 .~3 .~4 .~5 .~6]]])
  =/  assay-1x4-b-6  (en-ray:la [meta-1x4-6 ~[~[.~0.5 .~0.25 .~0.125 .~0.0625 .~0.03125 .~0.015625]]])
  =/  canon-1x1-6  (en-ray:la [meta-1x1-6 ~[~[.~1.625]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-6)
      !>((dot:la assay-1x4-a-6 assay-1x4-b-6))
  ==

++  test-dot-1-7r  ^-  tang
  =/  meta-1x1-7  [~[1 1] 7 %i754 ~]
  =/  assay-1x1-7  (en-ray:la [meta-1x1-7 ~[~[.~~~10]]])
  =/  canon-1x1-7  (en-ray:la [meta-1x1-7 ~[~[.~~~100]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-7)
      !>((dot:la assay-1x1-7 assay-1x1-7))
  ==

++  test-dot-4-7r  ^-  tang
  =/  meta-1x1-7  [~[1 1] 7 %i754 ~]
  =/  meta-1x4-7  [~[1 4] 7 %i754 ~]
  =/  assay-1x4-a-7  (en-ray:la [meta-1x4-7 ~[~[.~~~1 .~~~2 .~~~3 .~~~4 .~~~5 .~~~6 .~~~7]]])
  =/  assay-1x4-b-7  (en-ray:la [meta-1x4-7 ~[~[.~~~0.5 .~~~0.25 .~~~0.125 .~~~0.0625 .~~~0.03125 .~~~0.015625 .~~~0.0078125]]])
  =/  canon-1x1-7  (en-ray:la [meta-1x1-7 ~[~[.~~~1.625]]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x1-7)
      !>((dot:la assay-1x4-a-7 assay-1x4-b-7))
  ==

:: TODO test 0x0

++  test-range-asc10-4r  ^-  tang
  =/  meta-1x5-4  [~[10] 4 %i754 ~]
  =/  canon-1x5-4  (en-ray:la [meta-1x5-4 ~[.~~0 .~~0.5 .~~1 .~~1.5 .~~2 .~~2.5 .~~3 .~~3.5 .~~4 .~~4.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-4)
      !>((range:la meta-1x5-4 [.~~0 .~~5] .~~0.5))
  ==

++  test-range-asc10-5r  ^-  tang
  =/  meta-1x5-5  [~[10] 5 %i754 ~]
  =/  canon-1x5-5  (en-ray:la [meta-1x5-5 ~[.0 .0.5 .1 .1.5 .2 .2.5 .3 .3.5 .4 .4.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-5)
      !>((range:la meta-1x5-5 [.0 .5] .0.5))
  ==

++  test-range-asc10-6r  ^-  tang
  =/  meta-1x5-6  [~[10] 6 %i754 ~]
  =/  canon-1x5-6  (en-ray:la [meta-1x5-6 ~[.~0 .~0.5 .~1 .~1.5 .~2 .~2.5 .~3 .~3.5 .~4 .~4.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-6)
      !>((range:la meta-1x5-6 [.~0 .~5] .~0.5))
  ==

++  test-range-asc10-7r  ^-  tang
  =/  meta-1x5-7  [~[10] 7 %i754 ~]
  =/  canon-1x5-7  (en-ray:la [meta-1x5-7 ~[.~~~0 .~~~0.5 .~~~1 .~~~1.5 .~~~2 .~~~2.5 .~~~3 .~~~3.5 .~~~4 .~~~4.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-7)
      !>((range:la meta-1x5-7 [.~~~0 .~~~5] .~~~0.5))
  ==

++  test-range-asc11-4r  ^-  tang
  =/  meta-1x5-4  [~[11] 4 %i754 ~]
  =/  canon-1x5-4  (en-ray:la [meta-1x5-4 ~[.~~0 .~~0.5 .~~1 .~~1.5 .~~2 .~~2.5 .~~3 .~~3.5 .~~4 .~~4.5 .~~5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-4)
      !>((range:la meta-1x5-4 [.~~0 .~~5.1] .~~0.5))
  ==

++  test-range-asc11-5r  ^-  tang
  =/  meta-1x5-5  [~[11] 5 %i754 ~]
  =/  canon-1x5-5  (en-ray:la [meta-1x5-5 ~[.0 .0.5 .1 .1.5 .2 .2.5 .3 .3.5 .4 .4.5 .5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-5)
      !>((range:la meta-1x5-5 [.0 .5.1] .0.5))
  ==

++  test-range-asc11-6r  ^-  tang
  =/  meta-1x5-6  [~[11] 6 %i754 ~]
  =/  canon-1x5-6  (en-ray:la [meta-1x5-6 ~[.~0 .~0.5 .~1 .~1.5 .~2 .~2.5 .~3 .~3.5 .~4 .~4.5 .~5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-6)
      !>((range:la meta-1x5-6 [.~0 .~5.1] .~0.5))
  ==

++  test-range-asc11-7r  ^-  tang
  =/  meta-1x5-7  [~[11] 7 %i754 ~]
  =/  canon-1x5-7  (en-ray:la [meta-1x5-7 ~[.~~~0 .~~~0.5 .~~~1 .~~~1.5 .~~~2 .~~~2.5 .~~~3 .~~~3.5 .~~~4 .~~~4.5 .~~~5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-7)
      !>((range:la meta-1x5-7 [.~~~0 .~~~5.1] .~~~0.5))
  ==

++  test-range-des10-4r  ^-  tang
  =/  meta-1x5-4  [~[10] 4 %i754 ~]
  =/  canon-1x5-4  (en-ray:la [meta-1x5-4 ~[.~~5 .~~4.5 .~~4 .~~3.5 .~~3 .~~2.5 .~~2 .~~1.5 .~~1 .~~0.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-4)
      !>((range:la meta-1x5-4 [.~~5 .~~0] .~~-0.5))
  ==

++  test-range-des10-5r  ^-  tang
  =/  meta-1x5-5  [~[10] 5 %i754 ~]
  =/  canon-1x5-5  (en-ray:la [meta-1x5-5 ~[.5 .4.5 .4 .3.5 .3 .2.5 .2 .1.5 .1 .0.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-5)
      !>((range:la meta-1x5-5 [.5 .0] .-0.5))
  ==

++  test-range-des10-6r  ^-  tang
  =/  meta-1x5-6  [~[10] 6 %i754 ~]
  =/  canon-1x5-6  (en-ray:la [meta-1x5-6 ~[.~5 .~4.5 .~4 .~3.5 .~3 .~2.5 .~2 .~1.5 .~1 .~0.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-6)
      !>((range:la meta-1x5-6 [.~5 .~0] .~-0.5))
  ==

++  test-range-des10-7r  ^-  tang
  =/  meta-1x5-7  [~[10] 7 %i754 ~]
  =/  canon-1x5-7  (en-ray:la [meta-1x5-7 ~[.~~~5 .~~~4.5 .~~~4 .~~~3.5 .~~~3 .~~~2.5 .~~~2 .~~~1.5 .~~~1 .~~~0.5]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-7)
      !>((range:la meta-1x5-7 [.~~~5 .~~~0] .~~~-0.5))
  ==

++  test-range-des11-4r  ^-  tang
  =/  meta-1x5-4  [~[11] 4 %i754 ~]
  =/  canon-1x5-4  (en-ray:la [meta-1x5-4 ~[.~~5 .~~4.5 .~~4 .~~3.5 .~~3 .~~2.5 .~~2 .~~1.5 .~~1 .~~0.5 .~~0]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-4)
      !>((range:la meta-1x5-4 [.~~5 .~~-0.1] .~~-0.5))
  ==

++  test-range-des11-5r  ^-  tang
  =/  meta-1x5-5  [~[11] 5 %i754 ~]
  =/  canon-1x5-5  (en-ray:la [meta-1x5-5 ~[.5 .4.5 .4 .3.5 .3 .2.5 .2 .1.5 .1 .0.5 .0]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-5)
      !>((range:la meta-1x5-5 [.5 .-0.1] .-0.5))
  ==

++  test-range-des11-6r  ^-  tang
  =/  meta-1x5-6  [~[11] 6 %i754 ~]
  =/  canon-1x5-6  (en-ray:la [meta-1x5-6 ~[.~5 .~4.5 .~4 .~3.5 .~3 .~2.5 .~2 .~1.5 .~1 .~0.5 .~0]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-6)
      !>((range:la meta-1x5-6 [.~5 .~-0.1] .~-0.5))
  ==

++  test-range-des11-7r  ^-  tang
  =/  meta-1x5-7  [~[11] 7 %i754 ~]
  =/  canon-1x5-7  (en-ray:la [meta-1x5-7 ~[.~~~5 .~~~4.5 .~~~4 .~~~3.5 .~~~3 .~~~2.5 .~~~2 .~~~1.5 .~~~1 .~~~0.5 .~~~0]])
  ;:  weld
    %+  expect-eq
      !>(canon-1x5-7)
      !>((range:la meta-1x5-7 [.~~~5 .~~~-0.1] .~~~-0.5))
  ==

--
