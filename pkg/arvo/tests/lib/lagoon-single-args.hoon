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

--
