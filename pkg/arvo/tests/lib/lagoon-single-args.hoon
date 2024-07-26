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
  =/  canon-linspace-11  (en-ray:la [meta=[shape=~[11] bloq=5 kind=%i754 prec=~] baum=~[~[.0.0 .0.1 .0.2 .0.3 .0.4 .0.5 .0.6 .0.7 .0.8 .0.9 .1.0]]])
  =/  assay-linspace-11  (linspace:la [~[11] 5 %i754 ~] [.0 .1] 11)
  %+  is-equal
    canon-linspace-11
  assay-linspace-11

++  test-linspace-des  ^-  tang
  =/  canon-linspace-11  (en-ray:la [meta=[shape=~[11] bloq=5 kind=%i754 prec=~] baum=~[~[.1.0 .0.9 .0.8 .0.7 .0.6 .0.5 .0.4 .0.3 .0.2 .0.1 .0.0]]])
  =/  assay-linspace-11  (linspace:la [~[11] 5 %i754 ~] [.1 .0] 11)
  %+  is-equal
    canon-linspace-11
  assay-linspace-11

--
