  ::  /sur/lagoon
::::  Types for Lagoon compatibility
::
|%
+$  ray               ::  $ray:  n-dimensional array
  $:  =meta           ::  descriptor
      data=@ux        ::  data, row-major order
  ==
::
+$  prec  [a=@ b=@]   ::  fixed-point precision, a+b+1=bloq
+$  meta              ::  $meta:  metadata for a $ray
  $:  shape=(list @)  ::  list of dimension lengths
      =bloq           ::  logarithm of bitwidth
      =kind           ::  name of data type
      fxp=(unit prec) ::  fixed-point scale
  ==
::
+$  kind              ::  $kind:  type of array scalars
  $?  %real           ::  IEEE 754 float
      %uint           ::  unsigned integer
      :: %int2           ::  2s-complement integer
      :: %cplx           ::  BLAS-compatible packed floats
      :: %unum           ::  unum/posit
      :: %fixp           ::  fixed-precision
  ==
::
+$  baum              ::  $baum:  ndray with metadata
  $:  =meta           ::
      data=ndray      ::
  ==
::
+$  ndray             ::  $ndray:  n-dim array as nested list
    $@  @             ::  single item
    (list ndray)      ::  nonempty list of children, in row-major order
::
+$  slice  (unit [(unit @) (unit @)])
--
