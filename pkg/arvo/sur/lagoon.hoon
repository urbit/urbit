  ::  /sur/lagoon
::::  Types for Lagoon compatibility
::
|%
+$  ray               ::  $ray:  n-dimensional array
  $:  =meta           ::  descriptor
      data=@ux        ::  data, row-major order, 1-pin MSB
  ==
::
+$  prec  [a=@ b=@]   ::  fixed-point precision, a+b+1=bloq
+$  meta              ::  $meta:  metadata for a $ray
  $:  shape=(list @)  ::  list of dimension lengths
      =bloq           ::  logarithm of bitwidth
      =kind           ::  name of data type
      tail=*          ::  placeholder for future data (jet convenience)
  ==
::
+$  kind              ::  $kind:  type of array scalars
  $?  %i754           ::  IEEE 754 float
      %uint           ::  unsigned integer
      %int2           ::  2s-complement integer (/lib/twoc)
      :: %cplx           ::  BLAS-compatible packed floats
      :: %unum           ::  unum/posit  @ruw, @ruh, @rub
      :: %fixp           ::  fixed-precision (/lib/fixed)
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
