  ::  /lib/bits
::::
::  Bitwise aliases for mortals
::
::  This file is not subject to kelvin versioning and the interface should
::  not be considered official.
::
|%
::  ++and
::
::  Compute the bitwise AND of two atoms.
::
::  Alias for +dis.
::
::  Source:
++  and  dis
::  ++or
::
::  Compute the bitwise OR of two atoms.
::
::  Alias for +con.
::
::  Source:
++  or   con
::  ++not
::
::  Compute the bitwise NOT of an atom.
::
::  Passthrough for +not.
::
::  Source:
++  not  ^not
::  ++xor
::
::  Compute the bitwise XOR of two atoms.
::
::  Alias for +mix.
::
::  Source:
++  xor  mix
--