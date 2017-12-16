::  /mar/collections/config/hoon
::
/-  *collections
|_  col=collections
::
::
++  grow
  |%
  ++  elem  :: web display
    ;div
      ;h1: Collections:
      ;ul
        ;*  %+  turn  (sort ~(tap by col) dor)
            |=  [top=@da collection]
            ;li:a/"{<top>}/":"{(trip desc.conf)}"
      ==
    ==
  --
::
++  grab
  |%
  ++  noun  collections
  --
--
