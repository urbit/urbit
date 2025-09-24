::  Get the size of a remote scry file
::
/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =spar:ames] arg)
;<  ~  bind:m  (whey:strandio /whey boq=13 spar)
;<  [spar:ames fragment-size=@ud num-fragments=@ud]
  bind:m  (take-whey:strandio /whey)
;<  ~  bind:m  (yawn:strandio /whey ship^path)
::
=+  kilo=(div (mul (div (bex fragment-size) (bex 3)) num-fragments) (bex 10))
|^
(pure:m !>((crip unit)))
::
++  unit  :: XX some other smarter way to get the right units
  ^-  tape
  ?:  &((lth kilo 1.000) (gte kilo 100))
    "{<kilo>} KB"
  ?.  (gth kilo 1.000)
    ?:  =(1 kilo)  "{<kilo>} byte"
    "{<kilo>} bytes"
  =+  short=(div:rs (sun:rs kilo) (sun:rs 1.000))
  =+  int=(need (toi:rs short))
  =+  dec=(abs:si (need (toi:rs (mul:rs .100 (sub:rs short (san:rs int))))))
  "{<(abs:si int)>}.{<dec>} MB"
--
