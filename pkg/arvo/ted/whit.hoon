/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =spar:ames] arg)
;<  ~  bind:m  (whit:strandio /whit spar)
;<  [* fragment-size=@ud num-fragments=@ud]
  bind:m  (take-size:strandio /whit)
;<  ~  bind:m  (yawn:strandio /whit spar)
::
=+  kilo=(div (mul (div (bex fragment-size) (bex 3)) num-fragments) (bex 10))
|^
(pure:m !>((crip unit)))
::
++  unit  :: XX some other smarter way to get the right units
  ^-  tape
  ?:  &((lth kilo 1.000) (gte kilo 100))
    "{<kilo>} Kb"
  ?.  (gth kilo 1.000)
    "{<kilo>} bytes"
  =+  short=(div:rs (sun:rs kilo) (sun:rs 1.000))
  =+  int=(need (toi:rs short))
  =+  dec=(abs:si (need (toi:rs (mul:rs .100 (sub:rs short (san:rs int))))))
  "{<(abs:si int)>}.{<dec>} Mb"
--
