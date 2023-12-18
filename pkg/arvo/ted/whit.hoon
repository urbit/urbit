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
(pure:m !>((crip "{<kilo>} Kb")))
