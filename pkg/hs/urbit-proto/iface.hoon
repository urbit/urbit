=/  num  ?(0 1 2)
=/  neg  `<|num num|>`{0 0, 1 2, 2 1}
=/  point-two-d  {|x num, y num|}
=/  point-tre-d  {|x num, y num, z num|}
=/  my-pt
  |%
  ++  x  0
  ++  y  0
  ++  z  1
  ++  to-string  %str
  --
=/  rot-ninety
  ^-  <|point-two-d point-two-d|>
  <pt/point-two-d {x (neg y:pt), y x:pt}>
=/  has-str  {|to-string $str|}
