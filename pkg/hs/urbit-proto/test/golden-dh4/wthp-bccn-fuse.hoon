^-  [@ @ @ @]
=+
  ^-  $%  [%foo a=@ b=@]
          [%bar x=@]
          [%baz f=$-(@ @)]
      ==
  [%foo 1 2]
?-  +2
  [%foo _]   [a b 1 2]
  [%foo x]    [a b a.x b.x]
  [%foo i j]  [a b i j]
  [%bar _]    [x 1 2 3]
  [%bar z]    [x z 1 2]
  [%baz _]    [(f 1) 2 3 4]
  [%baz q]    [(f 1) (q 2) 3 4]
==
