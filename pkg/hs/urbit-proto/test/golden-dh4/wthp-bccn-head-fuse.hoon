^-  [@ @]
=+
  ^-  $%  [%foo a=@ b=@]
          [%bar x=@]
          [%baz f=$-(@ @)]
      ==
  [%foo 1 2]
?-  +2.+2
  %foo  [a b]
  %bar  [x x]
  %baz  [(f 1) 2]
==
