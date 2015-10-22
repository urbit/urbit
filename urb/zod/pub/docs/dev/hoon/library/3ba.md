section 3bA, lite number theory
===============================

### `++fu`

    ++  fu                                                  ::  modulo (mul p q)
      |=  a=[p=@ q=@]
      =+  b=?:(=([0 0] a) 0 (~(inv fo p.a) (~(sit fo p.a) q.a)))
      |%

XX document

### `++dif`

      ++  dif
        |=  [c=[@ @] d=[@ @]]
        [(~(dif fo p.a) -.c -.d) (~(dif fo q.a) +.c +.d)]
      ::

XX document

### `++exp`

      ++  exp
        |=  [c=@ d=[@ @]]
        :-  (~(exp fo p.a) (mod c (dec p.a)) -.d)
        (~(exp fo q.a) (mod c (dec q.a)) +.d)
      ::

XX document

### `++out`

      ++  out                                               ::  garner's formula
        |=  c=[@ @]
        %+  add
          +.c
        (mul q.a (~(pro fo p.a) b (~(dif fo p.a) -.c (~(sit fo p.a) +.c))))
      ::

XX document

### `++pro`

      ++  pro
        |=  [c=[@ @] d=[@ @]]
        [(~(pro fo p.a) -.c -.d) (~(pro fo q.a) +.c +.d)]
      ::

XX document

### `++sum`

      ++  sum
        |=  [c=[@ @] d=[@ @]]
        [(~(sum fo p.a) -.c -.d) (~(sum fo q.a) +.c +.d)]
      ::

XX document

### `++sit`

      ++  sit
        |=  c=@
        [(mod c p.a) (mod c q.a)]

XX document
