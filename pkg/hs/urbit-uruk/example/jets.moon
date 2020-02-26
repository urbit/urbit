::  J^n for convenience
=/  JJ  (J J)
=/  JJJ  (J J J)
=/  JJJJ  (J J J J)

::  Olegs combinators.
=/  I  (J K (S K K))
=/  B  (JJJ K (S (K S) K))
=/  C  (JJJ K (S (K (S (K (S S (K K))) K)) S))

::  Church Encoded Naturals
=/  zero  (S K)
=/  succ  (S (S (K S) K))

::  Jetted Church Encoded Naturals
=/  pak  (J 16 (S (K (JJ K)) (S (S I (K succ)) (K zero))))
=/  zer  (JJ K zero)
=/  one  (JJ K (succ zero))
=/  inc  (J 1 (S (K pak) (S (S (K S) K))))

::  Pairs
=/  car  (J 13 (S I (K K)))
=/  cdr  (J 14 (S I (K (S K))))

::  Seq (for explicit evaluation order)
=/  seq  (JJ 17 (S K))

::  Delayed Evaluation
=/  W1  (JJ I I)
=/  W2  (JJJ I I)
=/  W3  (JJJJ I I)

::  Z Combinator
=/  Z
  %*  S
    (S (S (K S) K) (K (S W2 I)))
    (S (S (K S) K) (K (S W2 I)))
  ==

::  Fixed-Point Jet
=/  fix
  %-  (JJ K)
  %-  (S I)
  %*  W2
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
  ==

::  Exports List
['I' I B C zero succ pak zer one inc car cdr seq W1 W2 W3 fix]
