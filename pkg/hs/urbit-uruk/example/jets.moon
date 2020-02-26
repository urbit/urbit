=/  JJ  (J J)
=/  JJJ  (J J J)
=/  JJJJ  (J J J J)

=/  I  (J K (S K K))
=/  B  (JJJ K (S (K S) K))
=/  C  (JJJ K (S (K (S (K (S S (K K))) K)) S))

=/  zero  (S K)
=/  succ  (S (S (K S) K))

=/  pak  (J 16 (S (K (JJ K)) (S (S I (K succ)) (K zero))))
=/  zer  (JJ K zero)
=/  one  (JJ K (succ zero))
=/  inc  (J 1 (S (K pak) (S (S (K S) K))))

=/  car  (J 13 (S I (K K)))
=/  cdr  (J 14 (S I (K (S K))))

=/  seq  (JJ 17 (S K))

=/  W1  (JJ I I)
=/  W2  (JJJ I I)
=/  W3  (JJJJ I I)

=/  Z  %*  S
         (S (S (K S) K) (K (S W2 I)))
         (S (S (K S) K) (K (S W2 I)))
       ==

=/  fix
  %-  (JJ K)
  %-  (S I)
  %*  W2
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
    ((S (K ((S (K (JJ K))) (S I)))) ((S W2) I))
  ==

[I B C zero succ pak zer one inc car cdr seq W1 W2 W3 fix]
