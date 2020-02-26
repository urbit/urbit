=/  I  (J K (S K K))
=/  B  (J J J K (S (K S) K))
=/  C  (J J J K (S (K (S (K (S S (K K))) K)) S))

=/  zero  (S K)
=/  succ  (S (S (K S) K))

=/  pak  (J 16 (S (K (J J K)) (S (S I (K succ)) (K zero))))
=/  zer  (J J K zero)
=/  one  (J J K (succ zero))
=/  inc  (J 1 (S (K pak) (S (S (K S) K))))

=/  car  (J 13 (S I (K K)))
=/  cdr  (J 14 (S I (K (S K))))

=/  seq  (J J 17 (S K))

[I B C zero succ pak zer one inc car cdr seq]
