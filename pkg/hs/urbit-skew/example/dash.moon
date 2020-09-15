|%
::
::  I = \x -> x
::
++  I
  ~/  1  I
  (S K K)
::
::  B = \f g x -> f (g x)
::
++  B
  ~/  3  B
  (S (K S) K)
::
::  C = \f g x -> f x g
::
++  C
  ~/  3  C
  (S (K (S (K (S S (K K))) K)) S)
::
++  yes  ~/(2 bol K)
++  nah  ~/(2 bol (S K))
::
++  zer  (S K)
++  suc  (S (S (K S) K))
::
::  pak = \n -> (J J K (n inc zero))
::
++  pak
  ~/  1  pac
  (S (K (J J K)) (S (S I (K suc)) (K zer)))
::
::  inc = \n -> pak (\i z -> i (n i z))
::
++  inc
  ~/  1  inc
  (S (K pak) (S (S (K S) K)))
::
::  car = \p -> p (\x y -> x)
::
++  car
  ~/  1  car
  (S I (K K))
::
::  cdr = \p -> b (\x y -> y)
::
++  cdr
  ~/  1  cdr
  (S I (K (S K)))
::
::  con = \x y f -> f x y
::
++  con
  ~/  3  con
  (S (K (S (K (S (K (S (K (S S (K K))) K)) S)) (S I))) K)
::
::  seq = \x y -> y
::
++  seq
  ~/  2  seq
  (S K)
::
::  yet = \f x y -> f x y
::
++  yet
  ~/  3  yet
  I
::
::  Z = \f -> (\x -> f (\v -> yet x x v)) (\x -> f (\v -> yet x x v))
::
++  Z
  %+  S
    (S (S (K S) K) (K (S yet I)))
  (S (S (K S) K) (K (S yet I)))
::
::  fix = Z (\recur -> yet (J J %fix) (\f x -> f (recur f) x))
::
++  fix
  ~/  2  fix
  %-  (S I)
  %+  yet
    ((S (K ((S (K (J J %fix))) (S I)))) ((S yet) I))
  ((S (K ((S (K (J J %fix))) (S I)))) ((S yet) I))
::
::  ded = \err -> fix I err
::
++  ded
  ~/  1  ded
  (fix I)
::
::  uni = \x -> ded x
::
++  uni
  ~/  1  uni
  ded
::
::  lef = \x l r → l x
::
++  lef
  ~/  3  lef
  (S (K (S (K (S (K K))) (S I))) K)
::
::  rit = \x l r → r x
::
++  rit
  ~/  3  rit
  (S (K (S (K K) (S I))) K)
::
::  cas = \b l r → b l r
::
++  cas
  ~/  3  cas
  I
::
::  dec = \n → n (\x → C x (\y → rit 0) (\y → rit (inc y))) (lef uni)
::
++  dec
  ~/  1  dec
  (S (S I (K (S (S cas (K (K (rit 0)))) (K (S (K rit) inc))))) (K (lef ~)))
::
::  fec = \n → cas (dec n) (K 0) I
::
++  fec
  ~/  1  fec
  (S (S (S (K cas) dec) (K (K 0))) (K I))
::
::  add = \x y → pak (\i z → x i (y i z))
::
++  add
  ~/  2  add
  (S (K (S (K pak))) (S (K S) (S (K (S (K S) K)))))
::
::  mul = \x y → pak (\i z → x (y i) z)
::
++  mul
  ~/  2  mul
  (S (K (S (K pak))) (S (K S) K))
::
::  sub = \x y → y (\z → cas z lef dec) (rit x)
::
++  sub
  ~/  2  sub
  %+  S
    (K (S (S I (K (S (S cas (K lef)) (K dec))))))
  (S (K K) rit)
::
::  zer = \n → n (K nah) yea
::
++  zer
  ~/  1  zer
  (S (S I (K (K no))) (K ya))
::
::  eql = \x y → cas (sub x y) (K nah) zer
::
++  eql
  ~/  2  eql
  (S (S (K S) (S (S (K S) (S (K (S (K cas))) sub)) (K (K (K no))))) (K (K zer)))
::
::  lsh = \exp num → (mul (bex exp) num)
::
++  lsh
  ~/  2  lsh
  (S (K mul) bex)
::
::  bex = \x → x (mul 2) 1
::
++  bex
  ~/  1  bex
  (C (S I (C (C (B seq I) mul) 2)) 1)
::
--
