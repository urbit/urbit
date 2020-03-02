|%
::
++  (Z f)
  %-  <x (f <v (yet x x v)>)>
  <x (f <v (yet x x v)>)>
::
++  fix
  %-  (J J %fix)
  %-  Z
  |=  $
  (yet (J J %fix) <f x (f ($ f) x)>)
::
++  (I x)        x
++  (B f g x)    (f (g x))
++  (C f g x)    (f x g)
++  (seq x y)    y
++  (yet f x y)  (f x y)
++  (ded x)      (fix I x)
::
++  (uni x)      (ded %uni)
++  (con x y f)  (f x y)
++  (car p)      (p <x y x>)
++  (cdr p)      (p <x y y>)
++  (lef x l r)  (l x)
++  (rit x l r)  (r x)
++  (cas x l r)  (x l r)
::
++  (pak n)    (J J K (n (S (S (K S) K)) (S K)))
++  (inc n)    (pak <i z (i (n i z))>)
++  (dec n)    (n <x ?-(x; y (rit 0), y (rit (inc y)))> (lef uni))
++  (fec n)    (cas (dec n) (K 0) I)
++  (add x y)  (pak <i z (x i (y i z))>)
++  (mul x y)  (pak <i z (x (y i) z)>)
++  (sub x y)  (y <z (cas z lef dec)> (rit x))
++  (zer n)    (n (K %.n) %.y)
++  (eql x y)  (cas (sub x y) (K %.n) zer)
++  (bex x)    (x (mul 2) 1)
++  (lsh x n)  (mul (bex x) n)
::
--
