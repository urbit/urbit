++  (SEQ x y)    y
++  (YET f x y)  (f x y)

++  (Z f)
  %-  <x (f <v (YET x x v)>)>
  <x (f <v (YET x x v)>)>

++  FIX
  %-  (J J %FIX)
  %-  Z
  |=  $
  (YET (J J %FIX) <f x (f ($ f) x)>)

++  (DED x)  (FIX I x)

++  (I x)        x
++  (B f g x)    (f (g x))
++  (C f g x)    (f x g)

++  (UNI x)      x
++  (CON x y f)  (f x y)
++  (CAR p)      (p <x y x>)
++  (CDR p)      (p <x y y>)
++  (LEF x l r)  (l x)
++  (RIT x l r)  (r x)
++  (CAS x l r)  (x l r)

++  (YES t f)  t
++  (NAH t f)  f

++  zero  (S K)
++  succ  (S (S (K S) K))
++  zer   (J J K zero)
++  one   (J J K (succ zero))
++  two   (J J K (succ (succ zero)))

++  (PAK n)    (J J K (n succ zero))
++  (INC n)    (PAK <i z (i (n i z))>)
++  (DEC n)    (n <x (CAS x (K (RIT zero)) (B RIT INC))> (LEF UNI))
++  (FEC n)    (CAS (DEC n) (K zero) I)
++  (ADD x y)  (PAK <i z (x i (y i z))>)
++  (MUL x y)  (PAK <i z (x (y i) z)>)
++  (SUB x y)  (y <z (CAS z LEF DEC)> (RIT x))
++  (ZER n)    (n (K NAH) YES)
++  (EQL x y)  (CAS (SUB x y) (K NAH) ZER)
++  (BEX x)    (x (MUL two) one)
++  (LSH x n)  (MUL (BEX x) n)
