section 2eA, packing
====================

### `++cue`

Unpack atom to noun

    ++  cue                                                 ::  unpack atom to noun
      ~/  %cue
      |=  a=@
      ^-  *
      =+  b=0
      =+  m=`(map ,@ ,*)`~
      =<  q
      |-  ^-  [p=@ q=* r=_m]
      ?:  =(0 (cut 0 [b 1] a))
        =+  c=(rub +(b) a)
        [+(p.c) q.c (~(put by m) b q.c)]
      =+  c=(add 2 b)
      ?:  =(0 (cut 0 [+(b) 1] a))
        =+  u=$(b c)
        =+  v=$(b (add p.u c), m r.u)
        =+  w=[q.u q.v]
        [(add 2 (add p.u p.v)) w (~(put by r.v) b w)]
      =+  d=(rub c a)
      [(add 2 p.d) (need (~(get by m) q.d)) m]
    ::

Produces a noun unpacked from atom `a`. The inverse of jam.

`a` is an [atom]().

    ~zod/try=> (cue 12)
    1
    ~zod/try=> (cue 817)
    [1 1]
    ~zod/try=> (cue 4.657)
    [1 2]
    ~zod/try=> (cue 39.689)
    [0 19]

------------------------------------------------------------------------

### `++jam`

    ++  jam                                                 ::  pack
      ~/  %jam
      |=  a=*
      ^-  @
      =+  b=0
      =+  m=`(map ,* ,@)`~
      =<  q
      |-  ^-  [p=@ q=@ r=_m]
      =+  c=(~(get by m) a)
      ?~  c
        =>  .(m (~(put by m) a b))
        ?:  ?=(@ a)
          =+  d=(mat a)
          [(add 1 p.d) (lsh 0 1 q.d) m]
        =>  .(b (add 2 b))
        =+  d=$(a -.a)
        =+  e=$(a +.a, b (add b p.d), m r.d)
        [(add 2 (add p.d p.e)) (mix 1 (lsh 0 2 (cat 0 q.d q.e))) r.e]
      ?:  ?&(?=(@ a) (lte (met 0 a) (met 0 u.c)))
        =+  d=(mat a)
        [(add 1 p.d) (lsh 0 1 q.d) m]
      =+  d=(mat u.c)
      [(add 2 p.d) (mix 3 (lsh 0 2 q.d)) m]
    ::

Produces an atom unpacked from noun `a`. The inverse of cue.

`a` is a [noun]().

    ~zod/try=> (jam 1)
    12
    ~zod/try=> (jam [1 1])
    817
    ~zod/try=> (jam [1 2])
    4.657
    ~zod/try=> (jam [~ u=19])
    39.689

------------------------------------------------------------------------

### `++mat`

Length-encode

    ++  mat                                                 ::  length-encode 
      ~/  %mat
      |=  a=@
      ^-  [p=@ q=@]
      ?:  =(0 a)
        [1 1]
      =+  b=(met 0 a)
      =+  c=(met 0 b)
      :-  (add (add c c) b)
      (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))

Produces a cell whose tail `q` is atom `a` with a bit representation of
its length prepended to it (as the least significant bits). The head `p`
is the length of `q` in bits.

`a` is an atom.

    ~zod/try=> (mat 0xaaa)
    [p=20 q=699.024]
    ~zod/try=> (met 0 q:(mat 0xaaa))
    20
    ~zod/try=> `@ub`q:(mat 0xaaa)
    0b1010.1010.1010.1001.0000
    ~zod/try=> =a =-(~&(- -) `@ub`0xaaa)
    0b1010.1010.1010
    ~zod/try=> =b =-(~&(- -) `@ub`(xeb a))
    0b1100
    ~zod/try=> =b =-(~&(- -) `@ub`(met 0 a))
    0b1100
    ~zod/try=> =c =-(~&(- -) (xeb b))
    4
    ~zod/try=>  [`@ub`a `@ub`(end 0 (dec c) b) `@ub`(bex c)]
    [0b1010.1010.1010 0b100 0b1.0000]

------------------------------------------------------------------------

### `++rub`

Length-decode

    ++  rub                                                 ::  length-decode
      ~/  %rub
      |=  [a=@ b=@]
      ^-  [p=@ q=@]
      =+  ^=  c
          =+  [c=0 m=(met 0 b)]
          |-  ?<  (gth c m)
          ?.  =(0 (cut 0 [(add a c) 1] b))
            c
          $(c +(c))
      ?:  =(0 c)
        [1 0]
      =+  d=(add a +(c))
      =+  e=(add (bex (dec c)) (cut 0 [d (dec c)] b))
      [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]

The inverse of `++mat`. Accepts a cell of index `a` and a bitstring `b`
and produces the cell whose tail `q` is the decoded atom at index `a`
and whose head is the length of the encoded atom `q`, by which the
offset `a` is advanced. Only used internally as a helper cue.

`a` is an atom.

`b` is a bitstring as an atom.

    ~zod/try=> `@ub`(jam 0xaaa)
    0b1.0101.0101.0101.0010.0000
    ~zod/try=> (rub 1 0b1.0101.0101.0101.0010.0000)
    [p=20 q=2.730]
    ~zod/try=> `@ux`q:(rub 1 0b1.0101.0101.0101.0010.0000)
    0xaaa

------------------------------------------------------------------------
