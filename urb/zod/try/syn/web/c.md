This chapter covers....

##++bex

    ++  bex                                                 ::::::  binary exponent
      ~/  %bex                                              ::  jet
      |=  a=@                                               ::  gate, atom sample 
      ^-  @                                                 ::  cast result as atom
      ?:  =(0 a)                                            ::  if a is 0
        1                                                   ::  then, produce 1
      (mul 2 $(a (dec a)))                                  ::  mul 2*2 n times 


`++bex` takes an atom `a` and produces `2^a`.

###Summary
`++bex` is a [jetted arm]().  
`++bex` creates a dry `%gold` gate using [|=](), which accepts an atomic sample `a`, [axil @](), with [=, the irregular form of ^=]().  
The result of `++bex` is then cast to an atom with [^-]().  
If `a` is equal to 0, `++bex` produces 1.  
Otheriwse, `++bex` returns the product of 2 and `++bex` with the value of `a` replaced by [dec a]().  

###Examples
    ~ronrem-lonsem/try=> (bex 3)
    8
    ~ronrem-lonsem/try=> (bex 1)
    2
    ~ronrem-lonsem/try=> (bex 42)
    4.398.046.511.104
    ~ronrem-lonsem/try=> (bex 0)
    1


##++can

    ++  can                                                 ::  assemble
      ~/  %can                                              ::  jet
      |=  [a=bloq b=(list ,[p=@ q=@])]                      ::  gate, sample: bloq, list 
      ^-  @                                                 ::  cast result to atom
      ?~  b                                                 ::  if b is ~
        0                                                   ::  then, return 0
      (mix (end a p.i.b q.i.b) (lsh a p.i.b $(b t.b)))      ::  else,

++can accepts a bloq `a` and a list of cells `b`. ++can assembles bloqs of size `a` pulled from the list of atoms `b` and produces an atom.

###Summary
++can is a [jetted arm]() that takes a [bloq]() and [list](), labeled 'a' and 'b' respectively using [=, the irregular form of ^=](). Using [^-](), ++can casts its result to an atom. ++can then uses [?~]() to determine whether the value of 'b' is null, and thus the end of the list. If yes, then ++can returns 0. Otherwise, ++can calls ++mix with two arguments: 1. ++end with [bloq]() size 'a', used 'p' number of times (p.i.b means the 'p' value of the head of list b, so here 'p' is the first 'p' of list 'b'), applied to the first 'q' of list 'b'; and, 2. ++lsh with [bloq]() size 'a',the 'p' of the head of [list]() 'b', and the result of recursively calling ++can with the value of [list]() 'b' now set to its tail.
##Examples
~ronrem-lonsem/try=> `@ub`(can 3 ~[[1 1]])
0b1
~ronrem-lonsem/try=> `@ub`(can 0 ~[[1 255]])
0b1
~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 2]])
0b10
~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 3]])
0b11
~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 4]])
0b0
~ronrem-lonsem/try=> `@ub`(can 1 ~[[2 4]])
0b100


##++cat

    ++  cat                                                 ::  concatenate
          ~/  %cat                                              ::  jet
          |=  [a=bloq b=@ c=@]                                  ::  gate, bloq, 2 @ sample
          (add (lsh a (met a b) c) b)                           ::  add b to c lshifted by # of bloqs in b

++cat accepts a bloq a and two atoms b and c. ++cat produces b and c concatenated obeying the bloq size a.

###Summary
++cat is a [jetted arm]() that takes a [bloq]() 'a', and two atoms of [axil @]() labeled 'b' and 'c' with [=, the irregular form of ^=](). ++cat then uses [++met]() to measure the number of [bloqs]() of size 'a' that comprise 'b'. 'c' is then left-shifted by the same number of [bloqs]() of size 'a', and then added ([++add]()) to 'b'.

###Examples
~ronrem-lonsem/try=> `@ub`(cat 1 1 0)
0b1
~ronrem-lonsem/try=> `@ub`(cat 2 1 0)
0b1
~ronrem-lonsem/try=> `@ub`(cat 4 1 0)
0b1
~ronrem-lonsem/try=> `@ub`(cat 0 1 1)
0b11
~ronrem-lonsem/try=> `@ub`(cat 0 2 1)
0b110
~ronrem-lonsem/try=> `@ub`(cat 2 1 1)
0b1.0001
~ronrem-lonsem/try=> `@ub`256
0b1.0000.0000
~ronrem-lonsem/try=> `@ub`255
0b1111.1111
~ronrem-lonsem/try=> `@ub`(cat 3 256 255)
0b1111.1111.0000.0001.0000.0000
~ronrem-lonsem/try=> `@ub`(cat 2 256 255)
0b1111.1111.0001.0000.0000
~ronrem-lonsem/try=> (cat 3 256 255)
16.711.936


##++cut

    ++  cut                                                 ::  slice
      ~/  %cut                                              ::  jet
      |=  [a=bloq [b=@ c=@] d=@]                            ::  gate, sample: bloq, cell of @, @
      (end a c (rsh a b d))

++cut accepts a bloq a, a cell of atoms b and c and an atom d.
++cut takes the [++tail]() of d right-shifted ([++rsh()) by a bloq of size a, b number of times
??  this sentence should be different, but i'm a bit unclear on what exactly this does. perhaps the examples should be cast as @ub?

###Summary
++cut is a [jetted arm]() that creates a dry %gold gate using [|=](), whose sample takes a [bloq](), a cell of two atoms of[axil @](), labeled 'b' and 'c' respectively, and an atom of [axil @](), labeled 'd'. All of these lables are produced by [=, the irregular form of ^=](). ++cut then [right-shifts (++rsh)]() 'd' by 'b' number of bloqs of size 'a'. ++cut then calls the arm [++end]() to return the tail of the result of [right-shifting]() 'd'. The size of the tail is determined by the number of bloqs 'c' of size 'a'.
###Examples
~ronrem-lonsem/try=> (cut 0 [1 1] 2)
1
~ronrem-lonsem/try=> (cut 0 [2 1] 4)
1
~ronrem-lonsem/try=> (cut 3 [1 1] 256)
1
~ronrem-lonsem/try=> (cut 2 [1 1] 255)
15
~ronrem-lonsem/try=> (cut 1 [1 1] 255)
3
~ronrem-lonsem/try=> (cut 1 [1 2] 255)
15

##++end

    ++  end                                                 ::  tail
      ~/  %end                                              ::  jet 
      |=  [a=bloq b=@ c=@]                                  ::  gate, bloq, 2 @ sample
      (mod c (bex (mul (bex a) b)))                         ::  c % 2^(2^a * b)

++end takes a bloq a, and atoms c and d. ++end returns the tail of c, whose length is determined by the number of bloqs b, of size a

###Summary
++end is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes a [bloq]() and two atoms of [axil @](), labeled 'b' and 'c' with [=, the irregular form of ^=](). ++end returns the remainder of dividing c by the result of [++bex]() of [++bex]() multiplied by b.

###Examples
~ronrem-lonsem/try=> `@ub`12
0b1100
~ronrem-lonsem/try=> `@ub`(end 0 3 12)
0b100
~ronrem-lonsem/try=> (end 0 3 12)
4
~ronrem-lonsem/try=> `@ub`(end 1 3 12)
0b1100
~ronrem-lonsem/try=> (end 1 3 12)
12
~ronrem-lonsem/try=> (end 3 1 256)
0
~ronrem-lonsem/try=> (end 3 1 255)
255


##++lsh

    ++  lsh                                                 ::  left-shift
      ~/  %lsh                                              ::  jet
      |=  [a=bloq b=@ c=@]                                  ::  gate, bloq, 2 @ sample
      (mul (bex (mul (bex a) b)) c)                         ::  c * (2^(2^a * b))


++lsh takes a bloq a and atoms b and c. ++lsh produces c shifted 'b' bloqs of size 'a' to the left.

###Summary
++lsh is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes a [bloq]() and two atoms of [axil @](), labeled 'b' and 'c' with [=, the irregular form of ^=](). ++lsh multiplies 'c' by the result of [++bex]() of the product of [++bex]() of 'a' multiplied by 'b'.

###Examples
~ronrem-lonsem/try=> `@ub`1
0b1
~ronrem-lonsem/try=> `@ub`(lsh 0 1 1)
0b10
~ronrem-lonsem/try=> (lsh 0 1 1)
2

~ronrem-lonsem/try=> `@ub`255
0b1111.1111
~ronrem-lonsem/try=> `@ub`(lsh 3 1 255)
0b1111.1111.0000.0000
~ronrem-lonsem/try=> (lsh 3 1 255)
65.280


##++met

    ++  met                                                 ::  measure
      ~/  %met                                              ::  jet
      |=  [a=bloq b=@]                                      ::  gate, bloq, @ sample
      ^-  @                                                 ::  cast res to @
      =+  c=0                                               ::  c=0 to sample
      |-                                                    ::  trap
      ?:  =(0 b)                                            ::  if b is 0
        c                                                   ::  then return c
      $(b (rsh a 1 b), c +(c))                              ::  sub values, recurse


++met accepts a bloq a and an atom b. ++met produces a count of bloqs of size 'a' in atom 'b'.

###Summary
++met is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes a [bloq]() and an atom of [axil @](), labeled 'b' with [=, the irregular form of ^=](). ++met uses [^-]() to cast its result to an atom of [axil @](), and uses [=+]() to push a variable 'c' onto the sample with a value of 0. Then ++met declares a [trap]() to enable recursion. If 'b' is equal to 0, then 'c' is returned. Otherwise, ++met recurses, this time with the value of 'b' set to the result of right-shifting ([++rsh]()) 'b' by one bloq of size 'a' to the right, with with the value of c set to +(c) [.+](). When the value of 'b' reaches 0, ++met returns the value of 'c', which will represent the number of bloqs of size 'a' b initially posessed.

###Examples
~ronrem-lonsem/try=> (met 0 1)
1
~ronrem-lonsem/try=> (met 0 2)
2
~ronrem-lonsem/try=> (met 3 255)
1
~ronrem-lonsem/try=> (met 3 256)
2


++  rap                                                 ::  assemble nonzero
  ~/  %rap                                              ::  jet
  |=  [a=bloq b=(list ,@)]                              ::  gate, bloq, list in sample
  ^-  @                                                 ::  cast result to @
  ?@  b                                                 ::  if b is an @                                               
    0                                                   ::  then, return 0
  (cat a i.b $(b t.b))                                  ::  else ++cat


++  rep                                                 ::  assemble single
  ~/  %rep
  |=  [a=bloq b=(list ,@)]
  ^-  @
  =+  c=0
  |-
  ?@  b
    0
  (con (lsh a c (end a 1 i.b)) $(c +(c), b t.b))

++rep is a [jetted arm]() that takes a [bloq]() and [list](), labeled 'a' and 'b' respectively using [=, the irregular form of ^=](). ++rep casts its result to an atom using [^-]() and then, using [=+](), it pushes a variable 'c' onto the sample and sets its value equal to 0. In order to loop, ++rep then declares a [trap]() using [|-](). The trap uses [?@]() to test whether 'b' is an atom or not. If yes, ++rep returns 0. Otherwise, ++rep returns ++con, which it passes two arguments: 1. ++lsh with [bloq]() size 'a', number of times 'c', and the [tail] of the head of 'b'; and, 2.  
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  [a=bloq b=@]
  ^-  (list ,@)
  ?:  =(0 b)
    ~
  [(end a 1 b) $(b (rsh a 1 b))]
::


##++rsh

    ++  rsh                                                 ::  right-shift
      ~/  %rsh                                              ::  jet 
      |=  [a=bloq b=@ c=@]                                  ::  gate, bloq, 2 @ sample
      (div c (bex (mul (bex a) b)))                         ::  c / 2^(2^a * b)

++lsh takes a bloq a and atoms b and c. ++lsh produces c shifted 'b' bloqs of size 'a' to the right.

###Summary
++rsh [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes a [bloq]() and two atoms, [axil @](), labeled 'b' and 'c' with [=, the irregular form of ^=](). ++rsh divides 'c' by the result of [++bex]() of the product of [++bex]() of 'a' multiplied by 'b'. 

###Examples
~ronrem-lonsem/try=> `@ub`145
0b1001.0001
~ronrem-lonsem/try=> `@ub`(rsh 1 1 145)
0b10.0100
~ronrem-lonsem/try=> (rsh 1 1 145)
36
~ronrem-lonsem/try=> `@ub`(rsh 2 1 145)
0b1001
~ronrem-lonsem/try=> (rsh 2 1 145)
9


~ronrem-lonsem/try=> `@ub`10
0b1010
~ronrem-lonsem/try=> `@ub`(rsh 0 1 10)
0b101
~ronrem-lonsem/try=> (rsh 0 1 10)
5

~ronrem-lonsem/try=> `@ub`1
0b1
~ronrem-lonsem/try=> (rsh 0 1 1)
0
~ronrem-lonsem/try=> (rsh 0 1 1)
0
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cB, bit logic                ::
::
++  con                                                 ::  binary or
  ~/  %con                                              ::  jet
  |=  [a=@ b=@]                                         ::  gate, 2 @ sample
  =+  [c=0 d=0]                                         ::  c=0, d=0 on sample
  |-  ^-  @                                             ::  trap that returns @
  ?:  ?&(=(0 a) =(0 b))                                 ::  if both a, b are 0
    d                                                   ::  then, return d
  %=  $                                                 ::  else, loop with:
    a   (rsh 0 1 a)                                     ::  a r-shifted 1 bit 
    b   (rsh 0 1 b)                                     ::  b r-shifted 1 bit   
    c   +(c)                                            ::  c incremented
    d   (add d (lsh 0 c ?&(=(0 (end 0 1 a)) =(0 (end 0 1 b))))) Tall form?

++con accepts two atoms a and b and performs an inclusive binary OR.

###Summary
++con is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes two atoms, labeled 'a' and 'b' using [=, the irregular form of ^=](). ++con then pushes two atomic variables onto the subject, labeled 'a' and 'b' again by using [=](). With [^-](), ++con ensures its result is cast to an atom. Subsequently, ++con declares a [trap](), thus allowing it to loop. Using [?:]() and [?&](), ++con checks if both 'a' and 'b' are equal to 0. If yes, then ++con returns 'd'. Else, ++con uses the trap to loop, this time with the values of 'a' and 'b' both [right-shifted]() by one bit, and the value of 'c' now incremented using [+, the irregular form of .+](). The value of 'd'can also change: if either the last bit of 'a' (found using [++end]()) and the last bit of 'b' are equal ([.=]())to 0, then the value of 'd' becomes the sum of d and the result of left-shifting 1 by 'c' number of bits. Otherwise, d remains the same.     

###Examples
?


##++dis

    ++  dis                                                 ::  binary and
      ~/  %dis                                              ::  jet
      |=  [a=@ b=@]                                         ::  gate, 2 @ sample  
      =|  [c=@ d=@]                                         ::  bunt; c & d = 0
      |-  ^-  @                                             ::  trap; cast to @
      ?:  ?|(=(0 a) =(0 b))                                 ::  if a or b are 0
        d                                                   ::  then, return d
      %=  $                                                 ::  else, loop with:
        a   (rsh 0 1 a)                                     ::  a r-shifted 1 bit  
        b   (rsh 0 1 b)                                     ::  b r-shifted 1 bit
        c   +(c)                                            ::  c incremented
        d   (add d (lsh 0 c ?|(=(0 (end 0 1 a)) =(0 (end 0 1 b)))))  :: Tall form?

++dis accepts two atoms a and b and performs a binary AND.

###Summary
++dis is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes two atoms, labeled 'a' and 'b' using [=, the irregular form of ^=](). Then, using [=|](), two tiles, labeled 'c' and 'd', are [bunted]() onto the subject. In order to loop, ++dis then calls a trap with [|-](), whose result is cast to an atom with [^-](). The trap then uses an if statement by using [?:]():it uses [=, the irregular form of .= to check if either 'a' or 'b' is equal to 0 (the inclusive OR statement is called with [?|]()). If either statement returns true, then ++dis produces d. Otherwise, the trap loops, this time with the values of 'a' and 'b' both [right-shifted]() by one bit, and the value of 'c' now incremented with [+, the irregular form of .+](). The value of 'd'can also change: if neither the last bit of 'a' nor the last bit of 'b' are equal ([.=]())to 0, then the value of 'd' becomes the sum of d and the result ofleft-shifting 1 by 'c' number of bits. Otherwise, d remains the same.     

###Examples
~ronrem-lonsem/try=> `@ub`9
0b1001
~ronrem-lonsem/try=> `@ub`5
0b101
~ronrem-lonsem/try=> `@ub`(dis 9 5)
0b1
~ronrem-lonsem/try=> (dis 9 5)
1

~ronrem-lonsem/try=> `@ub`534
0b10.0001.0110
~ronrem-lonsem/try=> `@ub`987
0b11.1101.1011
~ronrem-lonsem/try=> `@ub`(dis 534 987)
0b10.0001.0010
~ronrem-lonsem/try=> (dis 534 987)
530


##++mix

    ++  mix                                                 ::  binary xor
      ~/  %mix                                              ::  jet
      |=  [a=@ b=@]                                         ::  gate, 2 atom sample
      ^-  @                                                 ::  cast result to atom
      =+  [c=0 d=0]                                         ::  c=0, d=0 to sample
      |-                                                    ::  trap
      ?:  ?&(=(0 a) =(0 b))                                 ::  If 'a' & 'b' are 0
        d                                                   ::  Then, return 0, (d)
      %=  $                                                 ::  Recall ++ mix with:
        a   (rsh 0 1 a)                                     ::  a r-shifted 1 bit
        b   (rsh 0 1 b)                                     ::  b r-shifted 1 bit
        c   +(c)                                            ::  c incremented
        d   (add d (lsh 0 c =((end 0 1 a) (end 0 1 b))))    ::  Tall form?

++mix accepts two atoms a and b and performs an exclusive binary OR.

##Summary
++mix is a [jetted arm]() which creates a dry %gold gate using [|=](), whose sample takes two atoms, labeled 'a' and 'b' using [=, the irregular form of ^=](). ++mix then uses [^-]() to cast its result to an atom. ++mix then pushes two atomic variables onto the subject, labeled 'a' and 'b' again by using [^-](). Subsequently, ++mix declares a [trap](), thus allowing it to loop. Using [?:]() and [?&](), ++mix checks if both 'a' and 'b' are equal to 0. If yes, then ++mix returns 'd'. Else, ++mix uses the trap to loop, this time with the values of 'a' and 'b' both [right-shifted]() by one bit, and the value of 'c' now incremented using [+, the irregular form of .+](). The value of 'd' is also replaced with the sum of 'd' and the result of [=(), the irregular form of .=]() then [left-shifted]() by 1 bit 'c' number of times.

###Examples:
~ronrem-lonsem/try=> `@ub`2
0b10
~ronrem-lonsem/try=> `@ub`3
0b11
~ronrem-lonsem/try=> `@ub`(mix 2 3)
0b1
~ronrem-lonsem/try=> (mix 2 3)
1

~ronrem-lonsem/try=> `@ub`(mix 2 2)
0b0
~ronrem-lonsem/try=> (mix 2 2)
0

