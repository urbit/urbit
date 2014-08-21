chapter 2c, simple noun surgery

section 2cA, bit surgery

##++bex  

Produces 2 to the nth power for some atom `n`.

####Summary
     
        Activate jet.
        Build dry %gold gate with sample atom `a`.
        Yield an atom.
        If: `a` is 0,
          Then: produce 1.
        Else: toss `a` for (dec a).  

####Examples:

        ~palryp-hocsyt/try=> (bex 4)
        16
        ~palryp-hocsyt/try=> (bex (add 19 1))
        1.048.576
        ~palryp-hocsyt/try=> (bex 0)
        1

##++xeb
  
Takes the base-2 logarithm of an atom.

        Build dry %gold gate with sample atom `a`
        Casts the result to an atom.
        Evaluates the logarithm by using `++met` to count the number of bits the number in question occupies.

####Examples:

        ~palryp-hocsyt/try=> (xeb 31)
        5
        ~palryp-hocsyt/try=> (xeb 32)
        6
        ~palryp-hocsyt/try=> (xeb 49)
        6
        ~palryp-hocsyt/try=> (xeb 0)
        0
        ~palryp-hocsyt/try=> (xeb 1)
        1
        ~palryp-hocsyt/try=> (xeb 2)
        2

##++can
  
Assembles an atom. 
 
####Summary

        Activate jet.
        Build dry %gold gate with a sample with bloq (atom) size `a`, and a list of cells of two atoms, `b`.
        Yield an atom.
        If: `b` is null.
          Then: produce 0.
        Else: call `++mix` with both the product of `++end` slammed with `a`, `p.i.b`, and
        `q.i.b`, and the product of `++lsh` slammed with `a`, `p.i.b`, and the toss of
        `b` for `t.b`. 
 
####Examples:
        
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

Concatenates two atoms, obeying the given block size.

####Summary

        Activate jet.
        Build dry %gold gate with sample which accepts a bloq (atom) size `a` and
        two atoms, `b` and `c`. 
        Use `++met` to measure the number of blocks of size `a` in `b`.
        Left shift `c` by the number of blocks measured above, using block size `a`.
        Sums the result of the left shift above with `b`.

####Examples:

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

Accepts a block size 'a', a cell of two atoms 'b' and 'c' and another atom 'd'.
Produces the tail of 'd' that is 'c' blocks long after right-shifting 'd' 'b'-blocks.
        
####Summary

        Activate jet.
        Build dry %gold gate with sample that accepts a block (atom) size `a`,
        a cell of two atoms, `b` and `c`, and an atom `d`.
        Right-shifts `d` by `b` blocks.
        Slams `++end` with `a`, `c`, and the result of the right-shift above.

####Examples:

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
        
++end takes a bloq `a`, and atoms `c` and `d`. It returns the tail of `c`, whose length is determined by the number of bloqs `b`, of size `a`
  
####Summary

        Build dry %gold gate that accepts a block `a`, and two atoms, `b` and `c`.
        Produces the `b` blocks of length `a` on the end of `c`.
        Activate jet.
        Build dry %gold gate with sample that accepts a block (an atom) `a` and
        two atoms, `b` and `c`. 
        Multiplies the binary exponent of `a` (2^a) with `b`, then takes the binary exponent
        of that (2^((2^a)*b)) to finally produce the modulus of 'c' and the ensuing product.

####Examples
    
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
 
##++fil  
        
fill bloqstream

####Summary
        
        Create a dry %gold gate with a sample which accepts a block (atom) size `a a        nd two other atoms, `b` and `c`. 
        Push `n` is 0.
        Push `d` is `c`.
        Kick a dry %gold trap.
        Yield an atom.
        If: n=b.
          Then: produce the right-shift of 'd' by one block.
        Else: recursively call the trap with `d` replaced by the sum of `c` and the one block
        left-shift of `b`, `n` replaced by the increment of `n`.

####Examples:

        ~sivtyv-barnel/try=> `@t`(end 3 2 %abc)
        'ab'
        ~sivtyv-barnel/try=> `@t`(fil 3 5 %a)
        'aaaaa'
        ~sivtyv-barnel/try=> `@t`(fil 5 10 %babe)
        'babebabebabebabebabebabebabebabebabebabe'
        ~sivtyv-barnel/try=> `@tas`(fil 5 10 %babe)
        %babebabebabebabebabebabebabebabebabebabe
        ~sivtyv-barnel/try=> `@tas`(fil 4 10 %babe)
        %ĆĆĆĆĆĆĆĆĆĆbe
        ~sivtyv-barnel/try=> `@tas`(fil 4 10 %bf)
        %bfbfbfbfbfbfbfbfbfbf
        
##++lsh
  
Accepts a block size `a` and two atoms `b` and `c`.  Produces `c` left-shifted 
`b` times by the block size.

####Summary

        Activate jet.
        Build dry %gold gate with a sample that accepts a block (atom) size and
        two atoms, `b` and `c`.
        Multiplies `c` times the binary exponent of the binary exponent of `a` times         `b` this producing the desired left-shift on 'c'.
  
####Examples:

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
  
Measures the number of blocks of size `a` in `b`.

####Summary

        Activate jet.
        Build dry %gold gate with sample that accepts a block (atom) size `a` and
        an atom `b`. 
        Casts the result to an atom.
        Let 'c' be 0.
        Creates and kicks a dry %gold trap.
        If: b=0,
          Then: produce c.
        Else: toss `b` replaced by the single-block right-shift of `b` and `c` by the increment of `c`.

####Examples:

        ~ronrem-lonsem/try=> (met 0 1)
        1
        ~ronrem-lonsem/try=> (met 0 2)
        2
        ~ronrem-lonsem/try=> (met 3 255)
        1
        ~ronrem-lonsem/try=> (met 3 256)
        2

##++rap  
  
Concatenate a list of atoms while obeying a given blocksize.

####Summary

        Activate jet.
        Build a dry %gold gate with sample bloq `a`, list of atoms `b`
        Yield atom
        If: `b` is null,
                Then: Produce 0.
        Else: Produce cat slammed with `a`, the head of `b`, and the toss of `b` for the tail of `b`

####Examples:

        ~palryp-hocsyt/try=> (rap 2 (limo [1 2 3 4 ~]))
        17.185
        ~palryp-hocsyt/try=> (rap 1 (limo [1 2 3 4 ~]))
        313
        ~palryp-hocsyt/try=> (rap 0 (limo [0 0 0 ~]))
        0
        ~palryp-hocsyt/try=> (rap 0 (limo [0 0 1 ~]))
        1

##++rep  
       
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        a list of atoms.
        Casts the result to an atom.
        Let 'c' be 0.
        Creates and kicks a dry %gold trap.
        Build an if-then-else statement on "b is an atom."
        If so, produce 0.
        Else, produce the binary logical OR of the 'c' time left-shift on the last block
        of the head of 'b' and the recursive call of the trap with:
        'c' replaced by the increment of 'c'.
        'b' replaced by the tail of 'b'.

####Examples:

        ~palryp-hocsyt/try=> (rep 0 (limo [1 2 3 4 ~]))
        5
        ~palryp-hocsyt/try=> (rep 1 (limo [1 2 3 4 ~]))
        57
        ~palryp-hocsyt/try=> (rep 0 (limo [1 0 0 ~]))
        1
        ~palryp-hocsyt/try=> (rep 0 (limo [1 0 0 0 ~]))
        1
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 0 ~]))
        2
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 1 ~]))
        10
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 1 0 1 ~]))
        42

##++rip
  
Produces a list of the bits of an atom, in little endian order, according to
block size.


####Summary

        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and 
        any number.
        Cast the result to a list of atoms.
        Builds an if-then-else statement on b=0.
        If so, produce null.
        Else, produce a tuple with head of (end a 1 b), the single-block tail of 'b', and
        the resursive call of rip with 'b' replaced by the single-block right-shift of 'b'.
        in little endian.

####Examples:

        palryp-hocsyt/try=> `@ub`155
        0b1001.1011
        ~palryp-hocsyt/try=> (rip 0 155)
        ~[1 1 0 1 1 0 0 1]
        ~palryp-hocsyt/try=> (rip 2 155)
        ~[11 9]
        ~palryp-hocsyt/try=> (rip 1 155)
        ~[3 2 1 2]
        ~palryp-hocsyt/try=> `@ub`256
        0b1.0000.0000
        ~palryp-hocsyt/try=> (rip 0 256)
        ~[0 0 0 0 0 0 0 0 1]
        ~palryp-hocsyt/try=> (rip 2 256)
        ~[0 0 1]
        ~palryp-hocsyt/try=> (rip 3 256)
        ~[0 1]
        
##++rsh
  
Accepts a block size 'a' and two atoms, 'b' and 'c'.  Right-shifts 'c' by 'b' blocks
of size 'a'.

####Summary

        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        two atoms.
        Takes the binary exponent of the binary exponent of 'a' multiplied by 'b',
        that is (2^(((2^a)*b))) and divides 'c' by it, producing the desired 
        right-shift on 'c'.

####Examples:

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

##++swap
 
Produces the reversed block order of a number, obeying block number.
Switches little ending to big and vice versa.

####Summary

        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        an atom.
        Rips apart the atom by the block size, then reverses the tape that is produced.
        Once it is reversed, it is re-assembled using rep.

####Examples:

        ~palryp-hocsyt/try=> `@ub`24
        0b1.1000
        ~palryp-hocsyt/try=> (swap 0 24)
        3
        ~palryp-hocsyt/try=> `@ub`3
        0b11
        ~palryp-hocsyt/try=> (swap 0 0)
        0
        ~palryp-hocsyt/try=> (swap 1 24)
        9
        ~palryp-hocsyt/try=> (swap 0 128)
        1

section 2cB, bit logic                

##++con  

Produces the bit-wise logical OR of two atoms.

####Summary

        Activate jet.
        Creates a dry %gold gate with a sample which accepts two atoms.
        Let 'c' be 0, d be 0.
        Creates and kicks a dry %gold trap.  Casts the result to an atom.
        Builds an if-then-else statement on a=b AND b=0.
        If so, produce 'd'.
        Else, recursively call the trap with:
        'a' replaced by the single 0-block right shift of 'a'.
        'b' replaced by the single 0-block right-shift of 'b'.
        'c' replaced by the increment of c.
        'd' replaced by the sum of 'd' and the 'c' 0-block left-shift of the
            logical AND of (last bit of 'a')=0 AND (last bit of 'b')=0.
        (==) terminates the list of changes.

####Examples:

        ~palryp-hocsyt/try=> (con 0 1)
        1
        ~palryp-hocsyt/try=> (con 1 0)
        1
        ~palryp-hocsyt/try=> (con 0 0)
        0
        ~palryp-hocsyt/try=> (con 4 4)
        4
        ~palryp-hocsyt/try=> (con 10.000 234)
        10.234

##++dis
  
Produces the bit-wise logical AND of two atoms.

####Summary

        Activate jet.
        Creates a dry %gold gate with a sample which accepts two atoms.
        Pushes the bunt of a two atom tuple onto the subject.
        Creates and kicks a dry %gold trap.  Casts its result to an atom.
        Builds an if-then-else statement on a=0 or b=0.
        If so, produce 'd'.
        Else, recursively call the trap with:
        'a' replaced by the single 0-block right-shift of 'a'.
        'b' replaced by the single 0-block right-shift of 'b'.
        'c' replaced by the increent of 'c'.
        'd' replaced by the sum of 'd' and the 'c' 0-block left-shift of the
        logical OR of (last bit of 'a')=0 OR (last bit of 'b')=0.
        (==)  terminates the list of changes.
  
####Examples:

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

Produces the bit-wise logical exclusive OR of two atoms.

####Summary

        Activate jet.
        Creates a dry %gold gate with a sample which accepts two atoms.
        Casts the result to an atom.
        Let 'c' be 0, 'd' be 0.
        A dry %gold trap is created and kicked.
        Builds an if-then-else statement on a=0 AND b=0.
        If so, produce 'd'.
        Else, recursively call mix with:
        'a' replaced by 
        'b' replaced by
        'c' replaced by the increment of 'c'.
        'd' replaced by
        (==) terminates the list of changes.

####Examples:

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

##++not  

Produces the bit-wise logical NOT over 'b' blocks of the given blocksize.

####Summary

        First produces the binary exponent of the binary exponent of the block size times       
        'b'.  This is decremented before being multiplied by 'c'.
        Finally, this product is exclusive ORed and produced.

####Examples:

        ~palryp-hocsyt/try=> `@ub`24
        0b1.1000
        ~palryp-hocsyt/try=> (not 0 5 24)
        7
        ~palryp-hocsyt/try=> `@ub`7
        0b111
        ~palryp-hocsyt/try=> (not 2 5 24)
        1.048.551
        ~palryp-hocsyt/try=> (not 2 5 1.048.551)
        24
        ~palryp-hocsyt/try=> (not 1 1 (not 1 1 10))
        10

section 2cC, noun orders              

##++aor
  
Alphabetic comparator gate.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Builds an if-then-else statement on a=b.
        If so, produce true.
        Else, build an unless-then-else statement on "a is an atom."
        If 'a' is not an atom, build an unless-then-else statement on "b is an atom."
        If so, build an if-then-else statement on (-.a=-.b) where -.a is the head of 'a'.
        If so, recursively call aor with 'a' and 'b' replaced by their respecitve tails.
        Else, recursively call aor with 'a' and 'b' replaced by their respective heads.
        Else (if 'b' is an atom), produce false.
        Else (if 'a' is an atom), build an unless-then-else statement on "b is an atom."
        If so, produce true.
        Else, create and kick a dry %gold gate.
        Let 'c' be the byte tail of 'a', 'd' be the byte tail of 'b'.
        Builds an if-then-else statement on (c=d).
        If so, produce the recursive call to the trap with 'a' replaced by the byte right-shift of 'a'
        and 'b' replaced by the byte right-shift of 'b'.
        Else (if !(c=d)), produce (c<d).

####Examples:

       ~tadbyl-hilbel/try=> (aor 'a' 'b')
        %.y
        ~tadbyl-hilbel/try=> (aor 'b' 'a')
        %.n
        ~tadbyl-hilbel/try=> (aor "foo" "bar")
        %.n
        ~tadbyl-hilbel/try=> (aor "bar" "foo")
        %.y
        ~tadbyl-hilbel/try=> (aor "abcdefz" "abcdefa")
        %.n
        ~tadbyl-hilbel/try=> (aor "abcdefa" "abcdefz")
        %.y
        ~tadbyl-hilbel/try=> (aor 10.000 17.000)
        %.y
        ~tadbyl-hilbel/try=> (aor 10 9)
        %.n

##++dor  

Numeric comparator gate.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Builds an if-then-else statement on a=b.
        If so, produce true.
        Else, build an unless-then-else statement on "a is an atom."
        If 'a' is not an atom, build an unless-then-else statement on "b is an atom."
        If so, build an if-then-else statement on (-.a=-.b) where -.a is the head of 'a'.
        If so, recursively call dor with 'a' and 'b' replaced by their respecitve tails.
        Else, recursively call dor with 'a' and 'b' replaced by their respective heads.
        Else ('b' is an atom), produce false.
        Else ('a' is an atom), build an unless-then-else statement on "b is an atom."
        If so, produce true.
        Else, produce (a<b).

####Examples:

        ~tadbyl-hilbel/try=> (dor 1 2)
        %.y
        ~tadbyl-hilbel/try=> (dor 2 1)
        %.n
        ~tadbyl-hilbel/try=> (dor ~[1 2 3] ~[1 2 4])
        %.y
        ~tadbyl-hilbel/try=> (dor ~[1 2 4] ~[1 2 3])
        %.n
        ~tadbyl-hilbel/try=> (dor (limo ~[99 100 10.000]) ~[99 101 10.000])
        %.y
        ~tadbyl-hilbel/try=> (dor ~[99 101 10.999] (limo ~[99 100 10.000]))
        %.n

##++gor  

        Hash comparator gate.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Let 'c' be the mug (FNV-1a hash) of 'a' an 'd' the mug of 'b'.
        Create an if-then-else statement on c=d.
        If so, produce the d-order of 'a' and 'd'.
        Else, produce the loobean (c<d).

####Examples

        ~palryp-hocsyt/try=> (gor 'd' 'c')
        %.y
        ~palryp-hocsyt/try=> 'd'
        'd'
        ~palryp-hocsyt/try=> 'c'
        ~palryp-hocsyt/try=> `@ud`'d'
        100
        ~palryp-hocsyt/try=> `@ud`'c'
        99
        ~palryp-hocsyt/try=> (mug 'd')
        1.628.185.714
        ~palryp-hocsyt/try=> (mug 'c')
        1.712.073.811
        ~palryp-hocsyt/try=> (gor 'd' 'c')
        %.y
        ~palryp-hocsyt/try=> (gor 'c' 'd')
        %.n
        ~palryp-hocsyt/try=> (gor "foo" "bar")
        %.n
        ~palryp-hocsyt/try=> (gor (some 10) (limo [1 2 3 ~]))
        %.n

##++hor
  
Recursive hash comparator gate.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Build an if-then-else statement on "a is an atom."
        If so, build na if-then-else statment on "b is an atom."
        If so, produce  the g-order of 'a' and 'b'.
        Else (if 'b' is not an atom), produce true.
        Else (if 'a' is not an atom), build an if-then-else statement on "b is an atom."
        If so, produce false.
        Else, build an if-then-else statement on (-.a=-.b), where '-.a' is the head of 'a'. 
        If so, produce the g-order of the tails of 'a' and 'b'.
        Else (if the heads of 'a' and 'b' are not equal), produce the g-order of the tails of 'a', 'b'.

####Examples

---
        
##++vor

Double hash comparator gate.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Let 'c' be the double mug (FNV-1a hash) of 'a', 'd' that of 'b'.
        Builds an if-then-else statement on (c=d).
        If so, produce the d-order of 'a' and 'b'.
        Else, produce the loobean of (c<d).

####Examples:

        ~palryp-hocsyt/try=> (vor 'f' 'g')
        %.y
        ~palryp-hocsyt/try=> (vor 'a' 'z')
        %.n
        ~palryp-hocsyt/try=> (vor 43.326 41.106)
        %.n

section 2cD, insecure hashing         

##++fnv

Hashes an atom with the 32-bit FNV non-cryptographic hash algorithm.
Multiplies 'a' by the prime number 16,777,619 and then takes the block of
size 5 off the product's end.

####Examples

        ~palryp-hocsyt/try=> (fnv 10.000)
        272.465.456
        ---
        ~palryp-hocsyt/try=> (fnv 10.001)
        289.243.075
        ---
        ~palryp-hocsyt/try=> (fnv 1)
        16.777.619

##++mug

Hashes any noun with the 31-bit nonzero FNV-1a non-cryptographic hash algorithm.
        
####Summary

        Activate jet.
        Creates a dry %gold gate with a sample accepting any noun.
        (?^ and 'p' subsection fill in.)      
        Let 'b' be 2,166,136,261.
        Create and kick a dry %gold gate.  Cast its result to an atom.
        Let 'c' be 'b'.
        Let 'd' be 0, 'e' be the number of bytes in 'a'.
        Create and kick a dry %gold gate.  Cast its result to an atom.
        Builds an if-then-else statement on d=e.
        If so, let 'f' be the bit-wise XOR on the 31 0-block shift on 'c' and the last
        31 blocks of 'c'.
        Builds an unless-then-else statement on f=0.  If then, produce 'f'.
        Else, recursively call the trap above our current trap with 'b' replaced by +(b).
        Else (d isn't equal to e), then recursively call the trap with 'c' replaced by
        the fnv hash of the logical XOR of 'c' and (cut 3 [d 1] a), 'd' replaced by
        the increment of 'd'.
        
####Examples

        ~palryp-hocsyt/try=> (mug 10.000)
        178.152.889
        ~palryp-hocsyt/try=> (mug 10.001)
        714.838.017
        ~palryp-hocsyt/try=> (mug 1)
        67.918.732
        ~palryp-hocsyt/try=> (mug (some 10))
        1.872.403.737
        ~palryp-hocsyt/try=> (mug (limo [1 2 3 4 5 ~]))
        1.067.931.605

section 2cE, phonetic base            

##++po
        
Provides the phonetic syllables and name generators for the Urbit naming system.

####Summary

        Activate jet.
        Create the cell [sis dex] where 'sis' and 'dex' are the togas on the 
        left-hand ("sinister") and right-hand ("Dexter") phonetic syllable cords, respectively.
        Build %gold core to contain the following arms.

##++ind 
        
####Summary

        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Let 'b' be 0.
        Creates and kicks a dry %gold trap, casting the result to an atomic unit.
        Builds an if-then-else statement on (b=256).  If so, produce null.
        Else, build an if-then-else statement on (a=(tod b)).  If so, produce the atomic unit [~ b].
        Else, recursively call the trap with 'b' replaced by the increment of 'b'.

  
##++ins 
        
####Summary

        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Let 'b' be 0.
        Creates and kicks a dry %gold trap with the result cast to an atomic unit.
        Builds an if-then-else statement on (b=256). If so, produce null.
        Else, build an if-then-else statement on (a=(tos b)).  If so, produce the atomic unit [~ b].
        Else, recursively call the trap with 'b' replaced by the increment of 'b'.

##++tod
 
Selects right-hand phonetic syllable from 'dex'.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Assert that 'a' is less than 256.
        Produce the three tail-end byte blocks in the rght-shift of dex.

####Examples

        ~palryp-hocsyt/try=> (tod:po 98)
        6.514.020
        ~palryp-hocsyt/try=> (tod:po 150)       
        6.781.298
        ~palryp-hocsyt/try=> (tod:po 255)
        7.562.598
        ~palryp-hocsyt/try=> (tod:po 256)
        ! exit

##++tos 

        Selects left-hand phonetic syllable from 'sin'.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Assert that 'a' is less than 256.
        Produce the three tail-end byte blocks in the rght-shift of dex.

####Examples

section 2cF, signed and modular ints  

##++si    


##++abs 

Produces the absolute value of a signed integer.

####Summary

        Creates a dry %gold gate with a sample which accepts a single atom.
        Sums the last bit of the atom with the single bit-wise block right-shift of the atom,
        producing the absolute value.
####Examples

        ~palryp-hocsyt/try=> (abs:si -2)
        2
        ~palryp-hocsyt/try=> (abs:si -10.000)
        10.000
        ~palryp-hocsyt/try=> (abs:si --2)
        2

#++dif 

Produces the difference between two signed integers.

####Summary

        Creates a dry %gold gate with a sample which accepts two signed integers.
        Sums the first signed integer with a new signed integer, made from the second by
        (new !(syn b) (abs b)), where !(syn b) is the negative of the second integer's sign.
        This sum, produced, is the difference.

####Examples

        ~palryp-hocsyt/try=> (dif:si --10 -7)
        --17
        ~palryp-hocsyt/try=> (dif:si --10 --7)
        --3
        ~palryp-hocsyt/try=> (dif:si `@s`0 --7)
        -7
        ~palryp-hocsyt/try=> (dif:si `@s`0 `@s`7)
        --4

##++dul 

Produces the modulus of two signed integers.

####Summary

        Creates a dry %gold gate which accepts a signed integer and an atom.
        Let 'c' be the [sign value] representation of 'a'.
        Builds an if-then-else statement on -.c, the sign of 'a'.
        If so ('a' is positive.), produce the modulus of the absolute value of 'c' and 'b'.
        Else, produce the differenece between 'b' and the absolute value of 'c'.

####Examples

        ~palryp-hocsyt/try=> (dul:si --9 3)
        0
        ~palryp-hocsyt/try=> (dul:si --9 4)
        1
        ~palryp-hocsyt/try=> (dul:si --9 5)
        4
        ~palryp-hocsyt/try=> (dul:si --9 6)
        3
        ~palryp-hocsyt/try=> (dul:si --90 --10)
        10

##++fra 

Produces the quotient of two signed integers.

####Summary

        Creates a dry %gold gate with a sample which accepts two signed integers.
        Divides the absolute value of 'a', the dividend, and 'b', the divisor, and
        passes that value as the unsigned integer value of a new signed integer.
        The sign of the new signed integer is the bitwise logical XOR of the two integer's 
        signs, meaning the quotient is only positive when both factors are positive.
        This new signed integer is produced.
####Examples
        ~palryp-hocsyt/try=> (fra:si --4 --2)
        --2
        ~palryp-hocsyt/try=> (fra:si -4 -2)
        --2
        ~palryp-hocsyt/try=> (fra:si -4 --2)
        -2
        ~palryp-hocsyt/try=> (fra:si --4 -2)
        -2
        ~palryp-hocsyt/try=> (fra:si `@s`4 `@s`2)
        --2
        ~palryp-hocsyt/try=> (fra:si `@s`4 2)
        ! type-fail
        ! exit

##++new 

[sign value] to @s

####Summary

        Produces a signed integer from a sign value (either & or |) and an atom.
        Creates a dry %gold gate with a sample which acccepts a loobean and an atom
        Builds an if-then-else statement on the sign value 'a'.
        If so, just produce 'b' multiplied by 2.
        Else, build an if-then-else statement on b=0.  If so, produce 0.
        Else, produce the increment of (2*(dec b)).
        The result is then cast to an integer and produced from new:si.

####Examples

        ~palryp-hocsyt/try=> (new:si [& 10])
        --10
        ~palryp-hocsyt/try=> (new:si [| 10])
        -10
        ~palryp-hocsyt/try=> (new:si [%.y 7])
        --7

####++old

Produces the cell [sign value] representations of a signed integer.

####Summary

        Create a dry %gold date with a with a sample which accepts a signed integer.
        Produce a cell with head (syn a), the sign of 'a', and tail (abs), the absolute value of 'a'.

####Examples

        ~palryp-hocsyt/try=> (old:si 7)
        ! type-fail
        ! exit
        ~palryp-hocsyt/try=> (old:si -7)
        [%.n 7]
        ~palryp-hocsyt/try=> (old:si --7)
        [%.y 7]
        ~palryp-hocsyt/try=> (old:si `@s`7)
        [%.n 4]
        ~palryp-hocsyt/try=> (old:si -0)
        [%.y 0]

##++pro       

Produces the product of two signed integers.

####Summary

        Creates a dry %gold gate with a sample which accepts two signed integers.
        Produces their product by evaluating a new signed integer whose sign is the bitwise 
        XOR of the two number's signs and whose value is the product of their two absolute values.

####Examples

        palryp-hocsyt/try=> (pro:si -4 --2)
        -8
        ~palryp-hocsyt/try=> (pro:si -4 -2)
        --8
        ~palryp-hocsyt/try=> (pro:si --10.000.000 -10)
        -100.000.000
        ~palryp-hocsyt/try=> (pro:si -1.337 --0)
        --0

##++rem 

Produces the remainder from a division of two signed integers.

####Summary

        Creates a dry %gold gate with a sample which accepts two signed integers.
        Produces the difference between 'a' and the (b*(a/b)).

####Examples

        ~palryp-hocsyt/try=> (rem:si -10 -4)
        -2
        ~palryp-hocsyt/try=> (rem:si --10 --4)
        --2
        ~palryp-hocsyt/try=> (rem:si --10 -4)
        --2
        ~palryp-hocsyt/try=> (rem:si --7 --3)
        --1
        ~palryp-hocsyt/try=> (rem:si --0 --10.000)
        --0

#++sum 

Sum two signed integers.

####Summary

        Creates a dry %gold gate which accepts two signed integers.
        Prints '%si-sum' in the stack trace if the following code crashes.
        Let 'c' and 'd' be the [sign value] representation of 'a' and 'b', respectively.
        Builds an if-then-else statement on "c is positive".
        If so, build an if-then-else statement on "d is positive".
        If so, produce a new, positive signed integer with value ((abs a)+(abs b))
        Else, build an if-then-else statement on (abs a)>=(abs b)
        If so, produce a new, positive integer with value ((abs a)-(abs d)).
        Else (if !((abs a)>=(abs b))), produce a new, negative signed integer
        with value ((abs d)-(abs c)).
        Else (if c is not positive), build an if-then-else statement on "d is positive".
        If so, build an if-then-else statement on (abs a)>=(abs b).
        If so, produce a new, negative signed intger with value ((abs a)-(abs b))
        Else, produce a new, positive signed integer with value ((abs c)-(abs d))
        Else (if d is not positive), produce a new, negative signed with value ((abs c)+(abs d)).

####Examples

        ~palryp-hocsyt/try=> (sum:si --10 --10)
        --20
        ~palryp-hocsyt/try=> (sum:si --10 -0)
        --10
        ~palryp-hocsyt/try=> (sum:si -10 -7)
        -17
        ~palryp-hocsyt/try=> (sum:si -10 --7)
        -3

##++sun 

Produces a signed integer from an unsigned integer.
Note that the result must be manually cast to some @s odor to be inferred as an
unsigned integer in the type system.

####Examples

        Build dry %gold gate with sample unsigned integer `a`
        Produce the integer multiplied by 2.
        ~palryp-hocsyt/try=> `@s`10
        --5
        ~palryp-hocsyt/try=> (sun:si 10)
        20
        ~palryp-hocsyt/try=> `@s`(sun:si 10)
        --10
        ~palryp-hocsyt/try=> `@sd`(sun:si 10)
        --10
        ~palryp-hocsyt/try=> `@sd`(sun:si 12.345)
        --12.345

##++syn 

Is a signed integer positive?
Produce the sign of a signed integer - & being posiitve, | negative.

####Summary

        Build dry %gold gate with sample signed integer `a`
        Is the last bit of 'a' 0?
        ~palryp-hocsyt/try=> (syn:si -7)
        %.n
        ~palryp-hocsyt/try=> (syn:si --7)
        %.y
        ~palryp-hocsyt/try=> (syn:si (new:si [& 7]))
        %.y
        ~palryp-hocsyt/try=> (syn:si -0)
        %.y
        ~palryp-hocsyt/try=> (syn:si --0)
        %.y

##+fe    

Binary block modulo math engine.  Defaults to bloq size 1.

####Summary

        Build dry %gold tray with sample bloq `a`

##++dif 

Produces the difference between two atoms in the modular basis representation.

####Summary

        Build dry %gold gate wtih sample atom `b`, atom `c`
        Produce sit slammed with:
                The difference between:
                        The sum of: 
                                `out` and slam of `b` to sit
                                Slam of `c` to sit

####Examples

        ~tadbyl-hilbel/try=> (~(dif fe 3) 63 64)
        255
        ~tadbyl-hilbel/try=> (~(dif fe 3) 5 10)
        251
        ~tadbyl-hilbel/try=> (~(dif fe 3) 0 1)
        255
        ~tadbyl-hilbel/try=> (~(dif fe 0) 9 10)
        1
        ~tadbyl-hilbel/try=> (~(dif fe 0) 9 11)
        0
        ~tadbyl-hilbel/try=> (~(dif fe 0) 9 12)
        1
        ~tadbyl-hilbel/try=> (~(dif fe 2) 9 12)
        13
        ~tadbyl-hilbel/try=> (~(dif fe 2) 63 64)
        15

##++inv 

Inverts the order of the modular field.

####Summary

        Build dry %gold gate with sample atom `b`
        Produce the difference between:
                The decrement of `out`
                Slam of `b` to sit.

####Examples

        palryp-hocsyt/try=> (~(inv fe 3) 255)
        0
        ~palryp-hocsyt/try=> (~(inv fe 3) 256)
        255
        ~palryp-hocsyt/try=> (~(inv fe 3) 0)
        255
        ~palryp-hocsyt/try=> (~(inv fe 3) 1)
        254
        ~palryp-hocsyt/try=> (~(inv fe 3) 2)
        253
        ~palryp-hocsyt/try=> (~(inv fe 3) 3)
        252

##++net 
        
####Summary

        Build dry %gold gate with sample atom `b`.  Yield atom.
        Push toss of `b` for the slam of `b` to sit on the context.
        Unless: `a` is less than or equal to 3,
                Then: Produce `b`,
        Else: Push `c` is the decrement of `a`
        Produce the slam of con with:
                The single c-block left-shift of:
                        The toss of `a` for `c`, `b` for the c-block [0 1] cut of `b`
                The toss of `a` for `c`, `b` for the c-block [1 1] cut of `b` 

####Examples

        ~tadbyl-hilbel/try=> (~(net fe 3) 64)
        64
        ~tadbyl-hilbel/try=> (~(net fe 3) 128)
        128
        ~tadbyl-hilbel/try=> (~(net fe 3) 255)
        255
        ~tadbyl-hilbel/try=> (~(net fe 3) 256)
        0
        ~tadbyl-hilbel/try=> (~(net fe 3) 257)
        1
        ~tadbyl-hilbel/try=> (~(net fe 3) 500)
        244
        ~tadbyl-hilbel/try=> (~(net fe 3) 511)
        255
        ~tadbyl-hilbel/try=> (~(net fe 3) 512)
        0
        ~tadbyl-hilbel/try=> (~(net fe 3) 513)
        1
        ~tadbyl-hilbel/try=> (~(net fe 3) 0)
        0
        ~tadbyl-hilbel/try=> (~(net fe 3) 1)
        1
        ~tadbyl-hilbel/try=> (~(net fe 0) 1)
        1
        ~tadbyl-hilbel/try=> (~(net fe 0) 2)
        0
        ~tadbyl-hilbel/try=> (~(net fe 0) 3)
        1
        ~tadbyl-hilbel/try=> (~(net fe 6) 1)
        72.057.594.037.927.936
        ~tadbyl-hilbel/try=> (~(net fe 6) 2)
        144.115.188.075.855.872
        ~tadbyl-hilbel/try=> (~(net fe 6) 3)
        216.172.782.113.783.808
        ~tadbyl-hilbel/try=> (~(net fe 6) 4)
        288.230.376.151.711.744
        ~tadbyl-hilbel/try=> (~(net fe 6) 5)
        360.287.970.189.639.680
        ~tadbyl-hilbel/try=> (~(net fe 6) 6)
        432.345.564.227.567.616
        ~tadbyl-hilbel/try=> (~(net fe 6) 7)
        504.403.158.265.495.552
        ~tadbyl-hilbel/try=> (~(net fe 6) 512)
        562.949.953.421.312
        ~tadbyl-hilbel/try=> (~(net fe 6) 513)
        72.620.543.991.349.248

##+out       

The maximum integer value that the current block can store.

####Summary

        Produce the binary exponent of:
        The binary expoenent of the block size, `a`

####Examples

        ~tadbyl-hilbel/try=> ~(out fe 0)
        2
        ~tadbyl-hilbel/try=> ~(out fe 1)
        4
        ~tadbyl-hilbel/try=> ~(out fe 2)
        16
        ~tadbyl-hilbel/try=> ~(out fe 3)
        256
        ~tadbyl-hilbel/try=> ~(out fe 4)
        65.536
        ~tadbyl-hilbel/try=> ~(out fe 10)
          179.769.313.486.231.590.772.930.519.078.902.473.361.797.697.894.230.657.273.430.081.
          157.732.675.805.500.963.132.708.477.322.407.536.021.120.113.879.871.393.357.658.789.
          768.814.416.622.492.847.430.639.474.124.377.767.893.424.865.485.276.302.219.601.246.
          094.119.453.082.952.085.005.768.838.150.682.342.462.881.473.913.110.540.827.237.163.
          350.510.684.586.298.239.947.245.938.479.716.304.835.356.329.624.224.137.216

##++rol 

####Summary

        Build dry %gold gate with sample bloq `b`, atom `c`, atom `d`. Yield atom.
        Push `e` is sit slammed with `d`, the modular representation of `d` 
        Push `f` is the binary expoenent of:
                The difference between 'a' and 'b'
        Push `g` is `c` modulus `f`
        Produce sit slammed with:
                con slammed with:
                        The `g` b-blocks right-shift of `e`
                        The difference between `f` and `g` b-blocks left-shift of `e`

####Examples

        ??
        
##++ror 

####Summary
 
        Build dry %gold gate with sample bloq `b`, atom `c`, atom `d`.  Yield atom.
        Push `e` is sit slammed with `d`, the modular representation of `d` 
        Push `f` is the binary expoenent of:
                The difference between 'a' and 'b'
        Push `g` is `c` modulus `f`
        Produce sit slammed with:
                con slammed with:
                        The `g` b-blocks left-shift of `e`
                        The difference between `f` and `g` b-blocks right-shift of `e`

####Examples

        ??

##++sum 

Sum two numbers in this modular field.

####Summary

        Build dry %gold gate with sample atom `b`, atom `c`
        Produce sit slammed with the sum of `b` and `c`.

####Examples

        ~tadbyl-hilbel/try=> (~(sum fe 3) 10 250)
        4
        ~tadbyl-hilbel/try=> (~(sum fe 0) 0 1)
        1
        ~tadbyl-hilbel/try=> (~(sum fe 0) 0 2)
        0
        ~tadbyl-hilbel/try=> (~(sum fe 2) 14 2)
        0
        ~tadbyl-hilbel/try=> (~(sum fe 2) 14 3)
        1
        ~tadbyl-hilbel/try=> (~(sum fe 4) 10.000 256)
        10.256
        ~tadbyl-hilbel/try=> (~(sum fe 4) 10.000 100.000)
        44.464

##++sit 

Produce an atom in the current modular block representation.

####Summary

        Build dry %gold gate with sample atom `b`
        Produce the last block of size `a` in `b`

####Examples

        ~tadbyl-hilbel/try=> (~(sit fe 3) 255)
        255
        ~tadbyl-hilbel/try=> (~(sit fe 3) 256)
        0
        ~tadbyl-hilbel/try=> (~(sit fe 3) 257)
        1
        ~tadbyl-hilbel/try=> (~(sit fe 2) 257)
        1
        ~tadbyl-hilbel/try=> (~(sit fe 2) 10.000)
        0
        ~tadbyl-hilbel/try=> (~(sit fe 2) 100)
        4
        ~tadbyl-hilbel/try=> (~(sit fe 2) 16)
        0
        ~tadbyl-hilbel/try=> (~(sit fe 2) 17)
        1
        ~tadbyl-hilbel/try=> (~(sit fe 0) 17)
        1
        ~tadbyl-hilbel/try=> (~(sit fe 0) 0)
        0
        ~tadbyl-hilbel/try=> (~(sit fe 0) 1)
        1
        
section 2cG, floating point           

##++  rlyd  
##++  rlyh  
##++  rlyq  
##++  rlys  
##++  ryld  
##++  rylh  
##++  rylq  
##++  ryls  


section 2cH, urbit time

####Note that entering '-<-' in the shell produces the current time in @da format. We use this for many of our examples.

~zod/try=> -<-
~2014.8.4..19.39.59..9288

##++year

Accept a parsed date of form [[a=? y=@ud] m=@ud t=tarp] and produce 
its @d representation.

####Summary

        Build dry %gold gate with sample parsed date `det`
        Yield @d.
        Push `yer` is:
            If: `a.det` is true,
                Then: The sum of 292,277,024,400 and `y.det`, the year,
            Else: The difference of 292,277,024,400 and the decrement of `y.det`, the year.
        Push `day` is yawn slammed with:
            `yer`, `m.det`, `d.t.det`
        Produce yule slammed with:
            `day`, `h.t.det`, `m.t.det`, `s.t.det`, `f.t.det`

####Examples

        ~zod/try=> (year [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=4 s=57 f=~[0xd940]]])
        0x8000000d227df4e9d940000000000000

##++yore  

        Produce the parsed date [[a=? y=@ud] m=@ud t=tarp] representation of a @d date. 

####Summary

        Build dry %gold gate with sample @d `now`.
        Yield date.
        Push `rip` is yell slammed with `now`, the
        Push `ger` is yall slammed with `d.rip`, the
        Pair:
            If:  y.ger is greater than 292.277.024.400,
                Then:  Produce the cell:
                        a is true, y is the difference between y.ger and 292.277.024.400,
            Else:  Proudce a is false, y is the difference between 292.277.024.400 and y.ger.
        [m.ger d.ger h.rip m.rip s.rip. f.rip], the tarp of the date.

####Examples

        ~zod/try=> (yore -<-)
        [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=17 s=1 f=~[0x700d]]]
        ~zod/try=> (yore -<-)
        [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=28 s=53 f=~[0x7b82]]]

##++yell  

Produce a parsed daily time format from an atomic date.

####Summary

        Build a dry %gold gate with sample @d, `now`.
        Yield tarp.
        Push `sec` is the 6-bit right-shift of `now`.
            Push `fan` is:
                Push `muc` is 4, `raw` is the 6-bit single block tail of `now`
                Kick dry %gold trap.  Yield a list of hexadecimal numbers.
                If:  `raw` is 0 or `muc` is 0,
                    Then:  Produce null,
                Else:  Use `muc` replaced by the decrement of `muc` as subject.
        Push `day` is `sec` divided by the constant `day:yo`
        Use `sec` replaced by the modulus of `sec` and the constant `day:yo` as subject.
        Push `hor` is `sec` divided by the constant `hor:yo`.
        Use `sec` replaced by the modulus of `sec` and the constant `hor:yo`.
        Push `mit` is `sec` divided by the constant `mit:yo`
        Use `sec` replaced by the modulus of`sec` and the constant `mit:yo`
        Produce the tuple `day`, `hor`, `mit`, `sec`, `fan`.

####Examples

        ~dovryp-toblug/try=> (yell ~2014.3.20..05.42.53..7456)
        [d=106.751.991.820.094 h=5 m=42 s=53 f=~[0x7456]]
        ~tadbyl-hilbel/try=> (yell ~2014.6.9..19.09.40..8b66)
        [d=106.751.991.820.175 h=19 m=9 s=40 f=~[0x8b66]]
        ~tadbyl-hilbel/try=> (yell ~1776.7.4)
        [d=106.751.991.733.273 h=0 m=0 s=0 f=~]

##++yule  

Accept a tarp, a parsed daily time, and produces a time atom, @d.

####Summary

        Build a dry %gold gate with sample tarp, `rip`.
        Yield @d.
        Push `sec` is the sum of:
            `d.rip` multiplied by the constant `day:yo`
            `h.rip` multiplied by the constant `hor:yo`
            `m.rip` multiplied by the constant `mit:yo`
        Terminate the sum statement.
        Push `fac` is:
            Push `muc` is 4.
            Kick dry %gold trap.  Yield atom.
            If: `f.rip` is null,
                Then:  `fac` is 0,
            Else:  Use `muc` as the decrement of `muc` as subject.
            The sum of:
                The 4-bit `muc` block left-shift of the head of `f.rip`
                The toss of `f.rip` for the tail of `f.rip`.
        Produce con (binary OR) slammed with:
            The 6-bit single block left-shift of `sec`,
            `fac`.

####Examples

        ~tadbyl-hilbel/try=> =murica (yell ~1776.7.4)
        ~tadbyl-hilbel/try=> murica
        [d=106.751.991.733.273 h=0 m=0 s=0 f=~]
        ~tadbyl-hilbel/try=> (yule murica)
        0x8000000b62aaf5800000000000000000
        ~dovryp-toblug/try=> (yule (yell ~2014.3.20..05.42.53..7456))
        0x8000000d21c88d5d7456000000000000
        ~tadbyl-hilbel/try=> (yule (yell ~2014.6.9..19.09.40..8b66))
        0x8000000d223413f48b66000000000000

##++yall

Produce the date tuple of [y=@ud m=@ud d=@ud] of the year, month, and day
from a number of days from the beginning of time.

####Summary

        Build dry %gold gate with sample @ud, `day`
        Yield the tuple [y=@ud m=@ud d=@ud]
        Push `era` is 0, `cet` is 0, `lep` is the bunt of a bean.
        Use as subject:
            `era` as `day` divided by the constant `era:yo`,
            `day` as the modulus of `day` by the constant `era:yo`
        Use as subject:
            Cast to the type of the subject,
            If:  `day` is less than the increment of the constant `cet:yo`
               Then:  Produce `lep` replaced with true, `cet` with false.   
            Else:  Use as subject:
                `lep` replaced with false,
                `cet` with 1,
                `day` with the differecne between `day` and the increment of `cet:yo`
            Produce `cet` replaced by the sum of `cet` and `day` divided by `cet:yo`,
            `day` replaced by the moduluso of `day` and `cet:yo`
        Push `yer` is the sum of:
            400 multiplied by `era`,
            100 multiplied by `cet`
        Kick dry %gold trap.  Yield a tuple of three atoms with togas `y`, `m`, `d`.
        Push `dis` is:
            If: `lep` is true,
                Then:  366,
            Else:  365
        Unless:  `day` is less than `dis`,
            Then:  Push `ner` is the increment of `yer`
            Produce the toss of:
                `yer` for `ner`, 
                `day` for the difference between `day` and the increment of `dis`
                `lep` for:  Is 0 the 0-bit 2-block tail of `ner`?
        Else:  Kick dry %gold trap.  Yield a tuple of three atoms with tags `y`, `m`, `d`.
        Push:
            `mot` is 0,
            `cah` is If:  `lep`,
                        Then:  The constant `moy:yo`
                     Else:  The constant `moh:yo`
        Kick dry %gold trap.  Yield a tuple of three atoms with togas `y`, `m`, `d`.
        Push `zis` is snag slammed with `mot`, `cah`
        If:  `day` is less than `zis`,
            Then: Produce the tuple of:
                `yer`,
                The increment of `mot`,
                The increment of `day`
        Else:  Produce the toss of:
                    `mot` for its increment,
                    `day` for the difference between `day` and `zis`

####Examples

        ~zod/try=> (yall 198)
        [y=0 m=7 d=17]
        ~zod/try=> (yall 90.398)
        [y=247 m=7 d=3]
        ~zod/try=> (yall 0)
        [y=0 m=1 d=1]

##++yawn

        Days since Jesus.  Accpet a year, month, and day (Three unsigned decimal integers) 
        and produce the day number it is in the CE.

####Summary

        Build dry %gold gate with sample atoms `yer`, `mot`, `day`
        Yield atom.
        Use `mot` replaced by its decrement, `day` by its decrement as subject.
        Use as subject:
            Cast the following the type of the subject.
            Evaluate the subject with `day` as:
                Push `cah` is:  If:  yelp slammed with `yer`
                                    Then:  Produce `moy:yo`
                                Else: `moh:yo`
        Kick dry %gold trap.  Yield atom.
        Unless:  Is 0 the modulus of `yer` by 4?
            Then:  Push `ney` is the decerement of `yer`.
            Produce the toss of:
                `yer` for `ney`,
                `day` for the sum of `day` and,
                    If:  yelp slammed with `ney`,
                       Then: 366,
                    Else: 365.
        Else:  Unless:  0 is the modulus of `yer` by 100?
                   Then:  Push `nef` is the difference between `yer` and 100,
                          Produce the toss of:
                              `yer` for `nec`,
                              `day` for the sum of `day` and,
                                  If: yelp slammed with `nec`,
                                      Then:  36,525,
                                  Else:  36,524
               Else:  Produce the sum of
                        `day`,
                        `yer` divided by 400 multiplied by the increment of 4*36,524.

####Examples

        ~zod/try=> (yawn 2.014 8 4)
        735.814
        ~zod/try=> (yawn 1.776 7 4)
        648.856

##++yelp  

        Is the given year a leap year?

####Summary

        Build dry %gold gate with sample atom `yer`.  Yield loobean.
        Produce the logical AND of:
            Is 0 the modulus of `yer` by 4?
            The logical OR of:
                Is 0 the modulus of `yer` by 100?
                Is 0 the modulus of `yer` by 400?

####Examples

        ~tadbyl-hilbel/try=> (yelp 2.014)
        %.n
        ~tadbyl-hilbel/try=> (yelp 2.008)
        %.y
        ~tadbyl-hilbel/try=> (yelp 0)
        %.y
        ~tadbyl-hilbel/try=> (yelp 14.011)
        %.n

##++yo

        Constants of time.

####Summary

        Build a %gold core.

##++  cet

        Days in a century.  Derived by multiplying the number of days in a year
        (365) by the number of years in a century (100), then adding the number
        days from leap years in a century (24).

####Examples

        ~tadbyl-hilbel/try=> cet:yo
        36.524
        ~tadbyl-hilbel/try=> (add 365 cet:yo)
        36.889
        ~tadbyl-hilbel/try=> (sub (add 24 (mul 100 365)) cet:yo)
        0

##++day 

        The number of seconds in a day.  Derived by multiplying the the number
        of seconds in an hour by the hours in a day.

####Examples

        ~tadbyl-hilbel/try=> day:yo
        86.400
        ~tadbyl-hilbel/try=> (add 60 day:yo)
        86.460

##++era 

####Examples

        ??        


##++hor 

The number of seconds in an hour.  Derived by multiplying the number of
seconds in a minute by the minutes in an hour.

####Examples

        ~tadbyl-hilbel/try=> hor:yo
        3.600

##++jes

####Examples
        

##++mit 

The number of seconds in a minute.  We just knew this one.

####Examples

        ~tadbyl-hilbel/try=> mit:yo
        60

##++moh 

        The days in each month of the Gregorian common year.  A list of 
        unsigned decimal atoms (Either 28, 30, or 31) denoting the number 
        of days in the month at the year at that index.

####Examples

        ~tadbyl-hilbel/try=> moh:yo
        ~[31 28 31 30 31 30 31 31 30 31 30 31]

##++moy 

        The days in each month of the Gregorian leap-year.  A list of
        unsigned decimal atoms (Either 29,30, or 31) denoting the number
        of days in the month at the leap-year at that index.

####Examples

        ~tadbyl-hilbel/try=> moy:yo
        ~[31 29 31 30 31 30 31 31 30 31 30 31]

##++qad 

        The number of seconds in four years.  Derived by adding one second
        to the number of seconds in four years.

####Examples

        ~tadbyl-hilbel/try=> qad:yo
        126.144.001

##++yer 

        The number of seconds in a year.  Derived by multiplying the number of
        seconds in a day by 365.

####Examples

        ~tadbyl-hilbel/try=> yer:yo
        31.536.000

section 2cI, almost macros

##++hard

Ruthlessly demands that a specific type be produced, crashing the program is it is not.

####Summary

        Creates a vulanized wet gate which accepts any gate which accepts any noun and produces
        any noun.
        Creates a dry %gold gate which accepts any noun and casts the result to the 
        higher gate argument's icon.
        Prints "%hard" in the stack trace if the code below crashes.
        Let gol be the higher gate argument slammed with the lower arbitrary noun.
        Assert that the result's icon is equal to that of the lower arbitrary noun
        before producing said result.

####Examples

        ~palryp-hocsyt/try=> ((hard (list)) (limo [1 2 3 ~]))
        ~[1 2 3]
        ~tadbyl-hilbel/try=> ((hard ,@) (add 2 2))
        4
        ~tadbyl-hilbel/try=> ((hard ,@t) (crip "Tape to cord, bro!"))
        'Tape to cord, bro'
        ~tadbyl-hilbel/try=> ((hard tape) (crip "...Tape to cord, bro?..."))
        ! hard
        ! exit

##++soft

Politely requests a specific type to be produced, producing null if it is not.

####Summary

        Creates a vulanized wet gate which accepts any gate which accepts any noun and produces
        any noun.
        Creates a dry %gold gate which accepts any noun and casts the result to the 
        a unit of the higher gate argument's icon.
        Let gol be the higher gate argument slammed with the lower arbitrary noun.
        Build an unless-then-else statement on the result icon's being equal to that of 
        the lower arbitrary noun.
        If so, produce null.
        Else, produce the unit of the result.

####Examples

        ~tadbyl-hilbel/try=> ((soft ,%4) (add 2 2))
        [~ %4]
        ~tadbyl-hilbel/try=> ((soft ,@) (add 2 2))
        [~ 4]
        ~tadbyl-hilbel/try=> ((soft ,%5) (add 2 2))
        ~
        ~tadbyl-hilbel/try=> ((soft ,@t) (crip "Tape to cord, Woohoo!"))
        [~ 'Tape to cord, Woohoo!']
        ~tadbyl-hilbel/try=> ((soft ,@t) (trip 'Cmon man... Tape to cord? Please?!'))
        ~
       
--- 
