##chapter 2a, basic unsigned math

##++add 
        
Sum two numbers.

####Summary

        Activate jet.
        Build dry %gold gate with sample atoms `a` and `b`
        Yield atom
        If: `a` is 0
          Then: Produce `b`
        Else: Produce the slam of the gate with (dec a) and +(b).

####Examples

        ~palryp-hocsyt/try=> (add 2 2)
        4
        ~palryp-hocsyt/try=> (add 1 1.000.000)
        1.000.001
        ~palryp-hocsyt/try=> (add 1.333 (mul 2 2))
        1.337

##++cap
          
Test if an atom is in the head or tail of a noun.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a`
        Yield either %2 or %3
        Switch on type of `a`
        If: %2, Then: produce %2 
        If: %3, Then: produce %3
        If: either %0 or %1, Then: fail
        If: noun, Then: slam gate with (div a 2)
        Else: Terminate switch statement.

####Examples

        ~palryp-hocsyt/try=> (cap 4)
        %2
        ~palryp-hocsyt/try=> (cap 6)
        %3
        ~palryp-hocsyt/try=> (cap (add 10 9))
        %2

##++dec  

Decrement a number - Subtracts one.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom a
        Error on crash: %decrement-underflow
        Deny that `a` is 0
        Let `b` be 0
        Kick dry %gold trap that yields atom.
        If: `a` is `+(b)`
          Then: Produce `b`.
        Else, slam trap with `+(b)` 

####Examples

        ~palryp-hocsyt/try=> (dec 7)
        6
        ~palryp-hocsyt/try=> (dec 0)
        ! decrement-underflow
        ! exit

##++div  

Divide one number by another.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a`, atom `b`.
        Yield atom
        Error on crash: 'div'
        Deny that `b` is 0
        Push `c` is 0.
        Kick dry %gold trap
        If: `a` is less than `b`
          Then: Produce `c`.
        Else, slam trap with (sub a b) +(c)

####Examples

        ~palryp-hocsyt/try=> (div 4 2)
        2
        ~palryp-hocsyt/try=> (div 17 8)
        2
        ~palryp-hocsyt/try=> (div 20 30)
        0

##++fac
  
Produce the factorial of a number n, n!.

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a` and atom `b`.
        Yield atom
        If: `a` is 0
          Then: Produce 1.
        Else: slam gate with (dec a)
          and multiply by `a`
        
####Examples

        ~palryp-hocsyt/try=> (fac 3)
        6
        ~palryp-hocsyt/try=> (fac 0)
        1
        ~palryp-hocsyt/try=> (fac 11)
        39.916.800

##++gte

Is the first greater than or equal to the second?

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a` and atom `b`
        Yield bean
        `a` is NOT less-than `b`

####Examples

        ~palryp-hocsyt/try=> (gte 100 10)
        %.y
        ~palryp-hocsyt/try=> (gte 4 4)
        %.y
        ~palryp-hocsyt/try=> (gte 3 4)
        %.n

##++gth

Is the first greater than the second?

####Summary

        Activate jet.
        Build dry %gold gate with sample atom `a` and atom `b`
        Yield bean.
        `a` is NOT less-equal `b`
        ---
        ~ronrem-lonsem/try=> (gth 4 5)
        %.n
        ~ronrem-lonsem/try=> (gth 5 4)
        %.y
        ~ronrem-lonsem/try=> (gth 5 5)
        %.n
        ~ronrem-lonsem/try=> (gth 0 0)
        %.n

##++lte
  
Is the first less than or equal to the second?

####Summary

          Activate jet
          Build dry %gold gate with sample atom a and atom b
          Yield bean
          `a` is b OR a is less-than `b`.
####Examples

          ~ronrem-lonsem/try=> (lte 4 5)
          %.y
          ~ronrem-lonsem/try=> (lte 5 4)
          %.n
          ~ronrem-lonsem/try=> (lte 5 5)
          %.y
          ~ronrem-lonsem/try=> (lte 0 0)
          %.y

##++lth
  
Is the first less than the second?

####Summary

          Activate jet
          Build dry %gold gate with a sample atom `a` and atom `b`
          Yield bean
          Use logical AND, and produce %.n if a is b.
          Kick a dry %gold trap
          produce %.y if `a` is 0
          produce %.n if `a` is NOT 0 AND `b`=0.
          Else, toss `a` for (dec a), and `b` for (dec b)

####Examples          

          ~ronrem-lonsem/try=> (lth 4 5)
          %.y
          ~ronrem-lonsem/try=> (lth 5 4)
          %.n
          ~ronrem-lonsem/try=> (lth 5 5)
          %.n
          ~ronrem-lonsem/try=> (lth 5 0)
          %.n

##++mas
  
Produce the axis of a within the head or the tail. 

####Summary

          Activate jet
          Build dry %gold gate with sample atom a
          Yield atom.
          Switch on `a`:
            If:1, Then: fail
            If 2, Then: produce 1
            If 3, Then: produce 1
            Else: add `a` modulo 2 to 2 times the toss of `a` for (div a 2)

####Examples
 
          1 ~ronrem-lonsem/try=> (mas 3)
          1
          ~ronrem-lonsem/try=> (mas 4)
          2
          ~ronrem-lonsem/try=> (mas 5)
          3
          ~ronrem-lonsem/try=> (mas 6)
          2
          ~ronrem-lonsem/try=> (mas 7)
          3
          ~ronrem-lonsem/try=> (mas 8)
          4
          ~ronrem-lonsem/try=> (mas 0)
          ! exit
          ~ronrem-lonsem/try=> (mas 1)
          ! exit

##++max
  
Produce the larger of two atoms.

####Summary

          Activate jet
          Build dry %gold gate with sample atom `a` and atom `b`
          Yield atom
          If: `a` is greater than `b`
            Then: produce `a`
          Else: produce `b`

####Examples

          ~palryp-hocsyt/try=> (max 10 100)
          100
          ~palryp-hocsyt/try=> (max 10.443 9)
          10.443
          ~palryp-hocsyt/try=> (max 0 1)
          1

##++  min  

Produce the smaller of two atoms.

####Summary

        Activate jet
        Build dry %gold gate with sample atom `a` and atom `b`
        Yield atom.
        If: `a` is less than `b` 
          Then: produce `a`
        Else: produce `b`

####Examples

        ~palryp-hocsyt/try=> (min 10 100)
        10
        ~palryp-hocsyt/try=> (min 10.443 9)
        9
        ~palryp-hocsyt/try=> (min 0 1)
        0

##++mod  

        Produce `a` modulo `b`

####Summary

        Activate jet
        Build dry %gold gate with sample atom `a` and atom `b`
        Yield atom
        Deny that `b` is 0
        Subtract from `a` the product of `b` and `a` divided by `b`
        
##++mul 
 
Multiply two numbers

####Summary

        Activate jet
        Build dry %gold gate with sample atom `a` and atom `b`
        Yield atom
        Push `c` is 0
        Kick a dry %gold trap.
        If: `a` is 0
          Then: produce `c`.
        Else: toss `a` for (dec a) and `c` for (add b c
        
####Examples

        ~sivtyv-barnel/try=> (mul 3 4)
         12 
        ~sivtyv-barnel/try=> (mul 0 1) 
        0

##++peg  

Produces the axis of b within the axis of a.

####Summary

        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Yield atom
        Switch on `b`
          If 1, Then: produce `a`
          If 2, Then: produce (mul a 2)
          If 3, Then: produce +((mul a 2))
          Else: add (mod b 2) to 2 times the toss of `b` for (div b 2)

####Examples

        ~ronrem-lonsem/try=> (mul 5 3)
        15
        ~ronrem-lonsem/try=> (mul 1 0)
        0

##++sub  

Subtract two numbers

####Summary

        Activate jet
        Build dry %gold gate with sample atom `a` and atom `b`
        Error on crash "%subtract-underflow"
        Yield atom
        If: `b` is 0
          Then: produce `a`.
        Else: toss `a` for (dec a) and `b` for (dec b)

####Examples

        ~ronrem-lonsem/try=> (sub 10 5)
        5
        ~ronrem-lonsem/try=> (sub 243 44)
        199
        ~ronrem-lonsem/try=> (sub 5 0)
        5
        ~ronrem-lonsem/try=> (sub 0 5)
        ! subtract-underflow
        ! exit


