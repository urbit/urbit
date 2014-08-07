volume 2, Hoon libraries and compiler

  chapter 2a, basic unsigned math

    ++  add 
        Sum two numbers.
        ---
        Activate jet.
        Build dry %gold gate with sample atoms a and b
        Yield atom
        If: a is 0
          Then: Produce `b`
        Else: Produce the slam of the gate with (dec a) and +(b).
        ---
        ~palryp-hocsyt/try=> (add 2 2)
        4
        ~palryp-hocsyt/try=> (add 1 1.000.000)
        1.000.001
        ~palryp-hocsyt/try=> (add 1.333 (mul 2 2))
        1.337

    ++  cap
        Test if an atom is in the head or tail of a noun.
        ---
        Activate jet.
        Build dry %gold gate with sample atom a
        Yield either %2 or %3
        Switch on type of a
        if %2, produce %2 
        if %3, produce %3
        if either %0 or %1, fail
        if noun, slam gate with (div a 2)
        Terminate switch statement.
        ---
        ~palryp-hocsyt/try=> (cap 4)
        %2
        ~palryp-hocsyt/try=> (cap 6)
        %3
        ~palryp-hocsyt/try=> (cap (add 10 9))
        %2

    ++  dec  
        Decrement a number - Subtracts one.
        ---
        Activate jet.
        Build dry %gold gate with sample atom a
        Error on crash: %decrement-underflow
        Deny that a is 0
        Let b be 0
        Kick dry %gold trap that yields atom.
        If: a is +(b)
          Then: Produce b.
        Else, slam trap with +(b) 
        ---
        ~palryp-hocsyt/try=> (dec 7)
        6
        ~palryp-hocsyt/try=> (dec 0)
        ! decrement-underflow
        ! exit

    ++  div  
        Divide one number by another.
        ---
        Activate jet.
        Build dry %gold gate with sample atom a, atom b.
        Yield atom
        Error on crash: 'div'
        Deny that b is 0
        Push `c` is 0.
        Kick dry %gold trap
        If: a is less than b
          Then: Produce c.
        Else, slam trap with (sub a b) +(c)
        ---
        ~palryp-hocsyt/try=> (div 4 2)
        2
        ~palryp-hocsyt/try=> (div 17 8)
        2
        ~palryp-hocsyt/try=> (div 20 30)
        0

    ++  fac  
        Produce the factorial of a number n, n!.
        ---
        Activate jet.
        Build dry %gold gate with sample atom a and atom b.
        Yield atom
        If: a is 0
          Then: Produce 1.
        Else: slam gate with dec a
          and multiply by a
        ---
        ~palryp-hocsyt/try=> (fac 3)
        6
        ~palryp-hocsyt/try=> (fac 0)
        1
        ~palryp-hocsyt/try=> (fac 11)
        39.916.800

    ++  gte
        Is the first greater than or equal to the second?
        ---
        Activate jet.
        Build dry %gold gate with sample atom a and atom b
        Yield bean
        a is NOT less-than b
        ---
        ~palryp-hocsyt/try=> (gte 100 10)
        %.y
        ~palryp-hocsyt/try=> (gte 4 4)
        %.y
        ~palryp-hocsyt/try=> (gte 3 4)
        %.n

    ++  gth
        Is the first greater than the second?
        ---
        Activate jet.
        Build dry %gold gate with sample atom a and atom b
        Yield bean.
        a is NOT less-equal b
        ---
        ~ronrem-lonsem/try=> (gth 4 5)
        %.n
        ~ronrem-lonsem/try=> (gth 5 4)
        %.y
        ~ronrem-lonsem/try=> (gth 5 5)
        %.n
        ~ronrem-lonsem/try=> (gth 0 0)
        %.n
      ++  lte  
          Is the first less than or equal to the second?
          ---
          Activate jet
          Build dry %gold gate with sample atom a and atom b
          Yield bean
          a is b OR a is less-than b
          ---
          ~ronrem-lonsem/try=> (lte 4 5)
          %.y
          ~ronrem-lonsem/try=> (lte 5 4)
          %.n
          ~ronrem-lonsem/try=> (lte 5 5)
          %.y
          ~ronrem-lonsem/try=> (lte 0 0)
          %.y
      ++  lth  
          Is the first less than the second?
          ---
          Activate jet
          Build dry %gold gate with a sample atom a and atom b
          Yield bean
          Use logical AND, and produce %.n if a is b.
          Kick a dry %gold trap
          produce %.y if a is 0
          produce %.n if a is NOT 0 AND b=0.
          Else, toss a for (dec a), and b for (dec b)
          ---
          ~ronrem-lonsem/try=> (lth 4 5)
          %.y
          ~ronrem-lonsem/try=> (lth 5 4)
          %.n
          ~ronrem-lonsem/try=> (lth 5 5)
          %.n
          ~ronrem-lonsem/try=> (lth 5 0)
          %.n
      ++  mas  
          Produce the axis of a within the head or the tail. 
          ---
          Activate jet
          Build dry %gold gate with sample atom a
          Yield atom.
          Switch on a:
            if 1, fail
            if 2, produce 1
            if 3, produce 1
            Else, add a modulo 2 to 2 times the toss of a for (div a 2)
          ---
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

      ++  max  
          Produce the larger of two atoms.
          ---
          Activate jet
          Build dry %gold gate with sample atom a and atom b
          Yield atom
          If: a is greater than b
            Then: produce a
          Else: produce b
          ---
          ~palryp-hocsyt/try=> (max 10 100)
          100
          ~palryp-hocsyt/try=> (max 10.443 9)
          10.443
          ~palryp-hocsyt/try=> (max 0 1)
          1

    ++  min  
        Produce the smaller of two atoms.
        ---
        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Yield atom.
        If: a is less than b 
          Then: produce a
        Else: produce b
        ---
        ~palryp-hocsyt/try=> (min 10 100)
        10
        ~palryp-hocsyt/try=> (min 10.443 9)
        9
        ~palryp-hocsyt/try=> (min 0 1)
        0

    ++  mod  
        Produce a modulo b
        ---
        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Yield atom
        Deny that b is 0
        Subtract from a the product of b and a divided by b
        ---

    ++  mul  
        Multiply two numbers
        ---
        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Yield atom
        Push 'c' is 0
        Kick a dry %gold trap.
        If: a is 0
          Then: produce c.
        Else: toss a for (dec a) and c for (add b c
        Examples:

    ++  peg  
        Produces the axis of b within the axis of a.
        ---
        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Yield atom
        Switch on b
          if 1, produce a
          if 2, produce (mul a 2)
          if 3, produce +((mul a 2))
          else, add (mod b 2) to 2 times the toss of b for (div b 2)
        ---
        ~ronrem-lonsem/try=> (mul 5 3)
        15
        ---
        ~ronrem-lonsem/try=> (mul 1 0)
        0

    ++  sub  
        Subtract two numbers
        ---
        Activate jet
        Build dry %gold gate with sample atom a and atom b
        Error on crash "%subtract-underflow"
        Yield atom
        If: b is 0
          Then: produce a.
        Else: toss a for (dec a) and b for (dec b)
        ---
        ~ronrem-lonsem/try=> (sub 10 5)
        5
        ---
        ~ronrem-lonsem/try=> (sub 243 44)
        199
        ---
        ~ronrem-lonsem/try=> (sub 5 0)
        5
        ---
        ~ronrem-lonsem/try=> (sub 0 5)
        ! subtract-underflow
        ! exit

chapter 2b, basic containers

Section 2bA, units                    

    ++  biff 
        Apply a function which yields a unit to the value of a unit
        ---
        Build wet gate with a sample unit a and tiled gate b
        If: a is null
          Then: produce null
        Else: slam b with u.a.
        ---
        ~palryp-hocsyt/try=> (biff (some 5) |=(a=@ (some (add a 2))))
        [~ u=7]
        ---
        ~palryp-hocsyt/try=> (biff ~ |=(a=@ (some (add a 2))))
        ~
    ++  bind 
        Apply a function to the value of a unit.
        ---
        Build wet %gold gate with sample unit a and gate b
        If: a is null
          Then: produce null
        Else, the unit of the slam of 'b' with u.a.
        ---
        ~talsur-todres/try=> (bind ((unit ,@) [~ 97]) ,@t)
        [~ `a`]
        ---
        ~talsur-todres/try=> =a |=(a=@ (add a 1))
        ~talsur-todres/try=> (bind ((unit ,@) [~ 2]) a)
        [~ 3]

    ++  clap 
        Apply a binary operation which yields a unit to the values of two units
        ---
        Build wet %gold gate with a sample unit a, unit b and gate c
        If: a is null
          Then: produce b
        Else: If: b is null
          Then: produce a
        Else: the unit of the slam of c with [u.a u.b]
        ---
        ~palryp-hocsyt/try=> =u ((unit ,@t) [~ 'a'])
        ~palryp-hocsyt/try=> =v ((unit ,@t) [~ 'b'])
        ~palryp-hocsyt/try=> (clap u v |=([a=@t b=@t] (welp (trip a) (trip b))))
        [~ u="ab"]
        ---
        ~talsur-todres/try=> =a ((unit ,@u) [~ 1])
        ~talsur-todres/try=> =b ((unit ,@u) [~ 2])
        ~talsur-todres/try=> =c |=([a=@ b=@] (add a b))
        ~talsur-todres/try=> (clap a b c)
        [~ 3]

    ++  drop 
        Produce a list of the unit-value 
        ---
        Build wet %gold gate with sample unit a
        If: a is null
          Then: produce null
        Else, produce the list [u.a ~]
        ---
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> (drop a)
        [i=97 t=~]
        ---
        ~divreg-misdef/try=> =a ((unit ,@) [~])
        ~divreg-misdef/try=> (drop a)
        ~

    ++  fall 
        A default value 'b' for the unit 'a' when 'a' is null
        ---
        Build wet %gold gate with sample unit a and noun b 
        If: a is null
          Then: produce b
        Else: produce the unit-value of a
        ---
        ~talsur-todres/try=> (fall ~ `a`)
        `a`
        ---
        ~talsur-todres/try=> (fall [~ u=0] `a`)
        0

    ++  mate 
        Produce the unit 'a' or 'b' which is not null and crashes with error "mate" if they are equal.
        ---
        Creates a wet %gold gate with a sample which accepts a two units.
        Builds an if-then-else statement on "b is null."
        If so, produce a.
        Else, build an if-then-else statement on "a is null."
        If so, produce b.
        Else, build an if-else-then statement on  u.a=u.b.  Crash on "mate" if false, produce a if true.
        ---
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~ 97])
        ~divreg-misdef/try=> (mate a b)
        [~ 97]
        ---
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~])
        ~divreg-misdef/try=> (mate a b)
        [~ 97]
        ---
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~ 98])
        ~divreg-misdef/try=> (mate a b)
        ! 'mate'
        ! exit

    ++  need 

    Retrieve the value from a unit and crash if the unit is null.

    ####Summary

        Build wet %gold gate with sample unit `a` of any type. 
  			If:  p is null, 
					Then: fail,
				Else: Produce u.a, the value of the unit.  

   ####Examples

				~zod/try=> =a ((unit ,[@t @t]) [~ ['a' 'b']])
        ~zod/try=> (need a)
        ['a' 'b']
        ~zod/try=> =a ((unit ,@ud) [~ 17])
        ~zod/try=> (need a)
        17
        ~zod/try=> =a ((unit ,@) [~])
        ~zod/try=> (need a)
        ! exit

    ++  some 

    Casts any noun a to its unit, [~ a].

    ####Summary

        Build wet %gold gate with sample noun `a`.
        Produce the tuple [~ u=a], the unit of value `a`.

    ####Examples

        ~divreg-misdef/try=> (some [`a` `b`])
        [~ u=[`a` `b`]]
        ---
        ~divreg-misdef/try=> (some &)
        [~ u=%.y]

Section 2bB, lists                    

++  flop 

Produces the list 'a' with the elements reversed.
       
    ####Summary

        Activate jet.
        Build wet %gold gate with sample list `a`.
        Use `a` replaced by the slam of `a` to homo, the homogenized list, as subject.
        Cast the following to the type of `a`.
        Push `b` is the 
        Kick dry %gold gate trap.
        If:  `a` is null,
            Then:  Produce `b`,
        Else: Produce the toss of `a` for the tail of `a`, `b` for the cell [i.a b].

    ####Examples

        ~palryp-hocsyt/try=> =lyst (limo [1 2 3 4 ~])
        ~palryp-hocsyt/try=> lyst
        [i=1 t=[i=2 t=[i=3 t=[i=4 t=~]]]]
        ~palryp-hocsyt/try=> (flop lyst)
        ~[4 3 2 1]
        ~palryp-hocsyt/try=> (flop (limo [1 'a' 2 'b' (some 10) ~]))
        ~[[~ u=10] 98 2 97 1]

++  homo 
        homogenize
  Description:
        Homogenizes a lists' type information.
        ---
        Creates a wet %gold gate with a sample which accepts a single list.
        Makes the type of the result the type of the product of the code below (^+).
        The subject of the arm ($) is then the product of the barcen statement below (=<).
        Creates a %gold core (|%) and the arm '+-  $'.
        XXX BLACK BOX, ABANDON ALL HOPE YE WHO ENTER HERE XXX
        Terminates the core.
        Produces list a.
  Examples:
        ~palryp-hocsyt/try=> lyst
        [i=1 t=[i=97 t=[i=2 t=[i=98 t=[i=[~ u=10] t=~]]]]]
        ~palryp-hocsyt/try=> (homo lyst)
        ~[1 97 2 98 [~ u=10]]
        ---
        ~palryp-hocsyt/try=> =a (limo [1 2 3 ~])
        ~palryp-hocsyt/try=> a
        [i=1 t=[i=2 t=[i=3 t=~]]]
        ~palryp-hocsyt/try=> (homo a)
        ~[1 2 3]
++  limo 
        listify
  Description:
        Produces a list from any null-terminated tuple.
        ---
        Creates a wet %gold gate with a sample which accepts a single list.
        Makes the type of the result the type of the product of the code below (^+).
        The subject of the arm ($) is then the product of the barcen statement below (=<).
        Creates a %gold core (|%) and the arm '+-  $'.
        XXX BLACK BOX, ABANDON ALL HOPE YE WHO ENTER HERE XXX
        Terminates the core.
        Produces list a.
  Examples:
        ~palryp-hocsyt/try=> (limo [1 'a' 2 'b' (some 10) ~])
        [i=1 t=[i=97 t=[i=2 t=[i=98 t=[i=[~ u=10] t=~]]]]]
        ---
        ~palryp-hocsyt/try=> (limo [`a` `b` ~])
        [i=`a` t=[i=`b` t=~]]
        ---
        ~palryp-hocsyt/try=> (limo [2 1 ~])
        [i=2 t=[i=1 t=~]]
++  lent 
        length
  Description:
        Produces the atomic length of any list.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accpets a single list.
        Must produce an atom.
        Let b be 0.
        Creates and kicks a dry %gold trap.
        Builds an if-then-else statement on "a is an atom."  If so, produces b.
        Else, recursively calls length with the list set to the tail of a and the accumulator, b, incremented.
  Examples:
        ~palryp-hocsyt/try=> (lent (limo [1 2 3 4 ~]))
        4
        ---
        ~palryp-hocsyt/try=> (lent (limo [1 'a' 2 'b' (some 10) ~]))
        5
++  levy
       all of
  Description:
        Applies the loobean gate 'b' to every element of the list 'a', producing the logical AND of all the results.
        ---
        Activate jet.
        Creates a wet %gold gate with a sample which accepts a list and a gate with a sample of any noun
        and produces a loobean.
        Then, a dry %gold trap is created and kicked.  It must produce a loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, build an if-then-else statement on (b i.a)
        If so, then recursively call levy with a replaced by the tail of a.
        Else, produce no.
  Examples:
        ~palryp-hocsyt/try=> =b |=(a=@ (gte a 1))
        ~palryp-hocsyt/try=> (levy (limo [0 1 2 1 ~]) b)
        %.n
        ---
        ~palryp-hocsyt/try=> =b |=(a=@ (gte a 0))
        ~palryp-hocsyt/try=> (levy (limo [0 1 2 1 ~]) b)
        %.y        
++  lien 
        Is the slam of any element in list `a` to `b` true?
        ---
        Activate jet.
        Build wet %gold gate with sample list `a`, gate which accepts a noun and produces a bean `b`
        Kick dry %gold trap.  Yield bean.
        If: `a` is null,
                Then: Produce false.
        Else: Unless the slam of the head of `a` to `b`,
                Then: Produce true.
        Else: Produce the toss of `a` for `t.a`
        ---
        ~palryp-hocsyt/try=> =a |=(a=@ (gte a 1))
        ~palryp-hocsyt/try=> (lien (limo [0 1 2 1 ~]) a)
        %.y
        ~palryp-hocsyt/try=> =a |=(a=@ (gte a 3))
        ~palryp-hocsyt/try=> (lien (limo [0 1 2 1 ~]) a)
        %.n
++  reel 
        Right fold - Move right to left recursively slamming a binary gate with an element from the list and an accumulator,
        producing the final value of the accumulator.
        ---
        Activate jet.
        Build wet %gold gate sample list `a`, bunt of gate `b` which accepts two nouns and produces `q`
        Kick dry %gold trap.  Cast the result to the type of `q` in `b`
        If: a is an atom:
                Then: Produce the noun `q` in the sample of `b`
        Else: Produce `b` slammed by:
                The head of `a`
                The toss of `a` for the tail of `a`
        ---
        ~palryp-hocsyt/try=> =sum =|([p=@ q=@] |.((add p q)))
        ~palryp-hocsyt/try=> (reel (limo [1 2 3 4 5 ~]) sum)
        15
        ~palryp-hocsyt/try=> =a =|([p=@ q=@] |.((sub p q)))
        ~palryp-hocsyt/try=> (reel (limo [6 3 1 ~]) a)
        4
        ~palryp-hocsyt/try=> (reel (limo [3 6 1 ~]) a)
        ! subtract-underflow
        ! exit
++  roll 
        Left fold - Move left to right recursively slamming a binary gate with an element from the list and an accumulator,
        producing the final value of the accumulator.
        ---
        Activate jet.
        Build wet %gold gate with sample list `a`, bunt of gate `b` which accepts two nouns and produces `q`
        Kick dry %gold trap.  Cast the result to the type of `q` in `b`
        If: a is an atom,
                Then: Produce `q` in `b`
        Else: Produce the toss of `a` for the tail of `a`, `b` for `b` with `q` replaced by `b` slammed by the head of `a`
        and `q` in `b`
        ---
        ~barred-tidset/try=> =a =|([p=@ q=@] |.((sub p q)))
        ~barred-tidset/try=> (roll (limo [1 2 3 ~]) a)
        2
        ~barred-tidset/try=> (roll (limo [3 6 3 ~]) a)
        0
++  skid 
        Seperate a list `a` into two lists - Those elements of `a` who produce true when slammed to `b` and those who produce false.
        ---
        Activate jet.
        Build wet %gold gate with sample list `a`, tile of gate `b` accepting a noun and producing a loobean.
        Kick a dry %gold trap.  Cast the result to the type of [p=a q=a], a cell of lists of type `a`
        If: a is null,
                Then: Produce [~ ~] a cell of null and null,
        Else: Push `c` is the toss of `a` for the tail of `a`
        If: The slam of the head of `a` to `b`,
                Then: Produce the cell with the head of `a` added to the left element,
        Else: Produce the cell with the head of `a` added to the right element.
                        Where `p.c` and `q.c` are the left and right elements, respectively.
        ---
        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skid (limo [0 1 2 3 ~]) a)
        [p=[i=2 t=[i=3 t=~]] q=[i=0 t=[i=1 t=~]]]
++  skim 
        only
  Description:
        Accepts a list 'a' and a gate 'b' which takes any noun and produces loobean.  
        Produces the sublist of 'a' whose elements produce true by slamming the gate with them.
        ---
        Activate jet.
        Creates a wet %gold gate with a sample which accepts a list and a gate which accepts any noun and 
        produces a loobean.
        Creates and kicks a dry %gold trap.
        Which must produce a list of a's type.
        Builds an if-then-else statement on "a is an atom."
        If so, prodoce null.
        Else, build an if-then-else statement on (b i.a).  If so, produce [i.a $(a t.a)], where $(a t.a) is the recursive
        call of skim with a replaced by the tail of a.
        Else, produce $(a t.a).
  Examples:
        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skim (limo [0 1 2 3 ~]) a)
        [i=2 t=[i=3 t=~]]
++  skip 
        except
  Description:
        Accepts a 'a' list and a gate 'b' which takes any noun and produces a loobean.
        Produces the sublist of 'a' whose elments produce false by slamming the gate with them.
        ---
        Activate jet.
        Creates a wet %gold gate with a sample which accepts a list and a gate which accepts any noun and 
        produces a loobean.
        Creates and kicks a dry %gold trap.
        Which must produce a list of a's type.
        Builds an if-then-else statement on "a is an atom."
        If so, produce null.
        Else build na if-then-else statement on (b i.a).  If so, produce the recursive call of skip with a replacd by
        the tail of a.
        Else, produce [i.a $(a t.a)].
  Examples:
        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skip (limo [0 1 2 3 ~]) a)
        [i=0 t=[i=1 t=~]]
++  scag 
        prefix
  Description:
        Accepts an atom 'n' and list 'b',  producing the first n elements at the front of the list.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts an atom and a list.
        A dry %gold trap is created and kicked.  It must produce a list of the same type as 'b'.
        Builds an if-then-else statement on the logical OR statement (a=0 or b is in null).
        If so, produce null.
        Else, produce [i.b $(b t.b, a (dec a))] where 'i.b' is the head of 'b' and $(b t.b, a (dec a))
        is the recursive call of scag with 'b' replaced by the tail of 'b' and 'a' decremented.
  Examples:
        ~palryp-hocsyt/try=> (scag 2 (limo [0 1 2 3 ~]))
        [i=0 t=[i=1 t=~]]
        ---
        ~palryp-hocsyt/try=> (scag 10 (limo [1 2 3 4 ~]))
        [i=1 t=[i=2 t=[i=3 t=[i=4 t=~]]]]
++  slag 
        suffix
  Description:
        Accepts an atom 'n' and a list 'b', producing the last n elemnents at the back of the list.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts an atom and a list.
        A dry %gold trap is created and kicked.  It must produce a list of the same type as 'b'.
        Builds an if-then-else statement on a=0.
        If so, produce 'b'.
        Else, build an if-then-else statement on "b is an atom."
        If so, produce null.
        Else, call slag recursively with b replaced by the tail of b, a replaced by the decrement of a.
  Examples:
        ~palryp-hocsyt/try=> (slag 2 (limo [0 1 2 3 ~]))
        [i=2 t=[i=3 t=~]] 
        ---
        ~palryp-hocsyt/try=> (slag 2 (limo [1 2 3 4 ~]))
        [i=3 t=[i=4 t=~]]
++  snag 
        index
  Description:
        Accepts an atom and a list, producing the element at the index of the atom in the list and failing is the list
        is null.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts an atom and a list.
        A dry %gold trap is created and kicked.
        Builds an if-then-else statement on "b is null."
        If so, fail with "snag-fail" in the stack trace.
        Else, build an if-then-else statement on a=0.
        If so, produce the head of 'b'.
        Else, recursively call snag with 'b' replaced by the tail of 'b' and 'a' decremented.
  Examples:
        ~palryp-hocsyt/try=> (snag 2 (limo [3 2 1 0 ~]))
        1
        ---
        ~palryp-hocsyt/try=> (snag 4 (limo [1 2 3 4 5 6 7 8 ~]))
        5
++  sort 
        quicksort
  Description:
        Accepts a list and a gate with a sample which accepts two nouns and produces a loobean.  'sort' then produces a 
        list of the elements of 'a' sorted according to 'b'.
        ---
        Activate jet.
        Creates a wet %gold gate with a sample which accepts a list and a gate which accepts two nouns and
        produces a loobean.
        Homogenizes the list and makes it the subject of the following code, casting the following to the
        homogenized list type.
        Creates and kicks dry %gold trap.  It must produce a list of a's type.
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Slam the weld gate with the q and r below.
        The q and r are then defined to be the recursive call of the trap with the skim of the tail by our sort gate.
        For q, it skims by (b c i.a).  For r, by !(b c i.a).
        r is first cast to the type of the tail of 'a' and produced as a tuple behind the head of 'a'.
  Examples:
        ~dovryp-toblug/try=> =a =|([p=@ q=@] |.((gth p q)))
        ~dovryp-toblug/try=> (sort (limo [0 1 2 3 ~]) a)
        ~[3 2 1 0]
++  swag 
        infix
  Description:
        A range in a list - Produces the values in list 'c' starting at index 'a' and spanning 'b' elements
        more than that.
        ---
        Creates a wet %gold gate with a sample which gate which accepts a tuple of atoms and a list 'c'.
        The last 'a' elements in 'c' are selected by slag.  Then the first 'b' elements of 
        those last elements are selected and produced by scag.
  Examples:
        ~palryp-hocsyt/try=> (swag [0 5] (limo [1 2 3 4 5 6 7 8 9 10 ~]))
        [i=1 t=[i=2 t=[i=3 t=[i=4 t=[i=5 t=~]]]]]
        ---
        ~palryp-hocsyt/try=> (swag [3 5] (limo [1 2 3 4 5 6 7 8 9 10 ~]))
        [i=4 t=[i=5 t=[i=6 t=[i=7 t=[i=8 t=~]]]]]
        ---
        ~palryp-hocsyt/try=> (swag [1 2] (limo [1 2 3 ~]))
        [i=2 t=[i=3 t=~]] 
++  turn 
        Accepts a list and a gate.  Produces the list with the gate applied to each element of the original list.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts a list and a gate.
        Creates and kicks a dry %gold trap.
        Builds an if-then-else statement on "a is an atom."
        If so, produce null.
        Else, produce the tuple with head (b i.a) and tail that is turn applied recursively to the tail of 'a'.
        ---
        ~dovryp-toblug/try=> (turn (limo [104 111 111 110 ~]) ,@t)
        <|h o o n|>
++  weld 
        concatenate
  Description:
        Concatenates two lists.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts two lists.
        Homogenizes both lists and makes them the subject of the following code.
        A dry %gold trap is created and kicked.  It must produce the type of list 'b'.
        Builds an if-then-else statement on "a is null."  If so, produce 'b'.
        Else, produce the tuple [i.a $(a t.a)].  'i.a' is the head of 'a', $(a t.a) the recursive call of weld
        with 'a' replaced by the tail of a.
  Examples:
        ~palryp-hocsyt/try=> (weld (limo [1 2 3 ~]) (limo [4 5 6 ~]))
        ~[1 2 3 4 5 6]
        ~palryp-hocsyt/try=> (weld "foo" "bar")
        ~[~~f ~~o ~~o ~~b ~~a ~~r]
++  welp 
        perfect weld
  Description:
        Concatenates two lists without losing their type information to homogenization.
        Produces a tape when passed two tapes.
        ---
        XXX DON'T WORRY ABOUT HOW THIS WORKS, IT IS A PERPETUAL MIYSTERY TO US ALL. XXX
  Examples:
        ~palryp-hocsyt/try=> (welp "foo" "bar")
        "foobar"
++  wild 
        concatenate
  Description:
        Concatenates two lists without casting the product back to a list.
        ---
        Creates a wet %gold gate with a sample which accepts two lists.
        Homogenizes both lists and makes them the subject of the following code.
        A dry %gold gate is created and kicked.
        Builds an if-then-else statement on "a is null."  If so, produce 'b'.
        Else, produce the tuple with head (b i.a) and tail that is turn applied recursively to the tail of 'a'.
  Examples:
        ~palryp-hocsyt/try=> =norm (limo [1 2 3 4 5 ~])
        ~palryp-hocsyt/try=> =norm2 (limo [6 7 8 ~])
        ~palryp-hocsyt/try=> (wild norm norm2)
        ~[1 2 3 4 5 6 7 8]
        ---
        ~palryp-hocsyt/try=> (wild "foo" "bar")
        ~[~~f ~~o ~~o ~~b ~~a ~~r]
        ---
        ~palryp-hocsyt/try=> (homo (weld "foo" "bar"))
        ~[~~f ~~o ~~o ~~b ~~a ~~r]
        ~palryp-hocsyt/try=> (homo (wild "foo" "bar"))
        ! -find-limb.t
        ! find-fork
        ! exit
++  zing 
        promote
  Description:
        Turns a list of lists into a single list by promoting the elements of each sublist into the higher.
        ---
        Creates a wet %gold gate with a sample that accepts a list of lists.
        Casts the result to the type the homogenized list head, asserting that 'a' is at least a cell.
        A dry %gold trap is created and kicked.
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Else, weld together the head of a with the recrusive call of zing on the tail of a.
  Examples:
        ~palryp-hocsyt/try=> (zing (limo [(limo ['a' 'b' 'c' ~]) (limo ['e' 'f' 'g' ~]) (limo ['h' 'i' 'j' ~]) ~]))
        ~['a' 'b' 'c' 'e' 'f' 'g' 'h' 'i' 'j']
        ~palryp-hocsyt/try=> (zing (limo [(limo [1 'a' 2 'b' ~]) (limo [3 'c' 4 'd' ~]) ~]))
        ~[1 97 2 98 3 99 4 100]

chapter 2c, simple noun surgery

section 2cA, bit surgery              **capitalization of "section" inconsistent in source**

++  bex  
        binary exponent
  Description:
        Produces 2 to the nth power for some atom 'n'.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts a single atom.
        Casts the result to an atom.
        Builds an if-then-else statement on a=0.
        If so, produce 1.
        Else, multiply two by the recursive call of bex on the decrement of a.
  Examples:
        ~palryp-hocsyt/try=> (bex 4)
        16
        ~palryp-hocsyt/try=> (bex (add 19 1))
        1.048.576
        ~palryp-hocsyt/try=> (bex 0)
        1
++  xeb  
        binary logarithm
  Description:
        Takes the base-2 logarithm of an atom.
        ---
        Creates a dry %gold gate with a sample which accepts an atom.
        Casts the result to an atom.
        Evaluates the logarithm by counting the number of bits the number in question occupies.
  Examples:
        ~palryp-hocsyt/try=> (xeb 31)
        5
        --
        ~palryp-hocsyt/try=> (xeb 32)
        6
        --
        ~palryp-hocsyt/try=> (xeb 49)
        6
        --
        ~palryp-hocsyt/try=> (xeb 0)
        0
        --
        ~palryp-hocsyt/try=> (xeb 1)
        1
        --
        ~palryp-hocsyt/try=> (xeb 2)
        2
++  can  
        assemble
  Description:
        Assembles a 
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a bloq size (an atom) and
        a list of atomic tuples.
        Casts the result to an atom.
        Builds an if-then-else statement on "b is null."
        If so, produce 0.
        Else, 
  Examples:
        ~ronrem-lonsem/try=> `@ub`(can 3 ~[[1 1]])
        0b1 
        ---
        ~ronrem-lonsem/try=> `@ub`(can 0 ~[[1 255]])
        0b1
        ---
        ~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 2]])
        0b10
        ---
        ~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 3]])
        0b11
        ---
        ~ronrem-lonsem/try=> `@ub`(can 1 ~[[1 4]])
        0b0
        ---
        ~ronrem-lonsem/try=> `@ub`(can 1 ~[[2 4]])
        0b100
++  cat  
        concatenate
  Description:
        Concatenates two atoms, obeying the given block size.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a bloq size (an atom) and
        two atoms.
        Measures the number of blocks of size 'a' are in 'b'.
        Left shifts 'c' that many times the bock size of 'a'.
        Sums the result of the left shift with 'b'.
  Examples:
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
++  cut  
        slice
  Description:
        Accepts a block size 'a', a cell of two atoms 'b' and 'c' and another atom 'd'.
        Produces the tail of 'd' that is 'c' blocks long after right-shifting 'd' 'b'-blocks.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom),
        a cell of two atoms, and another atom which will be "cut."
        Right-shifts 'd' by 'b' blocks.  Then produces the 'c' block long tail of this right-shift.
  Examples:
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
++  end  
        tail
  Description:
        Accepts a block 'a' and two atoms, 'b' and 'c'.
        Produces the 'b' blocks of length 'a' on the end of 'c'.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block (an atom) and
        two atoms.
        Multiplies the binary exponent of 'a' (2^a) with 'b', then takes the binary exponent
        of that (2^((2^a)*b)) to finally produce the modulus of 'c' and the ensuing product.
  Examples:
        ~ronrem-lonsem/try=> `@ub`12
        0b1100
        ---
        ~ronrem-lonsem/try=> `@ub`(end 0 3 12)
        0b100
        ---
        ~ronrem-lonsem/try=> (end 0 3 12)
        4
        ---
        ~ronrem-lonsem/try=> `@ub`(end 1 3 12)
        0b1100
        ---
        ~ronrem-lonsem/try=> (end 1 3 12)
        12
        ---
        ~ronrem-lonsem/try=> (end 3 1 256)
        0
        ---
        ~ronrem-lonsem/try=> (end 3 1 255)
        255 
++  fil  
        fill bloqstream
  Description:
        
        ---
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        two other atoms.
        Let 'n' be 0.
        Let 'd' be 'c'.
        Creates and kicks a dry %gold trap whose result is cast to a atom.
        Builds an if-then-else statement on n=b.
        If so, produce the right-shift of 'd' by one block.
        Else, recursively call the trap with 'd' replaced by the sum of 'c' and the one block
        left-shift of 'b', n replaced by the increment of n.
  Examples:
        
++  lsh  
        left shift
  Description:
        Accepts a block size 'a' and two atoms 'b' and 'c'.  Produces 'c' left-shifted 
        'b' times by the block size.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block (an atom) and
        two atoms.
        Multiplies 'c' times the binary exponent of the binary exponent of 'a' times 'b',
        that is ((2^((2^a)*b))*c), this producing the desired left-shift on 'c'.
  Examples:
        ~ronrem-lonsem/try=> `@ub`1
        0b1
        ---
        ~ronrem-lonsem/try=> `@ub`(lsh 0 1 1)
        0b10
        ---
        ~ronrem-lonsem/try=> (lsh 0 1 1)
        2
        ---
        ~ronrem-lonsem/try=> `@ub`255
        0b1111.1111
        ---
        ~ronrem-lonsem/try=> `@ub`(lsh 3 1 255)
        0b1111.1111.0000.0000
        ---
        ~ronrem-lonsem/try=> (lsh 3 1 255)
        65.280
++  met  
        measure
  Description:
        Measures the number of blocks of size 'a' are in 'b'.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        an atom.
        Casts the result to an atom.
        Let 'c' be 0.
        Creates and kicks a dry %gold trap.
        Builds an if-then-else statement on b=0.
        If so, produce c.
        Else, recursively call the trap, with 
        'b' replaced by the single-block right-shift of 'b' and 'c' by the increment of 'c'.
  Examples:
        ~ronrem-lonsem/try=> (met 0 1)
        1
        ~ronrem-lonsem/try=> (met 0 2)
        2
        ~ronrem-lonsem/try=> (met 3 255)
        1
        ~ronrem-lonsem/try=> (met 3 256)
        2
++  rap  
        Concatenate a list of atoms while obeying a given blocksize.
        ---
        Activate jet.
        Build a dry %gold gate with sample bloq `a`, list of atoms `b`
        Yield atom
        If: `b` is null,
                Then: Produce 0.
        Else: Produce cat slammed with `a`, the head of `b`, and the toss of `b` for the tail of `b`
        ---
        ~palryp-hocsyt/try=> (rap 2 (limo [1 2 3 4 ~]))
        17.185
        ~palryp-hocsyt/try=> (rap 1 (limo [1 2 3 4 ~]))
        313
        ~palryp-hocsyt/try=> (rap 0 (limo [0 0 0 ~]))
        0
        ~palryp-hocsyt/try=> (rap 0 (limo [0 0 1 ~]))
        1
++  rep  
        Assembles 

        ---
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
  Examples:
        ~palryp-hocsyt/try=> (rep 0 (limo [1 2 3 4 ~]))
        5
        ---
        ~palryp-hocsyt/try=> (rep 1 (limo [1 2 3 4 ~]))
        57
        ---
        ~palryp-hocsyt/try=> (rep 0 (limo [1 0 0 ~]))
        1
        ---
        ~palryp-hocsyt/try=> (rep 0 (limo [1 0 0 0 ~]))
        1
        ---
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 0 ~]))
        2
        ---
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 1 ~]))
        10
        ---
        ~palryp-hocsyt/try=> (rep 0 (limo [0 1 0 1 0 1 ~]))
        42
++  rip  
        disassemble
  Description:
        Produces a list of the bits of an atom, in little endian order, according to
        block size.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and 
        any number.
        Cast the result to a list of atoms.
        Builds an if-then-else statement on b=0.
        If so, produce null.
        Else, produce a tuple with head of (end a 1 b), the single-block tail of 'b', and
        the resursive call of rip with 'b' replaced by the single-block right-shift of 'b'.
        in little endian.
  Examples:
        palryp-hocsyt/try=> `@ub`155
        0b1001.1011
        ---
        ~palryp-hocsyt/try=> (rip 0 155)
        ~[1 1 0 1 1 0 0 1]
        ---
        ~palryp-hocsyt/try=> (rip 2 155)
        ~[11 9]
        ---
        ~palryp-hocsyt/try=> (rip 1 155)
        ~[3 2 1 2]
        ---
        ~palryp-hocsyt/try=> `@ub`256
        0b1.0000.0000
        ---
        ~palryp-hocsyt/try=> (rip 0 256)
        ~[0 0 0 0 0 0 0 0 1]
        ---
        ~palryp-hocsyt/try=> (rip 2 256)
        ~[0 0 1]
        ---
        ~palryp-hocsyt/try=> (rip 3 256)
        ~[0 1]
        
++  rsh  
        right shift
  Description:
        Accepts a block size 'a' and two atoms, 'b' and 'c'.  Right-shifts 'c' by 'b' blocks
        of size 'a'.
        ---
        Activate jet.
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        two atoms.
        Takes the binary exponent of the binary exponent of 'a' multiplied by 'b',
        that is (2^(((2^a)*b))) and divides 'c' by it, producing the desired 
        right-shift on 'c'.
  Examples:
        ~ronrem-lonsem/try=> `@ub`145
        0b1001.0001
        ---
        ~ronrem-lonsem/try=> `@ub`(rsh 1 1 145)
        0b10.0100
        ---
        ~ronrem-lonsem/try=> (rsh 1 1 145)
        36
        ---
        ~ronrem-lonsem/try=> `@ub`(rsh 2 1 145)
        0b1001
        ---
        ~ronrem-lonsem/try=> (rsh 2 1 145)
        9
        ---
        ~ronrem-lonsem/try=> `@ub`10
        0b1010
        ---
        ~ronrem-lonsem/try=> `@ub`(rsh 0 1 10)
        0b101
        ---
        ~ronrem-lonsem/try=> (rsh 0 1 10)
        5
        ---
        ~ronrem-lonsem/try=> `@ub`1
        0b1
        ---
        ~ronrem-lonsem/try=> (rsh 0 1 1)
        0
        ---
        ~ronrem-lonsem/try=> (rsh 0 1 1)
        0
++  swap 
        reverse bloq order
  Description:
        Produces the reversed block order of a number, obeying block number.
        Switches little ending to big and vice versa.
        ---
        Creates a dry %gold gate with a sample which accepts a block size (an atom) and
        an atom.
        Rips apart the atom by the block size, then reverses the tape that is produced.
        Once it is reversed, it is re-assembled using rep.
  Examples:
        ~palryp-hocsyt/try=> `@ub`24
        0b1.1000
        ---
        ~palryp-hocsyt/try=> (swap 0 24)
        3
        ---
        ~palryp-hocsyt/try=> `@ub`3
        0b11
        ---
        ~palryp-hocsyt/try=> (swap 0 0)
        0
        ---
        ~palryp-hocsyt/try=> (swap 1 24)
        9
        ---
        ~palryp-hocsyt/try=> (swap 0 128)
        1
section 2cB, bit logic                

++  con  
        binary or
  Description:
        Produces the bit-wise logical OR of two atoms.
        ---
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
  Examples:
        ~palryp-hocsyt/try=> (con 0 1)
        1
        ---
        ~palryp-hocsyt/try=> (con 1 0)
        1
        ---
        ~palryp-hocsyt/try=> (con 0 0)
        0
        ---
        ~palryp-hocsyt/try=> (con 4 4)
        4
        ---
        ~palryp-hocsyt/try=> (con 10.000 234)
        10.234
++  dis  
        binary and
  Description:
        Produces the bit-wise logical AND of two atoms.
        ---
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
  Examples:
        ~ronrem-lonsem/try=> `@ub`9
        0b1001
        ---
        ~ronrem-lonsem/try=> `@ub`5
        0b101
        ---
        ~ronrem-lonsem/try=> `@ub`(dis 9 5)
        0b1
        ---
        ~ronrem-lonsem/try=> (dis 9 5)
        1
        ---
        ~ronrem-lonsem/try=> `@ub`534
        0b10.0001.0110
        ---
        ~ronrem-lonsem/try=> `@ub`987
        0b11.1101.1011
        ---
        ~ronrem-lonsem/try=> `@ub`(dis 534 987)
        0b10.0001.0010
        ---
        ~ronrem-lonsem/try=> (dis 534 987)
        530
++  mix  
        binary xor
  Description:
        Produces the bit-wise logical exclusive OR of two atoms.
        ---
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
  Examples:
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
++  not  
        binary not (sized)
  Description:
        Produces the bit-wise logical NOT over 'b' blocks of the given blocksize.
        ---
        First produces the binary exponent of the binary exponent of the block size times       
        'b'.  This is decremented before being multiplied by 'c'.
        Finally, this product is exclusive ORed and produced.
  Examples:
        ~palryp-hocsyt/try=> `@ub`24
        0b1.1000
        ---
        ~palryp-hocsyt/try=> (not 0 5 24)
        7
        ---
        ~palryp-hocsyt/try=> `@ub`7
        0b111
        ---
        ~palryp-hocsyt/try=> (not 2 5 24)
        1.048.551
        ---
        ~palryp-hocsyt/try=> (not 2 5 1.048.551)
        24
        ---
        ~palryp-hocsyt/try=> (not 1 1 (not 1 1 10))
        10

section 2cC, noun orders              

++  aor  
        a-order
  Description:
        Alphabetic comparator gate.
        ---
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
  Examples:
       ~tadbyl-hilbel/try=> (aor 'a' 'b')
        %.y
        ~tadbyl-hilbel/try=> (aor 'b' 'a')
        %.n
        ---
        ~tadbyl-hilbel/try=> (aor "foo" "bar")
        %.n
        ~tadbyl-hilbel/try=> (aor "bar" "foo")
        %.y
        ---
        ~tadbyl-hilbel/try=> (aor "abcdefz" "abcdefa")
        %.n
        ~tadbyl-hilbel/try=> (aor "abcdefa" "abcdefz")
        %.y
        ---
        ~tadbyl-hilbel/try=> (aor 10.000 17.000)
        %.y
        ~tadbyl-hilbel/try=> (aor 10 9)
        %.n
++  dor  
        d-order
  Description:
        Numeric comparator gate.
        ---
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
  Examples:
        ~tadbyl-hilbel/try=> (dor 1 2)
        %.y
        ~tadbyl-hilbel/try=> (dor 2 1)
        %.n
        ---
        ~tadbyl-hilbel/try=> (dor ~[1 2 3] ~[1 2 4])
        %.y
        ~tadbyl-hilbel/try=> (dor ~[1 2 4] ~[1 2 3])
        %.n
        ---
        ~tadbyl-hilbel/try=> (dor (limo ~[99 100 10.000]) ~[99 101 10.000])
        %.y
        ~tadbyl-hilbel/try=> (dor ~[99 101 10.999] (limo ~[99 100 10.000]))
        %.n
++  gor  
        g-order
  Description:
        Hash comparator gate.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Let 'c' be the mug (FNV-1a hash) of 'a' an 'd' the mug of 'b'.
        Create an if-then-else statement on c=d.
        If so, produce the d-order of 'a' and 'd'.
        Else, produce the loobean (c<d).
  Examples
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
        ---
        ~palryp-hocsyt/try=> (gor "foo" "bar")
        %.n
        ---
        ~palryp-hocsyt/try=> (gor (some 10) (limo [1 2 3 ~]))
        %.n
++  hor  
        h-order
  Description:
        Recursive hash comparator gate.
        ---
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
  Examples:
        
++  vor
        v-order
  Description:
        Double hash comparator gate.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts two nouns.
        Casts the result to a loobean.
        Let 'c' be the double mug (FNV-1a hash) of 'a', 'd' that of 'b'.
        Builds an if-then-else statement on (c=d).
        If so, produce the d-order of 'a' and 'b'.
        Else, produce the loobean of (c<d).
  Examples:
        ~palryp-hocsyt/try=> (vor 'f' 'g')
        %.y
        ---
        ~palryp-hocsyt/try=> (vor 'a' 'z')
        %.n
        ---
        ~palryp-hocsyt/try=> (vor 43.326 41.106)
        %.n

section 2cD, insecure hashing         

++  fnv
        FNV scrambler
  Description:
        Hashes an atom with the 32-bit FNV non-cryptographic hash algorithm.
        ---
        Multiplies 'a' by the prime number 16,777,619 and then takes the block of
        size 5 off the product's end.
  Examples:
        ~palryp-hocsyt/try=> (fnv 10.000)
        272.465.456
        ---
        ~palryp-hocsyt/try=> (fnv 10.001)
        289.243.075
        ---
        ~palryp-hocsyt/try=> (fnv 1)
        16.777.619
++  mug
        31bit nonzero FNV1a
  Description:
        Hashes any noun with the 31-bit nonzero FNV-1a non-cryptographic hash algorithm.
        ---
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
        
  Examples:
        ~palryp-hocsyt/try=> (mug 10.000)
        178.152.889
        ---
        ~palryp-hocsyt/try=> (mug 10.001)
        714.838.017
        ---
        ~palryp-hocsyt/try=> (mug 1)
        67.918.732
        ---
        ~palryp-hocsyt/try=> (mug (some 10))
        1.872.403.737
        ---
        ~palryp-hocsyt/try=> (mug (limo [1 2 3 4 5 ~]))
        1.067.931.605

section 2cE, phonetic base            

++  po
        left-right syllable
  Description:
        Provides the phonetic syllables and name generators for the Urbit naming system.
        ---
        Activate jet.
        Create the cell [sis dex] where 'sis' and 'dex' are the togas on the 
        left-hand ("sinister") and right-hand ("Dexter") phonetic syllable cords, respectively.
        Build %gold core to contain the following arms.
  ++  ind 
        
  Description:
        ---
        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Let 'b' be 0.
        Creates and kicks a dry %gold trap, casting the result to an atomic unit.
        Builds an if-then-else statement on (b=256).  If so, produce null.
        Else, build an if-then-else statement on (a=(tod b)).  If so, produce the atomic unit [~ b].
        Else, recursively call the trap with 'b' replaced by the increment of 'b'.
  Examples:
  
  ++  ins 
        
  Description:
        ---
        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Let 'b' be 0.
        Creates and kicks a dry %gold trap with the result cast to an atomic unit.
        Builds an if-then-else statement on (b=256). If so, produce null.
        Else, build an if-then-else statement on (a=(tos b)).  If so, produce the atomic unit [~ b].
        Else, recursively call the trap with 'b' replaced by the increment of 'b'.
  Examples:

  ++  tod 
  Description:
        Selects right-hand phonetic syllable from 'dex'.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Assert that 'a' is less than 256.
        Produce the three tail-end byte blocks in the rght-shift of dex.
  Examples:
        ~palryp-hocsyt/try=> (tod:po 98)
        6.514.020
        ---
        ~palryp-hocsyt/try=> (tod:po 150)       
        6.781.298
        ---
        ~palryp-hocsyt/try=> (tod:po 255)
        7.562.598
        ---
        ~palryp-hocsyt/try=> (tod:po 256)
        ! exit
  ++  tos 
        Selects left-hand phonetic syllable from 'sin'.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts and atom.
        Assert that 'a' is less than 256.
        Produce the three tail-end byte blocks in the rght-shift of dex.
  Examples:
section 2cF, signed and modular ints  

++  si    
        signed integer
  ++  abs 
        absolute value
  Description:
        Produces the absolute value of a signed integer.
        ---
        Creates a dry %gold gate with a sample which accepts a single atom.
        Sums the last bit of the atom with the single bit-wise block right-shift of the atom,
        producing the absolute value.
  Examples:
        ~palryp-hocsyt/try=> (abs:si -2)
        2
        ---
        ~palryp-hocsyt/try=> (abs:si -10.000)
        10.000
        ---
        ~palryp-hocsyt/try=> (abs:si --2)
        2
  ++  dif 
        subtraction
  Description:
        Produces the difference between two signed integers.
        ---
        Creates a dry %gold gate with a sample which accepts two signed integers.
        Sums the first signed integer with a new signed integer, made from the second by
        (new !(syn b) (abs b)), where !(syn b) is the negative of the second integer's sign.
        This sum, produced, is the difference.
  Examples:
        ~palryp-hocsyt/try=> (dif:si --10 -7)
        --17
        ---
        ~palryp-hocsyt/try=> (dif:si --10 --7)
        --3
        ---
        ~palryp-hocsyt/try=> (dif:si `@s`0 --7)
        -7
        ---
        ~palryp-hocsyt/try=> (dif:si `@s`0 `@s`7)
        --4
  ++  dul 
        modulus
  Description:
        Produces the modulus of two signed integers.
        ---
        Creates a dry %gold gate which accepts a signed integer and an atom.
        Let 'c' be the [sign value] representation of 'a'.
        Builds an if-then-else statement on -.c, the sign of 'a'.
        If so ('a' is positive.), produce the modulus of the absolute value of 'c' and 'b'.
        Else, produce the differenece between 'b' and the absolute value of 'c'.
  Examples:
        ~palryp-hocsyt/try=> (dul:si --9 3)
        0
        ---
        ~palryp-hocsyt/try=> (dul:si --9 4)
        1
        ---
        ~palryp-hocsyt/try=> (dul:si --9 5)
        4
        ---
        ~palryp-hocsyt/try=> (dul:si --9 6)
        3
        ---
        ~palryp-hocsyt/try=> (dul:si --90 --10)
        10
  ++  fra 
        divide
  Description:
        Produces the quotient of two signed integers.
        ---
        Creates a dry %gold gate with a sample which accepts two signed integers.
        Divides the absolute value of 'a', the dividend, and 'b', the divisor, and
        passes that value as the unsigned integer value of a new signed integer.
        The sign of the new signed integer is the bitwise logical XOR of the two integer's 
        signs, meaning the quotient is only positive when both factors are positive.
        This new signed integer is produced.
  Examples:
        ~palryp-hocsyt/try=> (fra:si --4 --2)
        --2
        ---
        ~palryp-hocsyt/try=> (fra:si -4 -2)
        --2
        ---
        ~palryp-hocsyt/try=> (fra:si -4 --2)
        -2
        ---
        ~palryp-hocsyt/try=> (fra:si --4 -2)
        -2
        ---
        ~palryp-hocsyt/try=> (fra:si `@s`4 `@s`2)
        --2
        ---
        ~palryp-hocsyt/try=> (fra:si `@s`4 2)
        ! type-fail
        ! exit
  ++  new 
        [sign value] to @s
  Description:
        Produces a signed integer from a sign value (either & or |) and an atom.
        ---
        Creates a dry %gold gate with a sample which acccepts a loobean and an atom
        Builds an if-then-else statement on the sign value 'a'.
        If so, just produce 'b' multiplied by 2.
        Else, build an if-then-else statement on b=0.  If so, produce 0.
        Else, produce the increment of (2*(dec b)).
        The result is then cast to an integer and produced from new:si.
  Examples:
        ~palryp-hocsyt/try=> (new:si [& 10])
        --10
        ~palryp-hocsyt/try=> (new:si [| 10])
        -10
        ~palryp-hocsyt/try=> (new:si [%.y 7])
        --7
  ++  old
        [sign value]
  Description:
        Produces the cell [sign value] representations of a signed integer.
        ---
        Create a dry %gold date with a with a sample which accepts a signed integer.
        Produce a cell with head (syn a), the sign of 'a', and tail (abs), the absolute value of 'a'.
  Examples:
        ~palryp-hocsyt/try=> (old:si 7)
        ! type-fail
        ! exit
        ---
        ~palryp-hocsyt/try=> (old:si -7)
        [%.n 7]
        ---
        ~palryp-hocsyt/try=> (old:si --7)
        [%.y 7]
        ---
        ~palryp-hocsyt/try=> (old:si `@s`7)
        [%.n 4]
        ---
        ~palryp-hocsyt/try=> (old:si -0)
        [%.y 0]
  ++  pro       
        Produces the product of two signed integers.
        ---
        Creates a dry %gold gate with a sample which accepts two signed integers.
        Produces their product by evaluating a new signed integer whose sign is the bitwise 
        XOR of the two number's signs and whose value is the product of their two absolute values.
        ---
        palryp-hocsyt/try=> (pro:si -4 --2)
        -8
        ~palryp-hocsyt/try=> (pro:si -4 -2)
        --8
        ~palryp-hocsyt/try=> (pro:si --10.000.000 -10)
        -100.000.000
        ~palryp-hocsyt/try=> (pro:si -1.337 --0)
        --0
  ++  rem 
        Produces the remainder from a division of two signed integers.
        ---
        Creates a dry %gold gate with a sample which accepts two signed integers.
        Produces the difference between 'a' and the (b*(a/b)).
        ---
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
  ++  sum 
        Sum two signed integers.
        ---
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
        ---
        ~palryp-hocsyt/try=> (sum:si --10 --10)
        --20
        ---
        ~palryp-hocsyt/try=> (sum:si --10 -0)
        --10
        ---
        ~palryp-hocsyt/try=> (sum:si -10 -7)
        -17
        ---
        ~palryp-hocsyt/try=> (sum:si -10 --7)
        -3
  ++  sun 
        Produces a signed integer from an unsigned integer.
        Note that the result must be manually cast to some @s odor to be inferred as an
        unsigned integer in the type system.
        ---
        Build dry %gold gate with sample unsigned integer `a`
        Produce the integer multiplied by 2.
        ---
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
  ++  syn 
        Is a signed integer positive?
        Produce the sign of a signed integer - & being posiitve, | negative.
        ---
        Build dry %gold gate with sample signed integer `a`
        Is the last bit of 'a' 0?
        ---
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
++  fe    
        Binary block modulo math engine.  Defaults to bloq size 1.
        ---
        Build dry %gold tray with sample bloq `a`
  ++  dif 
        Produces the difference between two atoms in the modular basis representation.
        ---
        Build dry %gold gate wtih sample atom `b`, atom `c`
        Produce sit slammed with:
                The difference between:
                        The sum of: 
                                `out` and slam of `b` to sit
                                Slam of `c` to sit
        ---
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
  ++  inv 
        Inverts the order of the modular field.
        ---
        Build dry %gold gate with sample atom `b`
        Produce the difference between:
                The decrement of `out`
                Slam of `b` to sit.
        ---
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
  ++  net 
        
        ---
        Build dry %gold gate with sample atom `b`.  Yield atom.
        Push toss of `b` for the slam of `b` to sit on the context.
        Unless: `a` is less than or equal to 3,
                Then: Produce `b`,
        Else: Push `c` is the decrement of `a`
        Produce the slam of con with:
                The single c-block left-shift of:
                        The toss of `a` for `c`, `b` for the c-block [0 1] cut of `b`
                The toss of `a` for `c`, `b` for the c-block [1 1] cut of `b` 
        ---
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
  ++  out       
  Description:
        The maximum integer value that the current block can store.
        ---
        Produce the binary exponent of:
                        The binary expoenent of the block size, `a`
        ---
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
  ++  rol 
        
        ---
        Build dry %gold gate with sample bloq `b`, atom `c`, atom `d`. Yield atom.
        Push `e` is sit slammed with `d`, the modular representation of `d` 
        Push `f` is the binary expoenent of:
                The difference between 'a' and 'b'
        Push `g` is `c` modulus `f`
        Produce sit slammed with:
                con slammed with:
                        The `g` b-blocks right-shift of `e`
                        The difference between `f` and `g` b-blocks left-shift of `e`
        ---
        

  ++  ror 
        
        ---
        Build dry %gold gate with sample bloq `b`, atom `c`, atom `d`.  Yield atom.
        Push `e` is sit slammed with `d`, the modular representation of `d` 
        Push `f` is the binary expoenent of:
                The difference between 'a' and 'b'
        Push `g` is `c` modulus `f`
        Produce sit slammed with:
                con slammed with:
                        The `g` b-blocks left-shift of `e`
                        The difference between `f` and `g` b-blocks right-shift of `e`
        ---

  ++  sum 
        Sum two numbers in this modular field.
        ---
        Build dry %gold gate with sample atom `b`, atom `c`
        Produce sit slammed with the sum of `b` and `c`
        ---
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
  ++  sit 
        Produce an atom in the current modular block representation.
        ---
        Build dry %gold gate with sample atom `b`
        Produce the last block of size `a` in `b`
        ---
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

++  rlyd  
++  rlyh  
++  rlyq  
++  rlys  
++  ryld  
++  rylh  
++  rylq  
++  ryls  

section 2cH, urbit time

Note that entering '-<-' in the shell produces the current time in @da format.
We use this for many of our examples.

~zod/try=> -<-
~2014.8.4..19.39.59..9288

++  year
        Accept a parsed date of form [[a=? y=@ud] m=@ud t=tarp] and produce 
        its @d representation.
        ---
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
        ---
        ~zod/try=> (year [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=4 s=57 f=~[0xd940]]])
        0x8000000d227df4e9d940000000000000
++  yore  
        Produce the parsed date [[a=? y=@ud] m=@ud t=tarp] representation of a @d date. 
        ---
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
        ---
        ~zod/try=> (yore -<-)
        [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=17 s=1 f=~[0x700d]]]
        ~zod/try=> (yore -<-)
        [[a=%.y y=2.014] m=8 t=[d=4 h=20 m=28 s=53 f=~[0x7b82]]]
++  yell  
        Produce a parsed daily time format from an atomic date.
        ---
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
        ---
        ~dovryp-toblug/try=> (yell ~2014.3.20..05.42.53..7456)
        [d=106.751.991.820.094 h=5 m=42 s=53 f=~[0x7456]]
        ~tadbyl-hilbel/try=> (yell ~2014.6.9..19.09.40..8b66)
        [d=106.751.991.820.175 h=19 m=9 s=40 f=~[0x8b66]]
        ~tadbyl-hilbel/try=> (yell ~1776.7.4)
        [d=106.751.991.733.273 h=0 m=0 s=0 f=~]
++  yule  
        Accept a tarp, a parsed daily time, and produces a time atom, @d.
        ---
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
        ---
        ~tadbyl-hilbel/try=> =murica (yell ~1776.7.4)
        ~tadbyl-hilbel/try=> murica
        [d=106.751.991.733.273 h=0 m=0 s=0 f=~]
        ~tadbyl-hilbel/try=> (yule murica)
        0x8000000b62aaf5800000000000000000
        ~dovryp-toblug/try=> (yule (yell ~2014.3.20..05.42.53..7456))
        0x8000000d21c88d5d7456000000000000
        ~tadbyl-hilbel/try=> (yule (yell ~2014.6.9..19.09.40..8b66))
        0x8000000d223413f48b66000000000000
++  yall
        Produce the date tuple of [y=@ud m=@ud d=@ud] of the year, month, and day
        from a number of days from the beginning of time.
        ---
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
        ---
        ~zod/try=> (yall 198)
        [y=0 m=7 d=17]
        ~zod/try=> (yall 90.398)
        [y=247 m=7 d=3]
        ~zod/try=> (yall 0)
        [y=0 m=1 d=1]
++  yawn
        Days since Jesus.  Accpet a year, month, and day (Three unsigned decimal integers) 
        and produce the day number it is in the CE.
        ---
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
        ---
        ~zod/try=> (yawn 2.014 8 4)
        735.814
        ~zod/try=> (yawn 1.776 7 4)
        648.856
++  yelp  
        Is the given year a leap year?
        ---
        Build dry %gold gate with sample atom `yer`.  Yield loobean.
        Produce the logical AND of:
            Is 0 the modulus of `yer` by 4?
            The logical OR of:
                Is 0 the modulus of `yer` by 100?
                Is 0 the modulus of `yer` by 400?
        ---
        ~tadbyl-hilbel/try=> (yelp 2.014)
        %.n
        ~tadbyl-hilbel/try=> (yelp 2.008)
        %.y
        ~tadbyl-hilbel/try=> (yelp 0)
        %.y
        ~tadbyl-hilbel/try=> (yelp 14.011)
        %.n
++  yo
        Constants of time.
        ---
        Build a %gold core.
      ++  cet
        Days in a century.  Derived by multiplying the number of days in a year
        (365) by the number of years in a century (100), then adding the number
        days from leap years in a century (24).
        ---
        ~tadbyl-hilbel/try=> cet:yo
        36.524
        ~tadbyl-hilbel/try=> (add 365 cet:yo)
        36.889
        ~tadbyl-hilbel/try=> (sub (add 24 (mul 100 365)) cet:yo)
        0
      ++  day 
        The number of seconds in a day.  Derived by multiplying the the number
        of seconds in an hour by the hours in a day.
        ---
        ~tadbyl-hilbel/try=> day:yo
        86.400
        ~tadbyl-hilbel/try=> (add 60 day:yo)
        86.460
      ++  era 

        ---
        
        ---
        
      ++  hor 
        The number of seconds in an hour.  Derived by multiplying the number of
        seconds in a minute by the minutes in an hour.
        ---
        ~tadbyl-hilbel/try=> hor:yo
        3.600
      ++  jes
        
        ---
        
        ---
        
      ++  mit 
        The number of seconds in a minute.  We just knew this one.
        ---
        ~tadbyl-hilbel/try=> mit:yo
        60
      ++  moh 
        The days in each month of the Gregorian common year.  A list of 
        unsigned decimal atoms (Either 28, 30, or 31) denoting the number 
        of days in the month at the year at that index.
        ---
        ~tadbyl-hilbel/try=> moh:yo
        ~[31 28 31 30 31 30 31 31 30 31 30 31]
      ++  moy 
        The days in each month of the Gregorian leap-year.  A list of
        unsigned decimal atoms (Either 29,30, or 31) denoting the number
        of days in the month at the leap-year at that index.
        ---
        ~tadbyl-hilbel/try=> moy:yo
        ~[31 29 31 30 31 30 31 31 30 31 30 31]
      ++  qad 
        The number of seconds in four years.  Derived by adding one second
        to the number of seconds in four years.
        ---
        ~tadbyl-hilbel/try=> qad:yo
        126.144.001
      ++  yer 
        The number of seconds in a year.  Derived by multiplying the number of
        seconds in a day by 365.
        ---
        ~tadbyl-hilbel/try=> yer:yo
        31.536.000

section 2cI, almost macros

++  hard
        demand result type
  Description:
        Ruthlessly demands that a specific type be produced, crashing the program is it is not.
        ---
        Creates a vulanized wet gate which accepts any gate which accepts any noun and produces
        any noun.
        Creates a dry %gold gate which accepts any noun and casts the result to the 
        higher gate argument's icon.
        Prints "%hard" in the stack trace if the code below crashes.
        Let gol be the higher gate argument slammed with the lower arbitrary noun.
        Assert that the result's icon is equal to that of the lower arbitrary noun
        before producing said result.
  Examples:
        ~palryp-hocsyt/try=> ((hard (list)) (limo [1 2 3 ~]))
        ~[1 2 3]
        ~tadbyl-hilbel/try=> ((hard ,@) (add 2 2))
        4
        ~tadbyl-hilbel/try=> ((hard ,@t) (crip "Tape to cord, bro!"))
        'Tape to cord, bro'
        ~tadbyl-hilbel/try=> ((hard tape) (crip "...Tape to cord, bro?..."))
        ! hard
        ! exit
++  soft
        politely demand
  Description:
        Politely requests a specific type to be produced, producing null if it is not.
        ---
        Creates a vulanized wet gate which accepts any gate which accepts any noun and produces
        any noun.
        Creates a dry %gold gate which accepts any noun and casts the result to the 
        a unit of the higher gate argument's icon.
        Let gol be the higher gate argument slammed with the lower arbitrary noun.
        Build an unless-then-else statement on the result icon's being equal to that of 
        the lower arbitrary noun.
        If so, produce null.
        Else, produce the unit of the result.
  Examples:
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
        
chapter 2d, containers

section 2dA, sets     
                
++  apt       
        set invariant
  Description:
        Accepts any tree and produces a loobean indicating whether the tree is a set.
        ---
        Creates a dry %gold gate which accepts a tree.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, compute and produce the logical AND of:
        The if "l.a is an atom" then produce true, else (produce the logical AND of the
        v-order of n.a and n.l.a and the h-order of n.l.a and n.a) if-then-else statement.
        The if "r.a is an atom" then produce true, else (produce the logical AND of the 
        v-order of n.a and n.r.a and the h-order of n.a and n.r.a) if-then-else statement.
        (==)  terminates the tall logical AND statement.
  Examples 
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (apt b)
        %.y
        ---
        ~tadbyl-hilbel/try=> =m (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ['c' 4] ['d' 5] ~])
        ~tadbyl-hilbel/try=> m
        {[p='d' q=5] [p='a' q=1] [p='c' q=4] [p='b' q=[2 3]]}
        ~tadbyl-hilbel/try=> (apt m)
        %.y
++  in        
        set engine
  Description:
        Container arm for set operation arms.  The contained arms inherit it's sample set, 'a'. 
        ---
        Activate jet.
        Creates a %gold trap with sample 'a', a set.
  +-  all
        logical AND
  Description:
        Accepts a gate which accepts any noun and produces a loobean.  Slams the gate with each member
        of set 'a', produce the logical AND of the transformed set.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts any gate which produces a loobean.
        Creates and kicks a dry %gold gate, casts the result to a loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, produce the logical AND of (b n.a) and the recursive calls of the trap with
        'a' replaced by 'l.a' and 'a' replaced by 'r.a'.
  Examples:
        ~dovryp-toblug/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~dovryp-toblug/try=> (~(all in b) |=(a=* ?@(-.a & |)))
        %.n
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (~(all in b) |=(a=@t (gte a 100)))
        %.y
  +-  any
        logical OR
  Description:
        Accepts a gate which accepts any noun and produces a loobean.  Slams the gate with each member
        of set 'a', produce the logical OR of the transformed set.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts any gate which produces a loobean.
        Creates and kicks a dry %gold gate, casts the result to a loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce false.
        Else, produce the logical OR of (b n.a) and the recursive calls of the trap with
        'a' replaced by 'l.a' and 'a' replaced by 'r.a'.
  Examples:
        ~dovryp-toblug/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~dovryp-toblug/try=> (~(any in b) |=(a=* ?@(+.a & |)))
        %.y
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (~(any in b) |=(a=@t (lte a 100)))
        %.n
  +-  del
        b without any a
  Description:
        Accepts any noun 'b' and removes it from the set 'a'.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold gate, casts the result to the type of 'a'.
        Builds an if-then-else statement on "a is null."
        If so, produce null.
        Else, builds an unless-then-else on (b=n.a)
        If so, build an if-then-else statement by testing the h-order of 'b' and 'n.a'.
        If so, produce a the cell [n.a $(a l.a) r.a], where $(a l.a) is the recursive call of 
        the trap with 'a' replaced by the left 
  Examples:
        ~dovryp-toblug/try=> =b (sa `(list ,@t)`[`a` `b` `c` ~])
        ~dovryp-toblug/try=> (~(del in b) `a`)
        {`c` `b`}
        ---
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (~(del in b) 'john')
        {'bonita' 'madeleine' 'daniel'}
        ---
        ~tadbyl-hilbel/try=> (~(del in b) 'susan')
        {'bonita' 'madeleine' 'daniel' 'john'}

  +-  dig
        axis of b in a
  Description:
        Produces 
        ---
        Creates a dry %gold gate which accepts a single noun.
        Let 'c' be 1.
        Creates and kicks a dry %gold trap.  Casts the result to an atomic unit.
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Else, build an if-then-else statement on (b=n.a).  If so, produce the unit [~ u=(peg c 2)].
        Else, build an if-then-else statement on the g-order of 'b' and 'n.a'
        If so, produce the recursive call of the trap with 'a' replaced by 'l.a' and 'c' replaced by (peg c 6).
        Else, produce the recursive call of the trap with 'a' replaced by 'r.a' and 'c' replaced by (peg c 7).
  Examples:
        ~talsur-todres/try=> =a (sa `(list ,@)`[1 2 3 4 5 6 7 ~])
        ~talsur-todres/try=> a
        {5 4 7 6 1 3 2}
        ~talsur-todres/try=> -.a
        n=6
        ~talsur-todres/try=> (~(dig in a) 7)
        [~ 12]
        ~talsur-todres/try=> (~(dig in a) 2)
        [~ 14]
        ~talsur-todres/try=> (~(dig in a) 6)
        [~ 2]
  +-  gas
        concatenate
  Description:
        Accepts a list 'b' with members of the same type as the set 'a' and produces 
        the union set of 'a' and 'b'.
        ---
        Activate jet.
        Creates a dry %gold gate which accepts a list of elements of the same type as 'a'.
        Creates and kicks a dry %gold trap whose result is cast to the type of 'a'.
        Builds an if-then-else statement on "b is an atom."
        If so, produce 'a'.
        Else, recursively call the trap with 'b' replaced by the tail of 'b' and the head of 'b'
        put into 'a'.
  Examples:
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'rudolf' 'john'}
        ~tadbyl-hilbel/try=> (~(gas in b) `(list ,@t)`['14' 'things' 'number' '1.337' ~])
        {'1.337' '14' 'number' 'things' 'bonita' 'madeleine' 'rudolf' 'john'}
        ---
        ~tadbyl-hilbel/try=> (~(gas in s) `(list ,@t)`['1' '2' '3' ~])
        {'1' '3' '2' 'e' 'd' 'a' 'c' 'b'}
  +-  has
        b exists in a check
  Description:
        Accepts any noun and produces the loobean indicating whether or not that value (n.a) exists in 'a'.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold trap.  Casts the result to a loobean.
        Builds an if-then-else statement on "The set (a) is an atom."  If so, produce false.
        Else, build an if-then-else statement on (b=n.a).
        If so, produce true.
        Else, build an if-then-else statement on the h-order of 'b' and 'n.a'
        If so, produce the recursive call to the trap with 'a' replaced by 'l.a'
        If so, produce the recursive call to the trap with 'a' replaced by 'r.a'
  Examples:
        ~dovryp-toblug/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~dovryp-toblug/try=> (~(has in a) `a`)
        %.y
        ~dovryp-toblug/try=> (~(has in a) 'z')
        %.n
  +-  put
        Accept any noun 'b' and produce the set 'a' with 'b' added to its sorted location.
        ---  
        Activate jet.
        Creates a wet %gold gate which accepts any atom.
        Creates and kicks a dry %gold gate.  Casts the result to the type of set 'a'.
        Builds an if-then-else statement on "a is an atom."
        If so, produce the null-terminated tuple [b ~ ~].
        Else, build an if-then-else statement on (b=n.a).
        If so, produce the set 'a'.
        Else, build an if-then-else statement on the h-order of 'b' and 'n.a'.
        If so, let 'c' be the recursive call of the trap with 'a' replaced by 'l.a'.
        Then, assert that 'c' is a cell.
        Build an if-then-else statement on the v-order of 'n.a' and 'n.c'.
        If so (their v-order is true), produce the tuple [n.a c r.a]
        Else, produce [n.c l.c [n.a r.c r.a]].
        Else (if 'b' and 'n.a' are not well h-ordered.), let 'c' be the recursive call of the trap with
        'c' replaced 'r.a'.
        Then, assert that 'c' is a cell.
        Builds an if-then-else statement on the v-order of 'n.a' and 'n.c'
        If so, produce [n.a l.a c]
        Else, produce [n.c [n.a l.a l.c] r.c].
  Examples:
        ~talsur-todres/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~talsur-todres/try=> =b (~(put in a) `d`)
        ~talsur-todres/try=> b
        {`d` `a` `c` `b`}
        ~talsur-todres/try=> -.l.+.b
        n=`d`
  +-  rep
        Accept a noun and a binary gate.  Produce the 'a' with each member 'n.a' replaced by (c n.a b).
        ---
        XXX
        Creates a wet %gold gate which accpets a noun and a tile, 'a' and 'b'.
        Creates and kicks a dry %gold gate.
        Builds an if-then-else statement on "a is null."  If so, produce 'b'.
        Else, recursively call the trap with 'a' replaced by 'r.a' and 
        'b' replaced by the recursive call of the trap with 'a' replaced by 'l.a' and 'b' replaced by
        (c n.a b).
        ---
        ~talsur-todres/try=> =a (~(gas in *(set ,@)) [1 2 3 ~])
        ~talsur-todres/try=> a
        {1 3 2}
        ~talsur-todres/try=> (~(rep in a) 0 |=([a=@ b=@] (add a b)))
        6
        ---
        XXX
        
  +-  tap
        Accept a list of elements of the set and produce a cell of the set with the list concatenated.
        ---  
        Activate jet.
        Build dry %gold gate with sample list of the same
        Cast the following to the type of `b`
        If: `a` is null,
            Then: Produce `b`,
        Else: Produce the toss of `a` for `r.a`, `b` for [n.a $(a l.a)]),
            where $(a l.a) is the toss of `a` for the left twig of `a`.
        ---
        ~tadbyl-hilbel/try=> =s (sa `(list ,@t)`['a' 'b' 'c' 'd' 'e' ~])
        ~tadbyl-hilbel/try=> s
        {'e' 'd' 'a' 'c' 'b'}
        ---
        ~tadbyl-hilbel/try=> (~(tap in s) `(list ,@t)`['1' '2' '3' ~])
        ~['b' 'c' 'a' 'd' 'e' '1' '2' '3']
        ---
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~tadbyl-hilbel/try=> (~(tap in b) `(list ,@t)`['david' 'people' ~])
        ~['john' 'daniel' 'madeleine' 'bonita' 'david' 'people']
  +-  wyt
        Produce the cardinality (number of elements) of the set.
        ---
        Increment the following.
        Kick dry %gold trap.  Yield atom.
        If:  `a` is null,
            Then: Produce 0.
        Else: Produce the increment of the sum of:
            The toss of `a` for `l.a`, the left twig of `a`.
            The toss of `a` for `r.a`, the right twig of `a`.
        ---
        ~talsur-todres/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~talsur-todres/try=> ~(wyt in a)
        4
        ---
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~tadbyl-hilbel/try=> ~(wyt in b)
        5

section 2dB, maps                     

++  ept       

Is the given tree of cell pairs a map?

####Summary 

        Build a dry %gold gate with sample tree of [p=* q=*] cells `a`
        If: `a` is null,
            Then:  Produce true,
        Else:  Produce the logical AND of:
            If: `l.a` is null,
                Then:  Produce true.
            Else: Produce the logical AND of:
                    The v-order of `p.n.a` and `p.n.l.a`,
                    The h-order of 
            If:  `r.a` is null
                Then:  Produce true,
            Else:  Produce the logical AND of:
                    The v-order of `p.n.a` and `r.p.n.a`,
                    The h-order of `r.p.n.a` and `p.n.a`.
        Terminate top AND statement.

####Examples

        ~tadbyl-hilbel/try=> m
        {[p='d' q=5] [p='a' q=1] [p='c' q=4] [p='b' q=[2 3]]}
        ~tadbyl-hilbel/try=> (ept m)
        %.y
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~tadbyl-hilbel/try=> (ept b)
        ! type-fail
        ! exit
++  ja

The jar engine: A container arm for jar operation arms.  Jars are maps of lists.
The contained arms inherit the sample jar. 'a'.

        Build a wet %gold tray with a sample jar `a`...

  +-  get

  Retrieve a list from the map by its key.

  ####Summary 

        Build wet %gold gate with sample noun `b`
        Push `d` is the slug of by to get with `a` slammed with `b`.
        If: `c` is null,
            Then: Produce null,
        Else: Produce `u.c`, the unit value of `c`

  ####Examples 
        
        ~zod/try=> =l (mo `(list ,[@t (list ,@)])`[['a' `(list ,@)`[1 2 3 ~]] ['b' `(list ,@)`[4 5 6 ~]] ~])
        ~zod/try=> l
        {[p='a' q=~[1 2 3]] [p='b' q=~[4 5 6]]}
        ~zod/try=> (~(get ja l) 'a')
        ~[1 2 3]
        ~zod/try=> (~(get ja l) 'b')
        ~[4 5 6]
        ~zod/try=> (~(get ja l) 'c')
        ~
 
  +-  add
  
  Add a key-list value to the jar.

  ####Summary

        Build wet %gold gate with sample noun `b`, noun `c`
        Push `d` is the call of get with the subject replaced by `a`, slammed with `b`
        Produce the slam of by to put with `a` slammed with `b` and [c d].

  ####Examples

        ~zod/try=> =l (mo `(list ,[@t (list ,@)])`[['a' `(list ,@)`[1 2 3 ~]] ['b' `(list ,@)`[4 5 6 ~]] ~])
        ~zod/try=> l
        {[p='a' q=~[1 2 3]] [p='b' q=~[4 5 6]]}
        ~zod/try=> (~(add ja l) 'b' 7)
        {[p='a' q=~[1 2 3]] [p='b' q=~[7 4 5 6]]}
        ~zod/try=> (~(add ja l) 'a' 100)
        {[p='a' q=~[100 1 2 3]] [p='b' q=~[4 5 6]]}
        ~zod/try=> (~(add ja l) 'c' 7)
        {[p='a' q=~[1 2 3]] [p='c' q=~[7]] [p='b' q=~[4 5 6]]}
        ~zod/try=> (~(add ja l) 'c' `(list ,@)`[7 8 9 ~])
        ! type-fail
        ! exit

        Terminate the core.
        
++  ju

The jug engine: container arm for jug operation arms.  Jugs are maps of sets.
The contained arms inherit it's sample jug, 'a'.

        Build a wet %gold tray with a sample jug `a`.

  +-  del
        
  Delete a value in a set and produce the resulting jug.

  ####Summary

        Build wet %gold gate with sample noun `b`, noun `c`
        Cast the following to the type of `a`
        Push `d` is the call of get with the subject replaced by `a`, slammed with `b`
        Push `e` is slug del to in by `d` slammed with `c`
        If: `e` is null,
            Then: Slug tray by to del with `a` slammed with `b`
        Else: Produce the slug tray by to put with `a` slammedw ith `b`, `e`.

  ####Examples

        ~zod/try=> s
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(del ju s) 'a' 1)
        {[p='a' q={3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(del ju s) 'c' 7)
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}        

  +-  get

  Retrieve a set from the map by its key.

  ####Summary

        Build wet %gold gate with sample noun `b`
        Push `c` is the slug of by to get with `a` slammed with `b`
        If: `c` is null,
            Then: Produce null,
        Else: Produce `u.c`, the unit value of `c`

  ####Examples

        ~zod/try=> s
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(get ju s) 'a')
        {1 3 2}
        ~zod/try=> (~(get ju s) 'b')
        {5 4 6}
        ~zod/try=> (~(get ju s) 'c')
        ~
        
  +-  has

  Is the element `c` in the set `b`?

  ####Summary

        Build wet %gold gate with sample noun `b`, noun `c`
        Yield bean.
        Produce the slug of in to has with the call of get with the subject replaced by `a` slammed with:
            `b`, a set, slammed with:
                `c`.  
        I.e.: Check if `c` is in the set which is the value of the map key `b`.

  ####Examples

        ~zod/try=> s
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(has ju s) 'a' 3)
        %.y
        ~zod/try=> (~(has ju s) 'b' 6)
        %.y
        ~zod/try=> (~(has ju s) 'a' 7)
        %.n
        ~zod/try=> (~(has jus s) 'c' 7)
        ! -find-limb.jus
        ! find-none
        ! exit
        ~zod/try=> (~(has ju s) 'c' 7)
        %.n

  +-  put

  Add a value to a specific set in the jug.

  ####Summary

        Build wet %gold gate with sample noun b. noun c.
        Cast the following to the type of `a`.
        Push `d` is the call of get with the subject replaced by `a`, slammed with `b`.
        Produce the slug of by to put with `a` slammed with:
            `b`,
            The slug of in to put by `d` slammed with `c`.

  ####Examples

        ~zod/try=> s
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(put ju s) 'a' 7)
        {[p='a' q={7 1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(put ju s) 'a' 1)
        {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
        ~zod/try=> (~(put ju s) 'c' 7)
        {[p='a' q={1 3 2}] [p='c' q={7}] [p='b' q={5 4 6}]}

++  by
        Container arm for map operation arms.  The contained arms inherit it's sample map, 'a'. 
        ---
        Activate jet.
        Builds a %gold tray with a sample which accepts a map.
  +-  all
        Accepts a gate which accepts any noun and produces a loobean.  Slams the gate with each member
        of map 'a', produce the logical AND of the transformed map.
        ---
        Activate jet.
        Builds a wet %gold gate which accepts the tile of a gate accepts any noun and produces a loobean.
        Creates and kicks a dry %gold gate.  Casts the result to loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, produce the logical AND of (b q.n.a), the recursive call of the trap with 'a' replaced by
        'l.a', and the recursive call of the trap with 'a' replaced by 'r.a'.
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~talsur-todres/try=> (~(all by b) |=(a=* ?@(a & |)))
        %.n
        ---
        ~tadbyl-hilbel/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
        ~tadbyl-hilbel/try=> (~(all by a) |=(a=@ (lte a 6)))
        %.y
        ~tadbyl-hilbel/try=> (~(all by a) |=(a=@ (lte a 4)))
        %.n
  +-  any
        Accepts a gate which accepts any noun and produces a loobean.  Slams the gate with each member
        of map 'a', produce the logical OR of the transformed map.
        ---
        Activate jet.
        Builds a wet %gold gate which accepts the tile of a gate accepts any noun and produces a loobean.
        Creates and kicks a dry %gold gate.  Casts the result to loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce false.
        Else, produce the logical OR of (b q.n.a), the recursive call of the trap with 'a' replaced by
        'l.a', and the recursive call of the trap with 'a' replaced by 'r.a'.
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~talsur-todres/try=> (~(all by b) |=(a=* ?@(a & |)))
        %.y
        ---
        ~tadbyl-hilbel/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
        ~tadbyl-hilbel/try=> (~(any by a) |=(a=@ (lte a 4)))
        %.y
  +-  del
        delete at key b
  Description:
        Accepts a noun 'b', producing the map with the key-value pair of key 'b' removed.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts a noun.
        Creates and kicks a dry %gold trap.  Casts the result to the type of map 'a'.
        Builds an if-then-else statement on "a is null."
        If so, produce null.
        Else, build an unless-then-else statement on (b=(p.n.a)).
        If so, build the if-then-else statement on the g-order of 'b' and 'p.n.a'.
        If so, produce the the tuple [n.a $(a l.a) r.a] where $(a l.a) is the recursive call of the
        trap with 'a' replaced by 'l.a'.
        Else (g-order of 'b' and 'p.n.a' is not true.), produce [n.a l.a $(a r.a)].
        Else (!(b=(p.n.a))), create and kick a dry %gold trap.
        Cast the result to a fork between null and the tile of the map 'a'.
        Builds an if-then-else statement on "l.a is null."  If so, produce 'r.a'.
        Else, build an if-then-else statement on "r.a is null."  If so, produce 'l.a'.
        Else, build an if-then-else statement on the v-order of 'p.n.l.a' and 'p.n.r.a'.
        If so, produce [n.l.a l.l.a $(l.a r.l.a)],where $(l.a r.l.a) is the recursive call of the 
        trap with 'l.a' replaced by 'r.l.a'.
        Else, produce [n.r.a $(r.a l.r.a) r.r.a], ,where $(r.a l.r.a) is the recursive call of the
        trap with 'r.a' replaced by 'l.r.a'.
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(del by b) `a`)
        {[p=`b` q=[2 3]]}
        
  +-  dig
        axis of 'b' key
  Description:
        Accepts any noun 'b' and produces the axis of 'b' in within the values of 'p.a' in map 'a'.
        ---
        Creates a wet %gold gate which accepts a noun.
        Let 'c' be 1.
        Creates and kicks a dry %gold gate.  Casts the result to an atomic unit. 
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Else, build an if-then-else statement on (b=(p.n.a.)).  If so, produce the unit [~ u=(peg c 2)].
        Else, build an if-then-else statement on the g-order of 'b' and 'p.n.a'.
        If so, recursively call the trap with 'a' replaced by 'l.a' and 'c' by (peg c 6).
        Else, recursively call the trap with 'a' replaced by 'r.a' and 'c' by (peg c 7).
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(dig by b) `b`)
        [~ 2]

  +-  gas
        concatenate
  Description:
        Accepts any list 'b' of key-value pair cells and produces the map 'a' with the members of 'b' added.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts a list of cells.
        Replaces 'b' in the subject with the cast of 'b' to a list whose members have the 
        same type as the members of 'a'.
        Creates and kicks a dry %gold trap.  Casts the result to the type of map 'a'.
        Builds an if-then-else statement on "b is an atom."
        If so, produce 'a'.
        Else, recursively call the trap iwth 'b' replaced by the tail of 'b' and 'a' replaced by 
        'a' with the key and value of the head of 'b' added to it.
  Examples:
        ~talsur-todres/try=> =a (mo `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
        ~talsur-todres/try=> =b `(list ,[@t *])`[[`c` 3] [`d` 4] ~]
        ~talsur-todres/try=> (~(gas by a) b)
        {[p=`d` q=4] [p=`a` q=1] [p=`c` q=3] [p=`b` q=2]}
  +-  get
        grab value by key
  Description:
        Produces the value in the map at key 'b'.
        ---
        Creates a wet %gold gate which accepts a noun.
        Creates and kicks a dry %gold trap.  Casts its result to the type of the map's values.
        Builds an if-then-else statement on "a is an atom."
        If so, produces null.
        Else, build an if-then-else statement on (b=(p.n.a)).
        If so, produce the unit [~ u=p.n.a].
        Else, build an if-then-else statement on the g-order of 'b' and 'p.n.a'.
        If so, produce the recursive call to the trap with 'a' replaced by 'l.a'.
        Else, produce the recursive call to the trap with 'a' replaced by 'r.a'.
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(get by b) `b`)
        [~ [2 3]]
  +-  has
        key existence check
  Description:
        Accepts any noun 'b' and produces the loobean indicating whether the noun exists in map 'a'.
        ---
        Activate jet.
        Creates a wet %gold gate which accepts a noun.
        Attempts to get 'b' from 'a', then produces the logical NOT of the loobean "get 'b' in 'a' is null"
  Examples:
        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(has by b) `b`)
        %.y
        ~talsur-todres/try=> (~(has by b) `c`)
        %.n
  +-  mar
        add with validation
  Description:
        Accepts two nouns of the types of the map's keys and values, respectively. 
        Validates that the value is not null and puts the pair in the map. If the value is null, 
        it deletes the key.
        ---
        Creates a wet %gold gate which accepts a cell of two nouns: one of the type of the map's keys
        and one a unit of the map's value type.
        If "c is null", produce the map with 'b' deleted.
        Else, produce the map with the 'b' and 'u.c' key-value pair added.
  Examples:
        
  +-  put
        
        ---
        Activate jet.
        Build a wet %gold gate with sample noun `b`, noun `c`
        
        Creates and kicks a dry %gold trap.  Casts the result to the type of the map 'a'.
        If "a is an atom", produce the cell [[b c] ~ ~].
        Else, build the if-then-else statement if
        ---
        
  +-  rep
        replace by product
  Description:
        Walks through the map, replacing 'b' with the product of (c n.a b).  Produces the resulting
        map.
        ---
        Creates a wet %gold gate which accepts a noun and a gate.
        Creates and kicks a dry %gold trap.
        If "a is null", produce 'b'.
        Else, produce the recursive call to the trap with 'a' replaced by 'r.a' and 'b' replaced by
        the recursive call to the trap with 'a' replaced by 'l.a' and b replaced by 
        the product (c n.a b).
  Examples:
        
  +-  rib
        transform + product
  Description:
        
        ---

  Examples:
        
  +-  run
        turns to tuples
  Description:
        ---
  Examples:
  +-  tap
        listify pairs
  Description:
        ---
  Examples:
  +-  uni
        union, merge
  Description:
        ---
  Examples:
  +-  wyt
        depth of map
  Description:
        ---
  Examples:

section 2dC, queues                   

++  to
        queue engine
  Description:
        Container arm for queue operation arms.  The contained arms inherit it's sample queue, 'a'. 
        ---
        Builds a wet %gold tray with sample 'a' of type 'qeu'.
  +-  bal
        v-order queue
  Description:
        Walks through the queue using vor (v-order check) on all eleements.
        ---
        Creates and kicks a dry %gold trap.  Casts the result to the type of the queue 'a'.
        If "a is null", produce null.
        Else, build an unless-then-else statement on the logical OR of:
        "null is in l.a"
        "n.a" precedes "n.l.a" in v-order.
        If so, produce the recursive call to the trap with 'a' replaced by [n.l.a l.l.a $(a [n.a r.l.a r.a])]
        Else, build an unless-then-else statement on the logical OR of:
        "null is in r.a"
        "n.a" precedes "n.r.a" in v-order.
         If so, produce the recursvie call to the trap with 'a' replaced by [n.r.a $(a [n.a l.a l.r.a]) r.r.a].
  Examples:      
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[6 1 3 6 1 3 4 6 ~])
        ~palryp-hocsyt/try=> a
        {6 4 3 1 6 3 1 6}
        ~palryp-hocsyt/try=> ~(bal to a)
        {6 4 3 1 6 3 1 6}
        ---
        
  +-  dep
        max depth of queue
  Description:
        Produces the maximum depth of leaves (r.a and l.a) in the queue 'a'.
        ---
        Creates and kicks a dry %gold trap.  Casts the result to an atom.
        If "a is null", produce 0.
        Else, increment the maximum of the recursive calls of the 'dep' to the left and right leaves of 'a',
        $(a l.a) and $(a r.a).
  Examples:
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 7 ~])
        ~palryp-hocsyt/try=> ~(dep to a)
        4
        ---
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 ~])
        ~palryp-hocsyt/try=> ~(dep to a)
        3
        ---
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 ~])
        ~palryp-hocsyt/try=> ~(dep to a)
        2
  +-  gas
        insert list to que
  Description:
        Accepts a 
        ---
        Creates a dry %gold gate which accepts a list of the elements of the queue.
        Creates and kicks a dry %gold gate.  Casts the result to the type of 'a', the queue.
        If "b is null", produce 'a'.
        Else, Produce the recursive call to the trap with 'b' replaced by the tail of 'b' and 'a' replaced by the 
        result of putting the head of 'b' into 'a'.
  Examples:
        ~palryp-hocsyt/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
        {3 2 1}
        ---
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
        ~palryp-hocsyt/try=> =b `(list ,@)`[4 5 6 ~]
        ~palryp-hocsyt/try=> (~(gas to a) b)
        {6 5 4 3 2 1}      
  +-  get
        head-tail pair
  Description:
        Produces the queue 'a' in the format [p=head q=tail].
        ---
        Creates and kicks a dry %gold trap.  Casts the head term of the resulting tuple to the type of the queue's elements
        and the tail type to that of the queue itself.
        If "a is null", crash the program.
        Else, if "r.a is null", produce [n.a l.a].
        Else, let 'b' be the recursive call of the trap with 'a' replaced by 'r.a'.
        Produce the the following as a [p q] cell:
        As the p term, 'p.b'
        As the q term, the result of the if-then-else statement:
        If "null is in q.b" OR 'n.a' precedes 'n.q.b' in the v-order,
        produce [n.a l.a q.b].
        Else, produce [n.q.b [n.a l.q.b] r.q.b].
  Examples:
        
  +-  nap
        removes head
  Description:
        Removes the head of a queue, producing the resulting queue.
        ---
        Assert that 'a' is a cell.
        Builds an if-then-else statement on "l.a is null".  If so, produce r.a.
        Else, let 'b' be the result of getting the [p=head q=tail] pair from 'l.a'.
        Produce the queue v-order of bal(+< ^+(a [p.b q.b r.a])).
  Examples:
        ~talsur-todres/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 ~])
        ~talsur-todres/try=> -.a
        n=6
        ~talsur-todres/try=> =b ~(nap to a)
        ~talsur-todres/try=> -.b
        n=2
        ~talsur-todres/try=> b
        {5 4 3 2 1}
        ~talsur-todres/try=> a
        {6 5 4 3 2 1}
  +-  put
        insert new head
  Description:
        Accepts any noun and adds to the queue as the head, producing the resutling queue.
        ---
        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold trap.  Casts the result to the type of the queue 'a'.
        If "a is null", produce [b ~ ~].
        Else, produce bal(+< a(l $(a l.a))).
        
  Examples:
        ~dovryp-toblug/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
        ~dovryp-toblug/try=> (~(put to a) 7)
        {7 6 5 4 2 1 3}
  +-  tap
        adds list to end
  Description:
        Concatenates two lists from the first
        ---
        Creates a dry %gold gate which accepts a list of elements of the type of the queue's elements.
        Casts the result to the type of 'b', the list.
        If "a is null", produce 'b'.
        Else, produce the recursive call to the gate with 'a' replaced by 'r.a' and 'b' replaced by [n.a $(a l.a)],
        where $(a l.a) is the recursive call to the trap with 'a' replaced by 'l.a'.
  Examples:
        ~dovryp-toblug/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
        ~dovryp-toblug/try=> (~(tap to a) `(list ,@)`[99 100 101 ~])
        ~[3 1 2 4 5 6 99 100 101]
  +-  top
        produces head
  Description:
        
        ---
        Creates and kicks a dry %gold trap.  Casts the result to a unit of the type of the queue's element.
        If "a is null", produce null.
        Else, if "the right leaf of 'a' is null", produce [~ n.a].
        Else, produce $(a r.a), the recursive call to the trap with 'a' replaced by 'r.a'.
  Examples:
        ~talsur-todres/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 ~])
        ~talsur-todres/try=> ~(top to a)
        [~ 1]

section 2dD, casual containers        

++  mo
        make a map
  Description:
        Mapifiy.  Accepts a list of cells and produces a map of key-value pairs from the left-right cell pairs of the list.
        ---
        Creates a wet %gold gate which accepts a list, 'a'.
        Pushes the homogenized list onto the context.
        Casts the list 'a' to a list of cells whose left-right types correspond to the key-value type pairs.
        Let 'b' be the bunt of the map with the properly typed keys and values from the cell at the head of our list.
        Concatenate the elements of 'a' into the empty map of bunt 'b', and produce the result.
  Examples:
        ~talsur-todres/try=> (mo `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
        {[p=`a` q=1] [p=`b` q=2]}
        
++  sa        
        make a set
  Description:
        Setify.  Accepts a list and produces a set of the list's elements.
        ---
        Creates a wet %gold gate which accepts a list, 'a'.
        Pushes the homogenized list onto the context.
        Let 'b' be the bunt of the set with elements of the same type of the elements of 'a'.
        Concatenate the elements of 'a' into the empty set of bunt 'b', and produce the result.
  Examples:
        ~talsur-todres/try=> (sa `(list ,@)`[1 2 3 4 5 ~])
        {5 4 1 3 2}
        ---
        ~talsur-todres/try=> (sa `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
        {[`a` 1] [`b` 2]}
++  qu
        make a set
  Description:
        XXX THIS APPEARS TO BE A COPY OF ++sa. QUEUIFY IS NOT IMPLEMENTED YET. XXX
        ---
  Examples:

chapter 2e, miscellaneous libs

section 2eA, packing          
        
++  cue
        Unpack an atom to a noun.  The inverse of jam.
        ---
        Activate jet.
        Build dry %gold gate with sample atom `a`.
        Yield noun.
        Push `b` is 0.
        Push `m` is empty map of type (map ,@ ,*).
        Seek subject for q.
        Kick dry %gold trap, yield tuple [p=@ q=* r=_m]
        If (0=(cut 0 [b 1] a)),
                Then, push `c` is (rub +(b) a).
                Produce
         
        ---
        ~midlys-rocpet/try=> (cue (jam 1))
        1
        ~midlys-rocpet/try=> (cue 4.657)
        [1 2]
        ~midlys-rocpet/try=> (cue (jam [1 1]))
        [1 1]
        ~tadbyl-hilbel/try=> (cue 39.689)
        [0 19]
++  jam       
        Compress a noun to an atom.  The inverse of cue.
        ---
        Activate jet.
        Build wet %gold gate with sample noun `a`.
        Yield atom.
        Push `b` is 0.
        Push `m` is empty may of type (map ,@ ,*).
                
        ---
        ~midlys-rocpet/try=> (jam 1)
        12
        ~midlys-rocpet/try=> (jam [1 1])
        817
        ~tadbyl-hilbel/try=> (jam [~ u=19])
        39.689
++  mat       
        Encodes length.  Only used internally as helper function to jam and cue.
        ---
        Activate jet.
        Build dry %gold gate with sample atom a.
        Yield atom a, atom b.
        If: a is 0.
                Then: Produce [1 1]
        Else, push `b` is (met 0 a), the number of bits in `a`.
        Push `c` is (met 0 b), the number of bits in `b`.
        Produce pair: 
                (add (add c c) b) and
                (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))
++  rub 
        Decodes length.  Only used internally as a helper function to jam and cue.
        ---
        Activate jet.
        Build wet %gold gold with sample atom a, atom b.
        Yield atom p, atom q.
        Push label `c` on:
                Push `c` is 0, m is (met 0 b), the number of bits in `b`.
                Kick dry %gold trap.  Deny that (gth c m), `c` is greater than `m`.
                Unless:  (cut 0 [(add a c) 1] b)) is 0,
                        Then:  `c`
                Else:  Slam trap with +(c)
        If: c is 0,
        Then: Produce [1 0].
        Else, push `d` is (add a +(c))
        Push `e` is (add (bex (dec c)) (cut 0 [d (dec c)] b)).
        Produce [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]

section 2eB, parsing (tracing)        

++  last
        Compare two [line column] pairs and produce the one which is farther along in text.
        ---
        Build dry %gold gate with sample hair `zyc`, hair `naz`
        Yield hair.
        If: p.zyc is p.naz,
                Then: If: q.zyc is greater than q.naz,
                        Then: Produce zyc,
                      Else:  Produce naz.
        Else: If: p.zyc is greater than p.naz,
                        Then: Produce zyc,
              Else: Produce naz.
        ---
        ~tadbyl-hilbel/try=> (last [1 1] [1 2])
        [p=1 q=2]
        ~tadbyl-hilbel/try=> (last [2 1] [1 2])
        [p=2 q=1]
        ~tadbyl-hilbel/try=> (last [0 0] [99 0])
        [p=99 q=0]
        ~tadbyl-hilbel/try=> (last [7 7] [7 7])
        [p=7 q=7]
++  lust
        Produce the beginning of the next line after a newline character or increment the column number - The index of the next character to be parsed.
        ---
        Build dry %gold gate with sample char `weq`, hair `naz`
        Yield hair.
        If: `weq` is 10,
                Then: Produce [+(p.naz) 1].
        Else: Produce [p.naz +(q.naz)].
        ---
        ~tadbyl-hilbel/try=> (lust `a` [1 1])
        [p=1 q=2]
        ~tadbyl-hilbel/try=> (lust `@t`10 [1 1])
        [p=2 q=1]
        ~tadbyl-hilbel/try=> (lust '9' [10 10])
        [p=10 q=11]
        ~tadbyl-hilbel/try=> (lust `@t`10 [0 0])
        [p=1 q=1]

section 2eC, parsing (custom rules)   

++  cold  
        Build gate to parse a nail with a rule, then replaced the parsed texted with a constant.
        ---
        Activate jet.
        Build wet %gold gate with sample noun `cus`, bunt of a rule `sef`.
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`.
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: q.vex is an atom,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]]]
        ---
        ~midlys-rocpet/try=> ((cold %foo (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%foo q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cold %foo (just `a`)) [[1 1] "bc"])
        [p=[p=1 q=1] q=~]
++  cook
        Build gate to parse a nail with a rule, then slam a gate with the parsed text.
        ---
        Activate jet.
        Build wet %gold gate with sample clam gate `poq`, bunt of a rule `sef`.
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`.
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=[~ u=[p=(poq p.u.q.vex) q=q.u.q.vex]]],
                where (poq p.u.q.vex) is gate `poq` slammed with the parsed text.
        ---
        ~midlys-rocpet/try=> ((cook ,@ud (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook ,@tas (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=%a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook |=(a=@ +(a)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=98 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((cook |=(a=@ `@t`+(a)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=`b` q=[p=[p=1 q=2] q="bc"]]]]
++  easy
        Succeed but consume no characters - Produce an edge at the same text position with the text to parse unchanged, but with a
        ---
        Activate jet.
        Build wet %gold gate with sample noun, `huf`, a noun to produce as the parsed value.
        Activate extra parsing jet.
        Build dry %gold date with sample nail, `tub`
        Yield edge of type `huf`
        Produce [p=p.tub q=[~ u=[p=huf q=tub]]], the edge with the noun `huf` as it's parsed value and `tub` as unparsed.
        ---
        ~tadbyl-hilbel/try=> ((easy %foo) [[1 1] "abc"])
        [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="abc"]]]]
        ~tadbyl-hilbel/try=> ((easy %foo) [[1 1] "bc"])
        [p=[p=1 q=1] q=[~ [p=%foo q=[p=[p=1 q=1] q="bc"]]]]
        ~tadbyl-hilbel/try=> ((easy 'a') [[1 1] "bc"])
        [p=[p=1 q=1] q=[~ [p='a' q=[p=[p=1 q=1] q="bc"]]]]
++  fail  
        Fail to parse - Produce a nail at the same text position but with null text.
        ---
        Build wet %gold gate with sample nail, `tub`.
        Produce nail [p=p.tub q=~].
        ---
        ~tadbyl-hilbel/try=> (fail [[1 1] "abc"])
        [p=[p=1 q=1] q=~]
        ~tadbyl-hilbel/try=> (fail [[p=1.337 q=70] "Parse me, please?"])
        [p=[p=1.337 q=70] q=~]
++  full  
        Demand politely that the parsing rule parse the entire sample nail, produce a null edge otherwise.
        ---
        Build wet %gold gate with sample rule, `sab`
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule slammed with the text to parse.
        If: Parse of `vex` is null,
                Then: Produce `vex`
        Else: If: The unparsed text in the produced edge is nulll,
                Then: Produce `vex`
        Else: Produce [p=p.vex q=~], the edge with a null unit nail.
        ---
        ~tadbyl-hilbel/try=> ((full (just 'a')) [[1 1] "ab"])
        [p=[p=1 q=2] q=~]
        ~tadbyl-hilbel/try=> ((full (jest 'ab')) [[1 1] "ab"])
        [p=[p=1 q=3] q=[~ u=[p='ab' q=[p=[p=1 q=3] q=""]]]]
        ~tadbyl-hilbel/try=> ((full ;~(plug (just 'a') (just 'b'))) [[1 1] "ab"])
        [p=[p=1 q=3] q=[~ u=[p=[~~a ~~b] q=[p=[p=1 q=3] q=""]]]]
++  funk
        Prepend a tape to the text to be parsed, then parse the new tape.
        ---
        Build wet %gold gate with sample tape `pre`, rule `sef`
        Build dry %gold gate with sample nail, `tub`
        Produce the rule slammed with the hair index of `tub` and the concatenation of 
        the prefix tape and the `tub` tape.
        ---
        ~tadbyl-hilbel/try=> ((funk "abc prefix-" (jest 'abc')) [[1 1] "to be parsed"])
        [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=" prefix-to be parsed"]]]]
        ~tadbyl-hilbel/try=> ((funk "parse" (just 'a')) [[1 4] " me"])
        [p=[p=1 q=4] q=~]
++  here  
        Apply rule if parsing within a specific line and column range.
        ---
        Activate jet.
        Build wet %gold gate with sample bunted gate accepting pint `a`, noun `b` and producing cell [a b], and bunt of rule `sef`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`,
        Else: Produce the hair,
                p is `q.tub`
                q is the unit:
                        Null
                        u is: 
                                p is `hez` slammed with the pint cell of tub's position and vex's position.
                                q is `q.u.q.vex`, the text not parsed.
        ---
        
++  inde
        Apply rule to indented block starting at current column number,
        omitting the leading whitespace.
        ---
        Build wet %gold gate with sample rule, 'sef'
        Build dry %gold gate with sample nail and the same product type as sef.
        Let 'har' and 'tap' be p and q within that nail
        Let 'lev' be ' ' repeated once less than the column number in har.
        Let roq be the result of parsing the nail as any number of either
          Printable characters, or
          Newlines followed by ignored lev.
        If roq is a failure produce it.
        Let 'vex' be the result of parsing with sef, with column restarted to
        1, the result in roq which must be a tape.
        If vex is a failure produce it with its p set to that of roq.
        Produce an edge with parse reach from roq, succesful result from vex,
        hair from vex with column number advanced by q.har - 1 (which was 
        subtracted prior passing it to sef), and a continuation tape of:
          Let res be the continuation in vex(section of block unconsumed).
          Build an kick a trap (Do):
            If res is empty produce the continuation in roq.
            Unless res starts with newline produce the head of res followed
            by the result of tossing res for its tail.
            Welp together a newline, lev, and the result of tossing res for
            its tail.
        ---

++  jest  
        Match and consume a cord.
        ---
        Build dry %gold gate with sample cord `daf`
        Build dry %gold gate with sample nail `tub`
        Push `fad` is `daf`
        Kick dry %gold trap.  Yield edge of cord text.
        If: `daf` is 0,
                Then: Produce the edge with:
                        p is `p.tub`
                        q is the unit:
                                Null
                                u is the cell [p=fad q=tub]
        Else: If: fish for null in q.tub OR compile to Nock the last byte in `daf` and the 
                Then: Produce the failed parse of `tub`,
        Else: Toss `p.tub` for the index of the next character to be parsed, `q.tub` for the tail of `q.tub`, `daf` for the single byte right-shift of `daf`
        ---
        ~tadbyl-hilbel/try=> ((jest 'abc') [[1 1] "abc"])
        [p=[p=1 q=4] q=[~ [p='abc' q=[p=[p=1 q=4] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "abc" (jest 'abc'))
        'abc'
        ~tadbyl-hilbel/try=> (scan "abc" (jest 'acb'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> ((jest 'john doe') [[1 1] "john smith"])
        [p=[p=1 q=6] q=~]
        ~tadbyl-hilbel/try=> ((jest 'john doe') [[1 1] "john doe"])
        [p=[p=1 q=9] q=[~ [p='john doe' q=[p=[p=1 q=9] q=""]]]]
++  just
        Match and consume a single character.
        ---
        Activate jet.
        Build dry %gold gate with sample char `daf`
        Activate extra parsing jet.
        Build dry %gold gate wtih sample nail `tub`
        Yield char edge.
        If: `q.tub` is null,
                Then: Produce the failed parse of `tub`,
        Else:  Unless: `daf` is `i.q.tub`,
                Then: Produce the failed parse of `tub`,
        Else: Produce the parse of the next character of `tub`
        ---
        ~tadbyl-hilbel/try=> ((just 'a') [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~tadbyl-hilbel/try=> (scan "abc" (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "a" (just 'a'))
        ~~a
        ~tadbyl-hilbel/try=> (scan "%" (just '%'))
        ~~~25.
++  knee
        Callback 
        ---
        Build wet %gold gate with sample noun `gar`, rule trap `sef`
        Build dry %gold gate with sample nail `tub`
        Yield char edge.
        Produce `tub` slammed to `sef`
        ---
        
        
++  mask  
        Match the next char to a list of chars, a tape.
        ---
        Activate jet.
        Build wet %gold gate with sample (list char) `bud`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Yield char edge.        
        If: `q.tub` is an atom,
                Then: Produce the failed parse of `tub`
        Else: Unless: 
                
        ---
        ~tadbyl-hilbel/try=> (scan "a" (mask "cba"))
        ~~a
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "bbc"])
        [p=[p=1 q=2] q=[~ [p=~~b q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((mask "abc") [[1 1] "dbc"])
        [p=[p=1 q=1] q=~]
++  next  
        Always succeeds and consumes a character.
        ---
        Build dry %gold gate with sample nail `tub`
        Yield char edge.
        If: The text to parse `q.tub` is an atom,
                Then: Produce the failed parse of `tub`
        Else: Push `zac` is lust slammed with:
                The first chaarcter to parse (The head of `q.tub`) and its location in the text.
        Produce the edge with the hair `zac` and unit nail with:
                The character successfully consumed, the head of the text to parse.
                A nail of hair index `zac`, text to be parsed `t.q.tub` (The tail of the text to parse.)
        ---
        ~tadbyl-hilbel/try=> (next [[1 1] "ebc"])
        [p=[p=1 q=2] q=[~ [p=~~e q=[p=[p=1 q=2] q="bc"]]]] 
        ~tadbyl-hilbel/try=> (next [[1 1] "john jumps jones"])
        [p=[p=1 q=2] q=[~ [p=~~j q=[p=[p=1 q=2] q="ohn jumps jones"]]]]
++  sear  
        Conditional cook - Produce the slam of the parsed texted to `b` only if the result is not null.
        Else, produce null.
        ---
        Activate jet.
        Build wet %gold gate with sample tile of gate accepting a noun and producing a unit `pyq`, rule `sef`
        Activate extra parsing jet.
        Build dry %gold with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: The text to be parsed is an atom,
                Then: Produce `vex`,
        Else: Push `gey` is `pyq` slammed with the 
        If: `gey` is an atom,
                Then: Produce the cell with:
                        p is the hair index of the parse failure.
        Else: Produce the cell with:
                p is the hair index of the parse.
                q is the unit with value u is:
                        p is the value of the parsed text slammed to `pyq`
                        q is the value of the unparsed text.
        ---
        ~midlys-rocpet/try=> ((sear |=(a=* ?@(a (some a) ~)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((sear |=(a=* ?@(a [~ u=a] ~)) (just `a`)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=97 q=[p=[p=1 q=2] q="bc"]]]]
++  shim  
        Match characters within a range.
        ---
        Activate jet.
        Build wet %gold gate with sample atom `les`, atom `mos`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail, `tub`
        Yield char edge.
        If: `q.tub` is an atom,
                Then: Produce the failed parse of `tub`,
        Else: Unless: `i.q.tub` is greater than or equal to `les` AND `i.q.tub` is less than or equal to `mos`,
                Then: Produce the failed parse of `tub`,
        Else: Produce the single character parse of `tub`
        ---
        ~midlys-rocpet/try=> ((shim `a` 'z') [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ [p=~~a q=[p=[p=1 q=2] q="bc"]]]]
        ~midlys-rocpet/try=> ((shim `a` 'Z') [[1 1] "abc"])
        [p=[p=1 q=1] q=~]
        ~midlys-rocpet/try=> ((shim `a` 'Z') [[1 1] "Abc"])
        [p=[p=1 q=2] q=[~ [p=~~~41. q=[p=[p=1 q=2] q="bc"]]]]
++  stag  
        Add a label to an edge parsed by a rule.
        ---
        Activate jet.
        Build wet %gold gate with sample noun `gob`, bunt of a rule `sef`
        Activate extra parsing jet.
        Build dry %gold gate with sample nail `tub`
        Push `vex` is the rule `sef` slammed by the nail `tub`, an edge.
        If: `q.vex` is an atom,
                Then: Produce `vex`
        Else: Produce the edge with hair `p.vex` and unit with value hair u=[p=[gob p.u.q.vex] q=q.u.q.vex]
        ---
        ~tadbyl-hilbel/try=> ((stag %foo (just 'a')) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=[%foo ~~a] q=[p=[p=1 q=2] q="bc"]]]]
        ~tadbyl-hilbel/try=> ((stag "xyz" (jest 'abc')) [[1 1] "abc"])
        [p=[p=1 q=4] q=[~ u=[p=["xyz" 'abc'] q=[p=[p=1 q=4] q=""]]]]
        ~tadbyl-hilbel/try=> ((stag 10.000 (shim 0 100)) [[1 1] "abc"])
        [p=[p=1 q=2] q=[~ u=[p=[10.000 ~~a] q=[p=[p=1 q=2] q="bc"]]]]
++  stet
        Listify a list of text position and bunt of rule pairs.
        ---
        Build wet %gold gate with sample list of position and bunt of rule pairs `leh`
        Kick dry %gold trap.
        If: `leh` is null,
                Then: Produce null.
        Else: Produce the cell,
                with head: The cell of the head of the head of `leh`, p=-.i.leh, the tail of the head of `leh, q=+.i.leh.
                with tail: Toss `leh` for `t.leh`
        ---
        ~tadbyl-hilbel/try=> (stet (limo [[5 (just 'a')] [1 (jest 'abc')] [[1 1] (shim 0 200)] [[1 10] (cold %foo (just 'a'))]~]))
        ~[
          [p=5 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=1 q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
          [p=[1 10] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
        ]
        ~tadbyl-hilbel/try=> (stet (limo [[[1 1] (just 'a')] [[2 1] (shim 0 200)] ~]))
        ~[
          [p=[1 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>] 
          [p=[2 1] q=<1.lrk [tub=[p=[p=@ud q=@ud] q=""] <1.nqy [daf=@tD <394.imz 97.kdz 1.xlc %164>]>]>]
        ]
++  stew
        
        ---
        Activate jet.
        Build wet %gold gate with sample list of position and bunt of rule pairs `leh`
        Push label `wor` on:
                Build dry %gold gate with sample fork between `ort` , fork `wan`
        ---
++  stir
        
        ---
        Activate jet.
        Build wet %gold gate with sample noun `rud`, gate accepting two nouns and producing , rule `fel`
        Activate extra parsing jet.
        Build dry %gold with sample nail `tub`
        Yield edge of type of `rud`
        Push `vex` is the rule `fel` slammed with the nail `tub`A
        If: The parsed text is null,
                Then: Produce the edge of unit nail `rud` and `tub` at the hair index of `vex`
        Else: Push `wag` is the toss of `tub` for the unparsed text in the unit nail of `vex`
        Assert that the value 

        Produce the edge with:
                The farthest along hair index of `vex` and `wag`
                The unit nail of 

        ---
        
++  stun  
        Parse several times
        ---
        Activate jet.
        Build wet %gold gate with sample atom `les`, atom `mos`, rule `fel`
        Activate extra parsing jet.
        Build wet %gold gate with sample nail `tub`
        Yield edge of 
        
        If: `mos` is 0,
                Then: Produce the edge with 
        ---

section 2eD, parsing (combinators)    

++  bend  
        
        ---
        Activate jet.
        Build wet %gold gate with sample gate accepting a noun `a`, noun `b` and producing the unit of [a b]
        Activate extra parsing jet.
        Build wet %gold gate with sample edge `vex`, rule `sab`
        If: `q.vex` is an atom,
                Then: Produce `vex`,
        Else: Push `yit` is sab slammed with
        ---

++  comp
        Arbitrary compose
        ---
        Activate jet.
        Build wet %gold gate with sample gate accepting noun a, noun b and producing [a b], `raq`
        Activate extra parsing jet.
        Build wet %gold gate with sample edge `vex`, rule `sab`
        If: The parsing output in `vex` is an atom,
                Then: Produce the edge `vex`
        Else: Push `yit` is the rule slammed with the text to parse in `vex`
        Push `yur` is the hair of the edge that is farther along of `vex` and `yit`
        If: The unit of parsed text in `yit` is null,
                Then: Produce the edge with hair `yur` and unit nail from `yit`, which is null.
        Else: Produce the edge with hair `yur`, unit [p=* q=nail] cell where:
                p is `raq` slammed with `p.u.q.vex` and `p.u.q.yit`, the parsed results `yit` and `vex`
                q is `q.u.q.yit`, the unparsed text of `yit`
        ---
        

++  glue  
        Add rule.
        ---
        Activate jet.
        Build wet %gold gate with sample rule `bus`
        Activate extra parsing jet.
        Build wet %gold gate with sample edge `vex`, rule `sab`
        Slam plug with:
                The edge `vex`
                The tuple of slamming bus and sab with pfix
        ---
++  less        
        No first and second.
        ---
        Build wet %gold gate with sample edge `vex`, rule `sab`
        If: `q.vex` is null,
            Then: Push `roq` is `sab`.  Produce [p=(last p.vex p.roq) q=q.roq]
        Else: Produce vex with q tossed for null.
        ---

++  pfix
        Discard the first rule of a two rule cell.
        ---
        Activate jet.
        Produce comp slammed with:
                Build wet %gold gate slammed with sample noun `a`, noun `b`.
                Produce `b`
        ---
        
        
++  plug  
        Apply parsing rules in order to an edge.
        ---
        Activate jet.
        Build wet %gold gate with sample edge `vex`, rule `sab`
        If: The unit of text to parse is null,
                Then: Produce null.
        Else: Push `yit` is the rule `sab` slammed with the text to be parsed.
        Push `yur` is the hair of the edge that is farther along of `vex` and `yit`
        If: The unit of text parsed is null,
                Then: Produce the edge [p=yur q=null].
        Else: Produce the edge [p=yur q=[~ u=[p=[p.u.q.vex p.u.q.yit] q=q.u.q.yit]]],
        the edge of the text parsed with the rule.
        ---
        ~tadbyl-hilbel/try=> (;~(plug lus lus) [[1 1] "++"])
        [p=[p=1 q=3] q=[~ u=[p=[~~~2b. ~~~2b.] q=[p=[p=1 q=3] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "++" ;~(plug lus lus))
        [~~~2b. ~~~2b.]
        ~tadbyl-hilbel/try=> (scan "++" (cold "slus" ;~(plug lus lus)))
        "slus"
        ~tadbyl-hilbel/try=> (scan "john doe" ;~(plug (jest 'john') ace (jest 'doe')))
        ['john' ~~. 'doe']
        ~tadbyl-hilbel/try=> (scan "john doe" ;~(plug (jest 'doe') ace (jest 'john')))
        ! {1 1}
        ! 'syntax-error'
        ! exit 
        ~tadbyl-hilbel/try=> (;~(plug bar hep) [[1 1] "|-"])
        [p=[p=1 q=3] q=[~ u=[p=[~~~7c. ~~-] q=[p=[p=1 q=3] q=""]]]]
        ~tadbyl-hilbel/try=> (;~(plug bar hep lus) [[1 1] "|-"])
        [p=[p=1 q=3] q=~]
        ~tadbyl-hilbel/try=> (scan "|-" ;~(plug bar hep lus))
        ! {1 3}
        ! 'syntax-error'
        ! exit
++  pose  
        Build list of parsing rules and try to use any of them in order. 
        `pose` has the same usage as `plug`, but does not fail if the rules are not 
        successful in a certain order.
        ---
        Activate jet.
        Build wet %gold gate with sample edge `vex`, rule `sab`
        If: The unit of text to parse is null,
                Then: Push `roq` is the edge result of the rule `sab` applied to the 
                Produce the edge with the hair that is farther along of `vex` and `yit` and the parse
                results of the rule.
        Else: Produce the initial edge, `vex` 
        ---
        ~tadbyl-hilbel/try=> (;~(pose (just 'a') (just 'b') (just 'c')) [[1 1] "c"])
        [p=[p=1 q=2] q=[~ [p=~~c q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "c" ;~(pose (just 'a') (just 'b') (just 'c')))
        ~~c
        ~tadbyl-hilbel/try=> (;~(pose bar hep) [[1 1] "|-"])
        [p=[p=1 q=2] q=[~ [p=~~~7c. q=[p=[p=1 q=2] q="-"]]]]
        ~tadbyl-hilbel/try=> (scan "|-" (star ;~(pose bar hep)))
        "|-"
        ~tadbyl-hilbel/try=> (scan "|-" (star ;~(pose bar hep lus)))
        "|-"
        ~tadbyl-hilbel/try=> (scan "john doe" (star ;~(pose (jest 'doe') ace (jest 'john'))))
        ~['john' ' ' 'doe']
        
++  sfix  
        Discard second rule.
        ---
        Activate jet.
        Slam comp with a wet %gold gate accepting  noun `a`, noun `b` and producing noun `a`
        ---

section 2eE, parsing (composers)

++  bass
        ---
        Build wet %gold gate with sample atom `wuc`, rule `tyd`
        Slam cook with:
                Build dry %gold gate with sample list of atoms, `waq`
                Slam roll with:
        ---
++  boss
        ---
        Build wet %gold gate with sample atom `wuc`, rule `tyd`
        ---
++  ifix
        
        ---
        Build wet %gold gate with sample cell of rules `fel`, rule `hof`
        Produce pfix gonadified with:
            `p.fel`, the first rule in `fel`
            Gonadify sfix with `hof` and `q.fel`, the second rule in `fel`
        ---
        
++  more
        ---
        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce the gonadified:
        ---
++  most
        Parse to a list elements of the second rule seperated by the second.

        ---
        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce gonadified:
                Plug slammed with `fel`,
                        star slammed with gonadified:
                                pfix slammed with `bus` and `fel`, `bus` added as the prefix of `fel`
        ---
        
++  plus  
        Like 'star', but "one or more" instead of "0 or more"

        ---
        Build wet %gold gate with sample rule `fel`
        Produce gonadified:
                plug slammed with `fel` and star slammed with `fel`, the repeated application of `fel`.
        ---
        
        
++  slug

        ---
        Build wet %gold gate with sample noun `rud`, gate accepting  cell of two nouns and producing [a b] `raq`
        Build wet %gold gate with sample rule `bus`, rule `fel`
        Produce the gonadified:
                comp slammed with `raq`, 
                        slammed with `fel`, 
                                slammed with,
                                        stir slammed with `rud`, `raq`, and `fel` prefixed with `bus`
        ---
        
++  star
        Apply the parsing rule repeatedly until it fails.
        ---
        Build wet %gold gate with sample rule `fel,
        Produce stir slammed with:
                The list of elements of type of the icon of `fel` slammed to `wonk`
        
        ---
        ~tadbyl-hilbel/try=> (scan "aaaaa" (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "aaaaa" (star (just 'a')))
        "aaaaa"
        ~tadbyl-hilbel/try=> (scan "abcdef" (star (just 'a')))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "abcabc" (star (jest 'abc')))
        <|abc abc|>
        ~tadbyl-hilbel/try=> (scan "john smith" (star (shim 0 200)))
        "john smith"

section 2eF, parsing (ascii)          

++  ace
        Parse ASCII character 32, ace.
        ---
        Produce the rule just slammed with ' '
        ---
        ~tadbyl-hilbel/try=> (scan " " ace)
        ~~. 
        ~tadbyl-hilbel/try=> `cord`(scan " " ace)
        ' '
        ~tadbyl-hilbel/try=> (ace [[1 1] " "])
        [p=[p=1 q=2] q=[~ [p=~~. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ace [[1 1] " abc "])
        [p=[p=1 q=2] q=[~ [p=~~. q=[p=[p=1 q=2] q="abc "]]]]
++  bar 
        Parse ASCII character 124, bar.
        ---
        Produce the rule just slammed with '|'
        ---
       ~tadbyl-hilbel/try=> (scan "|" bar)
        ~~~7c. 
        ~tadbyl-hilbel/try=> `cord`(scan "|" bar)
        '|'
        ~tadbyl-hilbel/try=> (bar [[1 1] "|"])
        [p=[p=1 q=2] q=[~ [p=~~~7c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (bar [[1 1] "|="])
        [p=[p=1 q=2] q=[~ [p=~~~7c. q=[p=[p=1 q=2] q="="]]]]
++  bas 
        Parse ASCII character 92, bas.
        Note the extra '\' in the slam of bas with just is to escape the escape character, bas.
        ---
        Produce the rule just slammed with '\\'
        ---
        ~tadbyl-hilbel/try=> (scan "\\" bas)
        ~~~5c.
        ~tadbyl-hilbel/try=> `cord`(scan "\\" bas)
        '\'
        ~tadbyl-hilbel/try=> (bas [[1 1] "\"])
        ~ <syntax error at [1 18]>
        ~tadbyl-hilbel/try=> (bas [[1 1] "\\"])
        [p=[p=1 q=2] q=[~ [p=~~~5c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (bas [[1 1] "\""])
        [p=[p=1 q=1] q=~]
++  buc 
        Parse ASCII character 36, buc.
        ---
        Produce the rule just slammed with '$'
        ---
        ~tadbyl-hilbel/try=> (scan "$" buc)
        ~~~24.
        ~tadbyl-hilbel/try=> `cord`(scan "$" buc)
        '$'
        ~tadbyl-hilbel/try=> (buc [[1 1] "$"])
        [p=[p=1 q=2] q=[~ [p=~~~24. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (buc [[1 1] "$%"])
        [p=[p=1 q=2] q=[~ [p=~~~24. q=[p=[p=1 q=2] q="%"]]]]
++  cab 
        Parse ASCII character 95, cab.
        ---
        Produce the rule just slammed with '_'
        ---
        ~tadbyl-hilbel/try=> (scan "_" cab)
        ~~~5f.
        ~tadbyl-hilbel/try=> `cord`(scan "_" cab)
        '_'
        ~tadbyl-hilbel/try=> (cab [[1 1] "_"])
        [p=[p=1 q=2] q=[~ [p=~~~5f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (cab [[1 1] "|_"])
        [p=[p=1 q=1] q=~]
++  cen 
        Parse ASCII character 37, cen.
        ---
        Produce the rule just slammed with '%'
        ---
        ~tadbyl-hilbel/try=> (scan "%" cen)
        ~~~25.
        ~tadbyl-hilbel/try=> `cord`(scan "%" cen)
        '%'
        ~tadbyl-hilbel/try=> (cen [[1 1] "%"])
        [p=[p=1 q=2] q=[~ [p=~~~25. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (cen [[1 1] "%^"])
        [p=[p=1 q=2] q=[~ [p=~~~25. q=[p=[p=1 q=2] q="^"]]]] 
++  col 
        Parse ASCII character 58, col.
        ---
        Produce the rule just slammed with ':'
        ---
        ~tadbyl-hilbel/try=> (scan ":" col)
        ~~~3a.
        ~tadbyl-hilbel/try=> `cord`(scan ":" col)
        ':'
        ~tadbyl-hilbel/try=> (col [[1 1] ":"])
        [p=[p=1 q=2] q=[~ [p=~~~3a. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (col [[1 1] ":-"])
        [p=[p=1 q=2] q=[~ [p=~~~3a. q=[p=[p=1 q=2] q="-"]]]]
++  com 
        Parse ASCII character 44, com.
        ---
        Produce the rule just slammed with ','
        ---
        ~tadbyl-hilbel/try=> (scan "," com)
        ~~~2c.
        ~tadbyl-hilbel/try=> `cord`(scan "," com)
        ','
        ~tadbyl-hilbel/try=> (com [[1 1] ","])
        [p=[p=1 q=2] q=[~ [p=~~~2c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (com [[1 1] "not com"])
        [p=[p=1 q=1] q=~]
++  doq 
        Parse ASCII character 34, doq.
        ---
        Produce the rule just slammed with '"'
        ---
       ~tadbyl-hilbel/try=> (scan "\"" doq)
        ~~~22.
        ~tadbyl-hilbel/try=> `cord`(scan "\"" doq)
        '"'
        ~tadbyl-hilbel/try=> (doq [[1 1] "\""])
        [p=[p=1 q=2] q=[~ [p=~~~22. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (doq [[1 1] "not successfully parsed"])
        [p=[p=1 q=1] q=~]
        ~tadbyl-hilbel/try=> (scan "see?" doq)
        ! {1 1}
        ! 'syntax-error'
        ! exit 
++  dot 
        Parse ASCII character 46, dot.
        ---
        Produce the rule just slammed with '.'
        ---
        ~tadbyl-hilbel/try=> (scan "." dot)
        ~~~.
        ~tadbyl-hilbel/try=> `cord`(scan "." dot)
        '.'
        ~tadbyl-hilbel/try=> (dot [[1 1] "."])
        [p=[p=1 q=2] q=[~ [p=~~~. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (dot [[1 1] ".^"])
        [p=[p=1 q=2] q=[~ [p=~~~. q=[p=[p=1 q=2] q="^"]]]]
++  fas 
        Parse ASCII character 47, fas.
        ---
        Produce the rule just slammed with '/'
        ---
        ~tadbyl-hilbel/try=> (scan "/" fas)
        ~~~2f.
        ~tadbyl-hilbel/try=> `cord`(scan "/" fas)
        '/'
        ~tadbyl-hilbel/try=> (fas [[1 1] "/"])
        [p=[p=1 q=2] q=[~ [p=~~~2f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (fas [[1 1] "|/"])
        [p=[p=1 q=1] q=~]
++  gal 
        Parse ASCII character 60, gal.
        ---
        Produce the rule just slammed with '<'
        ---
        ~tadbyl-hilbel/try=> (scan "<" gal)
        ~~~3c.
        ~tadbyl-hilbel/try=> `cord`(scan "<" gal)
        '<'
        ~tadbyl-hilbel/try=> (gal [[1 1] "<"])
        [p=[p=1 q=2] q=[~ [p=~~~3c. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (gal [[1 1] "<+"])
        [p=[p=1 q=2] q=[~ [p=~~~3c. q=[p=[p=1 q=2] q="+"]]]]
        ~tadbyl-hilbel/try=> (gal [[1 1] "+<"])
        [p=[p=1 q=1] q=~]
++  gar 
        Parse ASCII character 62, gar.
        ---
        Produce the rule just slammed with '>'
        ---
        ~tadbyl-hilbel/try=> (scan ">" gar)
        ~~~3e.
        ~tadbyl-hilbel/try=> `cord`(scan ">" gar)
        '>'
        ~tadbyl-hilbel/try=> (gar [[1 1] ">"])
        [p=[p=1 q=2] q=[~ [p=~~~3e. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (gar [[1 1] "=>"])
        [p=[p=1 q=1] q=~]
++  hax 
        Parse ASCII character 35, hax.
        ---
        Produce the rule just slammed with '#'
        ---
        ~tadbyl-hilbel/try=> (scan "#" hax)
        ~~~23.
        ~tadbyl-hilbel/try=> `cord`(scan "#" hax)
        '#'
        ~tadbyl-hilbel/try=> (hax [[1 1] "#"])
        [p=[p=1 q=2] q=[~ [p=~~~23. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (hax [[1 1] "#!"])
        [p=[p=1 q=2] q=[~ [p=~~~23. q=[p=[p=1 q=2] q="!"]]]]
++  kel 
        Parse ASCII character 123, kel.
        Note that this, with ker, opens and closes a Hoon expression for Hoon string interpolation.  Escape kel to parse it.
        ---
        Produce the rule just slammed with '{'
        ---
        ~tadbyl-hilbel/try=> (scan "\{" kel)
        ~~~7b.
        ~tadbyl-hilbel/try=> `cord`(scan "\{" kel)
        '{'
        ~tadbyl-hilbel/try=> (kel [[1 1] "\{"])
        [p=[p=1 q=2] q=[~ [p=~~~7b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (kel [[1 1] " \{"])
        [p=[p=1 q=1] q=~]
++  ker 
        Parse ASCII character 125, ker.
        ---
        Produce the rule just slammed with '}'
        ---
        ~tadbyl-hilbel/try=> (scan "}" ker)
        ~~~7d.
        ~tadbyl-hilbel/try=> `cord`(scan "}" ker)
        '}'
        ~tadbyl-hilbel/try=> (ker [[1 1] "}"])
        [p=[p=1 q=2] q=[~ [p=~~~7d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ker [[1 1] "\{}"])
        [p=[p=1 q=1] q=~]
++  ket 
        Parse ASCII character 94, ket.
        ---
        Produce the rule just slammed with '^'
        ---
        ~tadbyl-hilbel/try=> (scan "^" ket)
        ~~~5e.
        ~tadbyl-hilbel/try=> `cord`(scan "^" ket)
        '^'
        ~tadbyl-hilbel/try=> (ket [[1 1] "^"])
        [p=[p=1 q=2] q=[~ [p=~~~5e. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ket [[1 1] ".^"])
        [p=[p=1 q=1] q=~]
++  lus 
        Parse ASCII character 43, lus.
        ---
        Produce the rule just slammed with '+'
        ---
        ~tadbyl-hilbel/try=> (scan "+" lus)
        ~~~2b.
        ~tadbyl-hilbel/try=> `cord`(scan "+" lus)
        '+'
        ~tadbyl-hilbel/try=> (lus [[1 1] "+"])
        [p=[p=1 q=2] q=[~ [p=~~~2b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (lus [[1 1] ".+"])
        [p=[p=1 q=1] q=~]
++  hep 
        Parse ASCII character 45, hep.
        ---
        Produce the rule just slammed with '-'
        ---
        ~tadbyl-hilbel/try=> (scan "-" hep)
        ~~-
        ~tadbyl-hilbel/try=> `cord`(scan "-" hep)
        '-'
        ~tadbyl-hilbel/try=> (hep [[1 1] "-"])
        [p=[p=1 q=2] q=[~ [p=~~- q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (hep [[1 1] ":-"])
        [p=[p=1 q=1] q=~]
++  pel 
        Parse ASCII character 40, pel.
        ---
        Produce the rule just slammed with '('
        ---
        ~tadbyl-hilbel/try=> (scan "(" pel)
        ~~~28.
        ~tadbyl-hilbel/try=> `cord`(scan "(" pel)
        '('
        ~tadbyl-hilbel/try=> (pel [[1 1] "("])
        [p=[p=1 q=2] q=[~ [p=~~~28. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pel [[1 1] ";("])
        [p=[p=1 q=1] q=~]
++  pam 
        Parse ASCII character 38, pam.
        ---
        Produce the rule just slammed with '&'
        ---
        ~tadbyl-hilbel/try=> (scan "&" pam)
        ~~~26.
        ~tadbyl-hilbel/try=> `cord`(scan "&" pam)
        '&'
        ~tadbyl-hilbel/try=> (pam [[1 1] "&"])
        [p=[p=1 q=2] q=[~ [p=~~~26. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pam [[1 1] "?&"])
        [p=[p=1 q=1] q=~]
++  per 
        Parse ASCII character 41, per.
        ---
        Produce the rule just slammed with ')'
        ---
        ~tadbyl-hilbel/try=> (scan ")" per)
        ~~~29.
        ~tadbyl-hilbel/try=> `cord`(scan ")" per)
        ')'
        ~tadbyl-hilbel/try=> (per [[1 1] ")"])
        [p=[p=1 q=2] q=[~ [p=~~~29. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (per [[1 1] " )"])
        [p=[p=1 q=1] q=~]
++  pat 
        Parse ASCII character 64, pat.
        ---
        Produce the rule just slammed with '@'
        ---
        ~tadbyl-hilbel/try=> (scan "@" pat)
        ~~~4.
        ~tadbyl-hilbel/try=> `cord`(scan "@" pat)
        '@'
        ~tadbyl-hilbel/try=> (pat [[1 1] "@"])
        [p=[p=1 q=2] q=[~ [p=~~~4. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (pat [[1 1] "?@"])
        [p=[p=1 q=1] q=~]
++  sel
        Parse ASCII character 91, sel.
        ---
        Produce the rule just slammed with '['
        ---
        ~tadbyl-hilbel/try=> (scan "[" sel)
        ~~~5b.
        ~tadbyl-hilbel/try=> `cord`(scan "[" sel)
        '['
        ~tadbyl-hilbel/try=> (sel [[1 1] "["])
        [p=[p=1 q=2] q=[~ [p=~~~5b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sel [[1 1] "-["])
        [p=[p=1 q=1] q=~]
++  sem 
        Parse ASCII character 59, sem.
        ---
        Produce the rule just slammed with ';'
        ---
        ~tadbyl-hilbel/try=> (scan ";" sem)
        ~~~3b.
        ~tadbyl-hilbel/try=> `cord`(scan ";" sem)
        ';'
        ~tadbyl-hilbel/try=> (sem [[1 1] ";"])
        [p=[p=1 q=2] q=[~ [p=~~~3b. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sem [[1 1] " ;"])
        [p=[p=1 q=1] q=~]
++  ser 
        Parse ASCII character 93, ser.
        ---
        Produce the rule just slammed with ']'
        ---
        ~tadbyl-hilbel/try=> (scan "]" ser)
        ~~~5d.
        ~tadbyl-hilbel/try=> `cord`(scan "]" ser)
        ']'
        ~tadbyl-hilbel/try=> (ser [[1 1] "]"])
        [p=[p=1 q=2] q=[~ [p=~~~5d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (ser [[1 1] "[ ]"])
        [p=[p=1 q=1] q=~]
++  sig 
        Parse ASCII character 126, sig.
        ---
        Produce the rule just slammed with '~'
        ---
        ~tadbyl-hilbel/try=> (scan "~" sig)
        ~~~~
        ~tadbyl-hilbel/try=> `cord`(scan "~" sig)
        '~'
        ~tadbyl-hilbel/try=> (sig [[1 1] "~"])
        [p=[p=1 q=2] q=[~ [p=~~~~ q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (sig [[1 1] "?~"])
        [p=[p=1 q=1] q=~]
++  soq 
        Parse ASCII character 39, soq.
        Note the extra '\' in the slam of soq with just is to escape the first soq because soq denotes a crip.
        ---
        Produce the rule just slammed with '\''
        ---
        ~tadbyl-hilbel/try=> (scan "'" soq)
        ~~~27.
        ~tadbyl-hilbel/try=> `cord`(scan "'" soq)
        '''
        ~tadbyl-hilbel/try=> (soq [[1 1] "'"])
        [p=[p=1 q=2] q=[~ [p=~~~27. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (soq [[1 1] ">'"])
        [p=[p=1 q=1] q=~]
++  tar 
        Parse ASCII character 42, tar.
        ---
        Produce the rule just slammed with '*'
        ---
        ~tadbyl-hilbel/try=> (scan "*" tar)
        ~~~2a.
        ~tadbyl-hilbel/try=> `cord`(scan "*" tar)
        '*'
        ~tadbyl-hilbel/try=> (tar [[1 1] "*"])
        [p=[p=1 q=2] q=[~ [p=~~~2a. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tar [[1 1] ".*"])
        [p=[p=1 q=1] q=~]
++  tec 
        Parse ASCII character 96, tec.
        ---
        Produce the rule just slammed with '`'
        ---
        ~tadbyl-hilbel/try=> (scan "`" tec)
        ~~~6.
        ~tadbyl-hilbel/try=> `cord`(scan "`" tec)
        '`'
        ~tadbyl-hilbel/try=> (tec [[1 1] "`"])
        [p=[p=1 q=2] q=[~ [p=~~~6. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tec [[1 1] " `"])
        [p=[p=1 q=1] q=~]
++  tis 
        Parse ASCII character 61, tis.
        ---
        Produce the rule just slammed with '='
        ---
        ~tadbyl-hilbel/try=> (scan "=" tis)
        ~~~3d.
        ~tadbyl-hilbel/try=> `cord`(scan "=" tis)
        '='
        ~tadbyl-hilbel/try=> (tis [[1 1] "="])
        [p=[p=1 q=2] q=[~ [p=~~~3d. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (tis [[1 1] "|="])
        [p=[p=1 q=1] q=~]
++  wut 
        Parse ASCII character 63, wut.
        ---
        Produce the rule just slammed with '?'
        ---
        ~tadbyl-hilbel/try=> (scan "?" wut)
        ~~~3f.
        ~tadbyl-hilbel/try=> `cord`(scan "?" wut)
        '?'
        ~tadbyl-hilbel/try=> (wut [[1 1] "?"])
        [p=[p=1 q=2] q=[~ [p=~~~3f. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (wut [[1 1] ".?"])
        [p=[p=1 q=1] q=~]
++  zap 
        Parse ASCII character 33, zap.
        ---
        Produce the rule just slammed with '!'
        ---
        ~tadbyl-hilbel/try=> (scan "!" zap)
        ~~~21.
        ~tadbyl-hilbel/try=> `cord`(scan "!" zap)
        '!'
        ~tadbyl-hilbel/try=> (zap [[1 1] "!"])
        [p=[p=1 q=2] q=[~ [p=~~~21. q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (zap [[1 1] "?!"])
        [p=[p=1 q=1] q=~]

section 2eG, parsing (whitespace)     

++  dog 
++  doh 
        Parse 
        ---
        Produce plug gonadified with dot and gay.
        ---

++  dun 
        Parse phep (--) to null (~).
        ---
        Produce cold slammed with:
                null
                plug gonadified with hep and hep, to parse phep.
        ---
        ~tadbyl-hilbel/try=> (scan "--" dun)
        ~
        ~tadbyl-hilbel/try=> (dun [[1 1] "--"])
        [p=[p=1 q=3] q=[~ u=[p=~ q=[p=[p=1 q=3] q=""]]]]
++  duz 
        Parse stet (==) to null (~).
        ---
        Produce cold slammed with:
                null
                plug gonadified with tis and tis, to parse stet
        ---
        ~tadbyl-hilbel/try=> (scan "==" duz)
        ~
        ~tadbyl-hilbel/try=> (duz [[1 1] "== |=..."])
        [p=[p=1 q=3] q=[~ u=[p=~ q=[p=[p=1 q=3] q=" |=..."]]]]
++  gah 

        ---
        Produce mask slammed with the tuple:
                `@`10, the newline character
                ' ', the ace character
                null
        ---

++  gap 
        
        ---
        Produce cold slammed with:
                null
                Plug gonadified with:
                        gaq
                        star slammed with pose gonadified with vul and gah
        ---
++  gaq

        ---
        Produce pose gonadifed with:
                just slammed with the newline character.
                Plug gonadified with gah and pose gonadified with gah and vul.
                vul
        ---
        
++  gay 
        
        ---
        Produce pose gonadified with:
                gap, which
                Slam of easy with null
        ---
        
++  vul 
        Parse comments and replace them with null.
        Note that a comment must be ended with a newline character.
        ---
        Produce cold slammed with: Pair null and,
                plug gonadified with col, col, and,
                        pose gonadified with:
                                shim slammed with 32 and 126
                                shim slammed with 128 and 255
                                just slammed with the newline operator.
        (==) Terminates the pair.
        ---

        
section 2eH, parsing (idioms)         

++  alf 
        Parse alphabetic characters, both upper and lowercase.
        ---
        Produce the rule of pose gonadified with low and hig.
        ---
        ~tadbyl-hilbel/try=> (scan "a" alf)
        ~~a
        ~tadbyl-hilbel/try=> (scan "A" alf)
        ~~~41.
        ~tadbyl-hilbel/try=> (scan "AaBbCc" (star alf))
        "AaBbCc"
++  aln 
        Parse alphanumeric characters - both alphabetic characters and numbers.
        ---
        Produce the rule of pose gonadified with low,hig, and nud.
        ---
        ~tadbyl-hilbel/try=> (scan "0" aln)
        ~~0
        ~tadbyl-hilbel/try=> (scan "alf42" (star aln))
        "alf42"
        ~tadbyl-hilbel/try=> (scan "0123456789abcdef" (star aln))
        "0123456789abcdef"
++  alp 
        Parse alphanumeric strings and hep, "-".
        ---
        Produce the rule pose gonadified with low, hig, nud, hep.
        ---     
        ~tadbyl-hilbel/try=> (scan "7" alp)
        ~~7
        ~tadbyl-hilbel/try=> (scan "s" alp)
        ~~s
        ~tadbyl-hilbel/try=> (scan "123abc-" (star alp))
        "123abc-"
++  bet 
        Parse the hep and lus axis syntax.
        ---
        Produce the rule pose gonadified with:
                (cold 2 hep), which replaces parsed heps with 2s.
                (cold 3 lus), which replaced parsed luses with 3s.
        ---
        ~tadbyl-hilbel/try=> (scan "-" bet)
        2
        ~tadbyl-hilbel/try=> (scan "+" bet)
        3
++  bin 
        Parse a tape of binary (0s and 1s) and produce its atomic representation.
        ---
        Produce the slam of bass with 2 and the (most gon but), which produces 
        
        ---
        ~tadbyl-hilbel/try=> (scan "0000" bin)
        0
        ~tadbyl-hilbel/try=> (scan "0001" bin)
        1
        ~tadbyl-hilbel/try=> (scan "0010" bin)
        2
        ~tadbyl-hilbel/try=> (scan "100000001111" bin)
        2.063
++  but 
        Parse a single binary digit.
        ---
        Produce rule cook slammed with a gate:
                With sample atom `a` that results in the difference between `a` and '0' (48).
                        All slammed with the rule shim slammed with '0' and '1', to parse either of those characters.
        ---     
        ~tadbyl-hilbel/try=> (scan "0" but)
        0
        ~tadbyl-hilbel/try=> (scan "1" but)
        1
        ~tadbyl-hilbel/try=> (scan "01" but)
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "01" (star but))
        ~[0 1]
++  cit 
        Parse a single octal digit.
        ---
        Produce rule cook slammed with a gate:
                With sample atom `a` that results in the difference between `a` and '0' (48).
                        All slammed with the rule shim slammed with '0' and '7', to parse any number between 0 and 7.
        ---
        ~tadbyl-hilbel/try=> (scan "1" cit)
        1
        ~tadbyl-hilbel/try=> (scan "7" cit)
        7
        ~tadbyl-hilbel/try=> (scan "8" cit)
        ! {1 1}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (scan "60" (star cit))
        ~[6 0]
++  dem 
        Parse a decimal number to an atom.
        ---
        Produce the slam of bass with 10 (The base number system) and (most gon dit), which produces 
        
        ---
        ~tadbyl-hilbel/try=> (scan "7" dem)
        7
        ~tadbyl-hilbel/try=> (scan "42" dem)
        42
        ~tadbyl-hilbel/try=> (scan "150000000" dem)
        150.000.000
        ~tadbyl-hilbel/try=> (scan "12456" dem)
        12.456
++  dit 
        Parse a single decimal digit.
        ---
        Produce the rule cook slammed with a gate:
                With sample atom `a` that results in the difference between `a` and '0' (48).
                        All slammed with the rule shim slammed with '0' and '9', to parse any number.
        ---
        ~tadbyl-hilbel/try=> (scan "7" dit)
        7
        ~tadbyl-hilbel/try=> (scan "42" (star dit))
        ~[4 2]
        ~tadbyl-hilbel/try=> (scan "26000" (star dit))
        ~[2 6 0 0 0]
++  gul 
        Parse the axis gal and gar axis syntax.
        ---
        Produce the rule pose gonadified with:
                (cold 2 gal), which replaces parsed gals with 2s.
                (cold 3 gar), which replaced parsed gars with 3s.
        ---
        ~tadbyl-hilbel/try=> (scan "<" gul)
        2
        ~tadbyl-hilbel/try=> (scan ">" gul)
        3
++  gon 
        Parse long numbers - Numbers which wrap around the shell with the line break characters bas and fas.
        ---
        Produce the rule pose gonadified with:
                The rule plug gonadified with:
                        bas, gay, and fas, to succeed to parse a bas, fas, or a gap in text.
                The rule (easy ~), to succeed to parse but produces null as the parsed text.
        ---
        ~tadbyl-hilbel/try=> (scan "\\/" gon)
        [~~~5c. ~ ~~~2f.]
        ~tadbyl-hilbel/try=> (gon [[1 1] "\\/"])
        [p=[p=1 q=3] q=[~ u=[p=[~~~5c. ~ ~~~2f.] q=[p=[p=1 q=3] q=""]]]]
++  hex 
        Parse any hexadecimal number to an atom.
        ---
        Produce bass slammed with 16 (The base number system) and (most gon hit), which produces the atom 

        ---
        ~tadbyl-hilbel/try=> (scan "a" hex)
        10
        ~tadbyl-hilbel/try=> (scan "A" hex)
        10
        ~tadbyl-hilbel/try=> (scan "2A" hex)
        42
        ~tadbyl-hilbel/try=> (scan "1ee7" hex)
        7.911
        ~tadbyl-hilbel/try=> (scan "1EE7" hex)
        7.911
++  hig 
        Parse a single uppercase letter.
        ---
        Produce the slam of shim with the characters 'A' (65) and 'Z' (90), to parse any character between them, inclusive.
        ---
        ~tadbyl-hilbel/try=> (scan "G" hig)
        ~~~47.
        ~tadbyl-hilbel/try=> `cord`(scan "G" hig)
        'G'
        ~tadbyl-hilbel/try=> (scan "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (star hig))
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        ~tadbyl-hilbel/try=> (hig [[1 1] "G"])
        [p=[p=1 q=2] q=[~ [p=~~~47. q=[p=[p=1 q=2] q=""]]]]
++  hit 
        Parse a hexadecimal digit. 
        ---
        Pose gonadified with:
                dit, parse a single decimnal digit.
                Slam cook with:
                        Build dry %gold gate with sample char `a`.  Produce the difference between `a` and 87.
                        The slam of shim with the characters 'a' (97) and 'z' (122), to parse any character between them, inclusive.
                        The slam of shim with the characters 'A' () and 'Z' (), to parse any character between them, inclusive.
        Terminate the gonadification.
        ---
        ~tadbyl-hilbel/try=> (scan "a" hit)
        10
        ~tadbyl-hilbel/try=> (scan "A" hit)
        10
        ~tadbyl-hilbel/try=> (hit [[1 1] "a"])
        [p=[p=1 q=2] q=[~ [p=10 q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "2A" (star hit))
        ~[2 10]
++  low 
        Parse a single lowercase letter.
        ---
        Produce the slam of shim with the characters 'a' (97) and 'z' (122), to parse any character between them, inclusive.
        ---
        ~tadbyl-hilbel/try=> (scan "g" low)
        ~~g
        ~tadbyl-hilbel/try=> `cord`(scan "g" low)
        'g'
        ~tadbyl-hilbel/try=> (scan "abcdefghijklmnopqrstuvwxyz" (star low))
        "abcdefghijklmnopqrstuvwxyz"
        ~tadbyl-hilbel/try=> (low [[1 1] "g"])
        [p=[p=1 q=2] q=[~ [p=~~g q=[p=[p=1 q=2] q=""]]]]
++  mes 
        Parse a hexbyte.
        ---
        Slam cook with:
                Build dry %gold gate with sample atom `a`, atom `b`.  Produce the sum of `a` multiplied by 16 and `b`
                Plug gonadified with hit and hit, parse two consecutive hex digits.
        ---
        ~tadbyl-hilbel/try=> (scan "2A" mes)
        42
        ~tadbyl-hilbel/try=> (mes [[1 1] "2A"])
        [p=[p=1 q=3] q=[~ u=[p=42 q=[p=[p=1 q=3] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "42" mes)
        66
++  nix 
        
        ---
        Slam boss with 256
        ---
        
++  nud 
        Parse a numeric character - A number.
        ---
        Produce the slam of shim with the characters '0' (48) and '9' (57), to parse  any character between them, inclusive.
        ---
        ~tadbyl-hilbel/try=> (scan "0" nud)
        ~~0
        ~tadbyl-hilbel/try=> (scan "7" nud)
        ~~7
        ~tadbyl-hilbel/try=> (nud [[1 1] "1"])
        [p=[p=1 q=2] q=[~ [p=~~1 q=[p=[p=1 q=2] q=""]]]]
        ~tadbyl-hilbel/try=> (scan "0123456789" (star nud))
        "0123456789"
++  poy 
        Parse an escape character.
        ---
        Produce pfix gonadified with:
                bas
                pose gonadifided with:
                         bas
                         soq
                         mes, to parse a hexbyte.
        ---
        
++  qit 
        Parse an individual character to its cord atom representation.
        ---
        Produce pose gonadified with:
                The slam of shim with 32 and 38, to parse any characters between them, inclusive.
                The slam of shim with 40 and 91, to parse any characters between them, inclusive.
                The slam of shim with 93 and 126, to parse any characters between them, inclusive.
                The slam of shim with 128 and 255, to parse any characters between them, inclusive.
        ---
        ~tadbyl-hilbel/try=> (scan "%" qit)
        37
        ~tadbyl-hilbel/try=> (scan "0" qit)
        48
        ~tadbyl-hilbel/try=> (scan "E" qit)
        69
        ~tadbyl-hilbel/try=> (scan "a" qit)
        97
        ~tadbyl-hilbel/try=> (scan "cord" (star qit))
        ~[99 111 114 100]
++  qut 
        Parse 
        ---
        Slam ifix with:
                [soq soq]
                boss slammed with 256 and (most gon qit)
        ---
        
        
++  sym
        
        ---

        ---

++  ven 

        ---

        ---

++  vit 
        Parse a text and produce its base 64 encoding
        ---
        Build list of falling rules to match on with ';~' and pose.
        Encodes capital letters by 

        ---

section 2eI, parsing (external)       

++  rash
        Parse a cord with a given rule and crash if the cord isn't entirely parsed.
        ---
        Build wet %gold gate with sample atom `naf`, rule `sab`
        Produce the slam of scan with:
                Trip slammed with `naf`, to turn `naf` into a tape.
                The rule `sab`
        ---
        ~tadbyl-hilbel/try=> (rash 'I was the world in which I walked, and what I saw' (star (shim 0 200)))
        "I was the world in which I walked, and what I saw"
        ~tadbyl-hilbel/try=> (rash 'abc' (just 'a'))
        ! {1 2}
        ! 'syntax-error'
        ! exit
        ~tadbyl-hilbel/try=> (rash 'abc' (jest 'abc'))
        'abc'
        `~tadbyl-hilbel/try=> (rash 'abc' (jest 'ab'))
        ! {1 3}
        ! 'syntax-error'
        ! exit
++  rush
        Parse a given with a given rule and produce null if the cord isn't entirely parsed.
        ---
        Build wet %gold gate with sample atom `naf`, rule `sab`
        Produce the slam of scan with:
                Trip slammed with `naf`, to turn `naf` into a tape.
                The rule `sab`
        ---
        ~tadbyl-hilbel/try=> (rush 'I was the world in which I walked, and what I saw' (star (shim 0 200)))
        [~ "I was the world in which I walked, and what I saw"]
        ~tadbyl-hilbel/try=> (rush 'abc' (just 'a'))
        ~
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'abc'))
        [~ 'abc']
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'ac'))
        ~
        ~tadbyl-hilbel/try=> (rush 'abc' (jest 'ab'))
        ~
++  rust
        Parse a tape with a given rule and produce null if the tape isn't entirely parsed.
        ---
        Build wet %gold gate with sample tape `los`, rule `sab`
        Push `vex` is the rule (full sab) slammed with the beginning of the `los` tape.
        If: `q.vex`, the parsed result, is null,
                Then: Produce null.
        Else: Produce the unit with value 'p.u.q.vex', the parsed text.
        ---
        ~tadbyl-hilbel/try=> (rust "I was the world in which I walked, and what I saw" (star (shim 0 200)))
        [~ "I was the world in which I walked, and what I saw"]
        ~tadbyl-hilbel/try=> (rust "Or heard or felt came not but from myself;" (star (shim 0 200)))
        [~ "Or heard or felt came not but from myself;"]
        ~tadbyl-hilbel/try=> (rust "And there I found myself more truly and more strange." (jest 'And there I'))
        ~
++  scan
        Parse a tape with a given rule and crash if the tape isn't entirely parsed.
        ---
        Build wet %gold gate with sample tape `los`, rule `sab`
        Push `vex` is the rule (full sab) slammed with the beginning of the `los` tape.
        If: `q.vex` is null,
                Then: Add to the crash with message 'syntax-error''s trace:
                        show slammed with [%m '{%d %d}'], `p.p.vex`, `q.p.vex`, and null
        Else: Produce the parsing output of `vex`
        ---
        ~tadbyl-hilbel/try=> (scan "I was the world in which I walked, and what I saw" (star (shim 0 200)))
        "I was the world in which I walked, and what I saw"
        ~tadbyl-hilbel/try=> (scan "Or heard or felt came not but from myself;" (star (shim 0 200)))
        "Or heard or felt came not but from myself;"
        ~tadbyl-hilbel/try=> (scan "And there I found myself more truly and more strange." (jest 'And there I'))
        ! {1 12}
        ! 'syntax-error'
        ! exit

section 2eJ, formatting (basic text)  

++  cass
        Produce the case insensitive (all lowercase) cord of a tape.
        ---
        Build wet %gold gate with sample tape `vib`
        Slam rap with:
                3, to rap by bytes
                Slam turn with:
                        `vib`
                        Build dry %gold gate with sample atom `a`,
                                Unless: `a` is greater than or equal to 'A' or less than or equal to 'Z',
                                        Then: Produce `a`,
                                Else: Produce the difference between `a` and 32.
        ---
       ~tadbyl-hilbel/try=> (cass "john doe")
        7.309.170.810.699.673.450
        ~tadbyl-hilbel/try=> `cord`(cass "john doe")
        'john doe'
        ~tadbyl-hilbel/try=> (cass "abc, 123, !@#")
        2.792.832.775.110.938.439.066.079.945.313
        ~tadbyl-hilbel/try=> `cord`(cass "abc, 123, !@#")
        'abc, 123, !@#' 
++  cuss
        Turn all occurances of lowercase letters in any tape into uppercase letters, as a cord.
        ---
        Build dry %gold gate with sample tape `vib`
        Yield cord
        Slam rap with:
                3, to rap by bytes
                Slam turn with:
                        `vib`
                        Build dry %gold gate with sample atom `a`,
                                Unless: `a` is greater than or equal to 'A' or less than or equal to 'Z',
                                        Then: Produce `a`,
                                Else: Produce the difference between `a` and 32.
        ---
        ~tadbyl-hilbel/try=> (cuss "john doe")
        'JOHN DOE'
        ~tadbyl-hilbel/try=> (cuss "abc ABC 123 !@#")
        'ABC ABC 123 !@#'
        ~tadbyl-hilbel/try=> `@ud`(cuss "abc")
        4.407.873
        ~tadbyl-hilbel/try=> (cuss "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsQqRrVvWwXxYyZz")
        'AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSQQRRVVWWXXYYZZ'
++  crip
        Produce the cord of a tape.
        ---
        Build dry %gold with sample tape `a`
        Produce the rap of `a` by bytes, cast to a cord.
        ---
        ~tadbyl-hilbel/try=> (crip "john doe")
        'john doe'
        ~tadbyl-hilbel/try=> (crip "abc 123 !@#")
        'abc 123 !@#'
        ~tadbyl-hilbel/try=> `@ud`(crip "abc")
        6.513.249
++  mesc
++  runt
++  sand
++  sane
++  trim
++  trip
++  teff
++  turf
++  tuba
++  tufa
++  tuft
++  wack
++  wick
++  woad
++  wood

section 2eK, formatting (layout)      

++  re
  ++  ram
  ++  win
    ++  din 
    ++  fit 
    ++  rig
    ++  wig

section 2eL, formatting (path)        

++  ab
  ++  bix  
  ++  hif  
  ++  huf  
  ++  hyf  
  ++  pev  
  ++  pew  
  ++  piv  
  ++  piw  
  ++  qeb  
  ++  qex  
  ++  qib  
  ++  qix  
  ++  seb  
  ++  sed  
  ++  sev  
  ++  sew  
  ++  sex  
  ++  sib  
  ++  siq  
  ++  sid  
  ++  siv  
  ++  siw  
  ++  six  
  ++  sov  
  ++  sow  
  ++  sox  
  ++  ted  
  ++  tip  
  ++  tiq  
  ++  tid  
  ++  til  
  ++  urs  
  ++  urt  
  ++  voy  
  ++  vym  
  ++  vyn  
++  ag
  ++  ape  
  ++  bay  
  ++  bip  
  ++  dem  
  ++  dim  
  ++  dum  
  ++  fed  
  ++  hex  
  ++  lip  
  ++  qut  
  ++  sym  
  ++  tyq  
  ++  viz  
  ++  vum  
  ++  wiz  
++  co
      ++  rear
      ++  rent
      ++  rend
      ++  a-co
      ++  d-co
      ++  r-co
      ++  s-co
      ++  v-co
      ++  w-co
      ++  x-co
      ++  y-co
      ++  z-co
  ++  em-co
  ++  ox-co
  ++  ro-co
++  ne
  ++  d  
  ++  x  
  ++  v  
  ++  w  
++  mu
  ++  zag
  ++  zig
  ++  zug
++  so
  ++  bisk
  ++  crub
  ++  nuck
  ++  nusk
  ++  perd
  ++  royl
  ++  tash
  ++  twid
  ++  zust
++  scot 
++  scow 
++  slav  
++  slaw
++  slay
++  smyt

section 2eM, regular-expressions      

++  pars
++  nor
++  les  
++  lep  
++  alm  
++  alb  
++  mis  
++  anns 
++  mall
++  bets
++  ranc
++  flap 
++  rang
++  chun
++  seac
++  sead
++  sade
++  seap
++  cape
++  lower
++  upper
++  digit
++  print
++  graph
++  blank
++  space
++  cntrl
++  alpha
++  alnum
++  punct
++  wordc
++  white
++  xdigi
++  chad
++  escd
++  escp
++  unid
++  proc 
++  cont
++  abor
++  matc
++  chet
++  blak 
++  deep
++  rexp 
++  repg 

section 2eN, pseudo-cryptography      

++  un   
  ++  wre
  ++  wre
  ++  xaf
  ++  xar
  ++  zaf
  ++  zar
  ++  zyf
  ++  zyr

section 2eO, virtualization           

++  mack
        Accpet a nock subject-formula cell.
        Produce a unit result, treating 11 as a crash (i.e. pure nock).
        ---
        Creates a dry %gold gate accepting cell ['sub' 'fol'].
        Its output is a unit (of a noun).
        Let 'ton' be the result of minking the sample, with a sky that produces
        ~ on any input, halting interpretation.
        Unless ton has stem 0, produce the empty unit, otherwise produce one
        containing ton's bulb.
        ---
        ~zod/try=> (mack [[1 2 3] [0 1]])
        [~ [1 2 3]]
        ~zod/try=> (mack [41 4 0 1])
        [~ 42]
        ~zod/try=> (mack [4 0 4])
        ~
        ~zod/try=> (mack [[[0 2] [1 3]] 4 4 4 4 0 5])
        [~ 6]
        ~zod/try=> ;;((unit ,@tas) (mack [[1 %yes %no] 6 [0 2] [0 6] 0 7]))
        [~ %no]
++  mink
        XX
  Description:
        Bottom-level mock (virtual nock) interpreter.
        Accepts a nock subject-formula cell, and an %iron gate which 
          accepts any noun and produces a unit, which is defined to be mock 11.
        Produces a ++tone, which is the result of the virtualized computation.
        ---
        For clarity, a ++tone with stem %0 will be referred to as a "success",
        one with stem %1 as a "block", and one with stem %2 as a "crash".
        ---
        Activate jet.
        Creates a dry %gold gate accepting cell ['sub' 'fol'] and gate 'sky'.
        Let 'tax' be a statically bunted list of term-noun pairs. (hint list)
        Do (recursion point) produce a tone:
        If fol is an atom
          Produce a crash of fol.
        Else if the head of fol is a cell
          Let hed be the result of recurring with fol replaced by its head.
          If hed is a crash
            Yield it
          Otherwise let 'tal' be the result of recurring with fol replaced 
          by its tail.
          Switch on the type of tal by stem:
            If tal is a success
              If hed is a block produce hed.
              Else (success) produce a success of a cell of the bulbs of hed 
              and tal.
            If tal is a block
              If hed is a success produce tal.
              Else (block) produce a block of welding the bulbs of hed and tal.
            Else (crash) produce tal
        Otherwise (the head of fol is an atom) switch on fol,
          by default producing a crash of tax.
            If fol has stem 0 and an atom bulb we name 'b'
          If b is 0 produce a crash of tax.
          If b is 1 produce a success of sub.
          If sub is an atom produce a crash of tax
          Otherwise let 'now' be the cap of b, and 'lat' be the mas of b
          Tail-recur with b replaced by lat, and sub replaced by: if now is 2,
          its head, else its tail.
            If fol has stem 1 and a bulb we name 'b'
          Produce a success of b
            If fol has stem 2 and a bulb whose head is a cell.
          Let 'ben' be the result of recurring with fol replaced by its bulb.
          Unless ben is a success, produce ben.
          Else assert that ben contains a cell, and tail-recur with 
          sub and fol replaced by the head and tail of ben's bulb
            If fol has stem 3 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else produce a success of (loobean) whether ben contains a cell.
            If fol has stem 4 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else unless ben contains an atom produce a crash of tax.
          Otherwise produce a success of ben's contents, incremented.
            If fol has stem 5 and a bulb we name 'b'
          Let 'ben' be the result of recurring with fol replaced by b.
          Unless ben is a success, produce ben.
          Else unless ben contains a cell produce a crash of tax.
          Otherwise produce a success of (loobean) whether the bulb of ben has
          a tail equal to its head.
            If fol has stem 6, 7, 8, or 9
          Tail-recur with its bulb expanded per nock specification.
            If fol has stem 10 and a cell bulb whose head is an atom
          Tail-recur with for replaced by its bulb's tail
            If fol has stem 10 and a bulb that can be destructured as [[b c] d]
          Let ben be the result of recurring with fol replaced by v.
          Unless ben is a success, produce ben.
          If b is %hunk, %lose, %mean, or %spot
            Tail-recur with fol replaced by d and tax prepended with a pair of
            b and the bulb of ben.
          Else tail-recur with just fol replaced by d.
  Examples:
        XX
++  mock
        XX
  Description:
        Accepts a nock subject-formula cell and an %iron gate which
          accepts any noun and produces a unit (this is used as nock 11).
        Produces a ++toon, which is a sucesful, blocked, or crashed result.
        ---
        Compose ++mook and ++mink.
  Examples 
        ~zod/try=> (mock [5 4 0 1] ,~)
        [%0 p=6]
        ~zod/try=> (mock [~ 11 1 0] |=(* `999))
        [%0 p=999]
        ~zod/try=> (mock [~ 0 1.337] ,~)
        [%2 p=~]
        ~zod/try=> (mock [~ 11 1 1.337] ,~)
        [%1 p=~[1.337]]
        ~zod/try=> (mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[+<]))
        [%0 p=14]
        ~zod/try=> (mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[<+<>]))
        [%0 p=[49 52 0]]
        ~zod/try=> ;;(tape +:(mock [[[4 4 4 4 0 3] 10] 11 9 2 0 1] |=(* `[<+<>])))
        "14"

++  mook
        XX
  Description:
        Intelligently render crash annotation.
        Accepts a ++tone, produces a ++toon
        ---
        Create a dry %gold gate accepting a tone we name 'ton'
        Its output is a toon.
        Unless the stem of ton is %2, produce ton.
        Produce a frond with a stem of 2 and the following bulb:
        Let yel be the length of ton's bulb.
        Replace the bulb of ton,
          If yel > 256 (otherwise keep it static)
          With the weld of
            its top 128 elements
          And a list of term-noun pairs:
          The last 128 elements of ton's bulb
          Preceded by a %lose-stemmed frond of
          A cord from the tape
            "[skipped "
            A @ud rendering of yel - 256
            " frames]"
        Do (*recursion point*) produce a list of tanks:
        For each element in the bulb of ton
        Switch on its leaf, by default leaving the element out of the product
          For each %hunk, clam it to a tank
          For each %lose, produce a leaf with the element clammed to an atom 
          and tripped (to a tape).
          For each %mean, if the elment is an atom treat it as a %lose
              Otherwise let mac be the element macked by its tail.
              If the computation fails, produce a "####" leaf, else clam
              the result to a tank.
          For each %spot, let sot be the element clammed to a spot.
              Produce a leaf with
              The weld of
                The path in sot converted to a tank and then a tape
                ":<["
                [[p.p ] ] in the pint in sot rendered as @ud
                " "
                [[ q.p] ] in the pint in sot rendered as @ud
                "].["
                [ [p.q ]] in the pint in sot rendered as @ud
                " "
                [ [ q.p]] in the pint in sot rendered as @ud
                "]>"
                
  Examples 
        ~zod/try=> (mook [%0 5 4 5 1])
        [%0 p=[5 4 5 1]]
        ~zod/try=> (mook [%2 ~[[%hunk %rose ["<" "," ">"] ~[[%leaf "err"]]]]])
        [%2 p=~[[%rose p=[p="<" q="," r=">"] q=[i=[%leaf p="err"] t=~]]]]
        ~zod/try=> (mook [%2 ~[[%malformed %elem] [%lose 'do print']]])
        [%2 p=~[[%leaf p="do print"]]]
        ~zod/try=> (mook [%2 ~[[%spot /b/repl [[1 1] 1 2]] [%mean |.(!!)]]])
        [%2 p=~[[%leaf p="/b/repl/:<[1 1].[1 2]>"] [%leaf p="####"]]]
++  mang
        XX
  Description:
        XX
        ---
        XX
  Examples 
        XX
++  mong
        XX
  Comment:
        XX
  Description:
        Mang is just like mack, but accepting a sky.
        It produces a unit computation result.
        ---
        Creates a dry %gold gate accepting cell ['sub' 'fol'] and an
        %iron unit-clam 'sky'.
        Its output is a unit (of a noun).
        Let 'ton' be the result of monging the sample.
        Unless ton has stem 0, produce the empty unit, otherwise produce one
        containing ton's bulb.
++  mung
        XX
  Description:
        XX
        ---
        XX
  Examples 
        XX
++  mule 
        XX
  Description:
        XX
        ---
        XX
  Examples 
        XX
++  mute 
        XX
  Description:
        XX
        ---
        XX
  Examples 
        XX

section 2eP, diff          **noted as "(move me)" in source**

++  berk 
++  diff 
++  loss 
  ++  abet
  ++  hink
  ++  lonk
  ++  lune
  ++  merg
  ++  main
++  locz  
++  lore  
++  role  
++  lump  
++  lure  
++  limp  
++  hump  
++  husk  
++  lurk  
++  lusk  
  ++  abet
  ++  done
  ++  main
++  nude   
  ++  axes 
  ++  tred 

section 2eW, lite number theory           

++  egcd   
++  pram   
++  ramp   
++  fo     
  ++  dif
  ++  exp
  ++  fra
  ++  inv
  ++  pro
  ++  sit
  ++  sum
++  ga     
      ++  dif 
      ++  dub 
      ++  pro 
      ++  toe 
      ++  sit 
  ++  fra     
  ++  inv     
  ++  pow     
  ++  pro     

section 2eX, jetted crypto                

++  aesc      
  ++  en      
  ++  de      
++  ahem      
        ++  cipa 
          ++  co 
          ++  ix 
          ++  ro 
          ++  su 
    ++  pen      
      ++  co  
      ++  ix  
      ++  ro  
      ++  su  
    ++  pin   
      ++  co  
      ++  ix  
      ++  ro  
      ++  su  
    ++  mcol
    ++  pode  
    ++  sube  
  ++  be      
          ++  ankh
          ++  sark
          ++  srow
          ++  subs
  ++  ex      
  ++  ix      
++  curt      
        ++  cla
        ++  sqr
        ++  inv
        ++  cad
        ++  cub
++  ed         
    ++  norm  
    ++  xrec  
    ++  ward  
    ++  scam  
    ++  etch  
    ++  curv  
    ++  deco  
    ++  bb
  ++  puck    
  ++  suck    
  ++  sign    
  ++  veri    

section 2eY, SHA-256 

++  shad  
++  shaf  
++  shak  
++  sham  
++  shas  
++  shax  
++  shaw  
++  og    
  ++  rad 
  ++  raw 
++  shaz  
++  shal  
++  shan  

section 2eZ, OLD rendering

++  show  
  ++  shep
  ++  shop
++  at
  ++  r
  ++  rf 
  ++  rn 
  ++  rt 
  ++  rta
  ++  rtam
  ++  rub 
  ++  rud 
  ++  rum
  ++  rup
  ++  ruv
  ++  rux 

chapter 2f, Hoon proper

section 2fA, miscellaneous funs       

++  bull
++  cain  
++  cell
++  core
++  cube
++  face
++  bean  
++  flay
++  flee
++  foil
++  fork
++  cove
++  comb
++  cond
++  cons
++  fitz
++  flan
++  flip
++  flor
++  hike
++  hoax
++  hoof
++  jock
++  look
++  make
++  noah  
++  onan  
++  rain
++  ream
++  reck
++  seed
++  seem  
++  seer  
++  sell
++  pave
++  loot
++  slam
++  slim
++  slit
++  slym
++  slap
++  slop
++  skol
++  spat  
++  spuc
++  spec
++  spud  
++  slot
++  slum
++  stab
++  wash

section 2fB, macro expansion          

++  ah
  ++  blue
  ++  gray
  ++  puce
++  al
  ++  blah
  ++  home
  ++  bunt
  ++  clam
  ++  cloq
  ++  whip
++  ap
  ++  etch
  ++  feck
  ++  hock
  ++  open
  ++  rake
  ++  rusk

section 2fC, compilation proper       

++  ut
  ++  burn
  ++  busk
  ++  conk
  ++  crop
    ++  dext
    ++  sint
  ++  cool
  ++  dank
  ++  dart
  ++  deal
  ++  dial
  ++  dish
  ++  doge
  ++  dole
  ++  duck
  ++  dune
  ++  dunk
  ++  fino
  ++  fink
  ++  finq
  ++  fire
  ++  firm
  ++  fish
  ++  fuse
  ++  gain
  ++  hang
  ++  harp
  ++  lose
  ++  chip
  ++  heal
  ++  mint
    ++  nice
    ++  grow
  ++  moot
  ++  mull
    ++  both
    ++  nice
    ++  grow
    ++  bake
  ++  meet  
  ++  nest
    ++  cong
    ++  cram
    ++  dext
    ++  sint
  ++  park
  ++  peek
  ++  play
  ++  reco
  ++  repo
  ++  rest
  ++  seek
  ++  seep
  ++  sift
  ++  snub
  ++  tack
  ++  tock
  ++  wrap

section 2fD, grammar                  

++  vang
++  vast
  ++  gash  
  ++  gasp  
  ++  glam  
  ++  hasp  
  ++  mota  
  ++  plex
  ++  pray
  ++  prey
  ++  phax
  ++  posh
  ++  poof
  ++  poon
  ++  poor
  ++  porc
  ++  rump
  ++  rood
  ++  rupl
  ++  sail 
    Templating language for rendering HTML web documents.
    ---
    Build dry %gold gate with sample bean `tol`.
    Push bunt of bean `lin` onto the core below.
    Build core,
    ---
    ++  ape
        The apex node of 
        ---
        Slam cook with:
            The reverse of `amp` and the following gate:
                Build dry %gold gate with sample tuna `tum`.  Yield twig.
                If: `tum` is [%e *], an element,
                    Then: Produce the twig of twig `p.tum` and sag slammed with `q.tum`
                Else: Produce the sag slammed with `tum` and null, the twig of `tum`
        ---
    ++  amp
        Entry point of the XML tree.  Start parsing a template at `sem`.
        Continue to parse with ++ba if in tall form and ++bat if in wide form.
        ---
        Gonadify pfix with:
                sem,
                If: tol,
                    Then: bam,
                Else: bat
        ---
    ++  bam
        Begin to parse the template in tall form.
        ---
        Slam knee with:
            The bunt of tuna.
            Build dry %gold trap which is cached (memoized)
            Gonadify pose with:
                Stag slammed with %f, pfix gonadified with:
                    (plus ace), 1 or whitespaces,
                    Cook slammed with rab, puv
                Stag slammed with %e, pfix gonadified with:
                    `ag,
                     nal
                Stag slammed with %e, hul, 
                Stag slammed with %f, nup, 
                Gonadify pfix with:
                    tis,
                    Stag slammed with %f, nol
                Gonadify pfix with:
                    hep
                    Stag slammed with:
                        %a,
                        Gonadify pfix with gap, talL
                Gonadify pfix with:
                    lus
                    Stag slammed with:
                        %b,
                        Gonadiffy pfix with gap, tall
                Gonadify pfix with:
                    tar
                    Stag slammed with:
                        %c,
                        Gonadify pfix with gap, tall
                Gonadify pfix with:
                    cen
                    Stag slammed with:
                        %d,
                        Gonadify pfix with gap, tall
                Slam easy with [%f [%a [%smdq 10 ~]] ~]
        ---
    ++  bat
        Begin to parse an entire template line in wide form.
        ---
        Slam knee with:
            The bunt of tuna
            Build dry %gold trap which is cached (memoized)
            Gonadify pose with:
                Stag slammed with %f, nup
                Stag slammed with %f, ped
                Stag slammed with:
                    %e, 
                    Gonadify plug with hip, lif
            Terminate pose gonadification.
        ---
    ++  bet
        Begin to parse an inner line section in wide form.
        ---
        Slam knee with:
            The bunt of tuna
            Build dry %gold trap which is cached (memoized)
            Gonadify pose with:
                bat,
               Gonadify pfix with:
                    hep
                    Stag slammed with %a, wide
               Gonadify pfix with:
                    lus
                    Stag slammed with %b, wide
               Gonadify pfix with:
                    tar
                    Stag slammed with %c, wide
               Gonadify pfix with:
                    cen
                    Stag slammed with %d, wide
            Terminate pose gonadification.
        ---
    ++  fry
        Element or attribute name.
        May contain '_', signifying ':', optionally followed by 
        class and/org id
        ---
        Slam cook with:
            Build dry %gold gate with sample term `a`, unit term `b`:
                If: `b` is null,
                    Then: Produce [%dtzz %tas a],
                Else: Produce [[%dtzz %tas a] [%dtzz %tas u.b]]
            Gonadify plug with:
                sym,
                Gonadify pose with:
                    Stag slammed with null, pfix gonadified with cab, sym.
                    Easy slammed with null.
    ++  hag
        Tag head
        ---
        Slam cook with:
            Build dry %gold gate with sample twig `a` and produces `a`.
            Gonadify plug with:
                Stag slammed with:
                    %dtzz,
                    Stag slammed with:
                        %tas
                        Pose gonadified with:
                            Jest slammed with %script,
                            Jest slammed with %style,
                Stag slammed with %clsg, jaw
            Terminate plug gonadification.
    ++  hig
        Simple tag head.
        ---
        Slam cook with:
            Build dry %gold gate with sample twig `a`, list of twigs `b`. Produce [a %clsg b].
            hog.
    ++  hog
        Tag head.
        ---
        Slam cook with:
            Build dry %gold gate with sample tile:
                twig `a`
                unit term `b`
                unit term `c`
                unit twig `d`
                list of twigs `e`
            Terminate tile construction.
        Yield pair of twig, list of twigs.
        Set `e` to: If: `b` is null,
                        Then: Produce `e`,
                    Else: Produce [[[%dtzz %tas %class] [%smdq (trip u.b)]] e]
        Set `e` to: If: `c` is null,
                        Then: Produce `e`,
                    Else: Produce [[[%dtzz %tas %id] [%smdq (trip u.c)]] e]
        Set `e` to: If: `d` is null,
                        Then: Produce `e`,
                    Else: Produce [[[%dtzz %tas %href] u.d] e]
        Produce pair [a e].
        Gonadify plug with:
            fry,
            Pose gonadified with:
                Stag slammed with null, pfix gonadified with dot, sym
                Easy slammed with null,
            Pose gonadified with stag slammed with
                Stag slammed with null, pfix gonadified with hax, sym
                Easy slammed with null,
            Pose gonadified with stag slammed with
                Stag slammed with null, pfix gonadified with dot, stag slammed with %smdg, soil
                Easy slammed with null,
            Pose gonadified with:
                ifix slammed with [pel per] and,
                    More slammed with plug gonadified with com, ace and,
                        Plug gonadified with fry, pfix gonadified with ace, wide
                Easy slammed with null
            Terminate pose gonadification.
        Terminate plug gonadification.
    ++  hoy
        Tail attributes.
        ---
        Star slammed with:
            pfix gonadified with:
                plug gonadified with gap, tis
                plug gonadified with fry, pfix gonadified with gap, tall
            Terminate pfix gonadification.
    ++  hul
        Tall preface.
        ---
        Cook slammed with:
            Build dry %gold gate with sample element tuna `a`, list twig `b`, list tuna `c`,
                Yield [twig (list tuna)] element tuna.
                Produce [[p.a %clsg (weld q.a b)] c]
            Gonadify plug with hog, hoy, nol
    ++  jaw
        Wide attributes
        ---
        Gonadify pose with:
            ifix slammed with [pel per] and,
                More slammed with plug gonadified with com, ace, and,
                plug gonadified fry, pfix gonadified with ace, wide
            Easy slammed with null.
        Terminate pose gonadification.
    ++  lif
        Wide elements.
        --- 
        Slam cook with:
            Build dry %gold gate with sample list of tuna `a` which produces `a`
            Gonadify pose with:
                pfix gonadied with col, pep
                cold slammed with null, sem
                easy slammed with null
        --- 
    ++  luf
        Wide elements.
        ---
        Slam cook with:
            Build dry %gold gate with sample list of tuna `a` which produces `a`
            Star slammed with:
                Gonadify pfix with ace, bet.
        --- 
    ++  nal
        Unescaped tall tail.
        ---
        Slam cook with sample:
            Build dry %gold gate with sample list of tuna `a` which produces `a`,
            Slam ifix with:
                Tile autoons gap and plug gonadified with gap, duz,
                Slam most with:
                    gap,
                    Gonadify pfix with:
                        Gonadify pose with:
                            Gonadify pfix with:
                                ace,
                                Cook slammed with:
                                    Build dry %gold gate with sample tape `a` which produces:
                                        [%a %smdq (weld a `tape`[`@`10 ~])],
                                        where (weld a `tape`[`@`10 ~] is the concatenation
                                        of tape `a` with the newline character.
                                    Star slammed with shim slamme with 32, 255.
                            Terminate pfix gonadification.
                        Terminate pose gonadification.
                    Terminate pfix gonadification.

        --- 
    ++  nol
        Tall tail.
       ---
        Assert that tol is true, that we are parsing a tall form part of the template.
        Slam cook with:
            Build dry %gold gate with sample list of tuna `a` which produces `a`,
            Gonadify pose with:
                Slam cold with null, sem
                pfix gonadified with col, pep called with tol replaced by false, to continue to parse in wide form.
                pfix gonadified with:
                    Gonadify plug with col, ace.
                    Slam cook with rab called with tol replaced by false, to continue to parse in wide form, and puv.
                ifix slammed with:
                    [gap :~(plug gap duz)], which disregards non-code before plug gonadifed with gap and duz.
                    Most slammed with gap, amp.
            Temrinate pose gonadification. 
       --- 
    ++  nup
        Wide quote.
        --- 
        Slam cook with:
            Build dry %gold gate with sample list of tuna `a` which produces `a`
            Gonadify pose with:
                Gonadify less with:
                    jest slammed with '"""'<
                    ifix slammed with:
                        [doq doq]
                        Cook slammed with rab, puv
                inde slammed with:
                    ifix slammed with:
                        Jest slammed with '"""\0a' and jest slammed with '\0a"""',
                        Cook slammed with rab, puv called with `lin` tossed for false.
            Terminate pose gonnadification. 
        --- 

    ++  pab
        Bracketed element.
        ---
        Slam ifix with [kel ker], plug gonadified with:
            hig, to parse an uppercase letter followed by,
            luf, to parse wide elements.
        ---

    ++  ped
        Wide flow.
        ---
        Slam cook with:
            Dry %gold gate with sample list of tuna `a` which produces `a`
            ifix slammed with [pel per], more slammed with ace, bet.
        ---
        
    ++  pep
        Wrapper tuna.
        ---
        Slam cook with:
            Build dry %gold gate with sample list of tuna `a` which produces `a`
            Gonadify pose with:
                ped,
                ifix slammed with:
                    [pel per],
                    More slammed with ace, bet
                cook slammed with:
                    Build dry %gold gate with sample cord,
                        Which produces list of element [%a %smdg (trip +<)].
                    qut
                Gonadify plug with bat, easy slammed with null.
                Terminate plug gonadification.
            Terminate pose gonadification.
        ---
         
    ++  puv
        Wide/tall flow.  Parse wide form interpolated Hoon code in any tag,
        
        ---
        Slam cook with:
            Build dry %gold with samlpe list of beets `a` which produces `a`
            The slam of star with:
                Gonadify pose with:
                    pfix gonadified with:
                        bas,
                        pose slammed with:
                            mask slammed with tape "-+*%;\{",
                            bas, doq, bix:ab.
                    pfix gonadified with hep, stag slammed with %a, sump
                    pfix gonadified with lus, stag slammed with %b, sump
                    pfix gonadified with tar, stag slammed with %c, sump
                    pfix gonadified with cen, stag slammed with %d, sump
                    pfix gonadified with sem, stag slammed with %e, 
                    less gonadified with:
                        bas, kel, 
                        If: tol, Then: fail, Else: doq
                        prn
                    If: lin, Then: fail,
                    Else: Gonadify less with:
                        Jest slammed with '\0a"""',
                        Just slammed with '\0a'
                    Stag slammed with %a, sump
                Terminate pose gonadification.
        ---
    ++  rab
        Beet to tuna.
        ---
        Build a dry %gold gate with sample list of beets, `reb`
        Yield list of tuna.
        Push bunt of [sim=(list ,@) tuz=(list tuna)]
        Kick dry %gold trap. Yield list of tuna.
        If: `reb` is null,
            Then: Set `sim` to Unless: tol, Then: Produce sim,
                Else: Produce [10 |-(?~(sim sim ?:(=(32 i.sim) $(sim t.tim) sim)))]
        Else: If: The head of `reb` is an atom,
            Then: Produce the toss `reb` for the tail of `reb`, `sim` for [i.reb sim]
        Else: Push `zut` is the toss of `reb` for the tail of `reb`, `sim` for null
        If: `sim` is null, Then: Produce [i.reb zut],
        Else: Produce [[%a %smdq (flop sim)] i.reb zut]
    ++  sag
        Produce a twig from a tuna.
        ---
        Build dry %gold gate with sample list of tunas, `lut`
        Yield twig.
        Pair %cltr and,
        Build dry %gold trap.  Yield list of twigs.
        If: `lut` is null,
            Then: Produce [[%dtzz %n ~] ~],
        Else: Switch on the head of the head of `lut`,
        if %a, produce [[%clfs p.i.lut] $(lut, t.lut)],
        if %b, produce [p.i.lut $(lut t.lut)]
        if %c, produce the reverse cell of null and,
            Triple %cndt, cast of [p.i.lut $(lut t.lut)] to a twig and,
            Triple %tsbr, cast of [[%axil %noun] [%axil %noun]] to a tile and,
            Pair   %brcn and,
            Yield a map of terms to feet,
            Reverse cell of [~ ~] and,
            Push `sug` is [[%& 12] ~]
            Triple %$, %elm, and,
            Quad %wtsg, `sug`, [%cnts `sug` [[[%& 1] ~] [~ 13]]]
        if %d, produce [%cnhp]
        if %e, produce 
        if %f, produce 
            where $(lut, t.lut) is the toss of `lut` for the tail of `lut`.
        Terminate switch statement.
        ---
  ++  scat
  ++  soil
  ++  sump 
  ++  noil
    ++  toad
    ++  rung
    ++  gunk
    ++  muck
    ++  butt
    ++  loaf
    ++  lobe
    ++  exqa
    ++  exqb
    ++  exqc
    ++  exqd
    ++  exqe
  ++  norm
    ++  boog
    ++  wisp
    ++  toad
    ++  rune
    ++  glop
    ++  gunk
    ++  butt
    ++  ulva
    ++  hank
    ++  loaf
    ++  lobe
    ++  mash
    ++  muck
    ++  teak
    ++  race
    ++  rack
    ++  rick
    ++  expa
    ++  expb
    ++  expc
    ++  expd
    ++  expe
    ++  expf
    ++  expg
    ++  exph
    ++  expi
    ++  expj
    ++  expk
    ++  expm
    ++  expn
    ++  expo
    ++  expp
    ++  expq
    ++  expr
    ++  exps
    ++  expt
    ++  expu
    ++  expv
    ++  expw
    ++  expx
    ++  expy
    ++  expz
    ++  hina
    ++  hinb
    ++  hinc
    ++  hind
    ++  hine
    ++  hinf
    ++  hing
    ++  bonk
    ++  bont
    ++  bony
    ++  bonz
  ++  lung
  ++  long
  ++  lobo
  ++  loon
  ++  lute
  ++  rope
  ++  tall
  ++  wide
  ++  hill
  ++  howl
  ++  toil
  ++  wart
++  vest
++  vice


