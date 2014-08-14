chapter 2b, basic containers

Section 2bA, units                    

##++biff 

Apply a function which produces a unit to the value of a unit

####Summary

        Build wet gate with a sample unit `a` and tiled gate `b`
        If: `a` is null
          Then: produce null
        Else: slam `b` with `u.a`.

####Examples

        ~palryp-hocsyt/try=> (biff (some 5) |=(a=@ (some (add a 2))))
        [~ u=7]
        ~palryp-hocsyt/try=> (biff ~ |=(a=@ (some (add a 2))))
        ~
 
##++bind 

Apply a function to the value of a unit.

####Summary
 
        Build wet %gold gate with sample unit `a` and gate `b`
        If: `a` is null
          Then: produce null
        Else, the unit of the slam of `b` with `u.a`.

####Examples

        ~talsur-todres/try=> (bind ((unit ,@) [~ 97]) ,@t)
        [~ `a`]
        ~talsur-todres/try=> =a |=(a=@ (add a 1))
        ~talsur-todres/try=> (bind ((unit ,@) [~ 2]) a)
        [~ 3]

##++bond
        
Evaluate trap `a` if `b` is null 

####Summary

        Build a wet %gold gate with sample trap `a`.
        Build a wet %gold gate with sample unit `b`.
        If: `b` is null,
          Then: pull `$` from `a`
        Else: produce `u.a`

####Examples

        ??

##++both

        Take a cell of units and produce a unit with a cell value of the values of the two uni        ts

####Summary

        Build a wet %gold gate with a sample cell of units, labeled a and b.
        If: a is null
          Then: produce null
        Else: If: b is null
          Then: produce null
        Else: produce a unit with a cell value of a b.

####Examples

        ??


##++clap 

Apply a binary operation which yields a unit to the values of two units

####Summary

        Build wet %gold gate with sample unit `a`, unit `b` and gate `c`
        If: `a` is null
          Then: produce `b`
        Else: If: `b` is null
          Then: produce `a`
        Else: the unit of the slam of `c` with [u.a u.b]

####Examples

        ~palryp-hocsyt/try=> =u ((unit ,@t) [~ 'a'])
        ~palryp-hocsyt/try=> =v ((unit ,@t) [~ 'b'])
        ~palryp-hocsyt/try=> (clap u v |=([a=@t b=@t] (welp (trip a) (trip b))))
        [~ u="ab"] 
        ~talsur-todres/try=> =a ((unit ,@u) [~ 1])
        ~talsur-todres/try=> =b ((unit ,@u) [~ 2])
        ~talsur-todres/try=> =c |=([a=@ b=@] (add a b))
        ~talsur-todres/try=> (clap a b c)
        [~ 3]

##++drop 

Produce a list of the unit-value 

####Summary

        Build wet %gold gate with sample unit `a`
        If: `a` is null
          Then: produce null
        Else, produce the list [u.a ~]

####Examples
 
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> (drop a)
        [i=97 t=~] 
        ~divreg-misdef/try=> =a ((unit ,@) [~])
        ~divreg-misdef/try=> (drop a)
        ~

##++fall 

A default value `b` for the unit `a` when `a` is null

####Summary

        Build wet %gold gate with sample unit `a` and noun `b` 
        If: `a` is null
          Then: produce `b`
        Else: produce the unit-value of `a`

####Examples

        ~talsur-todres/try=> (fall ~ `a`)
        `a`
        ~talsur-todres/try=> (fall [~ u=0] `a`)
        0

##++lift

Fmap; Accept a gate that accepts and produces an unwrapped value and pass it a
unit value, then produce a unit value.

####Summary

          Build wet %gold gate with sample gate `a`.
          Build wet %gold gate with sample unit `b`.
          Bind `a` and `b`.

##+mate 

Produce the unit `a` or `b` which is not null and crashes with error
"mate" unless `a` and `b` are equal.

####Summary
 
        Create a wet %gold gate with sample `unit a` and `unit b`.
        If: `b` is null,
          Then: produce `a`.
        Else: If: `a` is null,
          Then: produce `b`.
        Else: Unless: `u.a` is `u.b`,
          Then: error on crash.
        Else: produce `a`

####Examples

        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~ 97])
        ~divreg-misdef/try=> (mate a b)
        [~ 97]
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~])
        ~divreg-misdef/try=> (mate a b)
        [~ 97]
        ~divreg-misdef/try=> =a ((unit ,@) [~ 97])
        ~divreg-misdef/try=> =b ((unit ,@) [~ 98])
        ~divreg-misdef/try=> (mate a b)
        ! 'mate'
        ! exit

##++need 

Retrieve the value from a unit and crash if the unit is null.

####Summary

        Build wet %gold gate with sample unit `a` of any type. 
        If: `p` is null, 
          Then: fail,
        Else: produce `u.a`, the value of the unit.  

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

##++some 

Casts any noun `a` to its unit, [~ a].

####Summary

        Build wet %gold gate with sample noun `a`. 
        Produce the tuple [~ u=a], the unit of value `a`.

####Examples

        ~divreg-misdef/try=> (some [`a` `b`])
        [~ u=[`a` `b`]]
        ~divreg-misdef/try=> (some &)
        [~ u=%.y]

section 2bb, lists                    

##++flop 

Produces the list `a` with the elements reversed.
       
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

##++homo 

Homogenizes a lists' type information.

####Summary

        Build a wet %gold gate with single list sample.
        Cast `a` to the the type of the product of the code below. 
        The subject of the arm ($) is then the product of the barcen statement below (=<)        Creates a %gold core (|%) and the arm '+-  $'.
        XXX BLACK BOX, ABANDON ALL HOPE YE WHO ENTER HERE XXX
        Terminates the core.
        Produces list a.
  
####Examples

        ~palryp-hocsyt/try=> lyst
        [i=1 t=[i=97 t=[i=2 t=[i=98 t=[i=[~ u=10] t=~]]]]]
        ~palryp-hocsyt/try=> (homo lyst)
        ~[1 97 2 98 [~ u=10]]
        ~palryp-hocsyt/try=> =a (limo [1 2 3 ~])
        ~palryp-hocsyt/try=> a
        [i=1 t=[i=2 t=[i=3 t=~]]]
        ~palryp-hocsyt/try=> (homo a)
        ~[1 2 3]

##++limo 

Produces a list from any null-terminated tuple.

####Summary
       
        Build wet %gold gate with single list sample.
        Cast `a` to the the type of the product of the code below.        
        The subject of the arm ($) is then the product of the barcen statement below (=<).
        Creates a %gold core (|%) and the arm '+-  $'.
        XXX BLACK BOX, ABANDON ALL HOPE YE WHO ENTER HERE XXX
        Terminates the core.
        Produces list a.

####Examples:

        ~palryp-hocsyt/try=> (limo [1 'a' 2 'b' (some 10) ~])
        [i=1 t=[i=97 t=[i=2 t=[i=98 t=[i=[~ u=10] t=~]]]]]
        ~palryp-hocsyt/try=> (limo [`a` `b` ~])
        [i=`a` t=[i=`b` t=~]]
        ~palryp-hocsyt/try=> (limo [2 1 ~])
        [i=2 t=[i=1 t=~]]

##++lent 

Produces the atomic length of any list.

####Summary

        Activate jet.
        Build a dry %gold gate with single list sample
        Yield an atom
        Let `b` be 0.
        Create and kicks a dry %gold trap.
        If: `a` is an atom,
          Then: produce `b`.
        Else: recursively call length with the list set to the tail of `a`, and the accum        ulator, `b`, incremented.

####Examples:

        ~palryp-hocsyt/try=> (lent (limo [1 2 3 4 ~]))
        4
        ~palryp-hocsyt/try=> (lent (limo [1 'a' 2 'b' (some 10) ~]))
        5

##++levy
       
Applies the loobean gate `b` to every element of the list `a`, producing the logical AND of all the results. 

####Summary

        Activate jet.
        Build wet %gold gate with sample that accepts a list and a gate with a sample         of any noun and produces a loobean.
        A dry %gold trap is created and kicked.
        Yield a loobean. 
        If: `a` is an atom.
          Then: produce true.
        Else: If: `b` applied to `i.a` is true,
          Then: recursively call levy with `a` replaced by the tail of `a`.
        Else: produce false.

####Examples:

        ~palryp-hocsyt/try=> =b |=(a=@ (gte a 1))
        ~palryp-hocsyt/try=> (levy (limo [0 1 2 1 ~]) b)
        %.n
        ~palryp-hocsyt/try=> =b |=(a=@ (gte a 0))
        ~palryp-hocsyt/try=> (levy (limo [0 1 2 1 ~]) b)
        %.y        

##++lien 

Is the slam of any element in list `a` to `b` true? (Boolean 'or')

####Summary

        Activate jet.
        Build wet %gold gate with sample list `a`, gate which accepts a noun and produces a bean `b`
        Kick dry %gold trap.
        Yield bean.
        If: `a` is null,
          Then: Produce false.
        Else: Unless the slam of the head of `a` to `b`,
          Then: Produce true.
        Else: Produce the toss of `a` for `t.a`

####Examples

        ~palryp-hocsyt/try=> =a |=(a=@ (gte a 1))
        ~palryp-hocsyt/try=> (lien (limo [0 1 2 1 ~]) a)
        %.y
        ~palryp-hocsyt/try=> =a |=(a=@ (gte a 3))
        ~palryp-hocsyt/try=> (lien (limo [0 1 2 1 ~]) a)
        %.n

##++murn

Accepts a list of units and a gate.  Produces the list with the gate applied to each unit of the list that has a value; nulls are discarded.

####Summary

        Build a wet %gold gate with sample that accepts list and gate that accepts a noun and produces a unit.
        Kick dry %gold trap. 
        If: `a` is an atom,
          Then: produce null
        Else: push `c` as result of `b` slammed with `a`
        If: `c` is an atom,
          Then: toss `a` for the tail of `a`
        Else: Produce the cell `i` is `u.c`, `t` is the toss of `a` for `t.a`

##++reel 

Right fold - Move right to left recursively slamming a binary gate with an element from the list and an accumulator,
        producing the final value of the accumulator.

####Summary

        Activate jet.
        Build wet %gold gate sample list `a`, bunt of gate `b` which accepts two nouns and produces `q`
        Kick dry %gold trap.  Cast the result to the type of `q` in `b`
        If: `a` is an atom:
          Then: Produce the noun `q` in the sample of `b`
        Else: Produce `b` slammed by:
                The head of `a`
                The toss of `a` for the tail of `a`

####Examples

        ~palryp-hocsyt/try=> =sum =|([p=@ q=@] |.((add p q)))
        ~palryp-hocsyt/try=> (reel (limo [1 2 3 4 5 ~]) sum)
        15
        ~palryp-hocsyt/try=> =a =|([p=@ q=@] |.((sub p q)))
        ~palryp-hocsyt/try=> (reel (limo [6 3 1 ~]) a)
        4
        ~palryp-hocsyt/try=> (reel (limo [3 6 1 ~]) a)
        ! subtract-underflow
        ! exit

##++roll
 
Left fold - Move left to right recursively slamming a binary gate with an element from the list and an accumulator, producing the final value of the accumulator.

####Summary

        Activate jet.
        Build wet %gold gate with sample list `a`, and gate `b` which accepts two nouns and produces `q`
        Kick dry %gold trap.
        Yield the type of `q` in `b`.
        If: `a` is an atom,
          Then: Produce `q` in `b`
        Else: produce the toss of `a` for the tail of `a`, `b` for `b` with `q` replaced by `b` slammed by the head of `a`
        and `q` in `b`
        
####Examples

        ~barred-tidset/try=> =a =|([p=@ q=@] |.((sub p q)))
        ~barred-tidset/try=> (roll (limo [1 2 3 ~]) a)
        2
        ~barred-tidset/try=> (roll (limo [3 6 3 ~]) a)
        0

##++skid 

        Seperate a list `a` into two lists - Those elements of `a` who produce true when slammed to `b` and those who produce false.

####Summary

        Activate jet.
        Build wet %gold gate with sample list `a`, tile of gate `b` accepting a noun and producing a loobean.
        Kick a dry %gold trap.  Cast the result to the type of [p=a q=a], a cell of lists of type `a`
        If: `a` is null,
          Then: produce [~ ~] a cell of null and null,
        Else: push `c` is the toss of `a` for the tail of `a`
        If: The slam of the head of `a` to `b`,
          Then: produce the cell with the head of `a` added to the left element,
        Else: produce the cell with the head of `a` added to the right element, 
        where `p.c` and `q.c` are the left and right elements, respectively.
        ---
        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skid (limo [0 1 2 3 ~]) a)
        [p=[i=2 t=[i=3 t=~]] q=[i=0 t=[i=1 t=~]]]
##++skim
 
Accepts a list `a` and a gate `b` which takes any noun and produces a loobean.  
Produces the sublist of `a` whose elements produce true by slamming the gate with them.

####Summary

        Activate jet.
        Build wet %gold gate with sample thast accepts a list and a gate that accepts any noun and 
        Yield a loobean.
        Build and kick a dry %gold trap.
        Cast `a` to the type of the result of the trap        
        If: a is an atomm
          Then: produce null.
        Else: If: (b i.a),
          Then: produce [i.a $(a t.a)], where $(a t.a) is the recursive
          call of skim with a replaced by the tail of a.
        Else, toss `a` for `t.a`.

####Examples:

        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skim (limo [0 1 2 3 ~]) a)
        [i=2 t=[i=3 t=~]]

##+skip 

Accepts a list `a` and a gate `b` which takes any noun and produces a loobean.
Produces the sublist of `a` whose elments produce false when slammed with `b`. 

####Summary

        Activate jet.
        Build wet %gold gate with sample that accepts list `a` and gate `b` that accepts any noun and 
        produces a loobean.
        Kick dry %gold trap.
        Cast the list product to the type of `a`.
        If: `a` is an atom.
          Then: produce null.
        Else: If: (b i.a) yields true
          Then: produce the toss of `a` for`t.a`.
        Else: produce the list of i.a and the toss of `a` for`t.a`.

####Examples:

        ~dovryp-toblug/try=> =a |=(a=@ (gth a 1))
        ~dovryp-toblug/try=> (skip (limo [0 1 2 3 ~]) a)
        [i=0 t=[i=1 t=~]]

##+scag 

Accepts an atom `a` and list `b`,  producing the first `a` elements of the front of the list.

####Summary

        Activate jet.
        Build wet %gold gate with sample atom `a` and list `b`.
        Kick dry %gold trap.
        Cast the result to the type of `b`.
        If: `b` is null or `a` is 0,
          Then: produce null
        Else: produce the list `i.b` followed by the toss of `b` for `t.b`, `a` for (dec a).


        
####Examples:

        ~palryp-hocsyt/try=> (scag 2 (limo [0 1 2 3 ~]))
        [i=0 t=[i=1 t=~]]
        ~palryp-hocsyt/try=> (scag 10 (limo [1 2 3 4 ~]))
        [i=1 t=[i=2 t=[i=3 t=[i=4 t=~]]]]

##++slag 

Accepts an atom `a` and list `b`, producing the last `a` elements from the back of the list.

####Summary

        Activate jet.
        Creates a wet %gold gate which accepts an atom `a` and list `b`.
        Kick dry %gold trap. 
        Cast the result to the type of `b`.
        If: a is 0,
          Then: produce `b`
        Else: if `b` is an atom,
          Then: produce null
        Else: Toss `b` for `t.b`, `a` for (dec a).

####Examples:
        ~palryp-hocsyt/try=> (slag 2 (limo [0 1 2 3 ~]))
        [i=2 t=[i=3 t=~]] 
        ---
        ~palryp-hocsyt/try=> (slag 2 (limo [1 2 3 4 ~]))
        [i=3 t=[i=4 t=~]]

##++snag 

        Accepts an atom and a list, producing the element at the index of the atom in the list and failing if the list
        is null.

####Summary
        
        Activate jet.
        Build wet %gold gate that accepts an atom `a` and list `b`.
        Kick a dry %gold trap.
        If: `b` is null.
          Then: fail with "snag-fail" in the stack trace.
        Else: If: a=0,
          Then: produce the head of `b`.
        Else, toss `b` for `t.b` and `a` for (dec a).

####Examples:

        ~palryp-hocsyt/try=> (snag 2 (limo [3 2 1 0 ~]))
        1
        ~palryp-hocsyt/try=> (snag 4 (limo [1 2 3 4 5 6 7 8 ~]))
        5

##++sort 

Accepts a list `a` and a gate `b` with a sample which accepts two nouns and produces a loobean.  'sort' then produces a 
list of the elements of 'a' sorted according to 'b'.

####Summary

        Activate jet.
        Build wet %gold gate with sample that accepts list `a` and gate `b` that accepts two nouns and
        produces a loobean.
        Homogenize the list and make it the subject of the following code
        Cast the following to the homogenized list of type `a`.
        Kick dry %gold trap.
        Cast result to a list of type `a`
        If `a` is null,
          Then: produce null.
        Else: slam the weld gate with the q and r below.
        The q and r are then defined to be the recursive call of the trap with the skim of the tail by our sort gate.
        For q, it skims by (b c i.a).  For r, by !(b c i.a).
        r is first cast to the type of the tail of 'a' and produced as a tuple behind the head of 'a'.

####Examples:

        ~dovryp-toblug/try=> =a =|([p=@ q=@] |.((gth p q)))
        ~dovryp-toblug/try=> (sort (limo [0 1 2 3 ~]) a)
        ~[3 2 1 0]

##++swag

A range in a list - Produces the values in list 'c' starting at index 'a' and spanning 'b' elements
more than that.

####Summary

Build wet %gold gate with cell sample that accepts a cell of atoms, `a` and `b`, and a list `c`.
The last `a` elements in `c` are slammed to slag.
Scag is slammed with the atom `b`and the list just produced by slag.

####Examples:

        ~palryp-hocsyt/try=> (swag [0 5] (limo [1 2 3 4 5 6 7 8 9 10 ~]))
        [i=1 t=[i=2 t=[i=3 t=[i=4 t=[i=5 t=~]]]]]
        ---
        ~palryp-hocsyt/try=> (swag [3 5] (limo [1 2 3 4 5 6 7 8 9 10 ~]))
        [i=4 t=[i=5 t=[i=6 t=[i=7 t=[i=8 t=~]]]]]
        ---
        ~palryp-hocsyt/try=> (swag [1 2] (limo [1 2 3 ~]))
        [i=2 t=[i=3 t=~]] 

##++turn

Accepts a list `a` and a gate `b`. Produces the list with the gate applied to each element of the original list.

####Summary

        Activate jet.
        Build wet %gold gate with samples of list `a` and gate `b`.
        Kicks a dry %gold trap.
        If: `a` is an atom,
          Then: produce null.
        Else: produce the tuple with head (b i.a) and the toss of `a` for `t.a`.

####Examples

        ~dovryp-toblug/try=> (turn (limo [104 111 111 110 ~]) ,@t)
        <|h o o n|>

##++weld 

Concatenates two lists.

####Summary

        Activate jet.
        Build wet %gold gate that accepts two lists, `a` and `b`. 
        Homogenizes both lists and makes them the subject of the following code.
        Kick dry %gold trap.
        Cast the resulting list to the type of list `b`.
        If: a is null,
            Then: produce `b`. 
        Else: produce the tuple where `i.a` is the head, and the toss of `a` for `i.a` is the tail.

####Examples:

        ~palryp-hocsyt/try=> (weld (limo [1 2 3 ~]) (limo [4 5 6 ~]))
        ~[1 2 3 4 5 6]
        ~palryp-hocsyt/try=> (weld "foo" "bar")
        ~[~~f ~~o ~~o ~~b ~~a ~~r]

##++welp 

Concatenate two lists without losing their type information to homogenization.
Produces a tape when passed two tapes.

####Summary

        Identical to the internals of `++weld`, except it does not implement the list homogenization.
        You probably don't need to understand it right now.
        
####Examples:

        ~palryp-hocsyt/try=> (welp "foo" "bar")
        "foobar"

##++wild
 
Concatenate two lists without casting the product back to a list.

        Build wet %gold gate with two list sample, labeled `a`, `b`, respectively.
        Homogenize both lists and make them the subject of the following code.
        Build dry %gold gate.
        If `a` is null,
          Then: produce `b`.
        Else: produce the tuple where `i.a` is the head, and the toss of `a` for `i.a` is the tail.

####Examples:

        ~palryp-hocsyt/try=> =norm (limo [1 2 3 4 5 ~])
        ~palryp-hocsyt/try=> =norm2 (limo [6 7 8 ~])
        ~palryp-hocsyt/try=> (wild norm norm2)
        ~[1 2 3 4 5 6 7 8]
        ~palryp-hocsyt/try=> (wild "foo" "bar")
        ~[~~f ~~o ~~o ~~b ~~a ~~r]
        ~palryp-hocsyt/try=> (homo (weld "foo" "bar"))
        ~[~~f ~~o ~~o ~~b ~~a ~~r]
        ~palryp-hocsyt/try=> (homo (wild "foo" "bar"))
        ! -find-limb.t
        ! find-fork
        ! exit
##++zing 

Turns a list of lists into a single list by promoting the elements of each sublist into the higher.

####Summary

        Build wet %gold gate with a sample that accepts a list of lists.
        Casts the result to the type the homogenized list head, asserting that 'a' is at least a cell.
        A dry %gold trap is created and kicked.
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Else, weld together the head of a with the recrusive call of zing on the tail of a.

####Examples:

        ~palryp-hocsyt/try=> (zing (limo [(limo ['a' 'b' 'c' ~]) (limo ['e' 'f' 'g' ~]) (limo ['h' 'i' 'j' ~]) ~]))
        ~['a' 'b' 'c' 'e' 'f' 'g' 'h' 'i' 'j']
        ~palryp-hocsyt/try=> (zing (limo [(limo [1 'a' 2 'b' ~]) (limo [3 'c' 4 'd' ~]) ~]))
        ~[1 97 2 98 3 99 4 100]


