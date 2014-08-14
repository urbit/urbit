section 2dA, sets     
---
                
##++  apt

Accept any tree and produce a loobean indicating whether the tree is a set.

####Summary

        Creates a dry %gold gate which accepts a tree.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, compute and produce the logical AND of:
        The if "l.a is an atom" then produce true, else (produce the logical AND of the
        v-order of n.a and n.l.a and the h-order of n.l.a and n.a) if-then-else statement.
        The if "r.a is an atom" then produce true, else (produce the logical AND of the 
        v-order of n.a and n.r.a and the h-order of n.a and n.r.a) if-then-else statement.
        (==)  terminates the tall logical AND statement.

####Examples 

        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (apt b)
        %.y
        ---
        ~tadbyl-hilbel/try=> =m (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ['c' 4] ['d' 5] ~])
        ~tadbyl-hilbel/try=> m
        {[p='d' q=5] [p='a' q=1] [p='c' q=4] [p='b' q=[2 3]]}
        ~tadbyl-hilbel/try=> (apt m)
        %.y

---

##++  in

        Container arm for set operation arms.  The contained arms inherit it's sample set, 'a'. 

####Summary

        Activate jet.
        Creates a %gold trap with sample 'a', a set.
---

##+-  all

Accept a gate which accepts any noun and produce a loobean.  Slam the gate with each member
of set 'a', produce the logical AND of the transformed set.

####Summary

        Activate jet.
        Creates a wet %gold gate which accepts any gate which produces a loobean.
        Creates and kicks a dry %gold gate, casts the result to a loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce true.
        Else, produce the logical AND of (b n.a) and the recursive calls of the trap with
        'a' replaced by 'l.a' and 'a' replaced by 'r.a'.

####Examples

        ~dovryp-toblug/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~dovryp-toblug/try=> (~(all in b) |=(a=* ?@(-.a & |)))
        %.n
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (~(all in b) |=(a=@t (gte a 100)))
        %.y

---

##+-  any

Accept a gate which accepts any noun and produce a loobean.  Slam the gate with each member
of set 'a', produce the logical OR of the transformed set.

####Summary

        Activate jet.
        Creates a wet %gold gate which accepts any gate which produces a loobean.
        Creates and kicks a dry %gold gate, casts the result to a loobean.
        Builds an if-then-else statement on "a is an atom."
        If so, produce false.
        Else, produce the logical OR of (b n.a) and the recursive calls of the trap with
        'a' replaced by 'l.a' and 'a' replaced by 'r.a'.

####Examples:

        ~dovryp-toblug/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~dovryp-toblug/try=> (~(any in b) |=(a=* ?@(+.a & |)))
        %.y
        ~tadbyl-hilbel/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
        ~tadbyl-hilbel/try=> (~(any in b) |=(a=@t (lte a 100)))
        %.n

---

##+-  del

Accept any noun 'b' and removes it from the set 'a'.

####Summary

        Activate jet.
        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold gate, casts the result to the type of 'a'.
        Builds an if-then-else statement on "a is null."
        If so, produce null.
        Else, builds an unless-then-else on (b=n.a)
        If so, build an if-then-else statement by testing the h-order of 'b' and 'n.a'.
        If so, produce a the cell [n.a $(a l.a) r.a], where $(a l.a) is the recursive call of 
        the trap with 'a' replaced by the left 

####Examples

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

---

##+-  dig

Produce the axis of the noun `b` within the set `a`.

####Summary

        Creates a dry %gold gate which accepts a single noun.
        Let 'c' be 1.
        Creates and kicks a dry %gold trap.  Casts the result to an atomic unit.
        Builds an if-then-else statement on "a is null."  If so, produce null.
        Else, build an if-then-else statement on (b=n.a).  If so, produce the unit [~ u=(peg c 2)].
        Else, build an if-then-else statement on the g-order of 'b' and 'n.a'
        If so, produce the recursive call of the trap with 'a'
        replaced by 'l.a' and 'c' replaced by (peg c 6).
        Else, produce the recursive call of the trap with 'a'
        replaced by 'r.a' and 'c' replaced by (peg c 7).

####Examples

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

---

##+-  gas

Accept a list 'b' with members of the same type as the set 'a' and produce
the union set of 'a' and 'b'.

####Summary

        Activate jet.
        Creates a dry %gold gate which accepts a list of elements of the same type as 'a'.
        Creates and kicks a dry %gold trap whose result is cast to the type of 'a'.
        Builds an if-then-else statement on "b is an atom."
        If so, produce 'a'.
        Else, recursively call the trap with 'b' replaced by the tail of 'b' and the head of 'b'
        put into 'a'.

####Examples

        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'rudolf' 'john'}
        ~tadbyl-hilbel/try=> (~(gas in b) `(list ,@t)`['14' 'things' 'number' '1.337' ~])
        {'1.337' '14' 'number' 'things' 'bonita' 'madeleine' 'rudolf' 'john'}
        ---
        ~tadbyl-hilbel/try=> (~(gas in s) `(list ,@t)`['1' '2' '3' ~])
        {'1' '3' '2' 'e' 'd' 'a' 'c' 'b'}

---

##+-  has

Accepts any noun and produces the loobean indicating whether or not that value (n.a) exists in 'a'.

####Summary

        Activate jet.
        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold trap.  Casts the result to a loobean.
        Builds an if-then-else statement on "The set (a) is an atom."  If so, produce false.
        Else, build an if-then-else statement on (b=n.a).
        If so, produce true.
        Else, build an if-then-else statement on the h-order of 'b' and 'n.a'
        If so, produce the recursive call to the trap with 'a' replaced by 'l.a'
        If so, produce the recursive call to the trap with 'a' replaced by 'r.a'

####Examples

        ~dovryp-toblug/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~dovryp-toblug/try=> (~(has in a) `a`)
        %.y
        ~dovryp-toblug/try=> (~(has in a) 'z')
        %.n

---

##+-  put

Accept any noun 'b' and produce the set 'a' with 'b' added to its sorted location.

####Summary

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

####Examples

        ~talsur-todres/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~talsur-todres/try=> =b (~(put in a) `d`)
        ~talsur-todres/try=> b
        {`d` `a` `c` `b`}
        ~talsur-todres/try=> -.l.+.b
        n=`d`

---

##+-  rep

Accept a noun and a binary gate.  Produce the 'a' with each member 'n.a' replaced by (c n.a b).

####Summary

        XXX
        Creates a wet %gold gate which accpets a noun and a tile, 'a' and 'b'.
        Creates and kicks a dry %gold gate.
        Builds an if-then-else statement on "a is null."  If so, produce 'b'.
        Else, recursively call the trap with 'a' replaced by 'r.a' and 
        'b' replaced by the recursive call of the trap with 'a' replaced by 'l.a' and 'b' replaced by
        (c n.a b).

####Examples

        ~talsur-todres/try=> =a (~(gas in *(set ,@)) [1 2 3 ~])
        ~talsur-todres/try=> a
        {1 3 2}
        ~talsur-todres/try=> (~(rep in a) 0 |=([a=@ b=@] (add a b)))
        6

---

##+-  tap

Accept a list of elements of the set and produce a cell of the set with the list concatenated.

####Summary

        Activate jet.
        Build dry %gold gate with sample list of the same
        Cast the following to the type of `b`
        If: `a` is null,
            Then: Produce `b`,
        Else: Produce the toss of `a` for `r.a`, `b` for [n.a $(a l.a)]),
            where $(a l.a) is the toss of `a` for the left twig of `a`.

####Examples

        ~tadbyl-hilbel/try=> =s (sa `(list ,@t)`['a' 'b' 'c' 'd' 'e' ~])
        ~tadbyl-hilbel/try=> s
        {'e' 'd' 'a' 'c' 'b'}
        ~tadbyl-hilbel/try=> (~(tap in s) `(list ,@t)`['1' '2' '3' ~])
        ~['b' 'c' 'a' 'd' 'e' '1' '2' '3']
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~tadbyl-hilbel/try=> (~(tap in b) `(list ,@t)`['david' 'people' ~])
        ~['john' 'daniel' 'madeleine' 'bonita' 'david' 'people']

---

##+-  wyt

Produce the cardinality (number of elements) of the set.

####Summary

        Increment the following.
        Kick dry %gold trap.  Yield atom.
        If:  `a` is null,
            Then: Produce 0.
        Else: Produce the increment of the sum of:
            The toss of `a` for `l.a`, the left twig of `a`.
            The toss of `a` for `r.a`, the right twig of `a`.

####Examples

        ~talsur-todres/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
        ~talsur-todres/try=> ~(wyt in a)
        4
        ~tadbyl-hilbel/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~tadbyl-hilbel/try=> ~(wyt in b)
        5

---
