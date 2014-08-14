section 2dC, queues                   

##++  to

Container arm for queue operation arms.  The contained arms inherit it's sample queue, 'a'. 

####Summary

        Build wet %gold tray with sample 'a' of type 'qeu'.

##+-  bal

Walks through the queue using vor (v-order check) on all eleements.

####Summary

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

####Examples

        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[6 1 3 6 1 3 4 6 ~])
        ~palryp-hocsyt/try=> a
        {6 4 3 1 6 3 1 6}
        ~palryp-hocsyt/try=> ~(bal to a)
        {6 4 3 1 6 3 1 6}
        
---

##+-  dep

Produce the maximum depth of leaves (r.a and l.a) in the queue 'a'.

####Summary

        Creates and kicks a dry %gold trap.  Casts the result to an atom.
        If "a is null", produce 0.
        Else, increment the maximum of the recursive calls of the 'dep' to the left and right leaves of 'a',
        $(a l.a) and $(a r.a).

####Examples

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

---

##+-  gas

Accept a list `b` of elements of the type of the queue `a` elements and produce the queue
`a` with the elements of `b` added.

####Summary

        Creates a dry %gold gate which accepts a list of the elements of the queue.
        Creates and kicks a dry %gold gate.  Casts the result to the type of 'a', the queue.
        If "b is null", produce 'a'.
        Else, Produce the recursive call to the trap with 'b' replaced by the tail of 'b' and 'a' replaced by the 
        result of putting the head of 'b' into 'a'.

####Examples

        ~palryp-hocsyt/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
        {3 2 1}
        ---
        ~palryp-hocsyt/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
        ~palryp-hocsyt/try=> =b `(list ,@)`[4 5 6 ~]
        ~palryp-hocsyt/try=> (~(gas to a) b)
        {6 5 4 3 2 1}

---

##+-  get

Produces the queue 'a' in the format [p=head q=tail].

####Summary

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

####Examples

---

##+-  nap

Remove the head of a queue and produce the resulting queue.

####Summary

        Assert that 'a' is a cell.
        Builds an if-then-else statement on "l.a is null".  If so, produce r.a.
        Else, let 'b' be the result of getting the [p=head q=tail] pair from 'l.a'.
        Produce the queue v-order of bal(+< ^+(a [p.b q.b r.a])).

####Examples

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

---

##+-  put

Accept any noun and adds to the queue as the head, producing the resutling queue.

####Summary

        Creates a wet %gold gate which accepts any noun.
        Creates and kicks a dry %gold trap.  Casts the result to the type of the queue 'a'.
        If "a is null", produce [b ~ ~].
        Else, produce bal(+< a(l $(a l.a))).
        
####Examples

        ~dovryp-toblug/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
        ~dovryp-toblug/try=> (~(put to a) 7)
        {7 6 5 4 2 1 3}

---

  +-  tap

Concatenates two lists from the first

####Summary

        Creates a dry %gold gate which accepts a list of elements of the type of the queue's elements.
        Casts the result to the type of 'b', the list.
        If "a is null", produce 'b'.
        Else, produce the recursive call to the gate with 'a' replaced by 'r.a' and 'b' replaced by [n.a $(a l.a)],
        where $(a l.a) is the recursive call to the trap with 'a' replaced by 'l.a'.

#### Examples

        ~dovryp-toblug/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
        ~dovryp-toblug/try=> (~(tap to a) `(list ,@)`[99 100 101 ~])
        ~[3 1 2 4 5 6 99 100 101]

---

##+-  top

####Summary
 
        Creates and kicks a dry %gold trap.  Casts the result to a unit of the type of the queue's element.
        If "a is null", produce null.
        Else, if "the right leaf of 'a' is null", produce [~ n.a].
        Else, produce $(a r.a), the recursive call to the trap with 'a' replaced by 'r.a'.

####Examples

        ~talsur-todres/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 ~])
        ~talsur-todres/try=> ~(top to a)
        [~ 1]

---


