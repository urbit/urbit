section 2dB, maps
---

##++  ept       

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

---

##++  ja

The jar engine: A container arm for jar operation arms.  Jars are maps of lists.
The contained arms inherit the sample jar. 'a'.

        Build a wet %gold tray with a sample jar `a`...

---

##+-  get

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

---

##+-  add

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

---

      Terminate the core.

---

##++  ju

The jug engine: container arm for jug operation arms.  Jugs are maps of sets.
The contained arms inherit it's sample jug, 'a'.

        Build a wet %gold tray with a sample jug `a`.

##+-  del
        
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

---

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
      
---

##+-  has

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

---

##+-  put

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

---

##++  by

Container arm for map operation arms.  The contained arms inherit it's sample map, 'a'. 

####Summary

        Activate jet.
        Build a %gold tray with a sample which accepts a map.

---

##+-  all

Accept a gate which accepts any noun and produces a loobean.  Slams the gate with each member
of map 'a', produce the logical AND of the transformed map.

####Summary

        Activate jet.
        Build wet %gold gate with sample gate accepting any noun and producing a loobean, `b`.
        Kick dry %gold trap.  Yield bean.
        If: `a` is null,
            Then: Produce true,
        Else: Produce the logical AND of:
            `b` slammed with `q.n.a`
            The toss of `a` for `l.a`
            The toss of `a` for `r.a`
       
####Examples 

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~talsur-todres/try=> (~(all by b) |=(a=* ?@(a & |)))
        %.n
        ---
        ~tadbyl-hilbel/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
        ~tadbyl-hilbel/try=> (~(all by a) |=(a=@ (lte a 6)))
        %.y
        ~tadbyl-hilbel/try=> (~(all by a) |=(a=@ (lte a 4)))
        %.n

---

##+-  any

Accept a gate which accepts any noun and produces a loobean.  Slam the gate with each member
of map 'a' and produce the logical OR of the transformed map.

####Summary

        Activate jet.
        Kick dry %gold trap.  Yield bean.
        If: `a` is null,
            Then: Produce false,
        Else: Produce the logical OR of:
            `b` slammed with `q.n.a`
            The toss of `a` for `l.a`
            The toss of `a` for `r.a`

####Examples

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~talsur-todres/try=> (~(all by b) |=(a=* ?@(a & |)))
        %.y
        ---
        ~tadbyl-hilbel/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
        ~tadbyl-hilbel/try=> (~(any by a) |=(a=@ (lte a 4)))
        %.y

---

##+-  del

Accept a noun 'b', producing the map with the key-value pair of key 'b' removed.

####Summary 

        Activate jet.
        Build wet %gold gate with sample noun `b`.
        Kick dry %gold trap.  Cast the following to the type of `a`.
        If: `a` is null,
            Then: Produce null,
        Else:  Unless: `b` is `p.n.a`
            Then: If:  gor slammed with `b`, `p.n.a`
                Then: Produce the tuple [n.a $(a l.a) r.a]
            Else: Produce the tuple [n.a l.a $(a r.a)]
        Else: Kick dry %gold trap.  Yield either null or the type of `a`.
        If: `l.a` is null, Then: Produce `r.a`,
        Else: If: `r.a` is null, Then: Produce `l.a`,
        ELse: If:  vor slammed with `p.n.l.a` and `p.n.r.a`,
            Then:  Produce the tuple [n.l.a l.l.a $(l.a r.l.a)]
        Else: [n.r.a $(r.a l.r.a) r.r.a]

####Examples

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~talsur-todres/try=> (~(del by b) `a`)
        {[p=`b` q=[2 3]]}
        
---

##+-  dig

Accept any noun 'b' and produce the axis of 'b' in within the values of 'p.a' in map 'a'.

####Summary

        Creates a wet %gold gate which accepts a noun.
        Push `c` is 1.
        Kick dry %gold gate.  Yield atomic unit.
        If: `a` is null, Then: Produce null.
        Else:  If:  `b` is `p.n.a`, Then: Produce the unit with value: peg slammed with `c`, 2.
        Else:  If:  gor slammed with `b`, `p.n.a`.
            Then:  Produce the toss of `a` for `l.a`, `c` for peg slammed with `c`, 6.
        Else:  Produce the toss of `a` for `r.a`, `c` for peg slammed with `b, 7.

####Examples

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(dig by b) `b`)
        [~ 2]

---

##+-  gas

Accept any list 'b' of key-value pair cells and produce the map 'a'
with the members of 'b' added.

####Summary

        Activate jet.
        Build wet %gold gate with sample list of noun cells [p=* q=*], `b`.
        Use `b` replaced by the cast of `b` to the type of the map as subject.
        Kick dry %gold trap.  Cast the following to the type of the map `a`.
        If: `b` is null,
            Then: Produce `a`,
        Else: Produce the toss of `b` for the tail of `b`, `a` for the cell at the head of `b`
        inserted into the map as a `p`, `q` key, value pair.

####Examples

        ~talsur-todres/try=> =a (mo `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
        ~talsur-todres/try=> =b `(list ,[@t *])`[[`c` 3] [`d` 4] ~]
        ~talsur-todres/try=> (~(gas by a) b)
        {[p=`d` q=4] [p=`a` q=1] [p=`c` q=3] [p=`b` q=2]}

---

##+-  get

Produce the value in the map at key 'b'.

####Summary

        Activate jet.
        Build wet %gold gate with sample noun `b`.
        Kick dry %gold gate.  Yield a unit of the type of the map's values.
        If: `a` is null,
            Then: Produce null,
        Else:  If:  Is `b` equal to `p.n.a`?
            Then: Produce the unit of the value of the tree node, `q.n.a`
        Else:  If:  gor slammed with `b`, `p.n.a`
            Then:  Produce the toss of `a` for `l.a`,
        Else:  Produce the toss of `a` for `r.a`.

####Examples

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(get by b) `b`)
        [~ [2 3]]

---

##+-  got

####Summary

        Build wet %gold gate with sample noun `b`.
        Produce the slam of need with:
            The slam get with its sample replaced by `a` with `b`.
        
####Examples

        ~zod/try=> =m (mo `(list ,[@t *])`[['a' 1] ['b' 2] ~])
        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(get by m) 'a')
        [~ 1]
        ~zod/try=> (~(got by m) 'a')
        1
        ~zod/try=> (~(got by m) 'c')
        ! exit

---

##+-  has

Accept any noun 'b' and produces the loobean indicating whether the noun exists in map 'a'.

####Summary

        Activate jet.
        Build wet %gold gate with smaple noun `b`.
        Build dry %gold gate with null sample.
        Produce the slam of get with its sample replaced by `a` with `b`.

####Examples

        ~talsur-todres/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~talsur-todres/try=> (~(has by b) `b`)
        %.y
        ~talsur-todres/try=> (~(has by b) `c`)
        %.n

---

##+-  int

Produce the intersection of two maps of the same type.

####Summary

        Activate jet.
        Build wet %gold gate with sample map of the type of `a`, `b`.
        Kick dry %gold trap.   Cast the following to the type of `a`.
        If: `b` is null,
            Then: Produce null,
        Else:  If:  vor slammed with `p.n.a` and `p.n.b`,
            Then:  If:  `p.n.b` is `p.n.a`
                Then: Produce the tuple [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
            Else:  If:  hor slammed with `p.n.b` and `p.n.a`,
                Then:  Slam uni(+< $(a l.a, b [n.b l.b ~])) with $(b r.b)
            Else:  Slam uni(+< $(a r.a, b [n.b ~ r.b])) with $(b l.b)
        Else:  If:  `p.n.a` is `p.n.b`
            Then:  Produce [n.b $(b l.b, a l.a) $(b r.b, a r.b)]A
        Else:  If:  hor slammed with `p.n.a` p.n.b`
            Then:  Slam uni(+< $(b l.b, b [n.b l.b ~])) with $(a r.a)
        Else:  Slam uni(+< $(b r.b, b [n.b ~ r.b])) with $(a l.a)

        Where uni(+< $(b r.b, b [n.b ~ r.b])) is the call of uni with the map
        replaced by the toss of `b` for `r.b` and `b` for [n.b ~ r.b].

####Examples
  
        ~zod/try=> =n (mo `(list ,[@t *])`[['a' 1] ['c' 3] ~])
        ~zod/try=> n
        {[p='a' q=1] [p='c' q=3]}
        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(int by m) n)
        {[p='a' q=1]}
        ~zod/try=> =o (mo `(list ,[@t *])`[['c' 3] ['d' 4] ~])
        ~zod/try=> (~(int by m) o)
        {}
       
---

##+-  mar

Accept a noun and a unit of a noun of the type of the map's keys and values, respectively. 
Validate that the value is not null and put the pair in the map. If the value is null, 
delete the key.

####Summary

        Build wet %gold gate with sample noun of the type of the map's keys, `b`,
        the unit of the type of the map's values `c`.
        If:  `c`,
            Then:  Delete `b` from the map.
        Else:  Put key `b` in the map with the value of the unit `c`, `u.c`.

####Examples

        XXX This arm appears to be broken.
        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(mar by m) 'c' (some 3))
        ! -find-limb.n
        ! find-none
        ! exit
        ~zod/try=> (~(mar by m) 'c' ~)
        ! -find-limb.n
        ! find-none
        ! exit
        ~zod/try=> (~(mar by m) 'b' ~)
        ! -find-limb.n
        ! find-none
        ! exit

---

##+-  put

Add a key-value pair to the map.

####Summary

        Activate jet.
        Build a wet %gold gate with sample noun `b`, noun `c`
        
        Creates and kicks a dry %gold trap.  Casts the result to the type of the map 'a'.
        If "a is an atom", produce the cell [[b c] ~ ~].
        Else, build the if-then-else statement if

####Examples

        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(put by m) 'c' 3)
        {[p='a' q=1] [p='c' q=3] [p='b' q=2]}
        ~zod/try=> (~(put by m) "zod" 26)
        ! type-fail
        ! exit
        ~zod/try=> (~(put by m) 'a' 2)
        {[p='a' q=2] [p='b' q=2]}

---

##+-  rep

Walk through the map, replacing 'b' with the product of (c n.a b).  Produce the resulting
map.

####Summary

        Build wet %gold gate with sample noun `b`, gate `c`
        Kick dry %gold trap.
        If:  `a` is null, Then:  Produce `b`,
        Else:  Produce the toss of `a` for `r.a`, `b` for the toss of `a` for `l.a` and `b`
        for the slam of `c` with `n.a` and `b`.

####Examples

---

        
##+-  rib

Walk throught the map, replacing the values n.a with the product of (c n.a b) and produce
the transformed map with the accumulated. `b`.

####Summary

        Build wet %gold gate with sample noun `b`, gate `c`.
        Kick dry %gold trap.  Cast the following to the type of the tuple [b a].
        If:  `a` is null, Then:  Produce the tuple [b ~],
        Else:  Push `d` is the slam of `c` with `n.a` and `b`.
        Set `n.a` to `+.d`, the tail of `d`, in the subject.
        Push `e` is the toss of `a` for `l.a`, `b` for `-.d`, the head of `d`.
        Push `f` is the toss of `a` for `r.a`, `b` for `-.e`, the head of `e`.
        Produce the tuple [-.f [n.a +.e +.f]], that is, the cell of
        the head of accumulator `f` and the head of tree [n.a +.e +.f], with
        left and right sides of the tails of `e` nad `f`, respectively.
 

####Examples


---
        
##+-  run

####Summary

####Examples

---

##+-  tap

####Summary

####Examples

---

##+-  uni

Produce the union between two maps.

####Summary

        Acitvate jet.
        Build wey %gold gate with sample map of the type of `a`, `b`.
        Kick dry %gold gate.  Cast the following to the type of `a`.
        If:  `b` is null,
            Then:  Produce `a`,
        Else: If:  `a` is null,
            Then:  Produce `b`,
        Else:  If:  vor slammed with `p.n.a` and `p.n.b`
            Then:  If:  `p.n.b` is `p.n.a`
                Then:  Produce the tuple [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
            Else:  If:  hor slammed with `p.n.b` and `p.n.b`
                Then:  Produce the toss of `a` for the tree map root:
                    [n.a $(a l.a, b [n.b l.b ~]) r.a],
                        `b` for `r.b`.
            Else:  Produce the toss of `a` for the tree map root:
                    [n.a l.a $(a r.a, b [n.b ~ r.b])],
                        `b` for `l.b`.
        Else:  If:  `p.n.a` is `p.n.b`,
            Then:  Produce the tree map root:
                [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
        Else:  If:  hor slammed `p.n.a` and `p.n.b`,
            Then:  Produce the toss of `b` for:
                The tree map root of [n.b $(b l.b, a [n.a l.a ~]) r.b],
                `a` for `r.a`
        Else: Produce the toss of `b` for:
               The tree map root of [n.b l.b $(b r.b, a [n.a ~ r.a])],
                `a` for `l.a`

####Examples

        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> o
        {[p='d' q=4] [p='c' q=3]}
        ~zod/try=> (~(uni by m) o)
        {[p='d' q=4] [p='a' q=1] [p='c' q=3] [p='b' q=2]}
        ~zod/try=> (~(uni by m) ~)
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> n
        {[p='a' q=1] [p='c' q=3]}
        ~zod/try=> (~(uni by o) n)
        {[p='d' q=4] [p='a' q=1] [p='c' q=3]}

---

##+-  urn

Turn over the values of the map and produce the tranformed map.

####Summary

        Build wet %gold gate with sample gate accepting two nouns and producing a noun, `b`.
        Kick dry %gold trap.
        If:  `a` is null,
            Then:  Produce null,
        Else:  Produce the tuple [n=[p=p.n.a q=(b p.n.a q.n.a)] l=$(a l.a) r=$(a r.a)]
        The root of the tree map with the value:A
            The slam of `b` of with `p.n.a` and `q.n.a`
        The left and right trees are the toss of `a` for their respective maps.

####Examples

        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(urn by m) |=(a=[p=* q=*] q.a))
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(urn by m) |=(a=[p=* q=*] 7))
        {[p='a' q=7] [p='b' q=7]}
        ~zod/try=> (~(urn by m) |=(a=[p=* q=*] p.a))
        {[p='a' q=97] [p='b' q=98]}

---

##+-  wyt

Produce the depth of the tree map.

####Summary

        Increment the following.
        Kick dry %gold trap.  Yield an atom.
        If:  `a` is null,
            Then:  Produce 0,
        Else:  Produce the increment of the sum of:
            The toss of `a` for `l.a`,
            the toss of `a` for `r.a`. 

####Examples

        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> o
        {[p='d' q=4] [p='c' q=3]}
        ~zod/try=> ~(wyt by m)
        3
        ~zod/try=> ~(wyt by o)
        3
        ~zod/try=> ~(wyt by (~(uni by m) o))
        5

---


