section 2dB, maps
=================

### `++ept`

Map invariant.

    ++  ept                                                 ::  map invariant
      |=  a=(tree ,[p=* q=*])
      ?~  a
        &
      ?&  ?~(l.a & ?&((vor p.n.a p.n.l.a) (hor p.n.l.a p.n.a)))
          ?~(r.a & ?&((vor p.n.a p.n.r.a) (hor p.n.a p.n.r.a)))
      ==

Computes whether `a` is a [map](), producing a loobean.

`a` is a [tree]().

        ~zod/try=> m
        {[p='d' q=5] [p='a' q=1] [p='c' q=4] [p='b' q=[2 3]]}
        ~zod/try=> (ept m)
        %.y
        ~zod/try=> b
        {'bonita' 'madeleine' 'daniel' 'john'}
        ~zod/try=> (ept b)
        ! type-fail
        ! exit

------------------------------------------------------------------------

### `++ja`

Jar engine

    ++  ja                                                  ::  jar engine
      |/  a=(jar)

A container arm for `++jar` operation arms. A `++jar` is a `++map` of
`++list`s. The contained arms inherit the [sample]() jar.

`a` is a [jar]().

    ~zod/try=> ~(. ja (mo (limo a/"ho" b/"he" ~)))
    <2.dgz [nlr([p={%a %b} q=""]) <414.fvk 101.jzo 1.ypj %164>]>

------------------------------------------------------------------------

### `+-get:ja`

Grab value by key

      +-  get                                               ::  grab value by key
        |*  b=*
        =+  c=(~(get by a) b)
        ?~(c ~ u.c)

Produces a list retrieved from jar `a` using the key `b`.

`a` is a [`++jar`](/doc/hoon/library/1#++jar).

`b` is a key of the same type as the keys in `a`.

    ~zod/try=> =l (mo `(list ,[@t (list ,@)])`[['a' `(list ,@)`[1 2 3 ~]] ['b' `(list ,@)`[4 5 6 ~]] ~])
    ~zod/try=> l
    {[p='a' q=~[1 2 3]] [p='b' q=~[4 5 6]]}
    ~zod/try=> (~(get ja l) 'a')
    ~[1 2 3]
    ~zod/try=> (~(get ja l) 'b')
    ~[4 5 6]
    ~zod/try=> (~(get ja l) 'c')
    ~

------------------------------------------------------------------------

### `+-add:ja`

Prepend to list

      +-  add                                               ::  adds key-list pair
        |*  [b=* c=*]
        =+  d=(get(+< a) b)
        (~(put by a) b [c d])

Produces jar `a` with value `c` prepended to the list located at key
`b`.

`a` is a [jar]().

`b` is a key of the same type as the keys in `a`.

`c` is a value of the same type as the values in `a`.

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

------------------------------------------------------------------------

### `++ju`

Jug operations

    ++  ju                                                  ::  jug engine
      |/  a=(jug)

Container arm for jug operation arms. A `++jug` is a `++map` of
`++sets`. The contained arms inherit its [sample]() jug, `a`.

`a` is a [jug]().

    ~zod/try=> ~(. ju (mo (limo a/(sa "ho") b/(sa "he") ~)))
    <2.dgz [nlr([p={%a %b} q={nlr(^$1{@tD $1}) nlr(^$3{@tD $3})}]) <414.fvk 101.jzo 1.ypj %164>]>

### `+-del:ju`

Remove

      +-  del                                               ::  delete at key b
        |*  [b=* c=*]
        ^+  a
        =+  d=(get(+< a) b)
        =+  e=(~(del in d) c)
        ?~  e
          (~(del by a) b)
        (~(put by a) b e)

Produces jug `a` with value `c` removed from set located at key `b`.

`a` is a [jug]().

`b` is a key of the same type as the keys in `a`.

`c` is the value of the same type of the keys in `a` that is to be
removed.

    ~zod/try=> s
    {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(del ju s) 'a' 1)
    {[p='a' q={3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(del ju s) 'c' 7)
    {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}        

------------------------------------------------------------------------

### `+-get:ju`

Retrieve set

      +-  get                                               ::  gets set by key
        |*  b=*
        =+  c=(~(get by a) b)
        ?~(c ~ u.c)

Produces a set retrieved from jar `a` using key `b`.

`a` is a [jar]().

`b` is a key of the same type as the keys in `a`.

    ~zod/try=> s
    {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(get ju s) 'a')
    {1 3 2}
    ~zod/try=> (~(get ju s) 'b')
    {5 4 6}
    ~zod/try=> (~(get ju s) 'c')
    ~

------------------------------------------------------------------------

### `+-has:ju`

Check contents

      +-  has                                               ::  existence check
        |*  [b=* c=*]
        ^-  ?
        (~(has in (get(+< a) b)) c)

Computes whether a value `c` exists within the set located at key `b`
with jar `a`. Produces a loobean.

`a` is a [set]().

`b` is a key as a [noun]().

`c` is a value as a [noun]().

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

------------------------------------------------------------------------

### `+-put:ju`

Add key-set pair

      +-  put                                               ::  adds key-element pair
        |*  [b=* c=*]
        ^+  a
        =+  d=(get(+< a) b)
        (~(put by a) b (~(put in d) c))

Produces jar `a` with `c` added to the set value located at key `b`.

`a` is a [set]().

`b` is a key as a [noun]().

`c` is a [value]().

    ~zod/try=> s
    {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(put ju s) 'a' 7)
    {[p='a' q={7 1 3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(put ju s) 'a' 1)
    {[p='a' q={1 3 2}] [p='b' q={5 4 6}]}
    ~zod/try=> (~(put ju s) 'c' 7)
    {[p='a' q={1 3 2}] [p='c' q={7}] [p='b' q={5 4 6}]}

------------------------------------------------------------------------

### `++by`

Map operations

    ++  by                                                  ::  map engine
      ~/  %by
      |/  a=(map)

Container arm for map operation arms. A map is a set of key, value
pairs. The contained arms inherit it's [sample]() [map](), `a`.

`a` is a [map]().

    ~zod/try=> ~(. by (mo (limo [%a 1] [%b 2] ~)))
    <19.irb [nlr([p={%a %b} q=@ud]) <414.rvm 101.jzo 1.ypj %164>]>

------------------------------------------------------------------------

### `+-all:by`

Logical AND

      +-  all                                               ::  logical AND
        ~/  %all
        |*  b=$+(* ?)
        |-  ^-  ?
        ?~  a
          &
        ?&((b q.n.a) $(a l.a) $(a r.a))

Computes the logical AND on the results of slamming every element in map
`a` with gate `b`. Produces a loobean.

`a` is a [map]().

`b` is a [wet gate]().

    ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
    ~zod/try=> (~(all by b) |=(a=* ?@(a & |)))
    %.n
    ~zod/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
    ~zod/try=> (~(all by a) |=(a=@ (lte a 6)))
    %.y
    ~zod/try=> (~(all by a) |=(a=@ (lte a 4)))
    %.n

------------------------------------------------------------------------

### `+-any:by`

Logical OR

      +-  any                                               ::  logical OR
        ~/  %any
        |*  b=$+(* ?)
        |-  ^-  ?
        ?~  a
          |
        ?|((b q.n.a) $(a l.a) $(a r.a))

Computes the logical OR on the results of slamming every element with
gate `b`. Produces a loobean.

`a` is a [map]().

`b` is a [wet gate]().

    ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
    ~zod/try=> (~(all by b) |=(a=* ?@(a & |)))
    %.y
    ~zod/try=> =a (mo `(list ,[@t @u])`[['a' 1] ['b' 2] ['c' 3] ['d' 4] ['e' 5] ~])
    ~zod/try=> (~(any by a) |=(a=@ (lte a 4)))
    %.y

------------------------------------------------------------------------

### `+-del:by`

Delete

      +-  del                                               ::  delete at key b
        ~/  %del
        |*  b=*
        |-  ^+  a
        ?~  a
          ~
        ?.  =(b p.n.a)
          ?:  (gor b p.n.a)
            [n.a $(a l.a) r.a]
          [n.a l.a $(a r.a)]
        |-  ^-  ?(~ _a)
        ?~  l.a  r.a
        ?~  r.a  l.a
        ?:  (vor p.n.l.a p.n.r.a)
          [n.l.a l.l.a $(l.a r.l.a)]
        [n.r.a $(r.a l.r.a) r.r.a]

Produces map `a` with the element located at key `b` removed.

`a` is a [map]().

`b` is a key as a [noun]().

        ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
        ~zod/try=> (~(del by b) `a`)
        {[p=`b` q=[2 3]]}

------------------------------------------------------------------------

### `+-dig:by`

Axis of key

      +-  dig                                               ::  axis of key
        |=  b=*
        =+  c=1
        |-  ^-  (unit ,@)
        ?~  a  ~
        ?:  =(b p.n.a)  [~ u=(peg c 2)]
        ?:  (gor b p.n.a)
          $(a l.a, c (peg c 6))
        $(a r.a, c (peg c 7))

Produce the axis of key `b` within map `a`.

`a` is a [map]().

`b` is a key as a [noun]().

        ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~zod/try=> (~(dig by b) `b`)
        [~ 2]

------------------------------------------------------------------------

### `+-gas:by`

Concatenate

      +-  gas                                               ::  concatenate
        ~/  %gas
        |*  b=(list ,[p=* q=*])
        =>  .(b `(list ,_?>(?=(^ a) n.a))`b)
        |-  ^+  a
        ?~  b
          a
        $(b t.b, a (put(+< a) p.i.b q.i.b))

Insert a list of key-value pairs `b` into map `a`.

`a` is a [map]().

`b` is a [list]() of [cells]() of key-value nouns `p` and `q`.

    ~zod/try=> =a (mo `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
    ~zod/try=> =b `(list ,[@t *])`[[`c` 3] [`d` 4] ~]
    ~zod/try=> (~(gas by a) b)
    {[p=`d` q=4] [p=`a` q=1] [p=`c` q=3] [p=`b` q=2]}

------------------------------------------------------------------------

### `+-get:by`

Grab unit value

      +-  get                                               ::  unit value by key
        ~/  %get
        |*  b=*
        |-  ^-  ?(~ [~ u=_?>(?=(^ a) q.n.a)])
        ?~  a
          ~
        ?:  =(b p.n.a)
          [~ u=q.n.a]
        ?:  (gor b p.n.a)
          $(a l.a)
        $(a r.a)

Produce the unit value of the value located at key `b` within map `a`.

`a` is a [map]()

`b` is a [key]() as a [noun]()

        ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~zod/try=> (~(get by b) `b`)
        [~ [2 3]]

------------------------------------------------------------------------

### `+-got:by`

Assert

      +-  got
        |*  b=*
        %-  need
        %-  get(+< a)  b

Produce the value located at key `b` within map `a`. Crash if key `b`
does not exist.

`a` is a [map]().

`b` is a [key]().

        ~zod/try=> =m (mo `(list ,[@t *])`[['a' 1] ['b' 2] ~])
        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(get by m) 'a')
        [~ 1]
        ~zod/try=> (~(got by m) 'a')
        1
        ~zod/try=> (~(got by m) 'c')
        ! exit

------------------------------------------------------------------------

### `+-has:by`

Key existence check

      +-  has                                               ::  key existence check
        ~/  %has
        |*  b=*
        !=(~ (get(+< a) b))

Checks whether map `a` contains an element with key `b`, producing a
loobean.

`a` is a [map]().

`b` is a key as a [noun]().

        ~zod/try=> =b (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])  
        ~zod/try=> (~(has by b) `b`)
        %.y
        ~zod/try=> (~(has by b) `c`)
        %.n

------------------------------------------------------------------------

### `+-int:by`

Intersection

      +-  int                                               ::  intersection
        ~/  %int
        |*  b=_a
        |-  ^+  a
        ?~  b
          ~
        ?~  a
          ~
        ?:  (vor p.n.a p.n.b)
          ?:  =(p.n.b p.n.a)
            [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
          ?:  (hor p.n.b p.n.a)
            %-  uni(+< $(a l.a, b [n.b l.b ~]))  $(b r.b)
          %-  uni(+< $(a r.a, b [n.b ~ r.b]))  $(b l.b)
        ?:  =(p.n.a p.n.b)
          [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
        ?:  (hor p.n.a p.n.b)
          %-  uni(+< $(b l.b, a [n.a l.a ~]))  $(a r.a)
        %-  uni(+< $(b r.b, a [n.a ~ r.a]))  $(a l.a)

Produces a map of the (key) intersection between two maps of the same
type, `a` and `b`. If both maps have an identical key that point to
different values, the element from map `b` is used.

`a` is a [map]().

`b` is a [map]().

        ~zod/try=> =n (mo `(list ,[@t *])`[['a' 1] ['c' 3] ~])
        ~zod/try=> n
        {[p='a' q=1] [p='c' q=3]}
        ~zod/try=> m
        {[p='a' q=1] [p='b' q=2]}
        ~zod/try=> (~(int by m) n)
        {[p='a' q=1]}
        ~ravpel-holber/try=> =p (mo `(list ,[@t *])`[['a' 2] ['b' 2] ~])
        ~zod/try=> p
        {[p='a' q=2] [p='b' q=2]}
        ~zod/try=> (~(int by p) n)
        {[p='a' q=2]}
        ~zod/try=> =q (mo `(list ,[@t *])`[['a' 2] ['c' 2] ~])
        ~zod/try=> q
        {[p='a' q=2] [p='b' q=2]}
        ~zod/try=> (~(int by p) q)
        {[p='a' q=2] [p='b' q=2]}
        ~zod/try=> =o (mo `(list ,[@t *])`[['c' 3] ['d' 4] ~])
        ~zod/try=> (~(int by m) o)
        {}
       

------------------------------------------------------------------------

### `+-mar:by`

Assert and Add

      +-  mar                                               ::  add with validation
        |*  [b=_?>(?=(^ a) p.n.a) c=(unit ,_?>(?=(^ a) q.n.a))]
        ?~  c
          (del b)
        (put b u.c)

Produces map `a` with the addition of a key-value pair, where the value
is a nonempty unit.

Accept a noun and a unit of a noun of the type of the map's keys and
values, respectively. Validate that the value is not null and put the
pair in the map. If the value is null, delete the key.

XX This arm is broken, asana task 15186618346453

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

------------------------------------------------------------------------

### `+-put:by`

Add key-value pair

      +-  put                                               ::  adds key-value pair
        ~/  %put
        |*  [b=* c=*]
        |-  ^+  a
        ?~  a
          [[b c] ~ ~]
        ?:  =(b p.n.a)
          ?:  =(c q.n.a)
            a
          [[b c] l.a r.a]
        ?:  (gor b p.n.a)
          =+  d=$(a l.a)
          ?>  ?=(^ d)
          ?:  (vor p.n.a p.n.d)
            [n.a d r.a]
          [n.d l.d [n.a r.d r.a]]
        =+  d=$(a r.a)
        ?>  ?=(^ d)
        ?:  (vor p.n.a p.n.d)
          [n.a l.a d]
        [n.d [n.a l.a l.d] r.d]

Produces `a` with the addition of the key-value pair of `b` and `c`.

`a` is a [map]().

`b` is a key of the same type as the keys in `a`.

`c` is a value of the same type of the values in `a`.

    ~zod/try=> m
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> (~(put by m) 'c' 3)
    {[p='a' q=1] [p='c' q=3] [p='b' q=2]}
    ~zod/try=> (~(put by m) "zod" 26)
    ! type-fail
    ! exit
    ~zod/try=> (~(put by m) 'a' 2)
    {[p='a' q=2] [p='b' q=2]}

------------------------------------------------------------------------

### `+-rep:by`

      +-  rep                                               ::  replace by product
        |*  [b=* c=_,*]
        |-
        ?~  a  b
        $(a r.a, b $(a l.a, b (c q.n.a b)))

Accumulate using gate from values in map

XX interface changing.

------------------------------------------------------------------------

### `+-rib:by`

      +-  rib                                               ::  transform + product
        |*  [b=* c=_,*]
        |-  ^+  [b a]
        ?~  a  [b ~]
        =+  d=(c n.a b)
        =.  n.a  +.d
        =+  e=$(a l.a, b -.d)
        =+  f=$(a r.a, b -.e)
        [-.f [n.a +.e +.f]]

Replace values with accumulator

XX interface changing, possibly disappearing

------------------------------------------------------------------------

### `+-run:by`

Transform values

      +-  run                                               ::  turns to tuples
        |*  b=_,*
        |-  
        ?~  a  a
        a(n (b q.n.a), l $(a l.a), r $(a r.a))

Iterates over every value in set `a` using gate `b`. Produces a map.

`a` is a [map]().

`b` is a [wet gate]().

    ~zod/try=> m
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> ^+(m (~(run by m) dec))
    {[p='a' q=0] [p='b' q=1]}
    ~zod/try=> `(map ,@tas ,@t)`(~(run by m) (cury scot %ux))
    {[p=%a q='0x1'] [p=%b q='0x2']}

------------------------------------------------------------------------

### `+-tap:by`

Listify pairs

      +-  tap                                               ::  listify pairs
        ~/  %tap
        |=  b=(list ,_?>(?=(^ a) n.a))
        ^+  b
        ?~  a
          b
        $(a r.a, b [n.a $(a l.a)])

Produces the list of all elements in map `a` that is prepended to list
`b`, which is empty by default.

`a` is a [map]().

`b` is a [list]().

    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> `*`m
    [[98 2] [[97 1] 0 0] 0]
    ~zod/try=> (~(tap by m))
    ~[[p='b' q=2] [p='a' q=1]]
    ~zod/try=> `*`(~(tap by m))
    [[98 2] [97 1] 0]

------------------------------------------------------------------------

### `+-uni:by`

Union

      +-  uni                                               ::  union, merge
        ~/  %uni
        |*  b=_a
        |-  ^+  a
        ?~  b
          a
        ?~  a
          b
        ?:  (vor p.n.a p.n.b)
          ?:  =(p.n.b p.n.a)
            [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
          ?:  (hor p.n.b p.n.a)
            $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
          $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
        ?:  =(p.n.a p.n.b)
          [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
        ?:  (hor p.n.a p.n.b)
          $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
        $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)

Produces a map of the union between the keys of `a` and `b`. If `b`
shares a key with `a`, the tuple from `a` is preserved.

`a` is a [map]().

`b` is a [map]().

    ~zod/try=> m
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> o
    {[p='d' q=4] [p='c' q=3]}
    ~zod/try=> (~(uni by m) o)
    {[p='d' q=4] [p='a' q=1] [p='c' q=3] [p='b' q=2]}
    ~zod/try=> (~(uni by m) ~)
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> n
    {[p='a' q=1] [p='c' q=9]}
    ~zod/try=> (~(uni by o) n)
    {[p='d' q=4] [p='a' q=1] [p='c' q=3]}
    ~zod/try=> =n (mo `(list ,[@t *])`[['a' 1] ['c' 9] ~])
    ~zod/try=> n
    {[p='a' q=1] [p='c' q=9]}
    ~zod/try=> (~(uni by o) n)
    {[p='d' q=4] [p='a' q=1] [p='c' q=9]}

------------------------------------------------------------------------

### `+-urn:by`

Turn (with key)

      +-  urn                                               ::  turn
        |*  b=$+([* *] *)
        |-
        ?~  a  ~
        [n=[p=p.n.a q=(b p.n.a q.n.a)] l=$(a l.a) r=$(a r.a)]

Iterates over every value in map `a` using gate `b`, which accepts both
the key and the value of each element as its sample.

`a` is a [map]().

`b` is a [wet gate]() that accepts two nouns (a key and a value) and
produces a noun (the new value).

    ~zod/try=> m
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> (~(urn by m) |=(a=[p=* q=*] q.a))
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> (~(urn by m) |=(a=[p=* q=*] 7))
    {[p='a' q=7] [p='b' q=7]}
    ~zod/try=> (~(urn by m) |=(a=[p=* q=*] p.a))
    {[p='a' q=97] [p='b' q=98]}

------------------------------------------------------------------------

### `+-wyt:by`

Depth

      +-  wyt                                               ::  depth of map
        |-  ^-  @
        ?~(a 0 +((add $(a l.a) $(a r.a))))

Produce the depth of the tree map `a`.

`a` is a [map]().

    ~zod/try=> m
    {[p='a' q=1] [p='b' q=2]}
    ~zod/try=> o
    {[p='d' q=4] [p='c' q=3]}
    ~zod/try=> ~(wyt by m)
    2
    ~zod/try=> ~(wyt by o)
    2
    ~zod/try=> ~(wyt by (~(uni by m) o))
    4

------------------------------------------------------------------------
