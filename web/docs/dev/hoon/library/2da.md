section 2dA, sets
=================

### `++apt`

Set verification

    ++  apt                                                 ::  set invariant
      |=  a=(tree)
      ?~  a
        &
      ?&  ?~(l.a & ?&((vor n.a n.l.a) (hor n.l.a n.a)))
          ?~(r.a & ?&((vor n.a n.r.a) (hor n.a n.r.a)))
      ==
    ::

Produces a loobean indicating whether `a` is a set or not.

`a` is a [tree]().

    ~zod/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
    ~zod/try=> (apt b)
        %.y
    ~zod/try=> =m (mo `(list ,[@t *])`[['a' 1] ['b' [2 3]] ['c' 4] ['d' 5] ~])
    ~zod/try=> m
        {[p='d' q=5] [p='a' q=1] [p='c' q=4] [p='b' q=[2 3]]}
    ~zod/try=> (apt m)
        %.y

------------------------------------------------------------------------

### `++in`

Set operations

    ++  in                                                  ::  set engine
      ~/  %in
      |/  a=(set)

Input arm.

    ~zod/try=> ~(. in (sa "asd"))
    <13.evb [nlr(^$1{@tD $1}) <414.fvk 101.jzo 1.ypj %164>]>

`a` is a [set]()

### `+-all:in`

Logical AND

      +-  all                                               ::  logical AND
        ~/  %all
        |*  b=$+(* ?)
        |-  ^-  ?
        ?~  a
          &
        ?&((b n.a) $(a l.a) $(a r.a))
      ::

Computes the logical AND on every element in `a` slammed with `b`,
producing a loobean.

`a` is a [set]().

`b` is a [wet gate]() that accepts a noun and produces a loobean.

    ~zod/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
    ~zod/try=> (~(all in b) |=(a=* ?@(+.a & |)))
        %.n
    ~zod/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
    ~zod/try=> (~(all in b) |=(a=@t (gte a 100)))
        %.y

------------------------------------------------------------------------

### `+-any:in`

Logical OR

      +-  any                                               ::  logical OR
        ~/  %any
        |*  b=$+(* ?)
        |-  ^-  ?
        ?~  a
          |
        ?|((b n.a) $(a l.a) $(a r.a))
      ::

Computes the logical OR on every element of `a` slammed with `b`.

`a` is a [set]().

`b` is a [gate]() that accepts a noun and produces a loobean.

    ~zod/try=> =b (sa `(list ,[@t *])`[['a' 1] ['b' [2 3]] ~])
    ~zod/try=> (~(any in b) |=(a=* ?@(+.a & |)))
        %.y
    ~zod/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
    ~zod/try=> (~(any in b) |=(a=@t (lte a 100)))
        %.n

------------------------------------------------------------------------

### `+-del:in`

Remove noun

      +-  del                                               ::  b without any a
        ~/  %del
        |*  b=*
        |-  ^+  a
        ?~  a
          ~
        ?.  =(b n.a)
          ?:  (hor b n.a)
            [n.a $(a l.a) r.a]
          [n.a l.a $(a r.a)]
        |-  ^-  ?(~ _a)
        ?~  l.a  r.a
        ?~  r.a  l.a
        ?:  (vor n.l.a n.r.a)
          [n.l.a l.l.a $(l.a r.l.a)]
        [n.r.a $(r.a l.r.a) r.r.a]
      ::

Removes `b` from the set `a`.

`a` is a [set]().

`b` is a [noun]().

    ~zod/try=> =b (sa `(list ,@t)`['a' 'b' 'c' ~])
    ~zod/try=> (~(del in b) 'a')
    {'c' 'b'}
    ~zod/try=> =b (sa `(list ,@t)`['john' 'bonita' 'daniel' 'madeleine' ~])
    ~zod/try=> (~(del in b) 'john')
    {'bonita' 'madeleine' 'daniel'}
    ~zod/try=> (~(del in b) 'susan')
    {'bonita' 'madeleine' 'daniel' 'john'}

------------------------------------------------------------------------

### `+-dig:in`

Axis a in b

      +-  dig                                               ::  axis of a in b
        |=  b=*
        =+  c=1
        |-  ^-  (unit ,@)
        ?~  a  ~
        ?:  =(b n.a)  [~ u=(peg c 2)]
        ?:  (gor b n.a)
          $(a l.a, c (peg c 6))
        $(a r.a, c (peg c 7))
      ::

Produce the axis of `b` within `a`.

`a` is a [set]().

`b` is a [noun]().

    ~zod/try=> =a (sa `(list ,@)`[1 2 3 4 5 6 7 ~])
    ~zod/try=> a
    {5 4 7 6 1 3 2}
    ~zod/try=> -.a
    n=6
    ~zod/try=> (~(dig in a) 7)
    [~ 12]
    ~zod/try=> (~(dig in a) 2)
    [~ 14]
    ~zod/try=> (~(dig in a) 6)
    [~ 2]

------------------------------------------------------------------------

### `+-gas:in`

Concatenate

      +-  gas                                               ::  concatenate
        ~/  %gas
        |=  b=(list ,_?>(?=(^ a) n.a))
        |-  ^+  a
        ?~  b
          a
        $(b t.b, a (put(+< a) i.b))
      ::

Insert the elements of a list `b` into a set `a`.

`a` is a [set]().

`b` is a [list]().

    ~zod/try=> b
    {'bonita' 'madeleine' 'rudolf' 'john'}
    ~zod/try=> (~(gas in b) `(list ,@t)`['14' 'things' 'number' '1.337' ~])
    {'1.337' '14' 'number' 'things' 'bonita' 'madeleine' 'rudolf' 'john'}
    ~zod/try=> (~(gas in s) `(list ,@t)`['1' '2' '3' ~])
    {'1' '3' '2' 'e' 'd' 'a' 'c' 'b'}

------------------------------------------------------------------------

### `+-has:in`

b in a?

      +-  has                                               ::  b exists in a check
        ~/  %has
        |*  b=*
        |-  ^-  ?
        ?~  a
          |
        ?:  =(b n.a)
          &
        ?:  (hor b n.a)
          $(a l.a)
        $(a r.a)
      ::

Checks if `b` is an element of `a`, producing a loobean.

`a` is a [set]().

`b` is a [noun]().

    ~zod/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
    ~zod/try=> (~(has in a) `a`)
    %.y
    ~zod/try=> (~(has in a) 'z')
    %.n

------------------------------------------------------------------------

### `+-int:in`

Intersection

    +-  int                                               ::  intersection
        ~/  %int
        |*  b=_a
        |-  ^+  a
        ?~  b
          ~
        ?~  a
          ~
        ?.  (vor n.a n.b)
          $(a b, b a)
        ?:  =(n.b n.a)
          [n.a $(a l.a, b l.b) $(a r.a, b r.b)]
        ?:  (hor n.b n.a)
          %-  uni(+< $(a l.a, b [n.b l.b ~]))  $(b r.b)
        %-  uni(+< $(a r.a, b [n.b ~ r.b]))  $(b l.b)

Produces a set of the intersection between two sets of the same type,
`a` and `b`.

`a` is a [set]().

`b` is a [set]().

    ~zod/try=> (~(int in (sa "ac")) (sa "ha"))
    {~~a}
    ~zod/try=> (~(int in (sa "acmo")) ~)
    {}
    ~zod/try=> (~(int in (sa "acmo")) (sa "ham"))
    {~~a ~~m}
    ~zod/try=> (~(int in (sa "acmo")) (sa "lep"))
    {}

------------------------------------------------------------------------

### `+-put:in`

Put b in a

      +-  put                                               ::  puts b in a
        ~/  %put
        |*  b=*
        |-  ^+  a
        ?~  a
          [b ~ ~]
        ?:  =(b n.a)
          a
        ?:  (hor b n.a)
          =+  c=$(a l.a)
          ?>  ?=(^ c)
          ?:  (vor n.a n.c)
            [n.a c r.a]
          [n.c l.c [n.a r.c r.a]]
        =+  c=$(a r.a)
        ?>  ?=(^ c)
        ?:  (vor n.a n.c)
          [n.a l.a c]
        [n.c [n.a l.a l.c] r.c]
      ::

Add an element `b` to the set `a`.

`a` is a [set]().

`b` is a [noun]().

    ~zod/try=> =a (~(gas in `(set ,@t)`~) `(list ,@t)`[`a` `b` `c` ~])
    ~zod/try=> =b (~(put in a) `d`)
    ~zod/try=> b
    {`d` `a` `c` `b`}
    ~zod/try=> -.l.+.b
    n=`d`

------------------------------------------------------------------------

### `+-rep:in`

Accumulate

      +-  rep                                               ::  replace by tile
        |*  [b=* c=_,*]
        |-
        ?~  a  b
        $(a r.a, b $(a l.a, b (c n.a b)))
      ::

Accumulate the elements of `a` using a gate `c` and an accumulator `b`.

`a` is a [set]().

`b` is a [noun]() that accepts a noun and produces a loobean.

`c` is a [gate]().

    ~zod/try=> =a (~(gas in *(set ,@)) [1 2 3 ~])
    ~zod/try=> a
    {1 3 2}
    ~zod/try=> (~(rep in a) 0 |=([a=@ b=@] (add a b)))
    6

------------------------------------------------------------------------

### `+-tap:in`

Set to list

      +-  tap                                               ::  list tiles a set
        ~/  %tap
        |=  b=(list ,_?>(?=(^ a) n.a))
        ^+  b
        ?~  a
          b
        $(a r.a, b [n.a $(a l.a)])
      ::

Flatten the set `a` into a list.

`a` is an [set]().

`a` is a [set]().

`b` is a [list]().

    ~zod/try=> =s (sa `(list ,@t)`['a' 'b' 'c' 'd' 'e' ~])
    ~zod/try=> s
    {'e' 'd' 'a' 'c' 'b'}
    ~zod/try=> (~(tap in s) `(list ,@t)`['1' '2' '3' ~])
    ~['b' 'c' 'a' 'd' 'e' '1' '2' '3']
    ~zod/try=> b
    {'bonita' 'madeleine' 'daniel' 'john'}
    ~zod/try=> (~(tap in b) `(list ,@t)`['david' 'people' ~])
    ~['john' 'daniel' 'madeleine' 'bonita' 'david' 'people']

------------------------------------------------------------------------

### `+-uni:in`

Union

      +-  uni                                               ::  union
        ~/  %uni
        |*  b=_a
        |-  ^+  a
        ?~  b
          a
        ?~  a
          b
        ?:  (vor n.a n.b)
          ?:  =(n.b n.a)
            [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
          ?:  (hor n.b n.a)
            $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
          $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
        ?:  =(n.a n.b)
          [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
        ?:  (hor n.a n.b)
          $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
        $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)

Produces a set of the union between two sets of the same type, `a` and
`b`.

`a` is a [set]().

`b` is a [set]().

    ~zod/try=> (~(uni in (sa "ac")) (sa "ha"))
    {~~a ~~c ~~h}
     ~zod/try=> (~(uni in (sa "acmo")) ~)
    {~~a ~~c ~~m ~~o}
    ~zod/try=> (~(uni in (sa "acmo")) (sa "ham"))
    {~~a ~~c ~~m ~~o ~~h}
    ~zod/try=> (~(uni in (sa "acmo")) (sa "lep"))
    {~~e ~~a ~~c ~~m ~~l ~~o ~~p}

------------------------------------------------------------------------

### `+-wyt:in`

Set size

      +-  wyt                                               ::  size of set
        |-  ^-  @
        ?~(a 0 +((add $(a l.a) $(a r.a))))

Produce the number of elements in set `a` as an atom.

`a` is an [set]().

    ~zod/try=> =a (~(put in (~(put in (sa)) 'a')) 'b')
    ~zod/try=> ~(wyt in a)
    2
    ~zod/try=> b
    {'bonita' 'madeleine' 'daniel' 'john'}
    ~zod/try=> ~(wyt in b)
    4

------------------------------------------------------------------------
