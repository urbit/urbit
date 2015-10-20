section 2dD, casual containers
==============================

### `++mo`

Map from list

    ++  mo                                                  :: map from list
      |*  a=(list)
      =>  .(a `_(homo a)`a)
      =>  .(a `(list ,[p=_-<.a q=_->.a])`a)
      =+  b=*(map ,_?>(?=(^ a) p.i.a) ,_?>(?=(^ a) q.i.a))
      (~(gas by b) a)
    ::

Produces a map of key-value pairs from the left-right cell pairs of list
`a`.

`a` is a [list]().

    ~zod/try=> (mo `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
    {[p=`a` q=1] [p=`b` q=2]}

------------------------------------------------------------------------

### `++sa`

Set from list

    ++  sa                                                  :: set from list
      |*  a=(list)
      =>  .(a `_(homo a)`a)
      =+  b=*(set ,_?>(?=(^ a) i.a))
      (~(gas in b) a)
    ::

Produces a set of the elements in list `a`.

`a` is a [list]().

    ~zod/try=> (sa `(list ,@)`[1 2 3 4 5 ~])
    {5 4 1 3 2}
    ~zod/try=> (sa `(list ,[@t *])`[[`a` 1] [`b` 2] ~])
    {[`a` 1] [`b` 2]}

------------------------------------------------------------------------

### `++qu`

Queue from list

    ++  qu                                                  ::  queue from list 
      |*  a=(list)
      =>  .(a `_(homo a)`a)
      =+  b=*(qeu ,_?>(?=(^ a) i.a))
      (~(gas to b) a)

Produces a queue from list `a`.

`a` is a [list]().

    ~zod/try=> (qu `(list ,@ud)`~[1 2 3 5])
    {5 3 2 1}
    ~zod/try=> (qu "sada")
    {'a' 'd' 'a' 's'}
    ~zod/try=> ~(top to (qu "sada"))
    [~ 's']

------------------------------------------------------------------------
