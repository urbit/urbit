section 2dC, queues
===================

### `++to`

Queue engine

    ++  to                                                  ::  queue engine
      |/  a=(qeu)

Container arm for queue operation arms. The contained arms inherit its
[sample]() `++qeu` `a`.

`a` is a queue, [++qeu]().

### `+-bal:to`

Balance

      +-  bal
        |-  ^+  a
        ?~  a  ~
        ?.  |(?=(~ l.a) (vor n.a n.l.a))
          $(a [n.l.a l.l.a $(a [n.a r.l.a r.a])])
        ?.  |(?=(~ r.a) (vor n.a n.r.a))
          $(a [n.r.a $(a [n.a l.a l.r.a]) r.r.a])
        a
      ::

Vertically rebalances queue `a`.

`a` is a [queue]().

    ~zod/try=> `(qeu tape)`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~]
    {"a" "b" "c" "d" "e" "f" "g"}
    ~zod/try=> `*`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~]
    [[97 0] 0 [98 0] 0 [99 0] 0 [100 0] 0 [101 0] 0 [102 0] 0 [103 0] 0 0]
    ~zod/try=> ~(bal to `(qeu tape)`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~])
    {"a" "b" "c" "d" "e" "f" "g"}
    ~zod/try=> `*`~(bal to `(qeu tape)`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~])
    [[100 0] [[99 0] [[98 0] [[97 0] 0 0] 0] 0] [101 0] 0 [102 0] 0 [103 0] 0 0]

------------------------------------------------------------------------

### `+-dep:to`

Maximum Depth

      +-  dep                                               ::  max depth of queue
        |-  ^-  @
        ?~  a  0
        +((max $(a l.a) $(a r.a)))
      ::

Produces the maximum depth of leaves (r.a and l.a) in queue `a`.

`a` is a [queue]().

    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 7 ~])
    ~zod/try=> ~(dep to a)
    4
    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 ~])
    ~zod/try=> ~(dep to a)
    3
    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 ~])
    ~zod/try=> ~(dep to a)
    2
    ~zod/try=> ~(dep to `(qeu tape)`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~])
    7
    ~zod/try=> ~(dep to ~(bal to `(qeu tape)`["a" ~ "b" ~ "c" ~ "d" ~ "e" ~ "f" ~ "g" ~ ~]))
    4

------------------------------------------------------------------------

### `+-gas`

Push list

      +-  gas                                               ::  insert list to queue
        |=  b=(list ,_?>(?=(^ a) n.a))
        |-  ^+  a
        ?~(b a $(b t.b, a (put(+< a) i.b)))
      ::

Push all elements of list `b` into the queue.

`a` is a [queue]().

`b` is a list.

    ~zod/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
    {3 2 1}
    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 ~])
    ~zod/try=> =b `(list ,@)`[4 5 6 ~]
    ~zod/try=> (~(gas to a) b)
    {6 5 4 3 2 1}

------------------------------------------------------------------------

### `+-get:to`

Pop

      +-  get                                               ::  head-tail pair
        |-  ^+  [p=?>(?=(^ a) n.a) q=a]
        ?~  a
          !!
        ?~  r.a
          [n.a l.a]
        =+  b=$(a r.a)
        :-  p.b
        ?:  |(?=(~ q.b) (vor n.a n.q.b))
          [n.a l.a q.b]
        [n.q.b [n.a l.a l.q.b] r.q.b]
      ::

Produces the head and tail queue of `a`.

`a` is a [queue]().

    ~zod/try=> =s (~(gas to *(qeu ,@)) `(list ,@)`~[1 2 3])
    ~zod/try=> ~(get to s)
    [p=1 q={3 2}]
    ~zod/try=> ~(get to ~)
    ! exit

------------------------------------------------------------------------

### `+-nap:to`

Remove last in

      +-  nap                                               ::  removes head
        ?>  ?=(^ a)
        ?:  =(~ l.a)  r.a
        =+  b=get(+< l.a)
        bal(+< ^+(a [p.b q.b r.a]))
      ::

Removes the head of queue `a`, producing the resulting queue.

`a` is a [queue]().

    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 ~])
    ~zod/try=> -.a
    n=6
    ~zod/try=> =b ~(nap to a)
    ~zod/try=> -.b
    n=2
    ~zod/try=> b
    {5 4 3 2 1}
    ~zod/try=> a
    {6 5 4 3 2 1}

------------------------------------------------------------------------

### `+-put:to`

Insert

      +-  put                                               ::  insert new tail
        |*  b=*
        |-  ^+  a
        ?~  a
          [b ~ ~]
        bal(+< a(l $(a l.a)))
      ::

Accept any noun `b` and adds to queue `a` as the head, producing the
resulting queue.

`a` is a [queue]().

`b` is any noun.

    ~zod/try=> (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
    ~zod/try=> (~(put to a) 7)
    {7 6 5 4 2 1 3}

------------------------------------------------------------------------

### `+-tap:to`

Queue to list

      +-  tap                                               :: queue to list 
        |=  b=(list ,_?>(?=(^ a) n.a))
        ^+  b
        ?~  a
          b
        $(a r.a, b [n.a $(a l.a)])
      ::

Produces queue `a` as a list from front to back.

`a` is a [queue]().

    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[3 1 2 4 5 6 ~])
    ~zod/try=> `*`a
    [6 0 2 [4 [5 0 0] 0] 1 0 3 0 0]
    ~zod/try=> (~(tap to a) `(list ,@)`[99 100 101 ~])
    ~[3 1 2 4 5 6 99 100 101]

------------------------------------------------------------------------

### `+-top:to`

      +-  top                                               ::  produces head
        |-  ^-  (unit ,_?>(?=(^ a) n.a))
        ?~  a  ~
        ?~(r.a [~ n.a] $(a r.a))

Produces the head of queue `a` as a unit (an empty queue has no head).

`a` is a [queue]().

    ~zod/try=> =a (~(gas to `(qeu ,@)`~) `(list ,@)`[1 2 3 4 5 6 ~])
    ~zod/try=> ~(top to a)
    [~ 1]

------------------------------------------------------------------------
