`?-`
====

Switch

Different from the traditional switch statement in that it
switches on a type instead of a value. More specifically, the
labels in `q` must match the [span]() of `p`. `q` must be
terminated with a `==`.

Produces
--------

Twig: `[%wthp p=wing q=tine]`

Accepts
-------

`p` is a [`++wine`](). `q` is a [`++tine`]().

Tall form
---------

Kingside:

    ?-  p
      p.i.q      q.i.q
      p.i.t.q    q.i.t.q
      p.i.t.t.q  q.i.t.t.q
    ==

Queenside:

    ?-    p
        p.i.q      
      q.i.q
        p.i.t.q    
      q.i.t.q
        p.i.t.t.q  
      q.i.t.t.q
    ==

Wide form
---------

    ?-(p p.i.q q.i.q, p.i.t.q q.i.t.q, p.i.t.t.q q.i.t.t.q)

Examples
--------

    ~zod/try=> 
      =cor  |=  typ=$?(%a %b)
            ?-  typ
              %a  1
              %b  2
            ==
    new var %cor
    ~zod/try=> 
      (cor %a)
    1
    ~zod/try=> 
      (cor %b)
    2

Here is a simple example of `?-` that thows its input must have a well
defined type for which all of the cases are covered. We create a core,
`cor`, that takes an input `typ` which must be either `%a` or `%b` with
[`$%`](). Calling `cor` with valid arguments selects one of our cases.

    ~zod/try=> 
      ?-  'a'
        %a  0
        %b  1
      ==
    ! /~zod/try/~2014.11.2..16.56.40..fca2:<[1 1].[4 7]>
    ! -lost.@t
    ! mint-lost
    ~zod/try=> 
      ?-  (?(%a %b) 0)
        %a  'a'
        %b  'b'
      ==
    'b'

Here we can see a common failure case with `?-`. In the first example
all of our possible input cases are not covered when we pass in a `@t`,
so we fail with `mint-vain`, a compilation error. When we construct a well
typed input and cover all of the possible cases, we get the desired result.
