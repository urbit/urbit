ketlus, `^+`, %ktls
============================

Cast to type

Syntax
======

`^+` is a natural rune that casts the product of `q` to the type of `p`,
verifying that it contains the type of `q`. `^+` is similar to `^-`, but
doesn't bunt the subject. Most often we use `^+` to cast when our type
is already defined by something inside our context.

See also
--------

kethep, `^-`, %kthp
============================

Produces
--------

Twig: `[%ktls p=twig q=twig]`

Sample
------

`p` is a [twig](). `q` is a [twig]().

Tall form
---------

    ^+  p
        q

Wide form
---------

    ^+(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> (add 90 7)
    97
    ~zod/try=> ^+('any text' (add 90 7))
    'a'

Here we use a cord (which could be any cord), `'any text'` to cast our
result to a ['++cord']().

    /~zod/try=>  =cor  |=  a=[q=@ta r=@ s=@]
                       [(cat 3 'new' q.a) (add r.a s.a) (sub r.a s.a)]
    new var %cor
    /~zod/try=> (cor 'start' 6 3)
    [8.390.876.208.525.960.558 9 3]
    /~zod/try=>  =cor  |=  a=[q=@ta r=@ s=@]
                       ^+  a
                       [(cat 3 'new' q.a) (add r.a s.a) (sub r.a s.a)]
    changed %cor
    /~zod/try=> (cor 'start' 6 3)
    [q=~.newstart r=9 s=3]

In this example we create a gate `cor` and lightly manipulate the tuple
it takes as a sample: we prepend `'new'` to its first member, produce
the sum of the latter two as the second, and the differene as the third.

In the first call you can see that our type information is lost, and we
produce our cord as an atom. By adding a `^+  a` we cast the result to
our input, and type information is retained.

Equivalent to
-------------

    ^-(_p q)
