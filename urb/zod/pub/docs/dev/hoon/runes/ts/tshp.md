`=-`
====

Reversed `=+`

Pushes variable `q` onto the subject and then executes `p`
against the new subject.  Allows us to place the larger of `p`
and `q` as the bottom expression, making for for more readable
code (see the section on [backstep]()).

Technically, `=-` is a synthetic rune that pushes `q` on the
subject and sends it to `p`. `=-` is the inverse of `=+`.

Produces
--------

Twig: `[%tshp p=twig q=twig]`

Accepts
-------

`p` and `q` are [twig]()s.

Tall form
---------

    =-  p
    q

Wide form
---------

    =-(p q)

Examples
--------

    ~zod/try=> =-  [%a a]
                   [a=1]
    [%a 1]

In this simple example we push `[a=1]` on to our subject, and produce
`[%a a]` which pulls the value of `a` from the subject producing
`[%a 1]`.

    ~zod/try=> 
    =cor  |=  [a=@ b=@]
          =-  [[%a a] [%b b]]
          [a b]=[(add a 2) (add a b)]
    new var %cor
    ~zod/try=> (cor 2 4)
    [[%a 4] %b 6]

Here we create a gate `cor` that takes two atoms `a` and `b`. We use
`=-` to put the product of our computation first.
