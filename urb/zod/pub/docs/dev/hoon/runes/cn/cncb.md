`%_`
====

`%=`, but product cast to `p`

The same as [`%=`](), except for that the product is cast
back to the type of `p`. Evaluates `p` with the changes specified in `q`, then casts the product back to the type of `p`. `%_` is used to change a batch of [`++wing`]()s all at once, ensuring that the resulting product is type checked.

You generally want to use `%_` unless you are intentionally trying to change the type.

Produces
--------

Twig: `[%cncb p=wing q=tram]`

Accepts
-------

`p` is a `++wing`, a list of [`++limb`]()s. `q` is a [`++tram`](), a
list of key value pairs of [`++wing`]()s to twigs.

Tall form
---------

    %_  p
      p.i.q  q.i.q
      p.i.t.q  q.i.t.q
    ==

Wide form
---------

    %_(p p.i.q q.i.q, p.i.t.q q.i.t.q)

Examples
--------

    /~zod/try=> =a [b=1 c=2 d=3]
    new var %a
    /~zod/try=> %_(a b (add 3 b.a), c (add 3 c.a), d (add 3 d.a))
    [b=4 c=5 d=6]

Here we're using `%_` to add 3 to all of the values inside of our shell
variable `a`.

    /~zod/try=> =a [b='odors' c='twigs' d='molds']
    changed %a
    /~zod/try=> %_(a b c.a, c d.a, d b.a)
    [b='twigs' c='molds' d='odors']

In this case we're using `%_` to swap the values of the faces in `a`.

    /~zod/try=> =+  a=1    
    =+  z=|=(b=@ (add a b))
    (z 1)
    2
    /~zod/try=> =+  a=1    
    =+  z=|=(b=@ (add a b))
    (%_(z a 100) 1)
    101

At first we set up a simple function `z` with a variable, `a` in its
context. Subsequently we use `%_` to change the value of `a` within the
context of `z` and compute the output again.

    =+(a=[var1=1 var2=2] %_(a var1 [3 4]))
    ...
    type-fail
    ford: build failed

The example above fails because the type of the result is a cell
of a cell and an atom (`[^ @]`) does not fit within the type of
`a` (`[@ @]`).

