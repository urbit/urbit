sigzap, `~!`, %sgzp
============================

[Short description]

`~!` is a natural rune for debugging uses only, semantically equivalent
to its own twig `q`. If compilation fails within `q`, `~!` will show the
type of `p` on the stacktrace.

Produces
--------

Twig: `[%sgzp p=twig q=twig]`

Sample
------

`p` is a [twig]() `q` is a [twig]()

Tall form
---------

    ~!  p
        q

Wide form
---------

    ~!(p q)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> a
    ! -find-limb.a
    ! find-none
    ! exit
    ~zod/try=> ~!('foo' a)
    ! @t
    ! -find-limb.a
    ! find-none
    ! exit

When trying to compute an unassigned variable, `a` we produce the type
of `'foo'`, `@t`.

    ~zod/try=> ~!(%foo a)
    ! %foo
    ! -find-limb.a
    ! find-none
    ! exit

Again, we use our unassigned variable `a` and the [cube]() `%foo`, whose
type is in fact `%foo`.
