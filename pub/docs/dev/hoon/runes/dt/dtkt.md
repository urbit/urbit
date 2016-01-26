dotket, `.^`, %dtkt
============================

Load from `%clay`

`.^`, `dotket`, `[%dtkt p=twig]` is a natural rune that generates nock
operator `11`, which in virtual userspace nock, [`++mock`]() loads a
file from the global namespace.

Produces
--------

Twig: `[%dtkt p=twig]`

Sample
------

`p` is a [twig]().

Tall form
---------

    .^  p

Wide form
---------

    .^(p)

Irregular form
--------------

None

Examples
--------

    ~zod/try=> .^(a//=name=/~zod)
    587.481.414.525.965.665.129.596
    ~zod/try=> (,@t .^(a//=name=/~zod))
    '|Tianming|'

First we use `.^` to load the contents of `a//=name=/~zod` as a noun.
Subsequently using `,@t` to cast to a cord, we can see that the contents
are `'|Tianming|'`.

    ~zod/try=> :into %/test 'hello'
    + /~zod/try/2/test
    ~zod/try=> .^(cx/%/test)
    478.560.413.032
    ~zod/try=> (,@t .^(cx/%/test))
    'hello'

This is similar to the previous example, but uses our simple `:into`
program to write a cord to a file. Then we follow the above method to
confirm its contents.
