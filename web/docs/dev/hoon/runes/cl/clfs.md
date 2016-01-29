colfas, `:/`, %clfs
============================

Internal interpolation

`:/`, `colfas`, is a synthetic rune that produces `[%$ [%$ p ~] ~]`,
i.e., `[0 [0 p 0] 0]`.

See also
--------

[`++manx`]() [`%smdq`](#smdq)

Produces
--------

Twig: `[%clfs p=twig]`

Sample
------

`p` is a [twig]()

Tall form
---------

    :/  p

Wide form
---------

    :/(p)

Irregular form
--------------

undefined

Examples
--------

    ~zod/try=> :/(20)
    [[%~. [%~. 20] ~] ~]
    ~zod/try=> :/(add 2 2)
    [[%~. [%~. 4] ~] ~]

Wraps twig in `[%$ [%$ .] ~]~`, used for interpolation.
