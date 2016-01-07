`:/`
====

Interpolation structure

`:/`, `colfas`, is a synthetic rune that produces `[%$ [%$ p ~] ~]`,
i.e., `[0 [0 p 0] 0]`.

Produces
--------

Twig: `[%clfs p=twig]`

Accepts
-------

`p` is a [`++twig`]()

Tall form
---------

    :/  p

Wide form
---------

    :/(p)

Examples
--------

    ~zod/try=> :/(20)
    [[%~. [%~. 20] ~] ~]
    ~zod/try=> :/(add 2 2)
    [[%~. [%~. 4] ~] ~]

Wraps `++twig` in `[%$ [%$ .] ~]~`, used for interpolation.
