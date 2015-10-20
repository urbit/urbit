buccom `$,` %bccm
==========================

Normalizing gate, `%clam`

`$,` is a synthetic rune that produces a `%leaf`, a normalizing gate or
[clam]() for `p`. `$,` is used to ensure an input value fits a certain
type: if it does match, the value is produced. If it doesn't, the
default value for the desired type is produced.

Produces
--------

[Twig](): `[%bccm p=tile]`

Sample
------

`p` is a [tile]()

Tall form
---------

    $,  p

Wide form
---------

None

Irregular form
--------------

    ,p

Examples
--------

    ++  cord  ,@t                                           ::  text atom (UTF-8)

In `++cord`, `,` creates a gate that validates atoms of the odor
[`@t`]().

    ~zod/try=> (,[1 2] "ham")
    [%1 %2]
    ~zod/try=> (,[@ 2] "ham")
    [104 %2]
    ~zod/try=> (,~ %foo)
    ~
    ~zod/try=> (,~ [%ba %r])
    ~
    ~zod/try=> (,$%([%a @] [%b ^]) [%b 5])
    [%b 0 0]
    ~zod/try=> (,$%([%a @] [%b ^]) [%a 5])
    [%a 5]
    ~zod/try=> (,$%([%a @] [%b ^]) [%a 6 7])
    [%a 0]
    ~zod/try=> (,$%([%a @] [%b ^]) [%b ~])
    [%b 0 0]
    ~zod/try=> (,$%([%a @] [%b ^]) [%b "ame"])
    [%b 97 [109 101 0]]
