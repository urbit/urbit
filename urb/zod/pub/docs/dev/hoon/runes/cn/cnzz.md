cenzaz, %cnzz
======================

Pull wing `p`

`cenzaz` is a synthetic rune that pulls wing `p` from the subject.
`cenzaz` is only used internally by `hoon.hoon`, but is used implicitly
whenever dereferencing occurs.

Produces
--------

Twig: `[%cnzz p=wing]`

Sample
------

`p` is a [`++wing`]()

Tall form
---------

None

Wide form
---------

None

Irregular form
--------------

None

Examples
--------

    /~zod/try=> (ream 'a')
    [%cnzz p=~[%a]]
    /~zod/try=> (ream 'c.+>.$')
    [%cnzz p=~[%c [%.y p=7] %$]]
    /~zod/try=> (slap !>(a=[p=20 q=6]) [%cnzz ~[%p %a]])
    [p=[%atom p=%ud] q=20]
