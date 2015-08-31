cenzey, %cnzy
======================

Pull limb `p`

`cenzey` is a synthetic internal twig that pulls limb `p` from the
subject. `cenzey` is only used internally by `hoon.hoon`.

Produces
--------

Twig: `[%cnzy p=term]`

Sample
------

`p` is a [++term]().

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

    /~zod/try=> (slap !>(a=[p=20 q=6]) [%cnzy %a])
    [p=[%cell p=[%face p=%p q=[%atom p=%ud]] q=[%face p=%q q=[%atom p=%ud]]] q=[20 6]]
    /~zod/try=> +:(slap !>(|.(42)) [%cnzy %$])
    q=42
