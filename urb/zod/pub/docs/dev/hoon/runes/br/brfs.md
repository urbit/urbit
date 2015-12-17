`|/`
====

`|_` with well typed output

Similar to `|_` except that it takes and produces well-typed
output.  More specifically, the children must be wet (with `+-`),
and they are compiled with the actual type of `p`, not the
declared type.

`|/` is a synthetic rune that produces a [`%gold`]() [door]()
with [sample]() `[%bctr p]` and list of  [wet or %elm]() [arm]()s
`q`. The list of arms must be closed with a `--`.

Produces
--------

Twig: `[%brfs p=mold q=(map term foot)]`

Accepts
-------

`p` is a [mold](). `q` is a [`++map`]() with [`++term`]() keys and
[`++foot`]() values.

Tall form
---------

    |/  p
    +-  p.n.q
      q.n.q
    --

Examples
--------

    /~zod/try=> =fas  |/
          a=@
          +-  two  (mul a 2)
          +-  for  (div a 2)
          --
    new var %fas
    /~zod/try=> ~(two fas 2)
    4
    /~zod/try=> ~(for fas ~(for fas ~(two fas 12)))
    6

In this simple example we're creating a door with two arms. One arm
divides our sample by two, the other divides by two.

    /~zod/try=> =kom
                      |_  a=(list)
                      ++  hed  -.a
                      ++  tal  +.a
                      --
    new var %kom
    /~zod/try=> =kot
                      |/  a=(list)
                      +-  hed  -.a
                      +-  tal  +.a
                      --
    new var %kot
    /~zod/try=> ~(tal kom "abc")
    t=~[98 99]
    /~zod/try=> ~(tal kot "abc")
    t="bc"
    /~zod/try=> ~(tal kot [1 2 3 ~])
    [2 3 ~]
    /~zod/try=> ~(tal kom [1 2 3 ~])
    t=~[2 3]

Here we're demonstrating the difference between `|_` and `|/`. We create
a nearly identical door using both runes, each with an arm that produces
the tail of the sample, `a`. You can see that our wet gates use the
[sample]() as a mold to produce well-typed output.

    ++  by                                                  ::  map engine
          ~/  %by
          |/  a=(map)
          ::
          +-  all
          ~/  %all
          |*  b=$+(* ?)
          |-  ^-  ?
          ?@  a
            &
          ?&((b q.n.a) $(a l.a) $(a r.a))

All of the container engines in `hoon.hoon` use `|/` to set up the mold
for their operations. In `++by`, the map engine, `|/` creates a door
that takes a map that is passed to its arms. See more about using `++by`
in the [library]().
