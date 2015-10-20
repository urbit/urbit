barfas, `|/`, %brfs
============================

Door with tile

`|/` is a synthetic rune that produces a [`%gold`]() [door]() with
sample `[%bctr p]` and list of [arm]()s `q`. The list of arms must be
closed with a `--`.

`|/` is similar to `|_` in that it accepts a sample for the set of arms
, but differs in that it accepts [wet or `%ash`]() arms.

See also
--------

[barcab, `|_`, `%brcb`]() [barcen, `|%`, `%brcn`]()

Produces
--------

Twig: `[%brfs p=tile q=(map term foot)]`

Sample
------

`p` is a tile. `q` is a [`map`]() with [`++term`]() keys and
[`++foot`]() values.

Tall form
---------

    |/  p
    +-  p.n.q
      q.n.q
    --

Wide form
---------

None

Irregular form
--------------

None

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
sample as a tile to produce well-typed output.

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

All of the container engines in `hoon.hoon` use `|/` to set up the tile
for their operations. In `++by`, the map engine, `|/` creates a door
that takes a map that is passed to its arms. See more about using `++by`
in the [library]().
