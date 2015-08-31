censig, `%~`, %cnsg
============================

Pull with sample

`%~` is a synthetic rune that [pull]()s `p` from the [door]() `q` with
its sample set to `r`. `%~` is used to produce an arm `p` inside of a
door `q` with the door's sample set to `r`.

Produces
--------

Twig: `[%cnsg p=wing q=twig r=twig]`

Sample
------

`p` is a [`wing`](). `q` and `r` are [`twig`]()s.

Tall form
---------

    %~  p
      q
    r

Wide form
---------

    %~(p q r)

Irregular form
--------------

    ~(p q r)

Examples
--------

    /~zod/try=> =door  |_  a=@
                       ++  fort  |=  b=@
                                 (add b a)
                       --
    new var %door
    /~zod/try=> (~(fort door 10) 1)
    11
    /~zod/try=> (~(fort door 10) 10)
    20

Here we create a door using [`|_`]() as the shell variable `door` which
takes an atom. `door` has one arm `fort`, a gate, which takes an atom as
well and produces the sum. We use the irregular form of `%~` to set the
sample in `door`, and call it.

    /~zod/try=> =a  (mo (limo [['a' 1] ['b' 2] ~]))
    changed %a
    /~zod/try=> a
    {[p='a' q=1] [p='b' q=2]}
    /~zod/try=> (~(get by a) 'b')
    [~ 2]

In this case we create a simple map, `a` using [`++mo`]() and
['++limo'](). Then we use [`++get:by`]() to pull a value out of it by
first setting the sample of [`++by`]() to `a` and pulling `get` using
the irregular form of `%~`. Passing `'b'` to the resulting gate produces
the value of `'b'` in our map.

This is a very common use of `%~`. Most of the container engines in
`hoon.hoon` use doors with samples, so we use `%~` to set the sample and
pull the door to produce a gate we can pass data to.
