tisket, `=^`, %tskt
============================

State machine

`=^` is a synthetic rune that handles a product which is a cell of a new
result, and a mutation to the subject.

Produces
--------

Twig: `[%tskt p=twig q=twig r=twig s=twig]`

Sample
------

`p`, `q`, `r` and `s` are [twig]()s.

Tall form
---------

Kingside:

    =^    p 
        q
      r
    s

Queenside:

    =^  p  q
      r
    s

Wide form
---------

    =^(p q r s)

Irregular form
--------------

None

Examples
--------

    > =+  rng=~(. og 0wrando.mseed.12345)
      =^  r1  rng  (rads:rng 100)
      =^  r2  rng  (rads:rng 100)
      [r1 r2 rng]
    [99 46 <4.tty [@uw <399.uhj 106.umz 1.lgo %163>]>]
    ~fodrem-michex:dojo/try>

This is an example of using the `++og` pseudorandom number generator
with the `=^` rune. `++og` is evaluated with the initial random seed and
stored in `rng`. `=^` takes a face (to put on the result), a wing of the
subject (to modify), and a twig that must evaluate to a cell. The head
of the cell is the result, which is pushed on the subject with the
specified face. The tail of the cell is the updated state, which
replaces the specified wing of the subject. The final child twig is then
evaluated with the new subject.

In the case above, `++rads` in `++og` returns a cell of the result and a
new `++og` core with the next state of the PRNG. `=^` is useful for any
computation that also updates some kind of state.
