`=^`
====

Declare variable with side effects

Used to both push a new variable on the subject as well as change
the value of another.  Similar to `=+` in that it pushes a value
on the stack with variable name (face) `p`. However, in addition
to pushing a new variable onto the subject, it also modifies `q`
within the subject.  `r` is the cell of `[new-product-p
updated-value-1]`. You can think of `=^` like a state monad. 

Technically, `=^` is a synthetic rune that handles a product
which is a cell of a new result, and a mutation to the subject.

Produces
--------

Twig: `[%tskt p=twig q=twig r=twig s=twig]`

Accepts
-------

`p`, `q`, `r` and `s` are [twig]()s.

Tall form
---------

    =^    p 
        q
      r
    s

    =^  p  q
      r
    s

Wide form
---------

    =^(p q r s)

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
