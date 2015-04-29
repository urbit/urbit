<div class="short">

`col : %col`
============

Tuples
------

The `:` runes construct [tuples]().

There is no natural `:` rune. Instead, all of them derive from the
autocons property of `++twig`, as show below.

    ++  twig  $&  [p=twig q=twig]

Namely, a cell of two twigs is a twig producing a cell of the results of
the two original sub-twigs.

The `:` runes produce one of three categories of tuples

</div>

#### Tuples of determinate size:

[`:-`]() Tuples of two elements, `p` and `q`. [`:_`]() Tuples of two
reversed elements, `q` and `p`. Used to ensure vertical code flow.\

[`:+`]() Tuples of three elements `p`, `q`, and `r`. [`:^`]() Tuples of
four elements `p`, `q`, `r`, and `s`.

<hr></hr>

#### Tuples of indeterminate size:

[`:*`]() Tuples of n elements. [`:~`]() Null-terminated tuples of n
elements.

#### Tuples used for interpolation:

[`:/`]() Tuple designed to be interpolated into an XML structure.

<hr></hr>

<kids></kids>