<div class="short">

`bar | %bar`
============

Core construction
-----------------

The `|` runes construct [core]()s. In the broadest case you can think of
a core as similar to an object with named properties that can contain
either functions or data.

The `|` runes accept an associative array of names ([++term]()) to
([++foot]()), each pair of which are called an [++arm](), producing one
of the three basic categories of core.

</div>

#### The natural, generic core:

[`|%`]() Generic core, with [++arms] generally containing [++twig]()s

<hr></hr>

#### Doors, generic core with a sample:

##### Gates:

Cores with one arm [`$`], the empty name and which takes a sample
`p`. The closest thing in Hoon to a function.

[`|=`]() [`dry`]() gate, where the [sample]() is typechecked at compile
time. [`|*`]() [`wet`]() gate, where the sample is typechecked at
runtime against its product type.

##### Traps:

Traps reduce to a `|%` with one arm `$`, the empty name. A trap is just
some computation that has been put inside of a wrapper so that it may be
passed around.

[`|.`]() Generic trap. [`|-`]() Trap automatically [kick]ed (called)
after construction.

<hr></hr>

<kids></kids>