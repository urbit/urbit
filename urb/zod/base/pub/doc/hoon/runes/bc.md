<div class="short">

`buc $ %buc`
============

Tile construction
-----------------

The `$` runes construct [`++tile`]()s. [`++tile`]()s are one of our
primary building blocks in hoon, they define how we reduce our ASTs into
well typed nouns that nock can compute. You can think of a [`++tile`]()
sort of like a typeclass in Haskell.

</div>

#### Forks

##### [`++tile`]()s that can be one of multiple cases.

[`$?`]() Non-empty list of cases.

[`$|`]() Fork between atom and cell.

[`$&`]() Fork between cells whose head is a cell and cells whose head is
an atom.

<hr></hr>

#### Tuples

[`$:`]() Unlabelled arrays.

[`$=`]() Tuple with [`++face`]()s.

<hr> </hr>

#### Reductions

##### Important convenience methods for working with tiles, and are very broadly used.

[`$*`]() [bunt]()

[`$,`]() [clam]()

[`$@`]() [whip]()

<hr></hr>

<kids></kids>
