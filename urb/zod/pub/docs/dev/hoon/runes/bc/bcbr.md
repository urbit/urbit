bucbar `$|` %bcbr
=================

<div class="short">

Creates a type (mold) that's a union between an atom `p` and cell `b`

`$|` is a mold rune that produces a [`%reed`](), a mold whose [span]()
is a [fork]() between two nouns: an [atom]() of `mold` `p` and a cell of
`mold` `q`. `$|` is similar to [`$?`](), but is more strict in that in
only contains one atom mold and one cell mold.

</div>

<hr>
</hr>
### Produces

[`mold`](): `[%reed p=mold q=mold]`

### Accepts

`p` is a [`mold`]() of an atom and `q` is a [`mold`]() of a cell.

### Tall form

    $|  p
        q

### Wide form

    $|(p q)

### Examples

    ++  list  |*  a=_,*                                     ::  null-terminated list
              $|(~ [i=a t=(list a)])                        ::

    ~zod/try=> *$|(~ [~ u=@])
    ~

In `++list`, `$|` specifies that every element in a noun that can be
cast to a `++list` is either the atom `~` or the cell
`[i=a t=(list a)]`.
