`$|`
====

Union type between atom and cell

[mold]() that's a union of an atom `p` and cell `q`.  `$|` is
similar to [`$?`]() but is more strict in that it only contains
one atom mold and one cell mold.

### Produces

A validator function that validates atoms as mold `p` and cells as mold `q`.

### Accepts

`p` is a `mold` of an atom and `q` is a `mold` of a cell.

### Tall form

    $|  p
    q

### Wide form

    $|(p q)

### Examples

    ~zod/try=> ($|(@t [@ud @ux]) 'hello')
    'hello'
    ~zod/try=> (,$|(@ [@t @ud]) 44 44)
    [',' 44]
    ~zod/try=> *$|(~ [~ u=@])
    ~

    ++  list  |*  a=_,*                                     ::  null-terminated list
              $|(~ [i=a t=(list a)])                        ::

In `++list`, `$|` specifies that every element in a noun that can be
cast to a `++list` is either the atom `~` or the cell
`[i=a t=(list a)]`.
