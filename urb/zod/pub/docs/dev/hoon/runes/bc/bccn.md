`$%` 
====

Tagged union

Produces a [mold]() of a tagged union of a list of case pairs.

Produces
--------

A validator function that uses the value's [head]() as a tag to select a mold validate the tail of the value.

Accepts
-------

A list of cells of molds whose heads must be a [cube]().

Tall form
---------

    $%  p
        q
    ==

Examples
--------

    ~zod/try=> $%([%& @p] [%| @t])
    <1.dhe [* @n <246.qra 41.uuw 374.glo 100.rip 1.ypj %164>]>
    ~zod/try=> *$%([%& @p] [%| @t])
    [%.y ~zod]
    ~zod/try=> :type; *$%([%& @p] [%| @t])
    [%.y ~zod]
    {[%.y @p] [%.n @t]}
    ~zod/try=> :type; *(each ,@p ,@t)
    [%.y p=~zod]
    {[%.y p=@p] [%.n p=@t]}
    ~zod/try=> $%(~ [%a 1])
    ~ <syntax error at [1 13]>
    ~zod/try=> $%([%~ ~] [%a 1])
    <1.yck [* @n <246.qra 41.uuw 374.glo 100.rip 1.ypj %164>]>
    ~zod/try=> *$%([%~ ~] [%a 1])
    [~ ~]

    ++  foot  $%  [%ash p=twig]                             ::  dry, geometric
                  [%elm p=twig]                             ::  wet, generic
                  [%oak ~]                                  ::  XX not used
                  [%yew p=(map term foot)]                  ::  XX not used
              ==

In `++foot`, `$%` creates a list of possible cases. That is, a `++foot`
can be either `%ash`, `%elm`, `%oak` or `%yew`.
