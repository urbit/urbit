Morphology
==========

Hoon is a statically typed language that compiles to Nock. 

A type is a function whose domain is the set of all nouns and whose range is the set of all nouns that are members of that type.

The compilation process is as follows:

First, a runic expression is parsed into an abstact syntax-tree, called a `twig`

    expression => twig

A subject type is generated from the twig. This type describes the subject
of the Nock formula that the twig compiles to.

    twig => [subject-type twig]

The twig is then compiled into nock formula, and the type of the product of
the formula is inferred.

    [subject-type twig] => [product-type nock-formula]

As long as subject-type is a correct description of some subject, you can
take any twig and compile it against subject-type, producing a formula such
that 

    *[subject formula] 

is a product correctly described by product-type.

This works well enough that in Hoon there is no direct syntax for defining or
declaring a type. There is only a syntax for constructing twigs.  Types are
always produced by inference.


Let's look at a simple example of the above proc


Hoon has 120 [XX count] digraph runes. The choice of glyph is not random. The
first defines a semantic category (with some exceptions). These categories are:

    |  bar    core construction
    $  buc    tiles and tiling
    %  cen    invocations
    :  col    tuples
    .  dot    nock operators
    ^  ket    type conversions
    ;  sem    miscellaneous macros
    ~  sig    hints
    =  tis    compositions
    ?  wut    conditionals, booleans, tests
    !  zap    special operations

