Morphology
==========

Hoon is a statically typed language that compiles to Nock.  You can test out
most of the compilation routines in the REPL, so we'll point out some of those
as we go along.  At the highest level, there is `++make`, which turns text into
nock.

    ~hoclur-bicrel/try=> `*`(make '|=  [@ @ud]  +<')
    [8 [1 0 0] [1 0 6] 0 1]

If you want to know how Hoon code is compiled, start your investigation in
`++make`.  Another way to do this, of course, is with `!=`.

    ~hoclur-bicrel/try=> !=(|=([@ @ud] +<))
    [8 [1 0 0] [1 0 6] 0 1]

The difference here is of course that `!=` requires you to put in the code
literally and syntactically correctly.  So, for example, you can't use tall
form runes within a wide form usage of `!=`.  `++make` may be called on literal
text, which may even be programmatically generated.  Essentially, `++make` is
more general, but `!=` is convenient for learning and debugging.

At a high level, the compilation process is as follows:

First, a runic expression is parsed into an abstact syntax tree, called a
`twig`:

    text => twig

This can be tested at the command line with `++ream`:

    ~hoclur-bicrel/try=> (ream '|=  [@ @ud]  +<')
    [ %brts
      p=[p=[%axil p=[%atom p=~.]] q=[%axil p=[%atom p=~.ud]]]
      q=[%cnts p=~[[%.y p=6]] q=~]
    ]

This is simply a parser -- no compilation happens here.  The details of the
parser can be found in the syntax section of this doc.

Next, we get into the real meat of the compiler.  In the compiler proper, which
is `++mint` in `++ut`, we take a twig and a subject type and turn it into a
product type and a blob of nock.

    [subject-type twig] => [product-type nock-formula]

For example, we can call `++mint` on the twig we produced earlier with a
subject type of any noun (pardon the necessity for splitting the input line).

    ~hoclur-bicrel/try=> (~(mint ut %noun) %noun
        [%brts [[%axil [%atom ~.]] [%axil [%atom ~.ud]]] [%cnts ~[[%.y 6]] ~]])

    [   p
      [ %core
        p=[%cell p=[%cell p=[%atom p=%$] q=[%atom p=%ud]] q=%noun]
          q
        [ p=%gold
          q=[%cell p=[%cell p=[%atom p=%$] q=[%atom p=%ud]] q=%noun]
          r=[p=[0 6] q={[p=%$ q=[%ash p=[%cnts p=~[[%.y p=6]] q=~]]]}]
        ]
      ]
      q=[%8 p=[%1 p=[0 0]] q=[p=[%1 p=[0 6]] q=[%0 p=1]]]
    ]

Note that in the result we get first a type, which is a gold core with an ash
gate.  The second part, though, is exactly the nock that was produced by
`++make`.  It looks a little different because it has some faces on it, but it
is in fact the same.

The astute reader will notice that we casually starting referring to types
without defining them.  We must rectify this heinous atrocity.

A "type" is simply a set of possible values, combined with a set of semantics
for operating on these values.  These are defined in Hoon as one of the ten
kinds of type found in `++type`.  At its most general, the `noun` type contains
all Hoon nouns.  Everything in Hoon is a noun.  The `%void` type contains no
values at all.  The `%atom` type contains only atoms.  The `%cube` type is
parameterized by a value, and it contains only that single value.  The other
types are defined in the lexicon.

Our type inference algorithms work well enough that in Hoon there is no direct
syntax for defining or declaring a type. There is only a syntax for
constructing twigs.  Types are always produced by inference.  Of course, since
we often need to refer to types, there are a number of runes that are specfically
used for referring to types.  These runes act on `tile`s.

What are the usual things we want to do with types?  We sometimes want to test
if some value is of a particular type.  We sometimes want to declare that a
gate takes a particular type of argument, or that it produces a particular type
of result.  We sometimes want to coerce an untyped value into a type so that we
always know what type we are operating on.  We could write twigs to do each of
these things for every type we wish to use, and our type inference algorithms
will figure out what type we're talking about each time.

It would work, but it would be miserable.  Thankfully, there's an easier way.
It is possible to generate twigs for each of the above use cases from two
pieces of information:  (1) a tile describing a type and (2) a reference to
which case we want to generate.  This eliminates much of the tedious
boilerplate.

The first and most important thing to remember about tiles is that tiles are
not types.  The second and nearly as important thing to remember about tiles
is that every tile has a unique "icon", which is an associated type.  A tile
is not a type, but every tile corresponds to some type.  But what is a tile?
A tile is a nice little representation of a type that may be acted upon and
reduced in several ways.  Remember, types never show up in our code -- they are
always produced by inference.  But sometimes it's convenient to have little
macros to do all the little things we usually want to do with types without
having to rewrite them for every type.

Formalizing the operations on a tile, there are exactly four.  We will briefy
describe them here, but they are documented more thoroughly elsewhere.

Bunting a tile is simply creating a blank default example of the tile's icon.
This may seem to have limited usefulness, but this is actually the most common
use of tiles.  This is due to the the way in which we create, for example,
gates.  The sample of a gate is usually defined in terms of a tile, as in `|=
[p=@ q=@]  (add p q)`.  The `|=` rune takes a tile (in this case `[p=@ q=@]`
and bunts it to create a default sample of `[0 0]` with the type of a cell of
two atoms.  Note that this is different from `|=  [p=%0 q=%0]  (add p q)`,
which still bunts to create a default sample of `[0 0]`, but whose type is a
cell of two constant `0`s.  In the first case, when we call our gate, the type
checker will make sure that we are only ever replacing the sample with a cell
of two atoms, while in the second case the type checker will enforce that we
are only ever replacing the sample with a cell of two `0`s.

Clamming a tile is creating a gate whose sample is a noun and whose product is
a member of the icon.  This allows us to coerce any noun into any type.  If the
noun is not a member of that type, then we produce some member of the type that
is either in some sense "similar" to the given noun or else we simply give a
default value, usually the bunt of this tile.

Fishing a tile is testing whether a particular value is of that type.  This
returns a loobean.  This obviously has many uses.

Whipping a tile is something you'll never have to do directly.  Whipping is
never directly used in any Hoon source code.  Whipping is used internally by
clamming.

In summary, a tile is simply a convenient syntax for creating well-typed nouns.
Say that again, *a tile is simply a convenient syntax for creating well-typed
nouns.*  A tile is not a tiwg, but tiles always are reduced statically in one
of four ways to a twig.  `++tile` is a sort of intermediate representation
between text and twigs that is often used when we're referring to types.

Returning from our digression about types and tiles to our discussion of the
compilation process, recall that `++mint` takes a subject-type and a twig and
compiles it into a product-type and a blob of nock.

    [subject-type twig] => [product-type nock-formula]

As we compile each twig, we compile it against a subject type.  During the
compilation of a twig, we obviously don't have access to the value of the
subject (else compilation would include running the code).  We do, however,
have some guarantees about its value.  We have its type.

Most runes don't change the subject type, but some do.  In the most simple
example, consider `=>  p  q`, which means to evaluate `q` with a subject of
`p`.  Here, the subject type of `q` is simply the type of `p`.

In a slightly more complicated example, consider `?:  p  q  r`, which means
simply to evaluate `q` if `p` is true, else evaluate `r`.  When compiling `q`,
we get to assume in the subject type that `p` is true while when compiling `r`,
we get to assume in the subject type that `p` is false.  This is used to great
practical purpose, for example, when handling lists.  If `p` is a test whether
the list is empty (which would be `?=(~ l)`), then in `q` we can assume that
the list is empty while in `r` we know that the list has a head and a tail.
Without that test, if you attempt to refer to the head of the list, the
compiler will complain that it cannot verify there is even a head to which to
refer.  Thus, the compiler will generate `find-limb` and `find-fork` errors.
The `find-limb` refers to its inability to find the head of the list while the
`find-fork` refers to the fact that it's looking in a fork type -- that is, it
believes that the values is of one of multiple types, and in at least one of
the constituent types there is no head.  The error messages are described in
detail in the lexicon.

Recapping, when compiling Hoon code, we compile each individual twig against
the subject with which it will eventually be called.  The product is a nock
formula and the type of value that it may produce.  Thus, both the nock formula
and the product type may depend on the both the subject type and the twig in
question.

It's obvious that the product type will usually depend on the subject type, but
it's less obvious when the nock formula will depend on the type.  It does,
however, happen at times.  When resolving a face, for example, the axis that
ends up in the nock formula depends on the where the face is in the subject.
We only know this because faces are in the subject type.  Thus, in `=>  [p=42
q=1.337]  p`, the `p` twig compiles to nock `[0 2]` while in `=> [q=42 p=1.337]
p`, the `p` twig compiles to nock `[0 3]`.  This is true even though the actual
nock produced by `[p=42 q=1.337]` is the same as that produced by `[q=42
p=1.337]`.  Thus, the nock formula may depend on the subject type.  It is for
this reason that we say that a type defines not only a set of values, but also
the semantics for operating on those values.

As long as some value is in the subject type, you can run it against the
produced nock formula as `*[subject formula]` and get a value in the product
type.

We've ignored one question thus far:  it's all well and good once we've started
compiling, for we know the subject type.  But what subject type do we start
with?  We could, of course, put some restrictions on the subject type of
compiled Hoon code, but (1) there's no reason to do that, and (2) since this
will be returned as nock, and nock is untyped, the compiler cannot actually
make any guarantees about what subject it will be called with.  Thus, we start
the compilation with a subject type of all nouns.





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

