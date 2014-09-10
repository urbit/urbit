Morphology
==========

Hoon is a statically typed language that compiles to Nock. 

Types
-----

Types are nouns the compiler keeps around as it turns your Hoon into Nock. 

A type serve two purposes:

1. It defines a set of nouns. Any finite noun is either in this set, or not in
   it.

2. it ascribes semantics to all nouns in this set. For example, a Hoon type
   exports a semantic namespace.


These are defined in Hoon as one of the ten kinds of type found in `++type`.  

At its most general, the `noun` type contains all Hoon nouns.  Everything in
Hoon is a noun.  The `%void` type contains no values at all.  The `%atom` type
contains only atoms.  The `%cube` type is parameterized by a value, and it
contains only that single value.  The other types are defined in the lexicon.

Type inference in Hoon works well enough that there is no direct
syntax for defining or declaring a type. There is only a syntax for
constructing twigs.  Types are always produced by inference.


When resolving a face, for example, the axis that
ends up in the nock formula depends on the where the face is in the subject.
We only know this because faces are in the subject type.  Thus, in `=>  [p=42
q=1.337]  p`, the `p` twig compiles to nock `[0 2]` while in `=> [q=42 p=1.337]
p`, the `p` twig compiles to nock `[0 3]`.  This is true even though the actual
nock produced by `[p=42 q=1.337]` is the same as that produced by `[q=42
p=1.337]`.  Thus, the nock formula may depend on the subject type.  It is for
this reason that we say that a type defines not only a set of values, but also
the semantics for operating on those values.


Tiles
-----

What are the usual things we want to do with types?  

- Test if a noun is of a particular type. 

- Create a blank default example of a type

- Coerce a noun into a type 

It is possible to generate twigs for each of the above use cases from two
pieces of information:  (1) a tile describing a type and (2) a reference to
which case we want to generate.  

Tiles are not types.  

Every tile has a unique associated type, or "icon".

A tile is not a type, but every tile corresponds to some type.  

Formalizing the operations on a tile, there are exactly four.

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
  
A tile is not a twig, but tiles always are reduced statically in one
of four ways to a twig.  


Type Inference
--------------

Hoon is a higher-order typed functional language. Most languages in this class,
Haskell and ML being prominent examples, use something called the
Hindley-Milner unification algorithm. Hoon uses its own special sauce instead.

Hoon's philosophy is that a language is a UI for programmers, and the basic
test of a UI is its predictability. It is impossible (for most programmers)
to learn a language properly unless they know what the compiler is doing, which
in practice means mentally stepping through the algorithms it uses (with the
exception of semantically neutral optimizations). Haskell is a hard language to
learn (for most programmers) because it's hard (for most programmers) to follow
what the Haskell compiler is thinking.


Broadly speaking, type inference in Hoon has three general limitations as
compared to Hindley-Milner inference.

1. Hoon does not think backwards. For instance, it cannot infer a function's
   argument type (or rather, a gate's sample type) from its body.

2. Hoon can infer through tail recursion, but not head recursion. It can check
   head recursion, however, given an annotation.

3. The compiler catches most but not all divergent inference problems - i.e.
   you can put the compiler into an infinite loop or exponential equivalent.
   An interrupt will still show you your error location.  

Although an inference algorithm that reasons only forward must and does require
a few more annotations from the programmer, the small extra burden on her
fingers is more than offset by the lighter load on her hippocampus.
Furthermore, programs also exist to be read. Some of these annotations (which a
smarter algorithm might infer by steam) may annoy the actual author of the code
but be a lifesaver for her replacement.

Our experience is that these limitations are minor annoyances at worst and
prudent restrictions at best. Your mileage may vary.

Type inference is a frightening problem, especially if you've been exposed to
a wall of math. Your only real problem in learning Hoon is to learn not to
fear it. Once you work past this reasonable but entirely unfounded fear of
inference, your Hoon experience will be simple, refreshing and delightful. So
first, let's talk through a few reassuring facts:

1. Type inference in Hoon never sees a tile. It operates exclusively on twigs.
   All tiles and synthetic twigs are reduced to natural twigs for the inference
   engine's benefit.

2. The semantics of Hoon are in ++ut in hoon.hoon, and nowhere else.

3. Within ++ut, all the semantics of Hoon are in the call graph of one arm: ++mint. 
   ++mint has a case for every natural hoon. So do ++play and ++mull,
   but their semantics are redundant with ++mint.

4. One leg in the sample of ++mint - gol - which looks for all the world like a
   mechanism for backward inference, is not. It is semantically irrelevant and
   only exists to get better localization on error reports.

5. ++mint is the gate that maps [type twig] to [type nock]:

        [subject-type twig] => [product-type nock-formula]

   When we have a type that describes the subject for the formula we're trying to
   generate, as we generate that formula we want to also generate a type for the
   product of that formula on that subject. As long as subject-type is a
   correct description of some subject, you can take any twig and compile it
   against subject-type, producing a formula such that *(subject formula) is a
   product correctly described by product-type.


Compilation
------------

`++make` is a top-level function that turns text into nock.

    ~hoclur-bicrel/try=> `*`(make '|=  [@ @ud]  +<')
    [8 [1 0 0] [1 0 6] 0 1]

Another way to do this is with `!=`.

    ~hoclur-bicrel/try=> !=(|=([@ @ud] +<))
    [8 [1 0 0] [1 0 6] 0 1]

`++make` is more general, in that it can be called on programmatically
generated text, but `!=` is convenient for learning and debugging.

The compilation process is as follows:

First, a runic expression is parsed into an abstact syntax tree, called a
`twig`:

    text => twig

Parsing code into a `twig` can be done with `++ream`:

    ~hoclur-bicrel/try=> (ream '|=  [@ @ud]  +<')
    [ %brts
      p=[p=[%axil p=[%atom p=~.]] q=[%axil p=[%atom p=~.ud]]]
      q=[%cnts p=~[[%.y p=6]] q=~]
    ]

Refer to the Syntax section for more detail on parsing.

The compiler proper, is `++mint` in `++ut`. 

++mint takes a twig and a subject type and produces a product type and a nock formula.

    [subject-type twig] => [product-type nock-formula]

For example, we can call `++mint` on the twig we produced earlier with a
subject type of any noun:

    ~hoclur-bicrel/try=> 
    (~(mint ut %noun) %noun [%brts [[%axil [%atom ~.]] [%axil [%atom ~.ud]]] [%cnts ~[[%.y 6]] ~]])

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

Note that the head of the above is a type, which in this case is a gold core
with an ash gate.  The second part, though, is (with a few labels, or faces added) the nock that was
produced by `++make`. 
