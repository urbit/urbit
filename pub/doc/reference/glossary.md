<div class="short">

Glossary
========

### [arm](#arm)

A key-value pair of a name ([++term]()) to an expression ([++foot()]).
Used primarily in [core]() construction. Arms can contain either
functions or data. You can think of them like named properties inside an
object.

------------------------------------------------------------------------

### [atom](#atom)

An atom is a natural number. More here..?

------------------------------------------------------------------------

### [axil](#axil)

An `[%axil p=base]` is a simple [`++tile`]() for a few basic icons: an
atom of any odor, a noun (`*`) , a cell of nouns (`^`), a loobean (`?`),
and null (`~`).

------------------------------------------------------------------------

### [battery](#battery)

[Cores], at the most basic level, are of the structure [battery
payload]. The battery consists of the code contained within a core.

------------------------------------------------------------------------

### [`%bark`](#bark)

A `[%bark p=term q=tile]` is a [`++tile`]() with a name wrapped around
it. Its [icon]() is a [`++face`](). The rune associated with a
[`%bark`]() is [`$=`]().

------------------------------------------------------------------------

### [bunt](#bunt)

The bunt of a [`++tile`]() produces a [`++twig`]() that creates a blank
default example its [icon](). Bunting is like explicitly asking for the
default value of a type. Unlike in other languages, this always exists
in Hoon. See also [`$*`]().

------------------------------------------------------------------------

### [`%bush`](%bush)

a [`[%bush p=tile q=tile]`]() is a [`++tile`]() in which there are two
kinds of [nouns](): cells whose head is a cell (`++tile` p) and cells
whose head is an atom (`++tile` q). Its default value is the value of
`q`, and its icon is a [`++fork`]()

------------------------------------------------------------------------

### [cell](#clam)

A cell is an ordered pair of nouns.

------------------------------------------------------------------------

### [clam](#clam)

The clam of a [`++tile`]() is a [gate]() that accepts an arbitrary
[noun]() and always produces a member of the [icon]() of the `++tile`.
If the gate is passed a [sample]() that is a member of the icon, it will
produce that sample. If the gate is passed a noun outside of the domain
of the icon, it will produced the [bunt]() of the icon. You can think of
a clam as a validator function for an icon. To clam a `++tile` is to
produce its clam. See also: [`$,`](). SEE ALSO!!

------------------------------------------------------------------------

### [context](#context)

In [gate]() construction, an arm is pulled from a [core]() and pushed
onto the subject creating a structure of [formula [sample context]],
where the context is the previous subject, commonly a core. In Hoon, the
whole kernel is typically included in your subject, so you can think of
context in hoon in a similar way to context in the traditional
functional programming sense.

------------------------------------------------------------------------

### [cons](#cons)

Cell constructor, similar to [cons in other functional
languages](http://en.wikipedia.org/wiki/Cons). Constructs a cell
containing two [`++twigs`]() into a twig that produces a cell of the
results of the two original sub-twigs.

------------------------------------------------------------------------

### [core](#core)

At the Nock level, a core is any [subject]() that contains both code and
data, named battery and payload respectively. At the Hoon level, a core
is very similar to an object with named properties that can be either
functions or data. For more information, see the [`|` rune section]() of
the Hoon library.

------------------------------------------------------------------------

### [`%cube`](#cube)

------------------------------------------------------------------------

### [door](#door)

A door is a [core]() with a sample. Door are used.../you can think of
doors...

------------------------------------------------------------------------

### [dry](#dry)

In a dry computation, typechecking occurs at compile-time to ensure that
all inputs match its [sample]() [++tile](). The formal term for dry is
`%ash`.

------------------------------------------------------------------------

### [engine](#engine)

Engines are [core]()s that contain several [arm]()s that each perform
one of a related set of operations on the core's sample. For example,
there is a container engine for all of the set operations. You can think
of engines as objects with methods that modify its data.

------------------------------------------------------------------------

### [`%face`](#%face)

------------------------------------------------------------------------

### [fern](#fern)

A `[%fern p=[i=tile t=(list tile)]]` is a [`++tile`]() for a non-empty
list of cases. Its icon is naturally a [`%fork`](). The programmer is
responsible for ensuring that the cases are actually orthogonal (unlike
with the structured `%fork`s, [`%bush`](), [`%kelp`]() and [`%reed`]).

------------------------------------------------------------------------

### [fishing](#fishing)

To fish is to test if a [noun]() matches a specific `++tile`, using the
natural rune [`?=`](). Some languages call fishing "pattern matching".

------------------------------------------------------------------------

### [frond](#frond)

A frond is a case of a [kelp](), which is a [discriminated (or tagged)
union](http://en.wikipedia.org/wiki/Tagged_union).

------------------------------------------------------------------------

### [gate](#gate)

A [gate]() is a [core]() with one arm [`$`]() with a [payload]() that is
a cell of the form [[sample]() [context]()]. Gates are the closest thing
Hoon has to functions in the traditional sense.

------------------------------------------------------------------------

### [`%gold`](#gold)

------------------------------------------------------------------------

### [herb](#herb)

An `[%herb p=twig]`....

------------------------------------------------------------------------

### [icon](#icon)

The icon of a [`++tile`]() is the type associated with that `++tile`. A
`++tile` is a convenient way of specifying a type, which is its icon.
`++tile`s are used in a similar way to [type signatures]() for their
icons.

------------------------------------------------------------------------

### [`%iron`](#%iron)

`%iron` is a variance type for [cores]() where their [sample]()s cannot
be read. You can think of can be thought of as similar to a private
function.

Not quite sure about this one.

------------------------------------------------------------------------

### [`%kelp`](%kelp)

a [`%kelp p=[i=line t=(list line)]`] is a [discriminated, or tagged,
union](http://en.wikipedia.org/wiki/Tagged_union). In Hoon, the head,
which is called the stem, must be a [`%leaf`](). The tail, which can be
anything, is the bulb. Cases of a kelp are known as [fronds]().

------------------------------------------------------------------------

### [kick](#kick)

To pull the empty name `$` on a core is to kick it. You can think of
kicking like calling a function with its default arguments.

------------------------------------------------------------------------

### [noun](#noun)

A noun is an [atom]() or a [cell](). Everything in Hoon is a noun.

------------------------------------------------------------------------

### [`%$`](#$)

`%$`, or `$` for short, is the empty name in Hoon.

------------------------------------------------------------------------

### [leg](#leg)

If the result of [pulling]() something from `x` is a subtree, then it is
a leg.

More here? Existing doc isn't quite clear here..

------------------------------------------------------------------------

### [`%lead`](%lead)

------------------------------------------------------------------------

### [`%leaf`](#%leaf)

A `%leaf` is a [`++tile`]() consisting of an atomic constant of value
`q` and odor `p`. Its icon is a [`%cube`](). The syntax for a leaf is
the same as the syntax for a [`++twig`](), except that % is never
required to generate a cube. For instance, as a twig, 7 has a type of
[%atom %ud]; %7 has a type of [%cube 7 [%atom %ud]]. But the icon of the
leaf 7 is, again, [%cube 7 [%atom %ud]].

Copied the bottom half from existing doc. Not sure about this one...

------------------------------------------------------------------------

### [loobean](#loobean)

------------------------------------------------------------------------

### [payload](#payload)

[Cores](), at the most basic level, are of the structure [battery
payload]. The payload consists of the data contained within a core. You
can think of the payload as similar to the data of an object.

------------------------------------------------------------------------

### [pull](#pull)

To access a [wing]() or [limb]() in a [core]() is to pull it. For
instance, when we write `a.b.x` (a within b from x), we are pulling the
wing `a.b` from `x`.

------------------------------------------------------------------------

### [`%reed`](#reed)

A `[%reed p=tile q=tile]` is a [`++tile`]() whose [icon]() contains two
kinds of nouns: atoms of `++tile` `p` and cells of `++tile` `q`. The
rune associated with reeds is [`$|`]().

------------------------------------------------------------------------

### [sample](#sample)

In [gate]() construction, an arm is pulled from a [core]() and pushed
onto the subject creating a structure of [formula [sample context]],
where the sample represents the gate's inputs. All gates are constructed
with a default sample value. Thus, when we call a gate with arguments,
we are actually replacing its sample.

------------------------------------------------------------------------

### [slam](#slam)

To pull the empty name `$` on a [gate]() `g` with its [sample]()
replaced by a given input `a` is to slam `g` with `a`. You can think of
slamming like passing input parameters to a function that's being
called.

------------------------------------------------------------------------

### [subject](#subject)

All Hoon expressions a parsed into abstract syntax trees, which in Hoon
are called [++twig]()s. Twigs are [nouns]() that are converted into Nock
expressions, which are all of the basic form [subject formula], where
the subject is the data and the formula is the program. Thus, in both
Hoon and Nock, subject can refer to any piece of data that is being
operated on by a formula.

------------------------------------------------------------------------

### [`++tile`](#++tile)

A `++tile` is a convenient way of specifying a type, which is its icon.
`++tile`s are used in a similar way to [type signatures]() for their
icons.

SOMETHING ABOUT THE DIFFERENCE BETWEEN TWIG AND TILE AUTOCONS.

------------------------------------------------------------------------

### [weed](#weed)

A `[%weed p=twig]`

------------------------------------------------------------------------

### [wet](#wet)

In wet computations, the product type is checked to be the same as the
input type, rather than the [sample]() [tile](). The formal term for wet
is `%elm`.

------------------------------------------------------------------------

### [wing](#wing)

A wing is a list of limbs. For example, when we [pull] `a.b` from `x`,
`a.b` is a wing. `a` and `b` individually are both [limbs]().

------------------------------------------------------------------------

</div>
