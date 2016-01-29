---
next: true
sort: 0
spam: true
title: Hoon 101.0: nouns, spans, and molds
---

# Hoon 101.0: nouns, spans and molds

Hoon is a strict, higher-order typed pure-functional language.

Why Hoon?  Typed functional languages are known for a pleasant
phenomenon: once your code compiles, it's quite likely to work.
But most typed functional languages are conceptually dependent on
abstract advanced math, and difficult to understand without it.

Hoon is a typed FP language for the common street programmer.
Well-written Hoon is as concrete and data-oriented as possible.
The less functional magic you use, the better.  But the magic is
there, mostly, if you need it.

The main disadvantage of Hoon is that its syntax and semantics
are unfamiliar.  The syntax will remind too many of Perl, but
like most human languages (and unlike Perl) it combines a regular
core structure with irregular variations.  And Hoon's semantic
complexity is bounded by the size of the compiler: type inference plus code
generation are 2000 lines of Hoon.  Most peoples' experience is that the
language is much easier to learn than it looks.

> The name "Hoon" is from the Wallace Stevens poem, _Tea at the
Palaz of Hoon_.  It also means "hooligan" in Australian.

## How to use this tutorial

Ideally, you've installed an Urbit planet (if you have a ticket)
or comet (if you don't).  See the [user doc](../../../user).  We
recommend opening up the dojo and just typing the examples; you
don't know a language until you know it in your fingers.

## Nouns: data made boring

A noun is an atom or a cell.  An atom is any unsigned integer.  A
cell is an ordered pair of nouns.

Nouns are trees; they have no cycles.  Noun comparison is always 
by value (the programmer can't test pointer equality).  Nouns are 
strict; there is no such thing as an infinite noun.  And nouns are 
immutable.  There's just no way to have any real fun with nouns.

> Nouns are Lisp's S-expressions, minus a lot of hacks, tricks,
and features that made sense 50 years ago.  In particular,
because atoms are not tagged (an atom can encode a string, for
instance), nouns depend on a static type system at a higher
layer.  How do you print an atom if you don't know whether it's a
string or a number?

## A type system for nouns

One obstacle to learning Hoon is that it has two quite distinct
concepts that might equally be called a "type."  Worse, most
other typed functional languages are mathy and share a basically
mathematical concept of "type."  Hoon does not have this concept
at all.  We can't avoid using the T-word occasionally, but it has
no precise meaning in Hoon and can be extremely confusing.

Hoon's two kinds of "type" are `span` and `mold`.  A span is both
a constructively defined set of nouns, and a semantic convention
for users in that set.  A `mold` is a function whose range is
some useful span.  A mold is always idempotent (for any noun `x`,
`f(x)` equals `f(f(x))`), and its domain is any noun. 

One way to explain this is that while a span is what most
languages call a "type," Hoon has no syntax for the programmer to
define a span directly.  Instead, we use inference to define it
as the range of a mold function.  This mold can also be used to
validate or normalize untrusted, untyped data -- a common problem
in modern programming, because networks.

> Sending a noun over the network is a good example of why Hoon
is different.  In a normal modern language, you serialize and
deserialize a data type by extending your type to implement a
serialization interface.  A really good language can process your
type definition and automatically generate this code.  In Hoon,
we have one function `jam` that converts any noun to an atom,
and another `cue` that inverts `jam`.  To validate, the receiver
applies its own mold to the cued noun, and we've sent typed data
over the network without any attack surface (except `jam` and
`cue`, which fit on a page).  No custom serialization code,
manual or generated, is required.  The mold itself is not sent;
protocol agreement is out of band.

Hoon's inference algorithm is dumber than the unification
algorithms (Hindley-Milner) used in most typed functional
languages.  Hoon thinks only forward, not backward.  Eg, Haskell
can infer the result type of a function from its argument
(forward), or the argument type from the result (backward).
Hoon can do the first but not the second.

So Hoon needs more manual annotations, which you usually want
anyway for prosaic software-engineering reasons.  Otherwise its
typesystem solves more or less the same job, including
pattern-matching, genericity / typeclasses, etc.

> A good test of a static higher-order typesystem is whether it can
infer the product type of a grammar defined as a combinator
parser.  The Hoon parser passes this test; when it typechecks, 
the parser's range nests within the span of the expression mold.

## Let's make some nouns

Let's make a noun:
```
~tasfyn-partyv:dojo> 42
```
You'll see the expression you entered, then the result:
```
> 42
42
```
Let's try a different noun.  Or is it different?
```
~tasfyn-partyv:dojo> 0x2a
```
You'll see:
```
> 0x2a
0x2a
```
`42` and `0x2a` are actually *the same noun*, because they're the
same number.  But we don't just have the noun to print - we have
a `[span noun]` cell (sometimes called a `vase`).

As you recall, a span defines a set of nouns and a semantic
interpretation.  As sets, both spans here are "any number".  But
semantically, `42` has a decimal span and `0x2a` hexadecimal, so
they print differently.

> It's important to remember that Hoon is a statically typed language.
We don't work with vases unless we're dynamically compiling code,
which is of course what we're doing here in the dojo.  In Hoon,
dynamic type equals static type plus runtime compilation.

Let's make some cells.  Try these on your own urbit:
```
~tasfyn-partyv:dojo> [42 0x2a]
~tasfyn-partyv:dojo> [42 [0x2a 420]]
~tasfyn-partyv:dojo> [42 0x2a 420]
```
We observe that cells associate right: `[a b c]` is just another
way of writing `[a [b c]]`.

Lisp masters beware: Hoon `[a b]` is Lisp `(a . b)`, Lisp
`(a b)` is Hoon `[a b ~]`.  `~` means nil, with value zero.  Lisp
and Hoon are both pair-oriented languages down below, but Lisp
has a layer of sugar that makes it look list-oriented.  Hoon
loves its "improper lists," ie, tuples.

## Looking at spans

What are these mysterious spans?  We can see them with the `?`
prefix, which prints the span along with the result.  Moving to
a more compact example format:
```
~tasfyn-partyv:dojo> ? 42
  @ud
42
~tasfyn-partyv:dojo> ? 0x2a
  @ux
0x2a
```
`@ud` and `@ux` stand for "unsigned decimal" and "unsigned hex,"
obviously.

> What is this span syntax?  We only derive spans through
inference.  So there's no parsing grammar for a span.  We have to
be able to print spans, if only for debugging and diagnostics,
but the syntax is output-only.  As in this case, it often looks
like the `mold` syntax, but the two are at opposite ends of the
type food chain.

## Looking at spans, part 2

Usually, good style in Hoon is concrete style.  When a Hoon
programmer defines an abstract semantic value in terms of a noun,
we rarely put a conceptual layer of abstraction between value and
noun.  We think of the value as an interpretation of the noun.
We don't think of the noun as an implementation of the noun.

But rules are made to be broken.  With the `?` command, we *do*
use an abstract layer, by printing our span noun in that custom
syntax.  But we can also look at the span noun directly, with the
`??` command.  As we'll see, `??` is mainly for newbies, but
newbies love it.

```
~tasfyn-partyv:dojo> ?? 42
  [%atom %ud]
42
~tasfyn-partyv:dojo> ?? [42 0x2a]
  [%cell [%atom %ud] [%atom %ux]]
[42 0x2a]
```
What is this `%atom` syntax?  Is it a real noun?  Can anyone
make one?
```
~tasfyn-partyv:dojo> %atom
%atom
~tasfyn-partyv:dojo> %foo
%foo
~tasfyn-partyv:dojo> [%foo %bar]
[%foo %bar]
```
What's the span of one of these symbols?
```
~tasfyn-partyv:dojo> ? %foo
  %foo
%foo
~tasfyn-partyv:dojo> ?? %foo
  [%cube 7.303.014 [%atom %tas]]
%foo
```
This takes a little bit of explaining.  `7.303.014` is just the
Urbit (and German) way of writing the English number `7,303,014`,
or the Urbit hex number `0x6f.6f66`, or the string "foo" as an
unsigned integer with least-significant byte first.

A `%cube` span is a constant -- a set of one noun, the atom
`7.303.014`.  But we still need to know how to print that noun.
In this case, it's an `[%atom %tas]`, ie, a text symbol.

Cubes don't have to be symbols -- in fact, we can take the
numbers we've just been using, and make them constants:
```
~tasfyn-partyv:dojo> %42
%42
~tasfyn-partyv:dojo> ? %42
  %42
%42
~tasfyn-partyv:dojo> ?? %42
  [%cube 42 [%atom %ud]]
%42
```

> Why `??`? Spans are an exception to concrete style, because they
use "manual laziness" to define logically recursive structures.
A recursive span contains Hoon code which is evaluated to apply
it.  In practice, this noun often contains the entire Urbit
kernel, so you wouldn't want to try to print it in the dojo.  If
you find `??` taking a weirdly long time, this may have happened;
press ^C.

## Our first mold

After seeing a few span examples, are we ready to describe the
set of all spans with a Hoon mold?  Well, no, but let's try it
anyway.  Ignore the syntax (which we'll explain later; this is a
tutorial, not a reference manual), and you'll get the idea:
```
++  span
  $%  [%atom p=@tas]
      [%cell p=span q=span]
      [%cube p=* q=span]
  ==
```
This mold is not the entire definition of `span`, just the cases
we've seen so far.  In English, a valid span is either:

- a cell with head `%atom`, and tail some symbol.
- a cell with head `%cell`, and tail some pair of spans.
- a cell with head `%cube`, and tail a noun-span pair.

The head of a span is essentially the tag in a variant record,
a pattern every programming language has.  To use the span, we
look at the head and then decide what to do with the tail.

> A conventional naming strategy for simple, self-explaining
structures is to name the legs of a tuple `p`, `q`, `r`, `s` and
`t`.  If you get all the way to `t`, your noun is probably not
simple or self-explaining; meaningful names are recommended.

## Progress

Believe it or not, at this point we understand nouns completely.
We don't understand spans and molds completely, but we get the
basics.  In the [next chapter](1-twigs), we'll see how Hoon
expressions (twigs) turn one noun into another.
