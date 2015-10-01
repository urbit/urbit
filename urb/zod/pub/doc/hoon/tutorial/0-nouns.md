# Hoon 0: introduction

Hoon is a strict, higher-order typed pure-functional language.

Why Hoon?  On the one hand, typed functional languages are known
for a particularly pleasant phenomenon: once your code compiles,
it's quite likely to work.  On the other hand, most typed
functional languages are influenced by advanced mathematics.
As Barbie once put it, math class is hard.

Hoon is a typed FP language for the common street programmer.
Well-written Hoon is as concrete and data-oriented as possible.
The less functional magic you use, the better.  One Haskell
hacker described Hoon as "imperative programming in a functional
language."  He didn't mean this as a compliment, but we choose to
take it as one.

Moreover, one task of a type system in network computing is
marshalling typed data on the sender, and validating untrusted
data on the receiver.  Hoon is very good at this task, which in
most typed languages is an afterthought at best.

The main disadvantage of Hoon is that its syntax and semantics
are unfamiliar.  The syntax will remind too many of Perl, but
like most human languages (and unlike Perl) it combines a regular
core structure with irregular variations.  Its semantic
complexity is bounded by the fact that the compiler is only 2000
lines of Hoon (admittedly an expressive language).  Most peoples'
experience is that Hoon is much easier to learn than it looks.

## Nouns: data made boring

A noun is an atom or a cell.  An atom is any unsigned integer.  A
cell is an ordered pair of nouns.

The noun is an intentionally boring data model.  Nouns (at least,
nouns in Urbit) don't have cycles (although a noun implementation
should take advantage of acyclic graph structure).  Noun
comparison is always by value (there is no way for the programmer
to test pointer equality).  Nouns are strict; there is no such
thing as an infinite noun.  And, of course, nouns are immutable.
So there's basically no way to have any real fun with nouns.

For language historians, nouns are Lisp's S-expressions, minus a
lot of hacks, tricks, and features that made sense 50 years ago.
In particular, because atoms are not tagged (an atom can encode a
string, for instance), nouns work best with a static type system.
How do you print an atom if you don't know whether it's a string
or a number?  You can guess, but...

## A type system for nouns

So learning nouns in practice involves learning them with a type
system that makes them usable.  Fortunately, we have that.

One obstacle to learning Hoon is that it has two quite distinct
concepts that might equally be called a "type."  Worse, most 
other typed functional languages are mathy and share a basically
mathematical concept of "type."  We can't avoid using the T-word
occasionally, but it has no precise meaning in Hoon and can be
extremely confusing.

Hoon's two kinds of "type" are `span` and `mold`.  A span is both
a constructively defined set of nouns, and a semantic convention
for users in that set.  A `mold` is a function whose range is
some useful span.  A mold is always idempotent (for any noun x,
`f(x)` equals `f(f(x))`), and its domain is any noun.

(One way to explain this is that while a span is what most
languages call a "type," Hoon has no way for the programmer to
express a span directly.  Instead, we use inference to define it
as the range of a function.  This same function, the mold, can
also be used to validate or normalize untrusted, untyped data --
a common problem in modern programming.)

(Hoon's inference algorithm is somewhat dumber than the
unification algorithms (Hindley-Milner) used in most typed
functional languages.  Hoon reasons only forward, not backward.
It needs more manual annotations, which you usually want anyway.
Otherwise, it gets more or less the same job done.)

## Let's make some nouns

This stuff isn't even slightly hard.  Let's make a noun:
```
~tasfyn-partyv:dojo> 42
```
You'll see the expression you entered, then the resulting value:
```
> 42
42
```
Let's try a different value: 
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

(It's important to note that Hoon is a statically typed language.
We don't work with vases unless we're dynamically compiling code,
which is of course what we're doing here in the shell.  Dynamic
type is static type compiled at runtime.)

Finally, let's make some cells.  Try these on your own ship:
```
~tasfyn-partyv:dojo> [42 0x2a]
~tasfyn-partyv:dojo> [42 [0x2a 420]]
~tasfyn-partyv:dojo> [42 0x2a 420]
```
We observe that cells associate right: `[a b c]` is just another
way of writing `[a [b c]]`.

Also, Lisp veterans beware: Hoon `[a b]` is Lisp `(a . b)`, Lisp
`(a b)` is Hoon `[a b ~]`(`~` represents nil, with a value of atom `0`). Lisp and Hoon are both pair-oriented
languages down below, but Lisp has a layer of sugar that makes it
look list-oriented.  Hoon loves its "improper lists," ie, tuples.

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
obviously.  But what is this syntax?

We only derive spans through inference.  So there's no language
syntax for a span.  We have to be able to print spans, though, if
only for debugging and diagnostics.  `@ud` is an print-only
syntax.  (In this case it happens to be the same as the `mold` 
syntax, but that's just a coincidence.)

## Looking at spans, part 2

A good way to teach yourself to think in nouns is to look not at
the prettyprinted span, but at the actual noun it's made of.
Since everything in Hoon is a noun, a span is a noun too.  When
we use `??` rather than `?` as a prefix, we see the noun:
```
~tasfyn-partyv:dojo> ?? 42
  [%atom %ud]
42
~tasfyn-partyv:dojo> ?? [42 0x2a]
  [%cell [%atom %ud] [%atom %ux]]
[42 0x2a]
```
What is this `%atom` notation?  Is it a real noun?  Can anyone
make one?
```
~tasfyn-partyv:dojo> %atom
%atom
~tasfyn-partyv:dojo> %foo
%foo
~tasfyn-partyv:dojo> [%foo %bar]
[%foo %bar]
```
What if we look at the span?
```
~tasfyn-partyv:dojo> ? %foo
  %foo
%foo
~tasfyn-partyv:dojo> ?? %foo
  [%cube 7.303.014 %atom %tas]
%foo
```
This takes a little bit of explaining.  First of all, `7.303.014`
is just the German (and Urbit) way of writing `7,303,014`, or the
hexadecimal number `0x6f.6f66`, or the string "foo" as an
unsigned integer.  (It's much easier to work with large integers 
when the digits are grouped.)  Second, remembering that cells
nest right, `[%cube 7.303.014 %atom %tas]` is really `[%cube
7.303.014 [%atom %tas]]`.

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
  [%cube 42 %atom %ud]
%42
```

## Our first mold

After seeing a few span examples, are we ready to describe the
set of all spans with a Hoon mold?  Well, no, but let's try it
anyway.  Ignore the syntax (which we'll explain later; this is a
tutorial, not a reference manual), and you'll get the idea:
```
++  span
  $%  [%atom @tas]
      [%cell span span]
      [%cube * span]
  ==
```
This mold is not the entire definition of `span`, just the cases
we've seen so far.  In English, a valid span is either:

- a cell with head `%atom`, and tail some symbol.
- a cell with head `%cell`, and tail some pair of spans.
- a cell with head `%cube`, and tail a noun-span pair.

The head of a span is essentially the tag in a variant record,
a pattern every programming language has.  To use the noun, we
look at the head and then decide what to do with the tail.
