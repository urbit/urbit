---
next: true
sort: 1
spam: true
title: Hoon 101.1: twigs and legs
---
# Hoon 101.1: twigs and legs

In the [last chapter](0-nouns), we learned how to make nouns.  In
this chapter we'll get into Hoon expressions, or *twigs*.

## How to use this tutorial

Ideally, you've installed an Urbit planet (if you have a ticket)
or comet (if you don't).  See the [user doc](../../../user).

We recommend opening up the dojo and just typing the examples;
you don't know a language until you know it in your fingers.
Also, make sure you've worked through the chapters in order.

## Nock for Hoon programmers

Hoon compiles itself to a pico-interpreter called
[Nock](../../nock), a combinator algebra defined in 200 words.  This
isn't the place to explain Nock (which relates to Hoon much as
assembly language relates to C), but Nock is just a way to
express a function as a noun.

Nock is a Turing-complete interpreter shaped like (pseudocode):
```
Nock(problem) => product
```
The `problem` is always a cell `[subject formula]`.   The
function is the `formula`.  The input to the function is the 
`subject`.  The output is the `product`.

## From Hoon to Nock

The Hoon parser turns a source expression (even one as simple as
`42` from the last chapter) into a noun called a `twig`.  If you
know what an AST is, a twig is an AST.  (If you don't know what
an AST is, it's not necessarily worth the student loans.)

To simplify slightly, the Hoon compiler is shaped like:
```
Hoon(subject-span function-twig) => [product-span formula-nock]
```
Hoon, like Nock, is a *subject-oriented* language.  Your code is
always executed against one input noun, the subject.  For any
subject noun in `subject-span` (ie, argument type), the compiler
produces a Nock formula that computes `function-twig` on that
subject, and a `product-span` that is the span of the product
(ie, result type).

> This is really a nontrivial difference.  In a normal,
non-subject-oriented language, your code executes against a
scope, stack, environment, or other variable context, probably
not even a regular user-level value.  For ordinary coders,
"subject-oriented programming" is one of the hardest things to
understand about Hoon; for some reason, your brain keeps wanting
the interpreter state to be more interesting.

## From constants to twigs

In the last chapter we were entering degenerate twigs like `42`.
Obviously a numeric constant doesn't use the subject at all, so
it's not a very interesting example.

Let's save a test subject as a dojo variable:
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] [6 7]]
```
The `=test` command tells the dojo to rearrange its stock subject
to include this `test` noun.  Let's check that it's there:
```
~tasfyn-partyv:dojo> test
[[[8 9] 5] 6 7]
```
> If you're wondering why `[6 7]` got printed as `6 7`, remember
that `[]` associates to the right.  Also, `=test` is not in any
way Hoon syntax; it's dojo syntax.  Every Hoon twig is a valid
dojo command, but not vice versa.

We want to use `test`, this harmless little noun, as the subject
for some equally harmless twigs.  We can do this with the `:`
syntax, which composes twigs in the functional sense.  The twig
`a:b` uses the product of twig `b` as the subject of twig `a`.
Trivial cases:
```
~tasfyn-partyv:dojo> 42:test
42
~tasfyn-partyv:dojo> 42:420
42
```

## Tree addressing

The simplest twigs produce a subtree, or "leg", of the subject.
A cell, of course, is a binary tree.  The very simplest twig is
`.`, which produces the root of the tree - the whole subject:
```
~tasfyn-partyv:dojo> .:test
[[[8 9] 5] 6 7]
```
Like human languages, Hoon is full of irregular abbreviations.
The `.` syntax is a shorthand for `+1`:
```
~tasfyn-partyv:dojo> +1:test
[[[8 9] 5] 6 7]
```
Hoon has a simple tree addressing scheme (inherited from Nock):
the root is `1`, the head of `n` is `2n`, the tail is `2n+1`.
The twig syntax for a tree address is `+n`.

In our example noun, each leaf is its own tree address:
```
~tasfyn-partyv:dojo> +2:test
[[8 9] 5]
~tasfyn-partyv:dojo> +3:test
[6 7]
~tasfyn-partyv:dojo> +4:test
[8 9]
~tasfyn-partyv:dojo> +5:test
5
~tasfyn-partyv:dojo> +6:test
6
~tasfyn-partyv:dojo> +7:test
7
```
> An instinct for binary tree geometry develops over time as you
use the system, rather the way most programmers learn to do
binary math.  No, really.

## Lark syntax

This alternative syntax for a tree address maps noun geometry
directly to a glyph.  Lark syntax creates a recognizable
geometric shape by alternating between two head/tail pairs, read
left to right: `-` and `+`, `<` and `>`.

Thus `-` is `+2`, `+` is `+3`, `+<` is `+6`, `->` is `+5`, `-<+`
is `+9`, etc.

> Why lark syntax?  Code full of numbers is ugly and distracting,
and looks like hardcoded constants.  We actually almost never use
the `+` syntax.

## Simple faces

Tree addressing is cool, but it would be pretty tough to program 
in Hoon if it was the only way of getting data out of a subject.

Let's introduce some new syntax:
```
~tasfyn-partyv:dojo> foo=42
foo=42
~tasfyn-partyv:dojo> ? foo=42
  foo=@ud
foo=42
~tasfyn-partyv:dojo> ?? foo=42
  [%face %foo [%atom %ud]]
foo=42
```
To extend our `++span` mold from the last chapter:
```
++  span
  $%  [%atom p=@tas]
      [%cell p=span p=span]
      [%cube p=* q=span]
      [%face p=@tas q=span]
  ==
```
The `%face` span wraps a label around a noun.  Then we can 
get a leg by name.  Let's make a new dojo variable:
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] foo=[6 7]]
```
The syntax is what you might expect:
```
~tasfyn-partyv:dojo> test
[[[8 9] 5] foo=[6 7]]
~tasfyn-partyv:dojo> foo:test
[6 7]
```
Does this do what you expect it to do?
```
~tasfyn-partyv:dojo> +3:test
foo=[6 7]
~tasfyn-partyv:dojo> ? +3:test
  foo=[@ud @ud]
foo=[6 7]
~tasfyn-partyv:dojo> ?? +3:test
  [%face %foo [%cell [%atom %ud] [%atom %ud]]]
foo=[6 7]
```

## Interesting faces; wings

Let's look at a few more interesting face cases.  First, suppose
we have two cases of `foo`? 
```
~tasfyn-partyv:dojo> =test [[foo=[8 9] 5] foo=[6 7]]
~tasfyn-partyv:dojo> foo:test
[8 9]
```
In the tree search, the head wins.  We can overcome this with a
`^` prefix, which tells the search to skip its first hit:
```
~tasfyn-partyv:dojo> ^foo:test
[6 7]
```
`^^foo` will skip two foos, `^^^foo` three, *ad infinitum*.
But what about nested labels?
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] foo=[6 bar=7]]
~tasfyn-partyv:dojo> bar:test
/~tasfyn-partyv/home/~2015.11.7..21.40.21..1aec:<[1 1].[1 9]>
-find-limb.bar
find-none
```
We can't search *through* a label.  If we want to get our `bar`
out, we need to search *into* it:
```
~tasfyn-partyv:dojo> bar.foo:test
7
```
`bar.foo` is what we call a `wing`, a search path in a noun.
Note that the wing runs from left to right, ie, the opposite of
most languages: `bar.foo` means "bar within foo."

Each step in a wing is a `limb`.  (Most languages use metaphors;
Hoon abuses them.)  A limb can be a tree address, like `+3` or
`.`, or a label like `foo`.  We can combine them in one wing:
```
~tasfyn-partyv:dojo> bar.foo.+3:test
7
```
It's important to note the difference between `bar.foo:test` 
and `bar:foo:test`, even though they produce the same product:
```
~tasfyn-partyv:dojo> bar:foo:test
7
```
`bar.foo` is one twig, which we run on the product of `test`.
That's different from running `bar` on the product of `foo` on
the product of `test`.

> You're probably used to name resolution in variable scopes
and flat records, but not in trees.  Partly this is because the
tradition in language design is to prefer semantics that make it
easy to build simple symbol tables, because linear search of a
nontrivial tree is a bad idea on '80s hardware.

## Mutation

Mutation?  Well, not really.  We can't modify nouns; the concept
doesn't even make sense in Hoon (or Nock).

Rather, we build new nouns which are copies of old ones, but 
with mutations.  Let's build a "mutated" copy of our test noun:
```
~tasfyn-partyv:dojo> test
[[[8 9] 5] foo=[6 bar=7]]
~tasfyn-partyv:dojo> test(foo 42)
[[[8 9] 5] foo=42]
~tasfyn-partyv:dojo> test(+8 %eight, bar.foo [%hello %world])
[[[%eight 9] 5] foo=[6 [%hello %world]]]
```
As we see, there's no need for the mutant noun to be shaped
anything like the old noun.  They're different nouns.

A mutation, like `+8 %eight`, specifies a wing and a twig.
The wing, like `+8` or `bar.foo`, defines a leg to replace.
The twig runs against the original subject.

Can we use mutation to build a cyclical noun?  Nice try, but no:
```
~tasfyn-partyv:dojo> test(+8 test)
[[[[[[8 9] 5] foo=[6 bar=7]] 9] 5] foo=[6 bar=7]]
```

## Progress

Now, not only can you build a noun, you can get data out of it and
even evolve new, related nouns.  We've still seen only two very
restricted kinds of twigs: constants and legs.  In the [next chapter](2-syntax), we'll actually write some interesting expressions.
