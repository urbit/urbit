# Hoon 1: twigs and legs

In the last chapter we learned how to make nouns.  In this
chapter we'll start programming a little.

## Nock for Hoon programmers

Hoon compiles itself to a pico-interpreter called Nock.  This
isn't the place to explain Nock (which is to Hoon much as
assembly language is to C), but Nock is just a way to express a
function as a noun.

Specifically, you can think of Nock as a (Turing-complete)
interpreter shaped like (pseudocode):
```
Nock(subject formula) => product
```
Your function is the noun `formula`.  The input to the function
is the noun `subject`.  The output is `product`.  If something
about this seems complicated or even interesting, you may be
misunderstanding it.

## From Hoon to Nock

The Hoon parser turns an source expression (even one as simple as
`42` from the last chapter) into a noun called a `twig`.  If you
know what an AST is, a twig is an AST.  (If you don't know what
an AST is, it's not worth the student loans.)

To simplify slightly, the Hoon compiler is shaped like:
```
Hoon(subject-span function-twig) => [product-span formula-nock]
```
Hoon, like Nock, is a *subject-oriented* language - your twig is
always executed against one input noun, the subject.  For any
subject noun in `subject-span`, the compiler produces a Nock
formula that computes `function-twig` on that subject, and a
`product-span` that is the span of the product.

(Pretty much no other language works this way.  In a normal
language, your code is executed against a scope, stack, or other
variable context, which may not even be a regular user-level
value.  This change is one of the hardest things to understand
about Hoon, mostly because it's hard to stay convinced that
subject-oriented programming is as straightforward as it is.)

## From constants to twigs

In the last chapter we were entering degenerate twigs like `42`.
Obviously this doesn't use the subject at all.

Let's use the dojo variable facility (this is *not* Hoon syntax,
just a dojo command) to make a test subject:
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] [6 7]]
```
We can evaluate twigs against this subject with the Hoon `:`
syntax (`a:b` uses the product of `b` as the subject of `a`).
```
~tasfyn-partyv:dojo> 42:test
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
(If you're wondering why `[6 7]` got printed as `6 7`, remember
that `[]` associates to the right.)

Hoon has a simple tree addressing scheme (inherited from Nock):
the root is `1`, the head of `n` is `2n`, the tail is `2n+1`.
The twig syntax is `+n`.  Hence:
```
~tasfyn-partyv:dojo> +1:test
[[[8 9] 5] 6 7]
```
Our example is a sort of Hoon joke, not very funny:
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
And so on.  An instinct for binary tree geometry develops over
time as you use the system, rather the way most programmers
learn to do binary math.

## Femur syntax

A "femur" is an alternative syntax for a tree address.  The femur
syntax creates a recognizable geometric shape by alternating
between two head/tail pairs, read left to right: `-` and `+`,
`<` and `>`.

Thus `-` is `+2`, `+` is `+3`, `+<` is `+6`, `->` is `+5`, `-<+`
is `+9`, etc.  The decimal numbers are distracting, whereas the
glyph string binds directly to the tree geometry as you learn it.
We actually almost never use the decimal tree geometry syntax.

## Simple faces

But it would be pretty tough to program in Hoon if explicit
geometry was the only way of getting data out of a subject.
Let's introduce some new syntax:
```
~tasfyn-partyv:dojo> foo=42
foo=42
~tasfyn-partyv:dojo> ? foo=42
  foo=@ud
foo=42
~tasfyn-partyv:dojo> ?? foo=42
  [%face %foo %atom %ud]
foo=42
```
To extend our `++span` mold:
```
++  span
  $%  [%atom @tas]
      [%cell span span]
      [%cube * span]
      [%face @tas span]
  ==
```
The `%face` span wraps a label around a noun.  Then we can 
get a leg by name.  Let's make a new dojo variable:
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] foo=[6 7]]
```
The syntax is what you might expect:
```
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
  [%face %foo %cell [%atom %ud] %atom %ud]
foo=[6 7]
```

## Interesting faces; wings

Again, you're probably used to name resolution in variable scopes
and flat records, but not in trees.  (Partly this is because the
tradition in language design is to eschew semantics that make it
hard to build simple symbol tables, because linear search of a
big tree is a bad idea on '80s hardware.)

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
~tasfyn-partyv:dojo> =test [[foo=[8 9] 5] foo=[6 7]]
~tasfyn-partyv:dojo> ^foo:test
[6 7]
```
`^^foo` will skip two foos, `^^^foo` three, up to `n`.
But what about nested labels?
```
~tasfyn-partyv:dojo> =test [[[8 9] 5] foo=[6 bar=7]]
~tasfyn-partyv:dojo> bar:test
/~tasfyn-partyv/home/~2015.9.16..21.40.21..1aec:<[1 1].[1 9]>
-find-limb.bar
find-none
```
It didn't seem to like that.  We'll need a nested search:
```
~tasfyn-partyv:dojo> bar.foo:test
7
```
`bar.foo` here is a `wing`, a search path in a noun.  Note that
the wing runs from left to right, ie, the opposite of most
languages: `bar.foo` means "bar inside foo."

Each step in a wing is a `limb`.  A limb can be a tree address,
like `+3` or `.`, or a label like `foo`.  We can combine them in
one wing:
```
~tasfyn-partyv:dojo> bar.foo.+3:test
7
```

## Mutation

Well, not really.  We can't modify nouns; the concept doesn't
even make sense in Hoon.  Rather, we build new nouns which are
(logical -- the pointers are actually shared) copies of old ones,
with changes.

Let's build a "mutated" copy of our test noun:
```
~tasfyn-partyv:dojo> test
[[[8 9] 5] foo=[6 bar=7]]
~tasfyn-partyv:dojo> test(foo 42)
[[[8 9] 5] foo=42]
~tasfyn-partyv:dojo> test(+8 %eight, bar.foo [%hello %world])
[[[%eight 9] 5] foo=[6 [%hello %world]]]
```
As we see, there's no obvious need for the mutant noun to be
shaped anything like the old noun.  They're different nouns.

At this point, you have a simplified but basically sound idea of
how Hoon builds and manages nouns.  Next, it's time to do some
programming.
