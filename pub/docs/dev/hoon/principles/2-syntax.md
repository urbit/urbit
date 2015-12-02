---
next: false
sort: 2
spam: true
title: Hoon 101.2: syntax
---
# Hoon 101.2: full-contact syntax

In [chapter 0](0-nouns), we read about nouns.  In [chapter 1](1-twigs), 
we discovered twigs and legs.

Up till now, we've done everything in the dojo, Hoon's shell /
REPL.  Now it's time to actually write a real Hoon source file,
and get a bit deeper into the syntax.

## How to use this tutorial

Ideally, you've installed an Urbit planet (if you have a ticket)
or comet (if you don't).  See the [user doc](../../../user).

We recommend opening up the dojo and just typing the examples;
you don't know a language until you know it in your fingers.
Also, make sure you've worked through the chapters in order.

## Building a simple generator

In Urbit there's a variety of source file roles, distinguished by
the magic paths they're loaded from: `/gen` for generators,
`/ape` for appliances, `/lib` for libraries, etc.

We'll start with a generator, the simplest kind of Urbit program.

### Create a sandbox desk

A desk is the Urbit equivalent of a `git` branch.  We're just
playing around here and don't intend to soil our `%home` desk with
test files.  So let's make a sandbox:
```
|sync %sandbox our %home
```

You just merged the `%home` desk on your own ship into a new
`%sandbox` desk.  `%sandbox` has everything in `%home`, but we'll
add more.  Future changes in `%home` (such as our over-the-air
updates) will also propagate into `%sandbox`.

> Desks are always born in merges; it makes no sense to create an
empty desk, because anything you do in a desk depends on other
files in the desk.  For instance, your file types (marks) are
defined by source files in the same desk.

### Mount the sandbox from Unix

Run the command
```
~tasfyn-partyv:dojo> |mount /=sandbox=/gen %gen
```
This mounts the `/gen` folder from the `%sandbox` desk in your Unix
directory  `~/tasfyn-partyv/gen`.

The mount is a two-way sync, "Dropbox style."  When you edit a
Unix file and save, your edit is automatically committed as a
change in the `%sandbox` desk.  If you change `%sandbox` files
from Urbit, these changes will also propagate to Unix.

### Execute from the sandbox

Let's set the dojo desk to `%sandbox`:
```
~tasfyn-partyv:dojo> =dir /=sandbox=
=% /~tasfyn-partyv/sandbox/~2015.11.13..02.49.37..9e6c/
```
Your prompt will change to:
```
~tasfyn-partyv:dojo/sandbox
```

### Write hello, world

Let's build the simplest possible kind of generator, a builder.

First, configure your favorite Unix text editor to work with
Hoon.  There are Hoon modes for vim, emacs and Sublime in the
`extras/` directory of the github repo.

Second, create the file `~/tasfyn-partyv/gen/test.hoon`.
Paste this text into it:
```
:-  %say  |=  *  :-  %noun
[%hello %world]
```
Get the spaces exactly right, please.  Hoon is not in general a
whitespace-sensitive language, but the difference between one space and
two-or-more matters.  And for the moment, think of
```
:-  %say  |=  *  :-  %noun
```
as gibberish boilerplate, like `#include "stdio.h"` at the start of a C
program. 

Now, run your builder:
```
~tasfyn-partyv:dojo/sandbox> +test
[%hello %world]
```
This is your first Hoon *program* per se.  Congrats!

## Hoon syntax 101

But what's up with this syntax?

### Some syntactic relatives

The relationship between ASCII and human programming languages
is like the relationship between the electric guitar and
rock-and-roll.  If it doesn't have a guitar, it's not rock.
If it doesn't use ASCII, it's not a programming language.

Some great rock guitarists play three chords, like Johnny Ramone;
some shred it up, like Jimmy Page.  If Lisp is the Ramones of
syntax, Perl or APL is Led Zeppelin.  No one has any right to rag
on Perl or APL, but Hoon is a "metalhead" language that shreds
its ASCII very differently.

### The case for heavy metal

The philosophical case for a metalhead language is threefold.
One, human beings are much better at associating meaning with
symbols than they think they are.  Two, a programming language is
a professional tool and not a plastic beach shovel.

> "There's a guitar player, a harp player, a double-bass player,
all holding up their blisters. Imagine if you downloaded a
library off the internet... and it gave you blisters! Right? The
horror! And yet... every musician has overcome a barrier to entry
similar to this."  — Rich Hickey

And three, the alternative to heavy metal is keywords.  When you
use a keyword language, not only are you forcing the programmer
to tiptoe around a ridiculous maze of reserved words.  You're
expressing your program through two translation steps:
symbol->English and English->computation.

When you shred, you are going direct: symbol->computation.  A
pure-functional language with syntax on the metalhead principle
creates a sense of "seeing the function" which no keyword
language can quite duplicate.  Also, all the words you see on the
screen are actually meaningful terms in the program.

But a great metalhead language should *not* be user-extensible.
That way lies King Crimson.  (Maybe Haskell is King Crimson: the
prog-rock of programming languages.)  A language is a standard;
if users can do whatever with ASCII, there is no standard.  If a
metal language can work, it's only by rigorous consistency and
predictability.  No macros, operator overloading, etc, etc.

### A glyphic bestiary

Any metalhead language you don't yet know is line noise.  Let's
get you up to speed as fast as possible.

A programming language needs to be not just read but said.  But
no one wants to say "ampersand."  Therefore, we've taken the
liberty of assigning three-letter names to all ASCII glyphs.

Some of these bindings are obvious and some aren't.  You'll be
genuinely surprised at how easy they are to remember:
```
    ace [1 space]   gal <               pel (
    bar |           gap [>1 space, nl]  per )
    bas \           gar >               sel [
    buc $           hax #               sem ;
    cab _           hep -               ser ]
    cen %           kel {               soq '
    col :           ker }               tar *
    com ,           ket ^               tec `
    doq "           lus +               tis =
    dot .           pam &               wut ?
    fas /           pat @               zap !
```
It's fun to confuse the muggles by using these outside Urbit.

Hoon is not whitespace-sensitive, except that its grammar defines
two distinct whitespace tokens: `ace`, a single space, and `gap`,
anything else.  `\t`, `\r`, and `\l` choke the parser and should
not appear in your file.

A few digraphs also have irregular sounds:
```
==  stet
--  shed
++  slus
->  lark
-<  lush
+>  dark
+<  dish
```

You might remember wondering where the "lark syntax" of chapter 1
got its name.  Lark syntax is read in digraphs from the left, so
`+>+` is `darklus`.

> `+>+` is of course `cdddr` in Lisp.

### The shape of a twig

As we learned in chapter 1, a twig - the parsed form of a Hoon
expression - is a noun.  As usual in Hoon, the easiest way to
explain both the syntax that compiles into that noun, and the
semantic meaning of the noun, is the noun's physical structure.

#### Autocons

A twig is always a cell, and any cell of twigs is a twig
producing a cell.  As an homage to Lisp, we call this
"autocons."

Where you'd write `(cons a b)` in Lisp, you write `[a b]` in
Hoon.  The Lisp expression, in Hoon syntax, would be `[%cons a b
~]`.  But the Hoon twig is just `[a b]`.

The `???` prefix prints a twig as a noun instead of running it.
Let's see autocons in action:
```
~tasfyn-partyv:dojo/sandbox> ??? 42
[%dtzy p=%ud q=42]
~tasfyn-partyv:dojo/sandbox> ??? 0x2a
[%dtzy p=%ux q=42]
~tasfyn-partyv:dojo/sandbox> ??? [42 0xa]
[[%dtzy p=%ud q=42] [%dtzy p=%ux q=42]]
```

#### The stem-bulb pattern

If the head of your twig is a cell, it's an autocons.  If the
head is an atom, it's an unpronounceable four-letter symbol like
the `%dtzy` above.

Except for the funny autocons case, twigs have the same shape
we see in the `span` mold, which we met in chapter 0.  It's
essentially a variant record, the most common data structure
ever.  In Hoon it's called a *kelp*.

The head of a kelp (like `%dtzy` above) is called the *stem*.
The tail (like `[%ux 42]`) is the *bulb*.

> Think about how to encode tagged data in a noun.  It seems
obvious that the noun is a cell, where the head of the cell is
the tag (stem) and the tail is the data (bulb).  Then, all these
nouns are cells whose head is an atom.  This leaves two noun
shapes "out of band": atoms, and cells whose head is a cell.

> Since no twig is an atom, a cell `[twig twig]` is always a cell
whose head is a cell.  So we can distinguish "autocons" from all
stem-bulb nouns, and assign it specific semantics.

#### Runes and stems

A *rune* is a digraph - a sequence of two ASCII glyphs.  If you
know C, you know digraphs like `!=` and `?:` and are used to
reading them as single characters.

In Hoon you can *say* runes as words: "zaptis" and "wutcol"
respectively.  If we had to say "exclamation point equals" and
"question-colon" all the time, we'd just die.

Most twig stems are made from runes, by concatenating the glyph
names and removing the vowels.  For example, the rune `=+`,
pronounced "tislus," becomes the stem `%tsls`.  (Note that in
most noun implementations, this is a 31-bit direct value.)

> Some twig stems (like `%dtzy`) are not runes, simply because
they don't have regular-form syntax and don't need to use
precious ASCII real estate.  They are otherwise no different.

An important point to note about runes: they're organized.  The
first glyph in the rune defines a category.  For instance, runes
starting with `.` compute intrinsics; runes starting with `|`
produce cores; etc.

Another important point about runes: they come in two flavors,
"natural" (stems interpreted directly by the compiler) and
"synthetic" (macros, essentially).

> Language food fight warning: one advantage of Hoon over Lisp
is no gensyms.  All Hoon macros are perfectly hygienic.  Another
advantage is that Hoon has no (user-level) macros.  In Hoon
terms, nobody gets to invent their own runes, because that way
lies DSL write-only chaos.  But if we had user-level macros,
they'd be perfectly hygienic as well.

#### Tall and wide regular forms

A good rune example is the simple rune `=+`, pronounced "tislus",
which becomes the stem `%tsls`.  A `%tsls` twig has the mold
`[%tsls p=twig q=twig]`.

The very elegance of functional languages creates a visual
problem that imperative languages lack.  An imperative language
has distinct statements (with side effects) and (usually pure)
expressions; it's natural that in most well-formatted code,
statements flow vertically down the screen, and expressions grow
horizontally across this.  This interplay creates a natural and
relaxing shape on your screen.

In a functional language, there's no difference.  The trivial
functional syntax is Lisp's, which has two major problems.  One:
piles of expression terminators build up at the bottom of complex
functions.  Two: the natural shape of code is diagonal.  The more
complex a function, the more it wants to besiege the right
margin.  The children of a node have to start to the right of its
parent, so the right margin bounds the tree depth.

Hoon does not completely solve these problems, but alleviates
them.  In Hoon, there are actually two regular syntax forms for
most twig cases: "tall" and "wide" form.  Tall twigs can contain
wide twigs, but not vice versa, so the visual shape of a program
is very like that of a statements-and-expressions language.
 
Also, in tall mode, most runes don't need terminators.  Take
`=+`, for example.  Since the parser knows to expect exactly 
two twigs after the `=+` rune, it doesn't need any extra syntax
to tell it that it's done.

Let's try a wide `=+` in the dojo:
```
~tasfyn-partyv:dojo/sandbox> =+(planet=%world [%hello planet])
[%hello %world]
```
(`=+` seems to be some sort of variable declaration?  Let's not
worry about it right now.  We're on syntax.)

The wide syntax for a `=+` twig, or any binary rune: `(`, the 
first subtwig, one space, the second subtwig, and `)`).  To read
this twig out loud, you'd say:
```
tislus pel planet tis cen world ace sel cen hello ace planet ser per
```
> Various colloquialisms inevitably creep into this usage.  "tis" not 
in a rune gets contracted to "is"; "ace" is often just assumed; etc.

Let's try a tall `=+` in `test.hoon`: 
```
:-  %say  |=  *  :-  %noun
=+  planet=%world
[%hello planet]
```
The tall syntax for a `=+` twig, or any binary rune: the rune, at 
least two spaces or one newline, the first subtwig, at least two 
spaces or one newline, the second subtwig.  Again, tall subtwigs
can be tall or wide; wide subtwigs have to be wide.

(Note that our boilerplate line is a bunch of tall runes on one
line, with two-space gaps.  This is unusual but quite legal, and
not to be confused with the actual wide form.)

To read this twig out loud, you'd say:
```
tislus gap planet is cen world gap nep cen hello ace planet pen
```
#### Layout conventions

Should you use wide twigs or tall twigs?  When?  How?  What
should your code look like?  You're the artist.  Except for the
difference between one space (`ace`) and more space (`gap`), the
parser doesn't care how you format your code.  Hoon is not Go --
there are no fixed rules for doing it right.

> Keep lines under 80 characters, though.  The parser doesn't
enforce this yet.  But it will, so watch out!

#### Backstep indentation

Note that the "variable declaration" metaphor of `=+` works
perfectly here.  Because `[%hello planet]` -- despite being a
subtree of the the `=+` twig -- is at the same indent level.  A
variable declaration shouldn't add indent depth.  And `=+`
doesn't.  Our code flows down the screen, not down and to the
right, and of course there are no superfluous terminators.

Another example, using a ternary rune with a strange resemblance
to C:
```
:-  %say  |=  *  :-  %noun
=+  planet=%world
?:  =(%world planet)
  [%hello planet]
[%goodbye planet]
```
This is called "backstep" indentation.  Not all the children of
`?:` ("wutcol", `%wtcl`) are at the same indent as the parent;
but one of them is.

It's not always the case when backstepping that the largest
subtwig is at the bottom and loses no indent, but it often is.
And we do a lot to help you maintain this.

For instance, `=+` ("tislus") is a binary rune: `=+(a b)`.  In
most cases of `=+` the heavy twig is `b`, but sometimes it's `a`.
So we can use its friend the `=-` rune ("tishep") to get the same
semantics with the right shape: `=-(b a)`.  Similarly, instead of
`?:(a b c)`, we can write `?.(a c b)`.

Not all runes have tuple structure; some are n-ary, and use
the `==` terminator (again, pronounced "stet"):
```
:-  %say  |=  *  :-  %noun
=+  planet=%world
?+  planet
          [%unknown planet]
  %world  [%hello planet]
  %ocean  [%goodbye planet]
==
```
So we occasionally lose right-margin as we descend a deep twig.
But we can keep this lossage low with good layout design. 

#### Irregular forms

There are more regular forms than we've shown above, but not a
lot more.  Hoon would be quite easy to learn if it was only its
regular forms.  It wouldn't be as easy to read or use, though.
The learning curve is important, but not all-important.

Some stems (like the `%dtzy` constants above) obviously don't and
can't have any kind of regular form (which is why `%dtzy` is not
a real digraph rune).  Many of the true runes have only regular
forms.  But some have irregular forms.  Irregular forms are 
always wide, but there is no other constraint on their syntax.

We've already encountered one of the irregular forms: `foo=42`
from the last chapter, and `planet=%world` here.  Let's unpack
this twig:
```
~tasfyn-partyv:dojo/sandbox> ?? %world
  [%cube 431.316.168.567 %atom %tas]
%world

~tasfyn-partyv:dojo/sandbox> ??? %world
[%dtzz %tas 431.316.168.567]
```
Clearly, `%dtzz` is one of our non-regulars.  But we can wrap it
with our irregular form:
```
~tasfyn-partyv:dojo/sandbox> ?? planet=%world
  [%face %planet [%cube 431.316.168.567 %atom %tas]]
planet=%world

~tasfyn-partyv:dojo/sandbox> ??? planet=%world
[%ktts %planet %dtzz %tas 431.316.168.567]
```
Since `%ktts` is "kettis", ie, `^=`, this has to be the irregular
form of
```
~tasfyn-partyv:dojo/sandbox> ^=(planet %world)
planet=world
```
So if we wrote our example without this irregular form, it'd be
```
:-  %say  |=  *  :-  %noun
=+  ^=(planet %world)
[%hello planet]
```
Or with a gratuitous use of tall form:
```
:-  %say  |=  *  :-  %noun
=+  ^=  planet  %world
[%hello planet]
```
## Progress

Now you know how to read Hoon!  For fun, try to pronounce more of
the code on this page.  Please don't laugh too hard at yourself.

In the [next chapter](3-program), we'll actually write a real program...
