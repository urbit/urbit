Philosophy
==========

Hoon is a higher-order typed functional language that it compiles itself to
Nock in 3400 lines of Hoon. If this number is accurate (it is), Hoon is very
expressive, or very simple, or both. (It's both.) The bad news is that it
really has nothing at all in common, either syntactically or semantically, with
anything you've used before.

By understanding Nock tutorial, you've actually come closer than you realize to
knowing Hoon. Hoon is actually not much more than a fancy wrapper around Nock.
People who know C can think of Hoon as the C to Urbit's Nock - just a
sprinkling of syntax, wrapped around machine code and memory.

For instance, it's easy to imagine how instead of calculating tree axes by
hand, we could actually assign names to different parts of the tree - and those
names would stay the same as we pushed more data on the subject.

The way we're going to do this is by associating something called a type with
the subject. You may have heard of types before. Technically, Hoon is a
statically typed language, which just means that the type isn't a part of your
program: it's just a piece of data the compiler keeps around as it turns your
Hoon into Nock.

A lot of other languages use dynamic types, in which the type of a value is
carried along with the data as you use it. Even languages like Lisp, which are
nominally typeless, look rather typed from the Hoon perspective. For example, a
Lisp atom knows dynamically whether it's a symbol or an integer. A Hoon atom is
just a Nock atom, which is just a number. So without a static type, Hoon
doesn't even know how to print an atom properly.

Most higher-order typed languages, Haskell and ML being prominent examples, use
something called the Hindley-Milner unification algorithm. Hoon uses its own
special sauce instead.

Why? There are two obvious problems with Hindley-Milner as a functional type
system, the main one being the wall of heavy mathematics that greets you
instantly when you google it. We have heard some claims that Hindley-Milner is
actually quite simple. We urge all such claimants to hie themselves to its
Wikipedia page, which they'll surely be able to relieve of its present alarming
resemblance to some string-theory paper in Physics Review D.

Nor is this in any way an an anti-academic stance. Quite the contrary.
Frankly, OS guys really quite seldom find themselves in the math-department
lounge, cadging stray grants by shamelessly misrepresenting the CAP theorem as
a result in mathematics. It doesn't seem too much to expect the mathematicians
to reciprocate this basic academic courtesy.

Furthermore, besides the drawback that Hindley-Milner reeks of math and
programmers who love math are about as common as cats who love a bath - a
problem, but really only a marketing problem - Hindley-Milner has a genuine
product problem as well. It's too powerful.

Specifically, Hindley-Milner reasons both forward with evaluation, and backward
from constraints. Pretty unavoidable in any sort of unification algorithm,
obviously. But since the compiler has to think both forward and backward, and
the programmer has to predict what the compiler will do, the programmer has to
think backward as well.

Hoon's philosophy is that a language is a UI for programmers, and the basic
test of a UI is to be easy to use. It is impossible (for most programmers) to
learn a language properly unless they know what the compiler is doing, which in
practice means mentally stepping through the algorithms it uses (with the
exception of semantically neutral optimizations). Haskell is a hard language to
learn (for most programmers) because it's hard (for most programmers) to follow
what the Haskell compiler is thinking.

It's true that some programmers have an effective mathematical intuition that
let them "see" algorithms without working through them step by step. But this
is a rare talent, we feel. And even those who have a talent don't always enjoy
exercising it.

If a thorough understanding of any language demands high-grade mathematical
intuition in its programmers, the language as a UI is like a doorway that makes
you duck if you're over 6 feet tall. The only reason to build such a doorway in
your castle is if you and all your friends are short, and only your enemies are
tall. Is this really the case here?

Although an inference algorithm that reasons only forward must and does require
a few more annotations from the programmer, the small extra burden on her
fingers is more than offset by the lighter load on her hippocampus.
Furthermore, programs also exist to be read. The modern code monkey is above
all things a replaceable part, and some of these annotations (which a smarter
algorithm might infer by steam) may annoy the actual author of the code but be
a lifesaver for her replacement.
