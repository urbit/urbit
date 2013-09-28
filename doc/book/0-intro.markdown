1: Introduction and philosophy
==============================

Urbit is a new programming and execution environment
designed from scratch.  Any resemblance to existing languages
or operating systems is coincidental, cosmetic, or inevitable.

## Urbit is a stack ##

Nock is a stateless virtual machine defined in 200 words.  The
Nock machine is sealed - all execution is "pure."  Nock's goal is
extreme commoditization of computing semantics.

Hoon is a high-level language which defines itself in Nock.  Its
self-compiling kernel, 7000 lines of code, specifies Hoon
unambiguously; there is no Hoon spec.  Hoon can be classified as
a pure, strict higher-order static type-inferred functional
language, with co/contra/bivariance and genericity.  However,
Hoon does not use lambda calculus, unification, or other
constructs from "PL theory."  Hoon also excels at handling and
validating untyped data, a common task on teh Internets.  Its
syntax is entirely novel and initially quite frightening.

Arvo is a deterministic functional operating system defined in
Hoon.  While still basically a toy, it can serve web apps and
network securely with other Arvo instances.  An Arvo instance
is designed to be a simple independent computer in the cloud.

It's possible to use Nock without Hoon, but there is no obvious
reason to do so.  It's not necessary to learn Nock to learn Hoon,
but it helps a lot.  The same for Hoon and Arvo.  Therefore,
we'll work through all three in order.  in order.  If you're
convinced that you want to learn Urbit, feel free to skip the
justifications below.

## Nock ##

Nock is a virtual machine, like the Java VM or Microsoft CLR.
Just as you execute Java by compiling it to the JVM, you execute
Hoon by compiling it to Nock.  

Do you need to learn Nock to learn Hoon?  In theory, no, just as
you don't need to know anything about the JVM to learn Java.
Indeed, in theory we could compile Hoon to the JVM (or Java to
Nock).  What's the difference between Nock and the JVM, anyway?
Why do we need a new virtual machine?  And since we'll only be
programming in Hoon, why do we care how Hoon is executed?

There is no formal difference between Nock and the JVM.  In
theory, both are Turing-complete computers.  In practice, they
have just about nothing in common.

The main practical difference is that Nock is two orders of
magnitude simpler.  The JVM spec is a book.  The Nock spec is 200
words; it fits on a T-shirt; it gzips to 371 bytes.  There are
lots of other differences, but all follow from this.

Why does size matter?  The goal of any programming language is to
become a standard.  Universal standardization equals world
domination.  The simpler a specification is, the easier to
standardize.  Nock is a practical interpreter as simple as the
most frozen and fundamental of protocols or formats: IPv4, XML,
JSON.  A 200-word spec needs a standards process like a fish
needs swim lessons.  When a T-shirt goes to war with a book, it's
like tanks against cavalry - or should be, anyway.

Since every program in Hoon (including the Hoon compiler) reduces
to Nock, every program in Hoon inherits Nock's precision.  If two
Nock interpreters produce different results, one is wrong, and it
is always easy to tell which - without a "standards lawyer."  Can
we imagine the end of incompatiblity?

Essential to actually realizing this promise is a second
difference between Nock and the JVM, which is that the JVM can
call Unix system calls and libraries, and Nock can't.  Nock has
no "native methods" or foreign-function interface.  Nor is it
ever extended, embellished, forked, improved or advanced.

Java began as a portable language; so did C.  Most Java today is
server-side Java, dependent no less than C on library and OS
configuration.  It is possible to write portable Java; it is
possible to write portable C.  It is not possible to write
unportable Nock or Hoon.  It is also not possible to write
insecure Nock or Hoon, unless your interpreter is so broken
there's an actual hole in its skull.

How does Nock obtain native CPU performance, if it can't call
native code?  This is actually a much more pressing problem in
Nock than in conventional virtual machines, like the JVM, because
naive Nock is hopelessly inefficient.  Nock is defined in a page
of pseudocode, and a naive interpreter can be written in a page
of any language.  But since Nock's only arithmetic operator is
increment, decrement in your one-page interpreter is an `O(n)`
operation.  Addition is `O(n^2)`.  And so on.

The programmer cannot solve this problem by calling a C function,
because Nock can't do that.  In theory, an optimizing Nock
interpreter might be able to analyze the code and reduce it to a
simpler equivalent.  But this would be a true research problem.

Instead, a practical Nock engine simply recognizes code it knows
and substitutes equivalent C functions, or "jets."  For instance,
in theory there are many different ways to express decrement in
Nock, but in practice the Nock interpreter will execute only one:
the decrement function in the Hoon kernel.  Therefore, the only
Nock decrement that must be optimized is the code that Hoon
generates when it compiles its own decrement.  All others will
suck, so don't roll your own.  Code recognition, as opposed to
code analysis, is not a research problem.

Jet propulsion separates mechanism and policy, transferring the
task of achieving native performance from the programmer to the
sysadmin.  The Hoon programmer must still use hints to mark
functions for jet recognition, but cannot control or discern how
these functions are actually executed.  Of course, a correct jet
is indistiguishable in every way, except timing, from naive Nock.
The 200-word spec defines the result, not the algorithm.

We can see a jet as a sort of "functional device driver."  For
instance, an OpenGL programmer today has no idea whether her GL
operations are implemented in software or by a GPU.  This
abstraction is essential to modern graphics programming.

When we compare jets to native calls, what are the pros and cons?
Jets have only one disadvantage: high-performance code must be
written twice, once in Hoon and once in C.  Indeed, from the C
programmer's perspective, Hoon is a specification language for
your C functions.  Hoon specifications are executable, of course,
so you can test the two implementations against each other -
again, transparently to the programmer.  Moreover, the jet can
even fail in special cases and drop back to the interpreter.

Hoon ships with a Nock that jet-propels most of the Hoon kernel,
including most of the Hoon compiler.  If you're wondering how we
wrote Hoon in Hoon when we didn't have Hoon, the answer is that
we wrote Hoon in C and evolved this C code into a mere jet.  This
process is not recommended unless absolutely necessary - by far
the best way to write the jet pair is to write the Hoon first.

## Hoon ##

If I can summarize Hoon's goal, it's to be the C of functional
programming.  If you're not so arthritic that you learned to code
in Turbo Pascal, you may never fully appreciate the metaphor.

All languages in the Algol procedural family, including both C
and Pascal, map straightforwardly onto a conventional CPU.  But
Pascal and C handle this mapping very differently.  Pascal and C
both have pointers and arrays, but Pascal works hard to treat
both pointers and arrays as mathematical abstractions.  C drops
the abstraction; it makes no bones about the fact that a pointer
is a memory address.

To a Pascal purist, to anyone who thinks mathematically, this
seemed hideous.  C isn't really a high-level language at all -
it's a glorified macro assembler.  Mankind retreats to the cave.
But to programmers who are not natural mathematicians, whose
minds are mechanical rather than abstract, C is a lifesaver.
Since most mathematicians are also good mechanical thinkers,
whereas very few people are naturally adept at abstraction,
C slew and pillaged the once promising empire of Pascal.

There are two broad families of functional language available
today: Haskell/ML, and Lisp.  The Haskell family is relatively
abstract; the Lisp family, relatively concrete.  Perhaps Lisp is
about as abstract as Pascal; Haskell is far more abstract.  Both
rest on the fundamental abstraction of functional programming,
the lambda calculus, and more generally the foundational
metamathematics of the late 19th and early 20th centuries.

Hoon has nothing to do with any of this stuff.  It has functions
and types, or what appear to be functions and types.  On closer
inspection, they are not abstractions at all, just glorified Nock
macros.

If we compare these concrete patterns to the genuine abstractions
of Haskell, we see that - as with Pascal and C - Hoon is roughly
as expressive as Haskell.  Haskell has higher-order type
inference; Hoon has "higher-order" "type" "inference."  Some
Haskell extensions have dependent types - Hoon has "refined"
"types."  Hoon, like Lisp, unlike Haskell, is also very
comfortable with typeless data; it should be, because it has no
types, only "types."  The Hoon features and the Haskell
abstractions have nothing in common - except that they solve the
same problems for you, the programmer.  In short, Hoon next to
Haskell is a white shark next to a killer whale.  The apparent
resemblance is strictly superficial.

So we could describe Hoon as a pure, strict, higher-order typed
functional language.  But don't do this in front of a Haskell
purist, unless you put quotes around "typed," "functional," and
possibly even "language."  We could also say "object-oriented,"
with the same scare quotes for the cult of Eiffel.

Knowing Pascal made it harder, not easier, to learn C.  Knowing
Haskell or Lisp makes it harder to learn Hoon.  Indeed, knowing
either would have made it impossible for me to write Hoon.  I do
know C, of course, and the spirit of K&R is all over Hoon.  Or so
I'd like to think.  Just as C is little more than a macro
assembler for machine code, Hoon is little more than a macro
assembler for Nock.

The most basic difference between Hoon and other languages is
that Hoon is defined in Hoon.  There is no formal Hoon spec -
just a self-compiling compiler written in Hoon.  The target of
this compiler is, of course, Nock.  Thus Hoon is as precisely
defined as Nock, which is quite precisely indeed. 

This would be true regardless of the size of Hoon in Hoon, but
Hoon in Hoon is in fact quite small.  The Hoon kernel is 7000
lines; it gzips to 25K.  But this includes not only the
self-compiling compiler, but also all the standard libraries it
needs.  The compiler alone is 2500 lines, including a very
intricate "monadic" parser, a non-Hindley-Milner "type inference"
engine, and a Nock code generator.  This reflects both the
internal simplicity of Hoon and its expressiveness.  If you know
these 2500 lines, and an expert should, you _know_ Hoon.

On the other hand, the _apparent_ complexity of Hoon is very
high.  When you open a Hoon file, you are confronted with an
enormous avalanche of barely structured line noise.  Again this
reminds us of C, which makes no attempt at the kind of abstract
prettiness we expect from a Pascal or a Haskell.  Learning Hoon
involves learning nearly 100 ASCII digraph "runes."

Is this a harsh learning curve?  Of course it is.  On the other
hand, it is not a mathematical task, but a mechanical one.  It is
trivial compared to the task of learning the Chinese alphabet,
memorizing the Qu'ran, etc, all rote mental tasks routinely
performed by normal human 11-year-olds.  If you have an
11-year-old who understands the Hindley-Milner algorithm, you
have a remarkable young mathematician.

A practical programming language is first and foremost a UI for
programmers - meaning human programmers.  Concrete languages beat
abstract ones because they play to the strengths of the human
brain, and avoid its weaknesses.  Functional programming is
traditionally reserved for the topmost echelon of natural talent.
I'd like to think that anyone who can learn to fix a Chevy can
learn to write a Hoon function.  We'll see if that's true.

A programming language is called a language for a reason - it
should activate the human linguistic lobes.  Learning Hoon
is like learning a language very alien to your first, such as
Chinese from English.  Before you know Hoon, it looks like
squiggles.  Once you know Hoon, and the rote task of syntax
processing is hardwired, you look at your screen and _see_
the function.  Or, at least, I do - I hope you can too.

## Arvo ##

Lorem ipsum.