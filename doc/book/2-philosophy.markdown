2: Hoon - Philosophy
=========

Yo, let's write some Hoon!

In this tutorial we're going to try to learn Hoon, without first
learning either Nock or Arvo.  You probably should at least skim
the Nock doc though - learning Hoon without understanding Nock is
a little like learning C without understanding how a CPU works.
It is not easier than knowing both, it's harder.

As for Arvo, all we'll use of it here is the power to edit a
library and type expressions on the command line.  These are
not hard things and we'll explain them as we go along.

We'll cover Hoon in three sections - philosophy, syntax, and
semantics.  You should probably just skip to syntax.

The good news about Hoon is that it compiles itself to Nock in
3400 lines of Hoon.  If this number is accurate (it is), Hoon 
is very expressive, or very simple, or both.  (It's both.)  The
bad news is that it really has nothing at all in common, either
syntactically or semantically, with anything you've used before.

In particular, please remember that _you are not too stupid_ to
program in Hoon.  Hoon is FP made stupid - for the stupid, by the
stupid.  (This is the real reason it's only 3400 lines.)

Maybe you've had some bad experiences with a higher-order typed
functional language - like Haskell or OCaml.  Dare we suspect
that these experiences came on one of our nation's fine college
campuses?  Well, we have to admit, Hoon is a higher-order typed
functional language.  But really, don't let this discourage you.

First, Hoon hasn't been to college.  Second, at least it's
strict.  And third, we don't use any of that PL theory shit.
We're OS guys.  Our mission is not to praise PL, but to drive it
out of the CS department with a flaming sword, back to the
fundless ghetto of math where it belongs.  Hoon is 'street FP.'
The only functional language it gives any real props to is JS.
JS sucks, of course, but at least it's a player.  (Scala is a
player too, but it's also the size of a blue whale's ass.)

We hope we're not being too harsh on PL theory.  The fact is that
this branch of mathematics (constructive logic or whatever) is a
perfectly adequate and remarkably elegant formal description of
computing.  But, of course, it is not the only such description.
And it was originally created for mathematicians, not coders.

Nor is the math department's standard of elegance the only such
standard.  The programmer has his own design sense, which
corresponds more closely to usability in a UI sense.  To read a
program is to simulate it in your head, and the skill needed to
do this revolves around tracing long complex sequences of very
simple transformations.  

For these simple transformations, I think, we use a completely
different processing path - less a mathematical intution, than a
mechanical one.  This is why many good C programmers are bad at
symbolic math.  Of course, we all should be good at all things -
but are we?  And this is why Hoon is not Haskell.

For example, it's very true and cool that you can represent all
data as functions.  It's also pretty neat that you can take a
leak while standing on your head, and you always hit the bowl.
To an OS guy it seems more interesting to represent functions as
data.  Like, you know how to send data over the network.  Do you
know how to send functions over the network?

(On the other hand, if only because Hoon is very immature, Haskell 
is good at many things that Hoon isn't good at.  For instance,
good list/container comprehensions are the pons asinorum of a
functional language.  Hoon's actually kind of suck.  This is not 
(well, mostly not) because Hoon sucks, but because (a) they are
some of the oldest library code in the system, and (b) they have 
never seen either end of a top-notch functional programmer.
Despite the fact that category theory can go bite itself, people
with more FP experience on more mature languages probably have a
lot to contribute here.)
 
Anyway.  That's enough philosophy.  Come on, who needs it?
