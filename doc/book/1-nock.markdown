1: Crash course in Nock
=======================

So let's learn Nock!  But wait - why learn Nock?  After all,
we're going to be programming in Hoon, not Nock.

Like JVM bytecode, Nock is as inscrutable as assembly language.
In fact, you can think of it as a sort of "functional assembly
language."  There are sometimes reasons to program in real
assembly language.  There is never a reason to program in Nock.
Except to learn Nock.

Indeed, it is not necessary for the Hoon programmer to learn
Nock.  We recommend it strongly, however, because Hoon has a very
special relationship with Nock - not unlike the relationship
between C and assembly language.  

Just as C is a very shallow layer over the raw CPU, Hoon is a
very shallow layer over raw Nock - often little more than a
macro.  If you try to learn C without understanding the CPU under
it, you will be forever bemused by why it works the way it does.

So let's learn Nock!  But wait - which Nock?  Nock, though more
frozen than Walt Disney, does have versions.  Nock versions are
measured by integer degrees Kelvin, newer being colder.  The
newest, Nock 5K - roughly the temperature of Neptune.  No change
is anticipated between 5K and absolute zero, though you never
know.  Any such change would certainly be quite painful.

1.1 Definition
--------------

The best way to learn Nock is to read the spec and write your own
naive interpreter.  Here is Nock 5K:

**1. Structures**

	A noun is an atom or a cell.  An atom is any natural number.  
	A cell is any ordered pair of nouns.

**2. Pseudocode**

	1  ::    [a b c]           [a [b c]]
	2  ::    nock(a)           *a
	3  ::  
	4  ::    ?[a b]            0
	5  ::    ?a                1
	6  ::    +a                1 + a
	7  ::    =[a a]            0
	8  ::    =[a b]            1
	9  ::
	10 ::    /[1 a]            a
	11 ::    /[2 a b]          a
	12 ::    /[3 a b]          b
	13 ::    /[(a + a) b]      /[2 /[a b]]
	14 ::    /[(a + a + 1) b]  /[3 /[a b]]
	15 ::
	16 ::    *[a [b c] d]      [*[a b c] *[a d]]
	17 ::
	18 ::    *[a 0 b]          /[b a]
	19 ::    *[a 1 b]          b
	20 ::    *[a 2 b c]        *[*[a b] *[a c]]
	21 ::    *[a 3 b]          ?*[a b]
	22 ::    *[a 4 b]          +*[a b]
	23 ::    *[a 5 b]          =*[a b]
	24 ::
	25 ::    *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]
	26 ::    *[a 7 b c]        *[a 2 b 1 c]
	27 ::    *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]
	28 ::    *[a 9 b c]        *[a 7 c [2 [0 1] [0 b]]]
	29 ::    *[a 10 b c]       *[a c]
	30 ::    *[a 10 [b c] d]   *[a 8 c 7 [0 2] d]
	31 ::
	32 ::    +[a b]            +[a b]
	33 ::    =a                =a
	34 ::    /a                /a
	35 ::    *a                *a

Your interpreter should be no more than a page of code in
any language.  For extra credit, `6`-`10` are macros; implement 
them directly.  For extra extra credit, optimize tail calls.
To test your code, write a decrement formula b such that
`*[a b]` is `a - 1` for any atomic nonzero `a`.

1.2 Installation
----------------

The second best way to learn Nock is to boot up your own Arvo
virtual computer.  Using a Mac with OS X, install Homebrew and
add the following packages: `gmp`, `libsigsegv`, `libev`.  Then, make.
This will produce `bin/vere`, the virtual machine.

To create an Arvo computer, you need an Urbit identity (seat).
In theory, a computer can host one or more seats; in practice,
one is the right number.

There are two ways to get a seat - you can make your own or you
can get an invitation.  If you make your own, your seat is a 
128-bit hash of an RSA public key.  If you get an invitation,
it's a 32-bit number.  Either way, that number is encoded in a
phonetic base designed to make it easy to remember.  So if it's
a 128-bit seat or `pawn`, it looks like:

	~ralnyl-panned-tinmul-winpex--togtux-ralsem-lanrus-pagrup
 
And if it's a 32-bit seat, it looks like:

	~tasfyn-partyv

If you have an invitation file, create a computer by running

	vere -i $file

Otherwise, generate a 1024-bit RSA key with

	vere -n 10

This is just a toy for now, so use the defaults to generate
a passcode stored in your home directory.  Write this passcode
down on a piece of paper if you're especially concerned.

In either case, this generates a directory in `hub/`:

	hub/ralnyl-panned-tinmul-winpex--
	hub/tasfyn-partyv

Quit vere with ^D, then restart with 

	vere $directory

This is how you start your computer.  Arvo is a single-level
store which works by remembering all its events.  It should
also checkpoint its memory but doesn't, so restarting may be
quite slow if you have a lot of events.  To reset, `vere -R.`

In $directory are two things - a filesystem tree and an event
log (`~egz.hope`).  

Files in the tree are mirrored in Arvo's revision control system,
with changes detected automagically.  So, even though Arvo cannot
read the Unix filesystem, you can edit files externally and use
them within Arvo.  (At present the change detection is not
actually magic, but will happen every time you hit return on the
command line.)

`~egz.hope` contains your events, encrypted with your passcode.
Every keypress on the command line, filesystem change, network
packet, etc, is in this file.  Since it is encrypted, you can
check it into a public github repo or similar.

Now we're ready to continue and learn Nock.

1.3 Nock
--------

To make Nock make sense, let's work through Nock 5K line by line.
First the data model:

##1. Structures##

    A noun is an atom or a cell.  An atom is any natural number.
    A cell is any ordered pair of nouns.

Nouns are the dumbest data model ever.  Nouns make JSON look like
XML and XML look like ASN.1.  It may also remind you of Lisp's
S-expressions - you can think of nouns as "S-expressions without
the S."

To be exact, a noun _is_ an S-expression, except that classic
S-expressions have multiple atom types ("S" is for "symbol").
Since Nock is designed to be used with a higher-level type system
(such as Hoon's), it does not need low-level types.  An atom is
just an unsigned integer of any size.

For instance, it's common to represent strings (or even whole
text files) as atoms, arranging them LSB first - so "foo" becomes
`0x6f6f66`.  How do we know to print this as "foo", not `0x6f6f66`?
We need external information - such as a Hoon type.  Similarly,
other common atomic types - signed integers, floating point, etc
- are all straightforward to map into atoms.

It's also important to note that, unlike Lisp, Nock cannot create
cyclical data structures.  It is normal and common for nouns in a
Nock runtime system to have acyclic structure - shared subtrees.
But there is no Nock computation that can make a child point to
its parent.  One consequence: Nock has no garbage collector.
(Nor can dag structure be detected, as with Lisp `eq`.)

There is also no single syntax for nouns.  If you have nouns you
have Nock; if you have Nock you have Hoon; if you have Hoon, you
can write whatever parser you like.

Let's continue:

##2. Pseudocode##

It's important to recognize that the pseudocode of the Nock spec
is just that: pseudocode.  It looks a little like Hoon.  It isn't
Hoon - it's just pseudocode.  Or in other words, just English.
At the bottom of every formal system is a system of axioms, which
can only be written in English.  (Why pseudocode, not Hoon?  Since
Hoon is defined in Nock, this would only give a false impression
of nonexistent precision.)

The logic of this pseudocode is a pattern-matching reduction,
matching from the top down.  To compute Nock, repeatedly reduce
with the first line that matches.   Let's jump right in!

##Line 1:##

	1  ::    [a b c]           [a [b c]]

Ie, brackets (in our pseudocode, as in Hoon) associate to the
right.  For those with Lisp experience, it's important to note
that Nock and Hoon use tuples or "improper lists" much more
heavily than Lisp.  The list terminator, normally 0, is never 
automatic.  So the Lisp list 

	(a b c)

becomes the Nock noun

	[a b c 0]

which is equivalent to

	[a [b [c 0]]]

Note that we can and do use unnecessary brackets anyway, for
emphasis.

##Line 2##

	2  ::    nock(a)           *a

Nock is a pure (stateless) function from noun to noun.  In our
pseudocode (and only in our pseudocode) we express this with the
prefix operator `*`.

This function is defined for every noun, but on many nouns it
does nothing useful.  For instance, if `a` is an atom, `*a`
reduces to... `*a`.  In theory, this means that Nock spins
forever in an infinite loop.  In other words, Nock produces no
result - and in practice, your interpreter will stop.

(Another way to see this is that Nock has "crash-only" semantics.
There is no exception mechanism.  The only way to catch Nock
errors is to simulate Nock in a higher-level virtual Nock -
which, in fact, we do all the time.  A simulator (or a practical
low-level interpreter) can report, out of band, that Nock would
not terminate.  It cannot recognize all infinite loops, of
course, but it can catch the obvious ones - like `*42`.)

Normally `a` in `nock(a)` is a cell `[s f]`, or as we say

	[subject formula]

Intuitively, the formula is your function and the subject is 
its argument.  We call them something different because Hoon,
or any other high-level language built on Nock, will build its
own function calling convention which *does not* map directly
to `*[subject formula]`.

Let's move on to the axiomatic functions.  

##Lines 4-8:##

	4  ::    ?[a b]            0
	5  ::    ?a                1
	6  ::    +a                1 + a
	7  ::    =[a a]            0
	8  ::    =[a b]            1

Here we define more pseudocode operators, which we'll use in
reductions further down.  So far we have four built-in functions:
`*` meaning Nock itself, `?` testing whether a noun is a cell or
an atom, `+` incrementing an atom, and `=` testing for equality.
Again, no rocket science here.

We should note that in Nock and Hoon, `0` (pronounced "yes") is
true, and `1` ("no") is false.  Why?  It's fresh, it's different,
it's new.  And it's annoying.  And it keeps you on your toes.
And it's also just intuitively right.


##Lines 10-14:##

	10 ::    /[1 a]            a
	11 ::    /[2 a b]          a
	12 ::    /[3 a b]          b
	13 ::    /[(a + a) b]      /[2 /[a b]]
	14 ::    /[(a + a + 1) b]  /[3 /[a b]]
    
Slightly more interesting is our tree numbering.  Every noun is of course a tree.  The `/` operator - pronounced
"slot" - imposes an address space on that tree, mapping every
nonzero atom to a tree position.

1 is the root.  The head of every node `n` is `2n`; the tail is
`2n+1`.  Thus a simple tree:

         1
      2      3
    4   5  6   7
             14 15

If the value of every leaf is its tree address, this tree is

	[[4 5] [6 14 15]]

and, for some examples of addressing:

	/[1 [[4 5] [6 14 15]]] 

is `[[4 5] [6 14 15]]]`
	
	/[2 [[4 5] [6 14 15]]]	

is `[4 5]`
	
	/[3 [[4 5] [6 14 15]]]  
	
is `[6 14 15]`, and

	/[7 [[4 5] [6 14 15]]]  
	
is `[14 15]`

I do hope this isn't so terribly hard to follow.  

##Line 18:##

Now we enter the definition of Nock itself - ie, the `*`
operator.

    18 ::    *[a 0 b]          /[b a]

`0` is simply Nock's tree-addressing operator.  Let's try it out
from the Arvo command line.  

Note that we're using Hoon syntax here.  Since we do not use Nock
from Hoon all that often (it's sort of like embedding assembly in
C), we've left it a little cumbersome.  In Hoon, instead of
writing `*[a 0 b]`, we write 

	.*(a [0 b])

So, to reuse our slot example, let's try the interpreter:

	~tasfyn-partyv> .*([[4 5] [6 14 15]] [0 7])

gives, while the sky remains blue and the sun rises in the east:

	[14 15]

Even stupider is line 19:

##Line 19:##

	19 ::    *[a 1 b]          b

`1` is the constant operator.  It produces its argument without
reference to the subject.  So

	~tasfyn-partyv> .*(42 [1 153 218])

yields

	[153 218]


##Line 20:##

    20 ::    *[a 2 b c]        *[*[a b] *[a c]]

Line 27 brings us the essential magic of recursion.
`2` is the Nock operator.  If you can compute a subject and a
formula, you can evaluate them in the interpreter.  In most
fundamental languages, like Lisp, `eval` is a curiosity.  But
Nock has no `apply` - so all our work gets done with `2`.

Let's convert the previous example into a stupid use of `2`:

	~tasfyn-partyv> .*(77 [2 [1 42] [1 1 153 218]])

with a constant subject and a constant formula, gives the same

	[153 218]
	
Like so:

	*[77 [2 [1 42] [1 1 153 218]]
	
	20 ::    *[a 2 b c]        *[*[a b] *[a c]]
	
	*[*[77 [1 42]] *[77 [1 1 153 218]]]
	
	19 ::    *[a 1 b]          b
	
	*[42 *[77 [1 1 153 218]]]

	*[42 1 153 218]

	[153 218]

##Lines 21-23:##

	21 ::    *[a 3 b]          ?*[a b]
	22 ::    *[a 4 b]          +*[a b]
	23 ::    *[a 5 b]          =*[a b]
    
In lines 21-23, we meet our axiomatic functions again:

For instance, if `x` is a formula that calculates some product,
`[4 x]` calculates that product plus one.  Hence:

	~tasfyn-partyv> .*(57 [0 1])
	57

and

	~tasfyn-partyv> .*([132 19] [0 3])
	19

and

	~tasfyn-partyv> .*(57 [4 0 1])
	58

and 

	~tasfyn-partyv> .*([132 19] [4 0 3])
	20

If this seems obvious, you're doin' good.  Finally, we jump back up
to line 16, the trickiest in the spec:

##Line 16##

	16 ::    *[a [b c] d]      [*[a b c] *[a d]]

Um, what?

Since Nock of an atom just crashes, the practical domain of the
Nock function is always a cell.  Conventionally, the head of this
cell is the "subject," the tail is the "formula," and the result
of Nocking it is the "product."  Basically, the subject is your
data and the formula is your code.

We could write line 16 less formally:

	*[subject [formula-x formula-y]]
	=>  [*[subject formula-x] *[subject formula-y]]

In other words, if you have two Nock formulas `x` and `y`, a
formula that computes the pair of them is just `[x y]`.  We can 
recognize this because no atom is a valid formula, and
every formula that _does not_ use line 16 has an atomic head.

If you know Lisp, you can think of this feature as a sort of
"implicit cons."  Where in Lisp you would write `(cons x y)`,
in Nock you write `[x y]`.

For example,

	~tasfyn-partyv> .*(42 [4 0 1])

where `42` is the subject (data) and `[4 0 1]` is the formula
(code), happens to evaluate to `43`.  Whereas

	~tasfyn-partyv> .*(42 [3 0 1])

is `1`.  So if we evaluate

	~tasfyn-partyv> .*(42 [[4 0 1] [3 0 1]])

we get

	[43 1]

Except for the crash defaults (lines 32-35), we've actually
completed all the _essential_ aspects of Nock.  The operators up
through 5 provide all necessary computational functionality.
Nock, though very simple, is actually much more complex than it
formally needs to be.

Operators 6 through 10 are macros.  They exist because Nock is
not a toy, but a practical interpreter.  Let's see them all
together:

##Lines 25-30:##

	25 ::    *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]
	26 ::    *[a 7 b c]        *[a 2 b 1 c]
	27 ::    *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]
	28 ::    *[a 9 b c]        *[a 7 c [2 [0 1] [0 b]]]
	29 ::    *[a 10 b c]       *[a c]
	30 ::    *[a 10 [b c] d]   *[a 8 c 7 [0 2] d]

Whoa!  Have we entered rocket-science territory?  Let's try to
figure out what these strange formulas do - simplest first.
The simplest is clearly line 29:

	29 ::    *[a 10 b c]       *[a c]

If `x` is an atom and `y` is a formula, the formula `[10 x y]` 
appears to be equivalent to... `y`.  For instance:

	~tasfyn-partyv> .*([132 19] [10 37 [4 0 3]])
	20

Why would we want to do this?  `10` is actually a hint operator.
The `37` in this example is discarded information - it is not
used, formally, in the computation.  It may help the interpreter
compute the expression more efficiently, however.

Every Nock computes the same result - but not all at the same
speed.  What hints are supported?  What do they do?  Hints are a
higher-level convention which do not, and should not, appear in
the Nock spec.  Some are defined in Hoon.  Indeed, a naive Nock
interpreter not optimized for Hoon will run Hoon quite poorly.
When it gets the product, however, the product will be right.

There is another reduction for hints - line 30:

	30 ::    *[a 10 [b c] d]   *[a 8 c 7 [0 2] d]

Once we see what `7` and `8` do, we'll see that this complex hint
throws away an arbitrary `b`, but computes the formula `c`
against the subject and... throws away the product.  This formula
is simply equivalent to `d`.  Of course, in practice the product
of `c` will be put to some sordid and useful use.  It could even
wind up as a side effect, though we try not to get _that_ sordid.

(Why do we even care that `c` is computed?  Because `c` could
crash.  A correct Nock cannot simply ignore it, and treat both
variants of `10` as equivalent.)

We move on to the next simplest operator, `7`.  Line 26:

		26 ::    *[a 7 b c]        *[a 2 b 1 c]

Suppose we have two formulas, `b` and `c`.  What is the formula 
`[7 b c]`?  This example will show you:

	~tasfyn-partyv> .*(42 [7 [4 0 1] [4 0 1]])
	44
	
`7` is an old mathematical friend, function composition.  It's
easy to see how this is built out of `2`.  The data to evaluate
is simply `b`, and the formula is `c` quoted.

Line 27 looks very similar:

	27 ::    *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]

Indeed, `8` is `7`, except that the subject for `c` is not simply
the product of `b`, but the ordered pair of the product of `b`
and the original subject.  Hence:

	~tasfyn-partyv> .*(42 [8 [4 0 1] [0 1]])
	[43 42]

and 

	~tasfyn-partyv> .*(42 [8 [4 0 1] [4 0 3]])
	43

Why would we want to do this?  Imagine a higher-level language
in which the programmer declares a variable.  This language is
likely to generate an `8`, because the variable is computed
against the present subject, and used in a calculation which
depends both on the original subject and the new variable.

For extra credit, explain why we can't just define

	*[a 8 b c]        *[a 7 [b 0 1] c]

Another simple macro is line 28:

	28 ::    *[a 9 b c]        *[a 7 c [2 [0 1] [0 b]]]

`9` is a calling convention.  With `c`, we produce a noun which
contains both code and data - a _core_.  We use this core as the
subject, and apply the formula within it at slot `b`.

And finally, we come to the piece de resistance - line 25:

	25 ::    *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

Great giblets!  WTF is this doing?  It seems we've finally
arrived at some real rocket science.

Actually, `6` is a primitive known to every programmer - good old
"if."  If `b` evaluates to `0`, we produce `c`; if `b` evaluates
to `1`, we produce `d`; otherwise, we crash.

For instance:

	~tasfyn-partyv> .*(42 [6 [1 0] [4 0 1] [1 233]])
	43

and

	~tasfyn-partyv> .*(42 [6 [1 1] [4 0 1] [1 233]])
	233

In real life, of course, the Nock implementor knows that `6` is
"if" and implements it as such.  There is no practical sense in
reducing through this macro, or any of the others.  We could have
defined "if" as a built-in function, like increment - except that
we can write "if" as a macro.  If a funky macro. 

It's a good exercise, however, to peek inside the funk.

We can actually simplify the semantics of `6`, at the expense of
breaking the system a little, by creating a macro that works as
"if" only if `b` is a proper boolean and produces `0` or `1`.
Perhaps we have a higher-level type system which checks this.

This simpler "if" would be:

	*[a 6 b c d]    *[a [2 [0 1] [2 [1 c d] [[1 0] [4 4 b]]]]]

Or without so many unnecessary brackets:

	*[a 6 b c d]    *[a 2 [0 1] 2 [1 c d] [1 0] [4 4 b]]

How does this work?  We've replaced `[6 b c d]` with the formula
`[2 [0 1] [2 [1 c d] [[1 0] [4 4 b]]]]`.  We see two uses of `2`,
our evaluation operator - an outer and an inner.

Call the inner one `i`.  So we have `[2 [0 1] i]`.  Which means
that, to calculate our product, we use `[0 1]` - that is, the
original subject - as the subject; and the product of `i` as
the formula.

Okay, cool.  So `i` is `[2 [1 c d] [[1 0] [4 4 b]]]`.  We compute 
Nock with subject `[1 c d]`, formula `[[1 0] [4 4 b]]`.

Obviously, `[1 c d]` produces just `[c d]` - that is, the ordered
pair of the "then" and "else" formulas.  `[[1 0] [4 4 b]]` is a
line 23 cell - its head is `[1 0]`, producing just `0`, its tail
`[4 4 b]`, producing... what?  Well, if `[4 b]` is `b` plus `1`,
`[4 4 b]` is `b` plus `2`.

We're assuming that `b` produces either `0` or `1`.  So `[4 4 b]`
yields either `2` or `3`.  `[[1 0] [4 4 b]]` is either `[0 2]` or
`[0 3]`.  Applied to the subject `[c d]`, this gives us either
`c` or `d` - the product of our inner evaluation `i`.  This is
applied to the original subject, and the result is "if."

But we need the full power of the funk, because if `b` produces,
say, `7`, all kinds of weirdness will result.  We'd really like
`6` to just crash if the test product is not a boolean.  How can
we accomplish this?  This is an excellent way to prove to
yourself that you understand Nock: figure out what the real `6`
does.  Or you could just agree that `6` is "if," and move on.

(It's worth noting that in practical, compiler-generated Nock, we
never do anything as funky as these `6` macro internals.  There's
no reason we couldn't build formulas at runtime, but we have no
reason to and we don't - except when actually metaprogramming.
As in most languages, normally code is code and data is data.)

A good practice exercise for Nock is a decrement formula.  Ie, a
formula `f` which implements the partial function that produces
`(s - 1)` if `s` is a nonzero atom, and otherwise does not
terminate.  

The normal Hoon programmer has written one Nock formula: this
one.  Since decrement uses all the Nock techniques the Hoon
compiler uses, the exercise is a good foundation.  After you
write decrement (or just follow this example), you'll never need
to deal with Nock again.

As we know, the equivalent formula for increment is

	[4 0 1]

Thus:

	~>tasfyn-partyv .*(42 [4 0 1])
	43

Of course, increment is built into Nock.  So, ha, that's easy.

How do we decrement?  A good way to start is to gaze fondly on
how we'd do it if we actually had a real language, ie, Hoon.
Here is a minimal decrement in Hoon:

	=>  a=.                     ::  line 1
	=+  b=0                     ::  line 2
	|-                          ::  line 3
	?:  =(a +(b))               ::  line 4
	  b                         ::  line 5
	$(b +(b))                   ::  line 6

Or for fun, on one line:

	=>(a=. =+(b=0 |-(?:(=(a +(b)) b $(b +(b))))))

Does Hoon actually work?

	~tasfyn-partyv>  =>(42 =>(a=. =+(b=0 |-(?:(=(a +(b)) b $(b +(b)))))))
	41

Let's translate this into English.  How do we decrement the
subject?  First (line 1), we rename the subject `a`.  Second
(line 2), we add a variable, `b`, an atom with value `0`.
Third (line 3), we loop.  Fourth, we test if `a` equals `b` plus
1 (line 4), produce `b` if it does (line 5), repeat the loop with
`b` set to `b` plus 1 (line 6) if it doesn't.  Obviously, while
the syntax is unusual, the algorithm is anything but deep.  We
are calculating `b` minus one by counting up from `0`.

(Obviously, this is an O(n) algorithm.  Is there a better way?
There is not.  Do we actually do this in practice?  Yes and no.)

Unfortunately we are missing a third of our Rosetta stone.  We
have decrement in Hoon and we have it in English.  How do we
express this in Nock?  What will the Hoon compiler generate from
the code above?  Let's work through it line by line.

Nock has no types, variable names, etc.  So line 1 is a no-op.

How do we add a variable (line 2)?  We compute a new subject,
which is a cell of the present subject and the variable.  With
this new subject, we execute another formula.

Since `0` is a constant, a formula that produces it is

	[1 0]

To combine `0` with the subject, we compute

	[[1 0] [0 1]]

which, if our subject is 42, gives us

	[0 42]

which we can use as the subject for an inner formula, `g`.
Composing our new variable with `g`, we have `f` as

	[2 [[1 0] [0 1]] [1 g]]

which seems a little funky for something so simple.  But we
can simplify it with the composition macro, `7`:

	[7 [[1 0] [0 1]] g]

and still further with the augmentation macro, `8`:

	[8 [1 0] g]

If you refer back to the Nock definition, you'll see that all
these formulas are semantically equivalent.

Let's continue with our decrement.  So what's `g`?  We seem to
loop.  Does Nock have a loop operator?  It most certainly does
not.  So what do we do?

We build a noun called a _core_ - a construct which is behind any
kind of interesting control flow in Hoon.  Of course, the Nock
programmer is not constrained to use the same techniques as the
Hoon compiler, but it is probably a good idea. 

In Hoon, all the flow structures from your old life as an Earth
programmer become cores.  Functions and/or closures are cores,
objects are cores modules are cores, even loops are cores.

The core is just a cell whose tail is data (possibly containing
other cores) and whose head is code (containing one or more
formulas).  The tail is the _payload_ and the head is the
_battery_.  Hence your core is

	[bat pay]

To activate a core, pick a formula out of the battery, and use
the entire core (_not_ just the payload) as the subject.  

(A core formula is called an _arm_.  An arm is almost like an
object-oriented method, but not quite - a method would be an arm
that produces a function on an argument.  The arm is just a
function of the core, ie, a computed attribute.)

Of course, because we feed it the entire core, our arm can
invoke itself (or any other formula in the battery).  Hence, it
can loop.  And this is what a loop is - the simplest of cores.

We need to do two things with this core: create it, and activate
it.  To be precise, we need two formulas: a formula which
produces the core, and one which activates its subject.  We can
compose these functions with the handy `7` operator:

	[8 [1 0] [7 p a]]

`p` produces our core, `a` activates it.  Let's take these in
reverse order.  How do we activate a core?

Since we have only one formula, it's is the battery itself.
Thus we want to execute Nock with the whole core (already the
subject, and the entire battery (slot `2`).  Hence, `a` is

	[2 [0 1] [0 2]]

We could also use the handy `9` macro - which almost seems
designed for firing arms on cores:

	[9 2 [0 1]]

Which leaves us seeking 

	[8 [1 0] [7 p [9 2 0 1]]]

And all we have to do is build the core, `p`.  How do we build a
core?  We add code to the subject, just as we added a variable
above.  The initial value of our counter was a constant, `0`.
The initial (and permanent) value of our battery is a constant, 
the loop formula `l`.  So `p` is

	[8 [1 l] [0 1]]

Which would leave us seeking

	[8 [1 0] [7 [8 [1 l] [0 1]] [9 2 0 1]]]

except that we have duplicated the `8` pattern again, since we
know

	[7 [8 [1 l] [0 1]] [9 2 0 1]]

is equivalent to  

	[8 [1 l] [9 2 0 1]]

so the full value of `f` is

	[8 [1 0] [8 [1 l] [9 2 0 1]]]

Thus our only formula to compose is the loop body, `l`.
Its subject is the loop core:

	[bat pay]

where `bat` is just the loop formula, and `pay` is the pair `[a
b]`, `a` being the input subject, and `b` the counter.  Thus we
could also write this subject as

	[l b a]

and we see readily that `a` is at slot `7`, `b` `6`, `l` `2`.
With this subject, we need to express the Hoon loop body

	?:  =(a +(b))               ::  line 4
	  b                         ::  line 5
	$(b +(b))                   ::  line 6

This is obviously an if statement, and it calls for `6`.  Ie:

	[6 t y n]

Giving our decrement program as:

	[8 [1 0] [8 [1 6 t y n] [9 2 0 1]]]

For `t`, how do we compute a flag that is yes (`0`) if `a` equals
`b` plus one?  Equals, we recall, is `5`.  So `t` can only be
 
	[5 [0 7] [4 0 6]]

If so, our product `y` is just the counter `b`:

	[0 6]

And if not?  We have to re-execute the loop with the counter 
incremented.  If we were executing it with the same counter, 
obviously an infinite loop, we could use the same core:

	[9 2 0 1]

But instead we need to construct a new core with the counter 
incremented:

	[l +(b) a]

ie, 

	[[0 2] [4 0 6] [0 7]] 

and `n` is:

	[9 2 [[0 2] [4 0 6] [0 7]]]
 
Hence our complete decrement.  Let's reformat vertically so we
can actually read it:

	 [8 
	   [1 0] 
	   [ 8 
	     [ 1 
	       [ 6 
	         t 
	         y 
	         n
	       ]
	     ]
	     [9 2 0 1]
	   ]
	 ]

which becomes

	  [8 
	    [1 0] 
	    [ 8 
	      [ 1 
	        [ 6 
	          [5 [0 7] [4 0 6]]
	          [0 6]
	          [9 2 [[0 2] [4 0 6] [0 7]]]
	        ]
	      ]
	      [9 2 0 1]
	    ]
	  ]

or, on one line without superfluous brackets:

	[8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]

which works for the important special case, 42:

	~tasfyn-partyv> .*(42 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1])
	41

If you understood this, you understand Nock.  At least in principle!

If you want to play around more with Nock, the command line will
start getting unwieldy.  Fortunately, the standard install
contains the above Nock decrement packaged as an Arvo app, which
you can edit and change if you'd like to get ambitious.  Just run

	~tasfyn-partyv> :toy/ndec 19
	18

The file driving this is 
  
	hub/$seat/toy/app/ndec.holw

Edit this file, ignoring everything above the Nock formula, and
hit return in the console to see it update:

	~tasfyn-partyv> 
	: ~tasfyn-partyv/toy/app/ndec/holw/

If decrement seems fun - why not write add?  I wrote a Nock adder
a long, long time ago.  But I've forgotten where I put it.  There
is absolutely no use in this exercise, except to prove to
yourself that you've mastered Nock.

Appendix A: Operator Reductions
-------------------------------

##`6` Reduction:##


	25 ::    *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]
	
	*[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

	20 ::    *[a 2 b c]        *[*[a b] *[a c]]
	
	*[*[a 0 1] *[a 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]]

	18 ::    *[a 0 b]          /[b a]
	
	*[a *[a 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]]
	
	20 ::    *[a 2 b c]        *[*[a b] *[a c]]
	
	*[a *[*[a [1 c d]] *[a [1 0] 2 [1 2 3] [1 0] 4 4 b]]]
	
	19 ::    *[a 1 b]          b
	
	16 ::    *[a [b c] d]      [*[a b c] *[a d]]
	
	*[a *[[c d] [*[a 1 0] *[a 2 [1 2 3] [1 0] 4 4 b]]]]

	19 ::    *[a 1 b]          b
	
	*[a *[[c d] [0 *[a 2 [1 2 3] [1 0] 4 4 b]]]]
	
	20 ::    *[a 2 b c]        *[*[a b] *[a c]]
	
	*[a *[[c d] [0 *[*[a [1 2 3]] *[a [1 0] 4 4 b]]]]]
	
	19 ::    *[a 1 b]          b
	
	*[a *[[c d] [0 *[[2 3] *[a [1 0] 4 4 b]]]]]
	
	16 ::    *[a [b c] d]      [*[a b c] *[a d]]
	
	*[a *[[c d] [0 *[[2 3] [*[a [1 0]] *[a 4 4 b]]]]]]
	
	19 ::    *[a 1 b]          b
	
	*[a *[[c d] [0 *[[2 3] [0 *[a 4 4 b]]]]]]
	
	22 ::    *[a 4 b]          +*[a b]
	
	*[a *[[c d] [0 *[[2 3] [0 ++[a b]]]]]]
	
**`6` Reduced:**

	6r ::   *[a 6 b c d]               *[a *[[c d] [0 *[[2 3] [0 ++[a b]]]]]]
	
##`7` Reduction:##

	26 ::    *[a 7 b c]        *[a 2 b 1 c]

	*[a 2 b 1 c]

	20 ::    *[a 2 b c]        *[*[a b] *[a c]] 
	
	*[*[a b] *[a 1 c]]

	19 ::    *[a 1 b]          b
	 
	*[*[a b] c]
	 
**`7` Reduced:**

	7r ::     *[a 7 b c]         *[*[a b] c]

##`8` Reduction:##

    27 ::    *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]
    
    *[a 7 [[7 [0 1] b] 0 1] c]   
    
	7r ::     *[a 7 b c]         *[*[a b] c]
	
    *[*[a [7 [0 1] b] 0 1]] c]

	16 ::    *[a [b c] d]      [*[a b c] *[a d]]

	*[[*[a [7 [0 1] b]] *[a 0 1]] c]
	
	18 ::    *[a 0 b]          /[b a]
	
	*[[*[a [7 [0 1] b]] /[1 a]] c]
	
	10 ::    /[1 a]            a

	*[[*[a [7 [0 1] b]] a] c]
	
	7r ::     *[a 7 b c]         *[*[a b] c]
	
	*[[*[*[a 0 1]] b] a] c]
	
**`8` Reduced:**

	8r ::     *[a 8 b c]        *[[*[a b] a] c]


##`9` Reduction:##

    *[a 9 b c]        *[a 7 c [2 [0 1] [0 b]]]
    
    *[a 7 c [2 [0 1] [0 b]]]
    
	7r ::     *[a 7 b c]         *[*[a b] c]
    
	*[*[a c] [2 [0 1] [0 b]]]
	
    20 ::     *[a 2 b c]        *[*[a b] *[a c]]
    
    *[*[*[a c] [0 1]] *[*[a c] [0 b]]]
    
    18 ::     *[a 0 b]          /[b a]
    
**`9` Reduced:**    

    9r ::     *[a 9 b c]        *[*[a c] *[*[a c] 0 b]] 
    

##`10` Reduction:##

	*[a 10 [b c] d]   *[a 8 c 7 [0 2] d]
	
	8r ::     *[a 8 b c]        [[*[a b] a] c]

	*[[*[a c] a] 7 [0 2] d]
	
	7r ::     *[a 7 b c]        *[*[a b] c]

	*[*[[*[a c] a] 0 2] d]
	
	18 ::     *[a 0 b]          /[b a]

**`10` reduced:**

	10r ::    *[a 10 [b c] d]   *[a d]



