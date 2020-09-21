# SKEW

by Benjamin `~siprel` and Elliot `~littel-ponnys`

SKEW is a 4 letter extension of the SK Calculus with the ideas developed for
Nock. This post is designed to give an overview of SKEW's predecessors, what
SKEW is, why it should become the successor to Nock 4K and why it's better.

The [SK Calculus][skwiki] is a Turing complete combinator introduced by
Schönfinkel and Curry in the 1920s. There is a straightforward encoding of the
Lambda Calculus into the SK combinator. It is simple, and well understood. We
believe it is the best foundation to build on due to how well researched it is
and its simplicity.

Nock is both an aspirational thought experiment and a series of combinators,
[first proposed][ognock] by Curtis Yarvin. It was intended to define a frozen
core language for his Urbit system. Like many functional programming systems,
Urbit was divided into a simple core language which was impractical to work in
and a front end language which targeted that core language. In this document,
I'll refer to the aspiration as "Nock" and the current implementation with its
version number, such as "Nock 4K".

If you do not care about the history of Nock, you can easily skip to the
section "Introducing SKEW," but understanding the problems Nock was supposed to
solve and the problems with Nock 4K which lead to considering alternatives may
be interesting.

[ognock]: http://moronlab.blogspot.com/2010/01/nock-maxwells-equations-of-software.html
[skwiki]: https://en.wikipedia.org/wiki/SKI_combinator_calculus

# A Very Brief Background on Nocks and Why You'd Want Them

Aspirationally, Nock is supposed to be the final pure functional core language.

Nock and Urbit were [originally proposed as part of a thought
experiment][moronlabsurbit], where people from Mars have had millions of years
of experience developing software, over and over again until they escape the
trap of "big balls of mud," and build software which is "tiny and diamond-
perfect". Given that they've had millions of years to attempts to build [The
Right Thing][rightthing], they'd eventually succeed at building the "tiny and
diamond-perfect" system. Being "tiny and diamond-perfect", no further changes
would be needed ever again.

Since your core language could never change once frozen, any new capabilities
which would be primops in other languages must be expressed in pure code. To
make this practical, we come to what I consider Nock's big innovation: the
Jet. Your pure functional core language should be designed in a way which makes
it easy for the run-time system to replace a recognized pure functional
definition of what would otherwise be a new primop with an equivalent native
implementation. That way, your system can learn new capabilities in a
progressive enhancement way and can gracefully degrade if the user is using an
older interpreter.

The Jet has been the one thing about the Urbit system which has been copied by
other systems. David Barbour's [Awelon][awelon] knows them as "accelerators"
and Blockstream's [Simplicity][simplicity] adds Jets to a language much like
[Charity][charity].

But why would you want such a thing? Because it gives a pathway for the state
of the entire system to be a portable value, with no impure escape
hatches. With no FFI or escape hatches to purity, your system can be an
inspectable value which contains all of a user's programs and data forever,
forward and backward compatible between interpreters.

Urbit as a whole would be an event transducer whose entire state would be the
closure:

    type Urbit = Event -> (Urbit, [Effect])

Every action your Urbit would take would be because of a discrete input event
which caused a state change and/or an output side effect. You could write this
as snapshots or as a full write-ahead log. Everything that Urbit could do would
be loaded through new events passed to your Urbit.

Doing this moves the problems of persistence out of the system and into the
runtime. Programmers no longer need to worry about the persistence and fsyncs
and other such details because the runtime treats event processing like an ACID
database. Either a transaction succeeds or it fails: an event can't half
succeed and this greatly simplifies error handling.

Once you have a system whose state is deterministically generated purely from
input events, you can mitigate the [Trusting Trust][trustingtrust] attack by
verifying that different interpreters calculate the exact same state from the
same sequence of input events. Because it is so small and simple because it is
"tiny and diamond-perfect", any motivated college student should be able to
write their own compatible interpreter instead of relying on one built by
others, like the aspirations of the Cypherpunks of old. Contrast this to
something like the JVM, whose size and complexity make construction of
ammeature runtimes infeasible.

Once a user can, in principle, understand every layer of the system from the
bottom to the top, they have sovereignty over their computing in a way that
they don't with the current software stack which is incomprehensible.

This leads to the following system requirements:

1. The final Nock must be formally specified and permanently frozen. Once it's
   done, we can't change representations or add new features. Aesthetically, it
   should be as simple as possible. Otherwise, it's another iteration of a big
   ball of mud and should be replaced by a simpler system.

2. It must be possible to dynamically define new functions based on input
   data. Minus a tiny kernel written directly in the core language for
   bootstrapping the initial state, everything the system does is learned
   through new input events, including what would otherwise be new primops in
   current core languages.

3. It must be possible to introspect and/or serialize arbitrary data, including
   functions and closures. The format of closures must be inspectable and
   serializable in a standard way to meet the interoperability requirements
   between interpreters.

Current core languages, such as GHC Core, are not appropriate as a Nock both
because they don't aspire to be frozen and because it's practical reliance on
[primops][hsprimops] which could not be implemented purely. While Haskell's
`int2Integer#` could be implemented as a jet and could have a pure definition,
many of Haskell's primops are about IO, threading and STM, which are
non-deterministic and have side effects. This should not be held against GHC or
Haskell since their goals are rather different from ours, and it allows for
convenient things like FFI to arbitrary libraries and multi-threading. Nock
programs will never have completely arbitrary FFI and any concurrency would
have to be built out of actors.

[moronlabsurbit]: http://moronlab.blogspot.com/2010/01/urbit-functional-programming-from.html
[rightthing]: http://www.dreamsongs.com/RiseOfWorseIsBetter.html
[hsprimops]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/prim-ops
[awelon]: https://github.com/dmbarbour/wikilon/blob/master/docs/AwelonLang.md
[simplicity]: https://blockstream.com/simplicity.pdf
[charity]: http://pll.cpsc.ucalgary.ca/charity1/download/literature/papers_and_reports/about_charity.ps.gz
[trustingtrust]: https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf

# Nock in Reality

In reality, Nock 4K+ does not meet the aspirations. It is not "tiny and
diamond-perfect". Nock 4K is a big ball of mud.

Nock opcode 11, which implements the concept of jets, has the design flaw that
it works through impure side effects and is stateful. One can only have a
jetted value by running a piece of code which returns a jetted value: running
Nock opcode 11 has caused a registration side effect invisible to your
program. If you inspect the returned jetted value inside your program, it looks
like normal Nock, but if you try to execute it, the interpreter consults
additional state. This additional jet registration state breaks the
serialization requirements for nock: if you serialize a Hoon core, you don't
serialize its jet info, because that was in the Nock opcode 11 which produced
this value. If you serialize it at the interpreter level, you no longer have
values of the simple Noun data structure because you must also store the jet
registration side effects, which are not standardized. (There is a proposed
alternative of resolving jets by the cryptographic hash of the code, but this
turns runtime function calls into hash table lookups, and is not used in
production Urbit.)

Nock opcodes 6 through 10 are not needed for completeness, and are only there
as optimizations. Nock 9 in particular is an optimization for how Urbit's Hoon
frontend language implements closures and is a leak of the semantics of the
frontend language into the core language. Nock 10 was added during the
transition from Nock 5K to Nock 4K and is an optimization which makes efficient
modifications to a Nock data structure. Most of these should have been
implemented as jets and their inclusion is certainly not "tiny and
diamond-perfect". But why then are they here? Why aren't Nock opcodes 6 through
10 jets in the first place?

Nock opcode 9 and 10 are used in the calling convention; if they were jets that
you had to nominally call, how would you call them without them? But for the
rest, if they were jetted functions, you couldn't rely on them to exist in the
subject anyway.

Nock 4K's implementation of serializable closures is tied to the idea that a
function's entire environment will be passed to it as an argument. This is
called the "subject". The subject is a single binary tree value which contains
all parameters passed to the function being called, along with all functions
which may be called, including ones this function does not use. Values in this
binary tree are referred to by "tree indexes", a way of numbering all possible
ways to descend through a binary tree as a single natural number. Nock opcode 0
performs this resolution by descending through the nodes in a binary tree until
it finds the referred to node.

When a piece of Nock 4K code is compiled, it finds the references to the
functions it needs in the subject it is compiled against and emits the tree
indexes in the generated code. When a piece of Nock code is called, it requires
a compatible environment in which to lookup functions that it calls. This is at
least one pointer indirection and usually many more than one! While other
systems dispatch functions with either a direct or a single indirect jump,
function dispatch in Nock 4K requires multiple indirect jumps. Pointer chasing
is slow and Nock 4K does a ton of it. Finally, on the first dispatch with a
given subject, there's a hash table lookup to see if there's a jet match.

This is part of the calling convention for Nock 4K: you look up the code you're
going to call in your subject (at least one pointer indirection, usually a long
linked list traversal), you replace an empty version of where the arguments
would be with the real arguments you're going to pass (another pointer
indirection and an allocation).

Doing linked list traversal is bad. Allocating inside your function calling
convention is bad. Doing hash table lookups during function call dispatch is
bad. On a quick test running the Ackermann function, the Urbit v0.10.7
interpreter spent 45% of the runtime in the allocator or in reference count
management. It had an L2 cache miss rate of approximately 25%. This is
bonkers. We know it's slow, but why exactly is it slow?

I traced through running `(add 2 2)`. When you count up the work that goes into
dispatching a single function call, the current nock interpreter does the stack
and program counter management overhead to perform 23 internal bytecodes, which
allocates 7 cons cells, increases the refcount of 12 memory locations,
decreases the refcount of 4 nouns (which may affect other memory locations
because refcount decrement is recursive), and performs 3 arbitrary tree
walks. We do this even if we are calling a jet: since jet registration is a
stateful side effect, we must perform all the tree math to first get the noun
which has the jet registration. And the number of arbitrary binary tree
traversals and cons cell allocations grow with the number of arguments passed
to the function.

Perhaps some clever set of caching and guards could cache function lookup in
the subject after the first invocation. But I don't know how you would speed up
the construction of the subject you are passing to the function you are
calling: you must perform an arbitrary tree walk to fetch each argument from
your current context (which will be different each time and cannot be guarded
on), and then you must allocate cons cells to construct the resulting binary
tree that you'll pass to the function you are calling (and it is not clear that
performing effective escape analysis to elide these allocations is even
possible). Making a performant implementation is entirely outside the ability
of a determined college student.

The traditional rejoinder to the above is that it doesn't matter that
individual function dispatch is slow since the jet system implies you should be
making few calls to unjetted code; you should be calling jets which are large
and perform a lot of work. But one must keep the specification written in Nock
in sync with the native jet implementation. The majority of the mismatches
between the Nock code and the jet which replaces it have been in these large
jets. It is the small, numeric jets like `+add` and data structure manipulation
jets which have a better record of exactly matching the Nock specification. The
largest jets in the system were formerly the compiler jets and that iteration
of them has been removed because of mismatches and because their size made
changes to both the Nock specification and the matching jet code difficult. And
how would a requirement for large jets work with 3rd party code?

There are other small issues with Curtis' Nock 4K: why bake in natural numbers
and cons cells? Why make the assumption that equality is always structural
equality and then bake that into Nock opcode 5? Not all data types have
equality defined (i.e. functions), and not all types have equality defined
structurally (i.e. fractions).

Finally, when one counts up the lines of reduction rules, there are 33. From an
aesthetic point of view, this is actually quite large for a system which
aspires to be "tiny and diamond-perfect".

# Introducing SKEW

There is already an ultra-minimalist pure functional core language: the SK
encoding of the unenriched lambda calculus. Implementing an interpreter for it
is trivial. The encoding of all data as functions is beautiful.

However, the unenriched lambda calculus is not used in practice because it is
infeasibly slow. You have no operations other than function
application. Performing SHA-256 on the church numeral encoding of natural
numbers will never happen on even our most powerful hardware.

The traditional approach is to enrich the lambda calculus with additional
built-in operations and data types, usually many. Things such as different
numeric types, cons cells, hash tables, and other data structures, along with
primitive operations for working with them. This makes for competitive
performance at the expense of the radical simplicity of everything being a
lambda expression. The sets of operations differ among the enriched lambda
calculi, since there isn't a principled reason to choose one set of enrichments
over another. And changing requirements and hardware mean this set grows over
time; how would one add [bfloat16][bfloat16] support without additional
primitives?

[bfloat16]: https://en.wikipedia.org/wiki/Bfloat16_floating-point_format

Finally, in these enriched lambda calculi, code is not data. Functions are
opaque values and cannot be inspected. Therefore, they cannot be interpreted
for virtualization; you cannot simply write `eval` inside your system taking a
closure as an argument. Therefore, they cannot be serialized; I cannot write a
function which takes any value and serializes it to a bytestring.

## The Basic Reduction Rules

Let's first list our functional requirements:

- We need to be able to perform any computable computation in a way which can
  be frozen forever. That's SK and is a solved problem.

- We need to be able to recognize functions and values and replace them with
  optimized versions to make runtime practical. Jets are our alternative to
  unprincipled enrichments to the lambda calculus.

- We need to be able to virtualize computations / have every value be
  inspectable and serializable. This allows us to serialize the closure
  which represents your entire system in a standard way and lets us write
  virtualized interpreter functions.

We propose an extension of the SK calculus with *four* combinators, `S`, `K`,
`E`, and `W`. These combinators are primitive objects which sit at the leaves
of an unlabeled binary tree. The cons operation of this tree is called
“application.”  We write `(a b)` for the tree with a on the left and b on the
right and `(a b c)` is shorthand for `((a b) c)`, so that application is
left-associative.

        *(K x y)             -> x
        *(x y)               -> (*x y)
        *(x y)               -> (x *y)
        *(S x y z)           -> (x z (y z))
        *(E^n t f x1 … xn)   -> (f x1 … xn)
        *(W a s k e w (x y)) -> (a x y)
        *(W a s k e w S)     -> s
        *(W a s k e w K)     -> k
        *(W a s k e w E)     -> e
        *(W a s k e w W)     -> w

These rules are meant to be read from top to bottom, and you must apply the
first rule that matches: there must always be one deterministic reduction to
take next so all SKEW interpreters return the exact same value. (The system is
not confluent.)

The first four rules give us the SK calculus with a strict, well defined
reduction ordering. The first reduction is the `K` combinator. The `K`
combinator returns its first argument and discards its second. Mnemonic:
*Konstant*.

    (K K K)
    K

Simple enough. The next two reduction rules specify order of operations: we
reduce the left hand side before the right hand side. The reduction for `K`,
comes before application. We must do this because our reduction rules are read
from top to bottom, and the first match wins. If we did not do this, both
arguments to K would be evaluated before the first is returned.

    (K K (S K K))
    K

After that, we have the `S` combinator. The `S` combinator substitutes its
arguments, returning its first, third, and then calling its second with its
third. Mnemonic: *Substitution*.

    (S K (S K) (S K K))
    (K (S K K) (S K (S K K)))
    (S K K)

That's it for the core of the SK system we have in SKEW. With `S` and `K`, we
can encode the entire unenriched lambda calculus.

Our next requirement is making functions in code legible to the
interpreter. Nock's big idea is the Jet: formally specify every function or
piece of code in your pure-functional system without any side-effects or FFI
and signal to the interpreter that a series of raw combinator reductions can be
replaced with a native, optimized implementation.

The E combinator specifies a jet and specifies order of execution. A jetted
value is represented as a number of E letters which specify the arity of the
function, followed by a tag which is only used for matching, followed by the
function being jetted, and finally a number of arguments according to the
stated arity. The arguments are evaluated first. Mnemonic: *Enhance*.

Look at the order in which the arguments are reduced in this trace:

    (E E K (S K) (K K (K K)) (S K K K))
    (E E K (S K) K (S K K K))
    (E E K (S K) K (K K (K K)))
    (E E K (S K) K K)
    (S K K K)
    (K K (K K))
    K

Since the raw function specified in the jet is supposed to be replaceable with
an optimized implementation, we want to prevent the partial evaluation of that
function with only some of its arguments. Since `E` evaluates its arguments
first, this means we don't have to deal with keeping track of whether a
partially evaluated closure refers to a jet or with allocating closures in the
common case. And making the arity of a function legible to the interpreter, we
can implement the supercombinator optimization even for code that doesn't have
a matching jet in the interpreter.

In Nock 4K, the hint tag annotates the return value of an expression, and thus
jet declaration is a side effect that's not visible from within interpreted
code: if I give you a value, you have no way of knowing it has been jetted. In
SKEW, there's no need to do that because the jet hint is pure and part of the
value of the function. Unlike Nock 4K, SKEW is entirely pure: if I give you a
value, you can see whether it has a jet annotation, and you can properly
serialize that this value has a jet annotation.

Our final requirement is being able to virtualize computations: you must be
able to write a function which takes an arbitrary value in your core language
for virtualization or serialization. You must be able to write an interpreter
function in Nock which takes an arbitrary Nock value and evaluates
it. Likewise, for all Nock 4K values, you have a serialization function `+jam`
which converts any arbitrary Nock 4K value into a bytestring; the serialization
format for Nock 4K is a function written in Nock 4K.

We must preserve this property, so SKEW code must be able to inspect arbitrary
SKEW values. SKEW supports introspection with the W combinator, which is the
final, five reduction rules. For `(W a s k e w x)`, the `W` combinator will
switch on x. Mnemonic: *sWitch*.

    (W 0 1 2 3 4 K)
    2

sWitch gives us generalized introspection on SKEW code and is designed to look
similar to conditionals as implemented on top of the unenriched lambda
calculus. Nock 4K is able to reflect using its noun equality test and cell/atom
discrimination, and SKEW is able to reflect using the unrolled switch statement
that is `W`.

When we have `W`, we can implement functions which interpret arbitrary trees of
SKEW data, meaning we can write virtualization and serialization. That said,
this operation is very naive so we expect that the only usage of `W` is in the
specification of jets: much like math on Church numerals, the formal
specification is in SKEW, but execution relies on the more practical jet
implementation.

## Data Jets

In SKEW, everything is made up of the unenriched lambda calculus, including
numbers which are Church numerals, which are functions.  This has not been seen
as practical: most system enrich the lambda calculus with cons cells, strings,
numbers, etc. Nock 4K baked in natural numbers and cons cells.

But jets give us a way to recognize any function, including classes of
functions such as the functions for Church numerals, and this allows a SKEW
interpreter to store a natural number in memory instead of the raw series of S
and Ks which represent a number.

A Church numeral takes two functions as arguments, what to do in the zero case,
and what to do in the non-zero case so you can see that numbers have to start
with (E E ...).  For example, if (S K) is the encoding of 0 as a Church
numeral, then 0 as a jet recognized natural number is:

    (E E K (S K))

And in turn, 1 is:

    (E E K (S K K))

And so forth. But because they are jet-recognized, the underlying in-memory
representation is the natural number 1. And when we jet functions, such as
`+add`, we can match on the these internal representations, so that the jet
implementation of add can be given two jetted church numerals and the jet
implementation just adds the natural representations together. In the course of
normal operation, we never need to expand a natural number to it's raw church
numeral form, even when we run it as a function. But since there is a bijection
between the natural numbers and the church numerals, we can recover the raw
SKEW if we ever need it.

We've called these data jets in this writeup, but they're used anywhere you
want to match a class of functions, such as the Turner's set combinators. In
Nock 4K, opcode 10 was added for more efficient copy-on-write modifications of
data structures. In SKEW, Turner's Bn, Cn, and Sn pattern of combinators aren't
hard coded, but are jets from day one, as are many common control flow
combinators.

## Performance Comparisons with Nock 4K

`~littel-ponnys` wrote a [Mandelbrot set generator in hoon][mhoon]. We then
translated it, and parts of the hoon standard library into a [Mandelbrot set
generator for SKEW][mskew]. Theoretically, we should be able to run the two of
them side by side and see which is faster, right?

[mhoon]: https://gist.github.com/eglaysher/6e7e6bd0de4b4b26af7e8940d6704cb2
[mskew]: https://github.com/urbit/urbit/blob/uruk-skew/pkg/hs/urbit-uruk/example/mandelbrot.moon

Doing this in a principled way is actually hard, and we want to point out a few
differences first. While both systems use signed natural numbers, hoon and SKEW
use different layouts for these, and while the Nock 4K version uses jets for
math, the SKEW version jets the signed integer representation itself. We are
fine with these differences, [the purpose of better architecture is to create
unfair comparisons][fordfusion].

[fordfusion]: https://urbit.org/blog/ford-fusion/

The following tests have been run with Urbit 0.10.7 built locally and with
skew's `supermoon` binary from `42418be180e5c13`, both run on MacOS
10.14.5. The time on Urbit was taken with a stopwatch (since `-time` interferes
with `@put` syntax) and the time on `supermoon` was measured with the real time
via the `time` command:

| Image Size | Urbit 0.10.7 | SKEW    |
|------------|--------------|---------|
| 10x10      | 1.5s         | 0.15s   |
| 100x100    | 1m23s        | 1.02s   |
| 200x200    | 5m38s        | 2.87s   |
| 300x300    | 12m38s       | 6.26s   |
| 400x400    | 22m45s       | 10.89s  |
| 500x500    | 35m          | 17.93s  |
| 600x600    | 51m10s       | 24.61s  |
| 700x700    | 1h08m10s     | 33.32s  |
| 800x800    | -            | 50.9s   |
| 900x900    | -            | 1m2.33s |

Why is SKEW so much faster than Nock 4K?

There's been half a century of research into how to make lambda calculus based
systems fast. We're able to take the best, simplest tricks and just apply them
off the shelf. The supermoon interpreter above is doing a combination of lambda
lifting and the supercombinator optimizations. SKEW's structure helps here:
since you can see the jet registrations in the value of the functions, you do
not need to compute directly on SKEW, but instead on any structure which has a
bijection with it. The run time system can use a data structure which is a sum
type of S, K, E, W along with all recognized opcodes since `(E E %add ...)` is
a value which can be statically recognized, and turned back into its raw SKEW
value when needed.

But why is SKEW so much faster than Nock 4K? Lambda lifting and
supercombinators do help. The ability to have data jets helps a bit. Having
jets for common control flow combinators like the Turner combinators help a
bit. But the main speedup is that SKEW is neither doing multiple layers of
indirect pointer chasing to find the target of a function call, nor is it
allocating multiple times on every function call, nor are there hash table
lookups in the execution path!

In SKEW, function dispatch is either a direct jump (in the case of calling a
jet) or a single indirect pointer jump (all other cases). Unlike in Nock 4K,
there is no dynamic scope: a Nock 4K function requires all functions that it
could call to be passed in a tree data structure and looked up by linked-list
traversal, but a SKEW function is compiled against the specific instances of
functions that it calls. We can sympathize with how Nock 4K used dynamic
scoping as part of its strategy to implement serializable closures, but SKEW
meets those serialization requirements in a different way which doesn't pay the
dynamic lookup tax.

Urbit has a history of using the [Ackermann function][ackermann] in its
documentation. Let's benchmark it on both systems, as this is a more direct
comparison than the Mandelbrot example because Ackermann is mostly testing
function dispatch and both SKEW and Nock 4K have equivalent jets on natural
numbers. Because I've heard Nock 4K's speed compared to Python several times,
both by people who work for Tlon and don't, I've included the default python
interpreter on my Mac.

| (m, n)  | Urbit 0.10.7 | Python 2.7.10 | SKEW     | GHC 8.8.4 |
|---------|--------------|---------------|----------|-----------|
| (3, 8)  | 3.25s        | 0.472s        | 0.376s   | 0.134s    |
| (3, 9)  | 11s          | 2.166s        | 1.289s   | 0.453s    |
| (3, 10) | 45.5s        | 9.825s        | 6.257s   | 1.797s    |
| (3, 11) | 2m48s        | 44.241s       | 30.358s  | 7.795s    |
| (4, 1)  | *44m58s      | (SIGSEGV)     | 9m58.68s | 2m42.85s  |

(* The time for (4,1) on Urbit was taken with the `-time` command instead of a
stopwatch because I wasn't going to sit there with a stopwatch after I sat for
10 minutes waiting for the SKEW result for (4,1). Time may be off by up to 15
seconds. This does not matter at this order of magnitude.)

We also include a Haskell version of the Ackermann function to show that
there's still significant performance work that can be done. The current SKEW
interpreter is extremely simple at about three to four thousand lines of
Haskell; one of the design goals of a Nock is that you should be able to write
your own interpreter and have at least decent performance and our prototype is
within that complexity. But since we have none of Nock 4K's problematic memory
access patterns and since SK is a common internal representation for functional
programming environments, there is no reason we shouldn't be able to match the
performance of GHC's generated code if actual engineering effort is spent on
making a performant SKEW interpreter.

And to emphasize, performance matters. Now that we're expecting Urbit to have
[Providers][providers], or communities and companies which provide hosting
services, lower costs per unit of computation mean either easier shared hosting
for communities, or lower prices for consumers, or high profits for
companies. Nock 4K's profligate CPU and memory usages have costs born by the
people who run Urbit. We must remember that people are very [perceptive to even
small delays][googlespeed] and that this causes measurable differences in
system usage and adoption; successful systems are fast.

[ackermann]: https://urbit.org/docs/tutorials/hoon/ackermann/
[providers]: https://urbit.org/blog/providers/
[googlespeed]: https://ai.googleblog.com/2009/06/speed-matters.html

## Storage and Snapshot Strategy

So we've gone over the SKEW reduction rules, how we can store jetted data
instead of raw representations in S and K, and actual benchmarks between the
two systems. But what about larger scale data?

SKEW is intended to be a substrate for Urbit, which is a complete system as a
single value. Your Urbit is one closure which takes a command and returns a
pair of effects and a new closure. Everything lives in this one closure: all
system code, all user code, all data.

How do you handle all of a user's data as one value?

In the current Urbit system, Vere maps a 2 gigabyte memory image directly to
disk, which has fast writes to disk, but limits the size of the image. These
images are also not portable between interpreters. The alternative Jaque
interpreter serializes the entire system state on each save, but this is slow
and costly. Neither of these solutions scale.

We want to be able to page parts of your system value from disk on use, but we
don't want to hash-cons each application of a SKEW letter; that's even more
infeasible than hash consing every cell in Nock 4K. We only want to break the
value up into parts at specific boundaries. We want to specify from inside an
SKEW program that a value should be serialized separately. So we define two
functions which put a value in a box and take a value out of a box:

    ++  (box x)    (E %box (K x))
    ++  (unbox x)  (x uni)

All we are doing is declaring a function which returns a jetted function with a
tag of %box which returns the value when called with any argument. The
corresponding unbox function just calls the function with an unused
argument. Using the same data jet matching infrastructure from earlier, we can
data jet the value so that the interpreter knows to store the boxed value
separately.

It is important to note that this doesn't change the semantics of SKEW. From
the point of view inside a system written on top of SKEW, your system is still
one single value. An interpreter could just not implement the `%box` jet and
have completely equivalent semantics. Nor does this have any side effects which
are observable from within the system. The on-disk representation does not
change SKEW into something other than a single level store.

To illustrate how this works, let's make something analogous to our current
Arvo operating system: a function which takes a command, and returns a pair of
effects and a new function. A function of `type Fun a b = a -> (b, Fun)`. Let's
make a stateful Mandelbrot generator, which acts like an Arvo kernel, caching
all previously generated Mandelbrot sets and calculating a new set when needed.

The moon source code to this is in
`pkg/hs/urbit-uruk/examples/mandelbrot-arvo.moon`, on the uruk-skew branch. The
strategy is to build an association list of boxes, mapping a size to a boxed
value. The binaries to run after building with `stack install urbit-uruk-rts`
is `arvomoon`. You can boot a pier like so:

    $ arvomoon boot pkg/hs/urbit-uruk/example/mandelbrot-arvo.moon pier-dir
    [[compiling spam omitted]]
    $ arvomoon run pier-dir
    arvo> (5,5)
    "P3\n5 5\n255\n16 7 135 19 6 137 19 6 137 21 6 138 19 6 137 \n16 7 135 21 6 138 21 6 138 29 6 141 98 0 166 \n16 7 135 27 6 140 0 0 0 0 0 0 0 0 0 \n16 7 135 27 6 140 0 0 0 0 0 0 0 0 0 \n16 7 135 21 6 138 21 6 138 29 6 141 127 3 167 "
    arvo> ^D

If you restart arvomoon, you'll see that it will print precomputed value
instantly, with a printf alerting you the interpreter paged something in from
disk:

    $ arvomoon run pier-dir/
    arvo> (5,5)
    Loading "5df66d20aac1969b04a6786172b2cf10f8ffe723ccf11976ada4f8a98955258f"
    "P3\n5 5\n255\n16 7 135 19 6 137 19 6 137 21 6 138 19 6 137 \n16 7 135 21 6 138 21 6 138 29 6 141 98 0 166 \n16 7 135 27 6 140 0 0 0 0 0 0 0 0 0 \n16 7 135 27 6 140 0 0 0 0 0 0 0 0 0 \n16 7 135 21 6 138 21 6 138 29 6 141 127 3 167 "

If you'd like to precompute a bunch of sets in one go, you can use the preload
command:

    $ arvomoon preload pier-dir 50
    [a bunch of spam alerting you to all the sets it is generating]
    $ arvomoon run pier-dir/
    arvo> (15, 15)
    Loading "8a6b2469652f4ba9e7c319a0f215a743b05f2cb47e6fefd296304a313df13635"
    "P3\n15 15\n255\n16 7 135 16 7 135 16 7 135 19 6 137 19 6 137 19 6 137 19 6...

I was able to preload a pier with Mandelbrot images up to 1030 x 1030. This
pier was over 3 gigabytes, and each image was only paged in lazily on
demand.

Some caveats for this prototype and possible improvements:

- It keeps track of whether a boxed value is Unloaded, Saved, or Unsaved. It's
  smart enough to skip writing already Saved values to disk on snapshot write,
  and it's smart enough to skip Unloaded values since it assumes they're
  already on disk. However, there is no transition from Saved to Unloaded: a
  full implementation would also have to unpage data under memory pressure;
  this prototype does not do that.

- The snapshots so far make no aim towards portability across interpreters or
  even slightly different versions of the same interpreter. There is a plan on
  how to serialize data jets in a way where they'd be compatible across very
  different interpreters, but this hasn't been implemented yet.

- While a filesystem backed implementation is fine for now, we eventually might
  want to stuff this in a key-value database. If the in memory representation
  is the same as the on-disk format, this could even be a memory-mapped one
  like lmdb.

## Conclusion

I don't know that SKEW is perfect, but it's a much better contender for "tiny
and diamond-perfect" than Nock 4K+ is: SK, E and W each directly addresses one
of the requirements and each appear to be the simplest thing which could
address them, and is thus much closer to the Nock ideal of "tiny and diamond-
perfect" than previous Nocks. At 10 lines of reduction rules, it is roughly a
third of the length of the Nock 4K spec, and half the SKEW spec is an
explicitly unrolled switch statement. And as an extension of the unenriched
lambda calculus, it can act as a compile target for the pure parts of any
functional language which compiles down to the lambda calculus (any pure
primops needed can be written as jets), while Nock 4K is tied to the semantics
of its corresponding frontend language.

And we can do this in a really short, simple interpreter. The code in the
`urbit-uruk-rts` package for the SKEW runtime is about 2,600 lines of code
(including jets) with another 1,500 shared in the `urbit-uruk` package, giving
you a SKEW runtime with a full persistence engine. This is all fairly simple
Haskell code and is within the capabilities of a motivated college student to
implement with usable performance, and we believe there's a lot of room for
improvement if one relaxes the simplicity constraint.

Compare this simplicity with the `nock/` directory in the reference Nock 4K
interpreter: it is 11,200 lines of ANSI C which implements a complex bytecode
interpreter and is still orders of magnitude slower. This does not include any
jetting code. About 1,900 lines of that is spent in the code that deals with
Nock 4K's side-effect based jet registration system, which is a major part of
what makes implementing a Nock 4K interpreter hard, and which has no analogue
in SKEW.

We've presented the reduction rules for a candidate successor to Nock 4K, along
with interpreters for this system which are over 100x faster than the Vere Nock
4K interpreter on a real world benchmark, are over 4.5x faster than Vere on a
synthetic benchmark of just function dispatch and are slightly under a third of
the length with simpler code. We've implemented a minimal standard library for
this system, and a non-self-hosted compiler for an untyped hoon-like
language. We translated a large, numeric hoon program and benchmarked it,
favorably. We've presented the reading and writing of pier images to disk of
sizes larger than the current largest pier possible with Vere, with some
thoughts on further ways to improve the system.

For those who want to play with this, we have two different, working SKEW
interpreters on [the uruk-skew branch on urbit/urbit][skew-branch]. This is not
just theoretical work.

[skew-branch]: https://github.com/urbit/urbit/tree/uruk-skew
