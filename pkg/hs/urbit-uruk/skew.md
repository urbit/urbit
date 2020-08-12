# SKEW

SKEW is an extension of the SK Calculus with the ideas developed for Nock. This
demo is designed to teach you what SKEW is, why it should become the successor
to Nock 4K+, why it's faster, and to allow you to play with it in your
browser. In each section, the code in text boxes is live and you can edit it to
play with it.

The [SK Calculus][skwiki] is an encoding of the Lambda Calculus introduced by
Schönfinkel and Curry in the 1920s. It is Turing complete, simple, and well
understood. We believe it is the best foundation to build on due to how well
researched it is.

We have two different, working SKEW interpreters on [the uruk-skew branch on
urbit/urbit][skew-branch]. This is not just theoretical work.

[skwiki]: https://en.wikipedia.org/wiki/SKI_combinator_calculus
[skew-branch]: https://github.com/urbit/urbit/tree/uruk-skew

## The Basic Reduction Rules

What's the smallest practical combinator? Let's first list our functional
requirements:

- We need to be able to perform any computable computation.

- We need to be able to recognize functions and values and replace them with
  optimized versions to make runtime practical.
  
- We need to be able to virtualize computations.

We propose an extension of SK which has the following reduction rules:

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

The first four rules give us the SK calculus with a strict, well defined
reduction ordering. The first reduction is the `K` combinator. The `K`
combinator returns its first argument and discards its second. Mnemonic:
*Konstant*.

    (K K K)
    K
    
Simple enough. The next two reduction rules specify order of operations: we
reduce the left hand side before the right hand side.  We must do this for
short circuiting reasons. SKEW is strict instead of lazy and if we did not do
this, both arguments to K would be evaluated before the first is returned.

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
and signal to the interpreter that a piece of set of raw combinator reductions
can be replaced with a native, optimized implementation.

The E combinator specifies a jet and specifies order of execution.  The jet is
a number of E letters which specify the arity of the function, a tag which is
only used for matching, the function being jetted, and then the number of
arguments according to the stated arity. The arguments are evaluated
first. Mnemonic: *Enhance*.

Look at the order in which the arguments are reduced in this trace:

    (E E K (S K) (K K (K K)) (S K K K))
    (E E K (S K) K (S K K K))
    (E E K (S K) K (K K (K K)))
    (E E K (S K) K K)
    (S K K K)
    (K K (K K))
    K
    
Since the raw function specified in the jet is supposed to be replacable with
an optimized implementation, we want to prevent the partial evaluation of that
function with only some of its arguments. Since `E` evaluates its arguments
first, this means we don't have to deal with keeping track of whether a
partially evaluated closure refers to a jet or with allocating closures in the
common case.

In Nock 4K, the hint tag annotates the return value of an expression, so if an
expression returns a function, the interpreter must remember an extra bit of
matching information. In SKEW, there's no need to do that because the jet
matching happens at reduction time, allowing for both faster jet matching, and
ahead of time optimizations.

Our final requirement is being able to virtualize computations: we must have a
way of running a function which we don't know if it will error by infinite
looping in predictable ways. We thus need to be able to declare a +mock
function with the signature `a -> Either err a`. SKEW supports introspection
with the W combinator, which is the final, five reduction rules. For `(W a s k
e w x)`, the W combinator will switch on x. Mnemonic: *sWitch*.

    (W 0 1 2 3 4 K)
    2
    
sWitch gives us generalized introspection on SKEW code. However, this operation
is very naive so we expect that the only practical use will be in the
specification of jets.

## Jet Matching Data Types

In SKEW, everything is made up of the unenriched lambda calculus, including
numbers which are Church numerals, which are functions.  Traditionally, this
has not been seen as practical: most system enrich the lambda calculus with
cons cells, strings, numbers, etc. Nock 4K baked in natural numbers and cons
cells.

But jets give us a way to recognize any function, including classes of
functions such as the functions for Church numerals, and this allows a SKEW
interpreter to store a natural number in memory instead of the raw series of S
and Ks which represent a number.

A Church numeral takes two functions as arguments, what to do in the zero case
and what to do in the non-zero case so you can see that numbers have to start
with (E E ...).  For example, if (S K) is the encoding of 0 as a Church
numeral, then 0 as a jet recognized natural number is:

    (E E K (S K))
    
And in turn, 1 is:

    (E E K (S K K))
    
And so forth. But because they are jet recognized, the underlying in-memory
representation is the natural number 1, and since there is a bijection between
the natural numbers and the church numerals, we can recover the raw SKEW
whenever we need it.

And when we jet functions, such as +add, we can match on the these internal
representations, so that the jet implementation of add is given two jetted
church numerals, the jet implementation just adds the natural representations
together. And if there's a problem for any reason, we can just fall back to
executing the raw SK code.

## Performance Comparisons with Nock 4K

`~littel-ponnys` wrote a [Mandelbrot set generator in hoon][mhoon]. We then
translated it, and parts of the hoon standard library into a [Mandelbrot set
generator for SKEW][mskew]. Theoretically, we should be able to run the two of
them side by side and see which is faster, right?

[mhoon]: https://gist.github.com/eglaysher/6e7e6bd0de4b4b26af7e8940d6704cb2
[mskew]: https://github.com/urbit/urbit/blob/uruk-skew/pkg/hs/urbit-uruk/example/mandelbrot.moon

Doing this in a principled way is actually hard, and we want to point out a few
differences first. While both systems use signed natural numbers, hoon and skew
use different layouts for these, and while the Nock 4K version uses jets for
math, the SKEW version jets the signed integer representation itself.

The following tests have been run with Urbit 0.10.7 built locally and with
skew's `supermoon` binary from `42418be180e5c13`, both run on MacOS
10.14.5. The time on Urbit was taken with a stopwatch (since -time interferes
with @put syntax) and the time on `supermoon` was measured with the real time
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

## Storage and Snapshot Strategy

So we've gone over how SKEW's jetting strategy makes things performant, how we
can store jetted data instead of raw representations in S and K, and actual
benchmarks between the two systems. But what about larger scale data?

SKEW is intended to be a substrate for Urbit, which is a complete system as a
single value. Your Urbit is one closure which takes a command and returns a
pair of effects and a new closure. Everything lives in this one closure: all
system code, all user code, all data.

How do you handle all of a user's data as one value?

In the current Urbit system, Vere maps a 2 gigabyte memory image directly to
disk, which is fast but limits the size of the image.  The alternative Jaque
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
  and it's is smart enough to skip Unloaded values since it assumes they're
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

We've presented the reduction rules for a candidate successor to Nock 4K, along
with interpreters for this system which are over 100x faster than Vere. We've
implemented a minimal standard library for this system, and a non-self-hosted
compiler for an untyped hoon-like language. We translated a large, numeric hoon
program and benchmarked it, favorably. We've presented the reading and writing
of pier images to disk of sizes larger than the current largest pier possible
with Vere, with some thoughts on further ways to improve the system.

We've presented where to find this code.
