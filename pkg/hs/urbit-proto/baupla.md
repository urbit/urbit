# Hoon Language Plan

## Overview of Proposed Changes

The goal of the changes documented herein is to produce a language which is
more expressive and rests on far firmer and easer-to-understand foundations than
today's Hoon, while basically retaining the same essential feel of the thing;
that is, the informal principles and reasoning modes by which one wrote hoon
in the past should mostly continue to work, while the formal underpinnings or
the more difficult features change in character. Rather than present my vision
in one big blob, I've found it useful to factor it out into a bunch of separate
"proposals":

0. Aura nesting reform.
1. Mold and bunt reform.
2. Variance reform.
3. Pattern matching reform; removal of crop/fuse.
4. Fork reform; narrowing of use of `?^`.
5. Dependent types.
6. Collection runes.

Although these proposals are presented as though they are orthogonal changes,
this is merely a rhetorical device. The *ideas* are 80% orthogonal from each
other when explained in isolation, but considered as engineering projects, most
of these changes to Hoon 140 would force a bunch of other, lower-level changes
that would be shared between other work items, or would need to be done in a
forward-thinking way which anticipates these items.

So while it may be useful to prototype 1-4 separately in the context of a more
simply typed hoon prototype (i.e. no wetness, no dependent types) as practice
and to confirm my ideas, my current thinking is that after the prototypes have
been succesful, 1-5 should be implemented in one batch in a putative hoon 139.
Collection runes can go in separately. Aura nesting reform can also probably go
in separately, but given how small a change it is, I would prefer not to burn a
Kelven number on it.

## Aura nesting reform.

Hoon's subtyping relation, as embodied by the implementation in nest:ut, suffers
from a very serious theoretical flaw: it is not transitive. The nontransitivity
in the relation is generated (in part?) by the nontransitivity in atom nesting;
i.e. `@d` <= `@` <= `@t`, but `@d` </= `@t`. By "generated," I mean that there
is nontransitivity in this base case, and the recursion of nest propagates it to
more complex types.

(There may also be nontransitivity in the handling of faces or elsewhere; I
haven't checked recently.)

This theoretical problem is also a problem in practice, because it means the
following code is admissible

    =/  a  --1
    =/  b  -2
    (mul:si (add a b) -3)

(Do you see the error?) In other words, signed numbers become hellishly
difficult to use correctly.

Previous solutions have proposed choosing exactly one direction of aura
subtyping; e.g. `@s` < `@` or `@` < `@s`. But notice that *both* directions lead
to errors in the above code.

The answer is to require exact aura equality for subtyping (the relation used
to check whether you can pass something to a function / whether the actual
sample type of a dry core is compatible with its formal sample type), and to
provide a different partial order, called "coercibility," (arm name `fits`) in
which all auras are introconvertible (and which otherwise is identical to nest).
This second notion is what becomes used in `^-`, which now means "cast" rather
than merely "supply type annotation."

There are no theoretical open questions posed by this reform, nor is there any
practical difficulty in implementing it. It seems like a straightforward win
and solves a big problem in a simple and elegant way.

## Aside: Liskov, facial subtyping, and noun

The issue raised above around the transitivity of subtyping invites us to think
of broader issues around subtyping. The correct definition of subtyping is given
by the *Liskov substitution principle*:

**Def:** A is a *subtype* of B if
1. Every expression of type A is also an expression of type B, and
2. Every "hole" into which we can put an expression of type B is also a hole
   into which we can put an expression of type A.
By "hole" I mean a part of the syntax tree for expressions which "has not been
filled in yet." For example, on the right of `-:` is a hole of type `^`.

You may sometimes hear me refer to the (semantic) root node of an expression of
type A as "an introduction form for A" and the parent node of a hole expecting
an A as "an elimination form for A." E.g.
- For functions (which don't exist lol) `|=` is an introductor and `%-` is an
  eliminator.
- For cells, `:-` is an introductor and `+:` is an eliminator.
- For atoms, `5` is an introductor and `.+` is both an introductor and an
  eliminator
Using this terminology, the definition of A <= B is:
- Every introductor of A is also an introductor of B, and
- Every eliminator of B is also an eliminator of A.

My hope is that just looking at the definitions and thinking about all this can
give you a sense of why the idea is right. Like, what would it even mean to say
that A is a subtype of B if you can't use any A everywhere a B is expected? The
Liskov principle runs deep.

Evidently, Curtis agrees with me on the righteousness of Liskov because he
mentions it in the "Advanced Types" section of the documentation as the
foundation of the subtyping idea in Hoon. This is amusing, because Hoon does not
abide by it. We've already covered one such failure above: nontransitivity. If
you think about it for a little bit, you'll see that it promptly follows from
the definition that subtyping must be a preorder (reflexive and transitive
relation). But there are other ways in which Hoon fails the principle as well,
which we will now cover.

### Facial subtyping

The implementation of nest ignores (strips) faces. This means that `[@ @]` is a
subtype of `[a=@ b=@]` (and vice versa). Unfortunately, `a:` is an eliminator of
the second type but not the first, so this subtyping judgment is not kosher.

In other words, if I can do

```
=/  f  [a=1 b=2]
a.f
```

then I should also be able to do

```
=/  f  [1 2]
a.f
```

because `[1 2]` is of a type which is a subtype of `[a=1 b=2]`'s type. Needless
to say, I cannot do this.


This improper rule appears to be there so we can pass `1` to `|=(a=@ ...)`. On
the other hand it means we can pass `[1 2 3]` to `|=(=my-struct-with-faces)`
which may be less than desirable. The basic issue is that `a=@` in the "function
pattern" really means "take a `@`" and *then* apply the face a to it, while the
faces in the definition of the `my-struct-with-faces` type presumably mean that
that the value is *expected to already have* those faces. This is a genuine
distinction, and it should be captured in the syntax, rather than handwaved away
in nest.

If we draw this distinction, the language gains "real structure types" which are
more or less like JavaScript objects in user experience (minus some dynamism
which you can regain in vase mode if you want it, but you don't want it), except
that field order matters. The latter caveat is slightly annoying, but likely
doesn't matter in practice. Making `[1 2]` "feel different from" `[a=1 b=2]` is
probably a big win for people who want to write correct and readable code. It
illustrates yet again that paying attention to theoretical concerns reliably
yields practical gains.

In the subsequent section on pattern matching reform, I give a proposal that
happens to fix this problem. Part of the proposal is that "patterns" become
expressionlike rather than typelike in syntax. The foregoing discussion actually
explains why this shift is mandatory:
- We want to be able to "apply a face" during a pattern match, as distinct from
requiring it.
- But any notion of "optional face" in the type system (where not having the
face nests under having it) will give rise to a Liskov violation.
- Therefore such optionality must not be in the syntax of types.

### Noun; other notes

We have a problem in that you can take the `+6` of `*` but not the `+6` of
`[@ @]`. But since a `[@ @]` is a `*` we have a Liskov violation.

Noun is not a true top type. In principle you should have to `?@` it to
determine whether you can axis it. You "can also" `?@` lesser types, but you may
get dead code. Incidentally, this is an important reason why making this sort of
dead code a type error is considered a mistake.

Solutions:

1. Allow `+6:[@ @]`.
2. Ban `+6:*`.
3. 2, but add a separate type `+` which all types are convertable to and from,
and which allows arbitrary axials.

We also have a problem if equality requires one of the arguments to be a subtype
of the other. I can't remember if this is the case.

## Mold and bunt reform.

The effect of mold and bunt reform is to separate the notion of a type from the
notion of a gate. This change introdces "types proper."

In dependent types, a type is a first class value in a way that is incompatible
with it being usable directly as as mold function. Thus under mold reform,
instead of being able to say (my-type the-value-i-want-to-mold), you must
instead say, e.g. (!my-type the-value-i-want to mold), where ! "converts" a type
into its mold function. Now only types proper can be used in type position, not
arbitrary functions.

Bunt reform has two parts. In the first part, we permit you to take the bunt
only of a proper type, rather than an arbitrary function.

In the second part, called "sample reform," we remove the bunted sample from
every gate. Thus it is no longer the case that every gate has a mandatory
default argument, nock need no longer be generated to push this defalult sample
at gate-creation time, and so on. Most notably, it is no longer a requirement
of the compiler to have a complex "seminoun" system to ensure that this bunt-
pushing code does minimal work, removing a major source of complexity in the
compiler, and a major way in which the compiler today falls short of being a
"macro assembler."

More concretely, today, the structure of a gate in memory is

    [formula sample-bunt context]

To slam a gate, we first edit (nock 11) the sample bunt to the desired argument
value, then pull (nock 9) the formula. Under the proposed sample reform, the
structure will instead be

    [formula context]

To slam a gate in this new world, we must first "inflate" it with the actual
argument to

    [formula argument context]

then pull the formula as before. This inflation operation will use autocons
rather than nock 11. If it is desired to use nock 11 as before, or if avoiding
the shape change is a requirement for compatibility with the serf's jet
dashboard (I think it actually isn't; the dashboard will only in practice run
into inflated gates), we can instead use

    [formula 0 context]

as the latent form of a gate and use edit-pull rather than inflate-pull. The
operational semantics will be equivalent, and the type system reforms described
in the "variance reform" section will make it impossible for the user-programmer
to detect the difference in representation in ordinary programming (i.e. unless
they use an escape hatch like converting to noun.)

It may be more appropriate to think of sample reform as part of the next section
of this document. Or, you can think of "mold and bunt reform" and "variance 
reform" as forming a Venn diagram with sample reform as the intersection. Thus,
if the previous paragraph made you ask questions like "what about the sample of
a door," please continue reading.

Note that sample reform cannot be separated from bunt reform and "proper types,"
because what happens if you do `*add`?

(In the third part of bunt reform, not discussed further here, but pursued in
my dreams, bunts are removed from the language.)

This section presents no theoretical difficulties, but adding "proper types" to
existing hoon absent other changes will suck, because it goes against the grain
of the basic abstractions of the language at present. This is why it is worth
bundling this proposal with dependent types, which will change that grain.

## Variance reform

The system of core types in hoon suffers from the following flaws:

A. It is extremely complex, immemorable, and difficult to reason about.
B. Hoon doesn't have a function type, despite being a functional programming
language.
  - By function type, I mean something isomorphic to the Haskell type `a -> b`
  - Hoon has `((a, b), b -> c)` [gold cores with single $ arm] and
    `(a -> b, (a -> a) -> b)` [lead cores with single $ arm].
  - A simplification: in today's Hoon, gates always have default arguments.
C. There is no way to write a "general core type" in the type syntax. Instead
you must take the type of an expression which produces a specific core.
D. Something is wrong with wetness.
E. Cores don't do structural subtyping. If I ask for a core with arms foo and
bar, and yours has foo, bar, baz, then you can't use it.
F. Everyone hates the `~(arm core sample)` "door" syntax.

Under the heading of "variance reform," we propose a new model which fixes A-C
and F. D is saved for a later section ("dependent types"), and F is disregarded
for the time being.

Before I explain my proposal, I'd like to explain the existing system. That way
the contrast can be made clear. To begin, I'll present a "folk" model of how
core types work today. Imagine a hypothetical average experienced hoonist who is
not me is the author:

- Every core type has:
  - a battery, which is a collection of arm hoons
  - a context type
  - optionally, a payload type
  - a moisture, which is wet or dry
  - a variance, which is gold, lead, covariant or contravariant
- If it has a sample type, it's called a "door" and you use the `~()` notation
- If it has an arm `$`, it's called a "gate" and you use the `()` notation.
  Presumably, it is also a door.
- You can read the context of a gold core.
- You can change the context of a gold core but then it will "be a different
  type."
- You can change the sample of a contravariant core but cannot read it or read
  or change its context.
- Arm-compatible contravariant cores with nesting samples nest, regarless of
  context
- You can't read or change any part of a lead core.
- Arm-compatible lead cores nest regarless of sample or context types.
- Do not use covariant cores.

Already, this is pretty complex. Our hypothetical user must have done a lot of
learning to get to this understanding. There are some oddities, however, which
I must point out:

- Note that wetness is not explained. This is handwaved with "arm-compatible."
- The folkish user cannot remember the metals for covariant and contravariant,
  nor the variance names for gold and lead.
- The folkish user believes that cores may or may not "have" "samples" and this
  seems to have something to do with the variance model, but what is not clear.
- It's not clear why covariant cores are present or what they're for.

Also, the understanding is wrong, except at a surface level.

Here is (an expanded copy of) the entry in $type for cores:

```
$:  %core
    $=  p  type
    $=  q
      $:  p=(trel (unit term) ?(%wet %dry) ?(%gold %lead %zinc %iron))
          q=type
          r=(pair seminoun (map term (pair what (map term hoon))))
      ==
==
```

Where are the "context" and "optional sample" types? They don't exist. Instead
one has *two* *payload* types: the one the core was defined with ("formal", q.q)
and the one that it happens to have right now ("actual", p). In some variance
models, the +2 of this type (which of the two, lol?) is treated differently.
This occurs independently of the cues in hoon program text that would give rise
to a user thinking of his core as "having a sample." In some sense, all cores
have samples; some just suck to deal with more than others.

In addition to their type structure, to understand cores, we must also under-
stand how they nest and are finded through.

For a core A to nest under a core B, we must have:

1. A and B must have the same moisture
2. B's formal and actual payload types must be equivalent (mutually nest)
3. A's actual payload type must nest under A's formal payload type
4. A's variance metal must be equal to or "baser" than B's along the diamond
5. based on A's metal, the following must be true:
  - if lead, true
  - if gold, A's actual payload is equivalent to B's actual payload
  - if iron, the +2 of B's actual payload nests under the +2 of A's actual
  - if zinc, the +2 of A's actual payload nests under the +2 of B's actual
6. based on the moisture, the following must be true:
  - if wet, they must have equal hoon batteries
  - if dry, they must have the same "battery shape," and for each pair of
    matching arms, the inferred type of the arm in A must nest under the
    inferred type of the arm in B. This inference is done against a modified
    version of the original containing core type where the metal is taken to be
    %gold and (for some reason) the actual type is replaced by the formal type.

$$ To find in a core
- if reaching into a core via a wing in the edit list of a %=, write-mode
- if reaching into a core via a wing elsewhere, read mode
then the peek rules

I dare any hoon programmer anywhere in the world to reproduce correctly from
memory on the first try either the type of %core or the nesting rules. In fact,
I am not confident I have reproduced them correctly here, despite careful study
of hoon.hoon while writing this document. The folkish model is sort of
approximately correct, but understanding how it arises from the formal model is
a serious exercise in logic and close reading. Not good.

$$ Our proposal
- type
- nest
- find
- type check body of gate as though a single arm gold core

In contrast, in our proposal, we offer the following new structure of types:

```
+$  type
  ...
  [%core p=(map term type) q=(unit type)]
  [%gate p=type q=type]
```

In effect, we have three kinds of thing:
- *Gold cores*, where the q is `[~ context-type]`.
- *lead cores*, where the q is `~`.
- *Gates*, i.e. function types, replacing the old contravariant cores.

Under this proposal, `%-` and `|=` cease to desugar to `%=` and `|%`, becoming
basic runes in their own right for applying and creating gates, respectively. As
mentioned in the "bunt reform" section above, gates no longer carry around
default arguments ("bunted samples"). However, once the gate is inflated with
its actual argument as sample, the triple `[code sample context]` has the same
runtime shape as a core with one arm. Thus, we posit that "from the inside" a
gate appear to be a gold core with a single `$` arm. More formally, to infer the
type of a `|=`, one infers the type of the body against a gold core type
constructed with the body as its `$` arm to get the result type, then produces
`[%gate arg-type result-type]`. From the inside of a gate then, one can recurse
using the usual `$(foo 1, bar 2)` notation, retaining all the existing features,
including:
- leaving parts of the argument unchanged
- editing the deep context
The only change is that users *outside* the function are denied access to these
capabilities.

The proposed rules for nest checking are:
- A core `[%core bat1 con1]` nests under a core `[%core bat2 con2]` iff
  - `bat1` and `bat2` have equal keysets and corresponding value pairs nest, and
  - either:
    - the first core is lead (`con1` = `~`), or
    - both cores are gold and the contexts (`u.con`) nest
- A gate `g` nests under a gate `h` iff
  - `p.h` nests under `p.g` (contravariant in argument), and
  - `q.g` nests under `q.h` (covariant in results)

The proposed rules for access control on `%=` are:
- You cannot read from or write to the battery of any core. (alt: You can read
  but the type is `%noun`.)
- You can read from and write to the context (`+3`) of a gold core, but cannot
  to a lead core.
- You cannot read from or write to any part of a gate. (alt: You can read but
  the type is `%noun`.)

Unlike the present ones, these proposed rules have the remarkable property that
one can read the "folk" understanding right off of them. This is because the
folk understanding that would arise *is identical to the rules as stated*
because the rules are simple.

Under the new system, how does one construct a "door"? It's simple: Use a gate
which returns a core. Conceptually, that's what a door is anyway. So instead of
`(~(tap by my-map) my key)`, one now writes `(tap:(by my-map) my-key)` and
enjoys the absence of a famous visual wart. (Fun historical note: Ted asked me
to solve this problem *in my technical interview for the company*.)

If we want to go further and get rid of the extra layer of parens, I propose
`(gate arg :exp arg2)` as sugar for `(exp:(gate arg) arg2)`. So one could write,
e.g., `(by my-map :tap key)`. Or maybe `(by my-map tap: key)`, which is cute and
looks like smalltalk. But this is by no means necessary.

## Pattern matching

$$ Problems:
- A pattern should be like an expression, not like a type.
- Crop and fuse don't work
  - don't liskov; e.g. gaining faces of ref in fuse
  - want to "add together" patterns rather than incrementally subtracting them
    from the subject, which is intractable
- r.p.q.u.i etc
- with patterns that extract, we don't need to refine the subject, and if we
  don't need to refine, we don't need to crop. This is a good reason to require
  all data access on the discovered subtype be through extraction.
- cannot do nested patterns
- cannot do simultaneous tests on pairs; gives rise e.g. to dext/sint pattern
- confusion between applying a face and expecting type to have a face; no
  "real structs"

## Fork reform

$$ Problems
- Feels wrong to have free construction
- Existence of indescriminable types that in practice you can't do anything with
- Incompatible types are formed in a test, learn about the problem way later
  when we try to find across them; nonlocality of errors.
- any-all rule in nest is incorrect
- but the rule I'm most familiar with in dependent land has similar problems, so
maybe I should remove this section for now

## Dependent types

Something is wrong with the way that Hoon implements parametric polymorphism.
Wet gates, for reasons that nobody really understands, do not in practice work.
The result is that any substantially involved higher-order programming is
impossible or fiendishly difficult in Hoon; we have reproduced the C++ template
system rather than the Haskell or Java "generics" idea. Heck, even brain-dead
uninvolved generic programming, like doing stuff with lists, ranges from crufty
to non functional. This is really not okay. So in practice, all Hoon code that
people write today is monomorphic.

In a Haskell program, a type signature is worth its weight in gold. Just by
looking at one, you can tell what the attendant code does much of the time. It
stops you from writing incorrect code. The design of the genericity mechanism
lets *beginners* write correct super-generic code that even experts would
struggle to write with C++ templates. The reason for this is that Haskell (and
Java) are based on a simple mathematical model, while C++ is based on hundreds
of pages of complex English legaleese, with exceptions stacked upon exceptions
four layers deep. The reason for autistic simplicity in system design isn't that
we're smart, it's that we're dumb, and can't hold the consequences of non-autism
in our heads. (Thanks for bearing with me. I promise you this is the only whole-
paragraph evangelism in this doucument.)

Our goal here, then, is to find some simple, battle-tested formalism that we can
build parametric polymorphism in Hoon on top of, and then... build it on top of
that. We want to choose something that retains the surface-level feel of the
language as much as possible though. Part of that feel, for better or worse, is
subject orientation. Users are acustomed to having access to the *environment*,
or collection of variable bindings, as a first-class value, and they do in fact
make use of this capability daily. In Haskell-98-style rank-1 F-polymorphism,
there are *two* separate environments: one for tracking type variable bindings,
and the other for tracking ordinary variable bindings (which are assigned "type
schemes," rather than types). It is not immediately clear how to port this to
the subject-oriented context, which expects one. Where do type variable bindings
go in the subject? And if they are not to be found in the subject, then, well,
we have contextual info relevant to type checking not found inside the subject.

However, there *does* exist a 50-year-old idea which *is* compatible with having
a unified subject: dependent type theory. This venerable idea invites us to
regard types in themselves as first class values. Then you just place them in
your subject in the way you place anything else in your subject. This very much
fits the feel of Hoon, where we already treat "types" as first class values and
put them in our subject, except they're not actually types, they're mold gates
lol. But because of this, the basic motions should be familiar to users. As a
result of treating types as values, rather than having two "languages"

```
+$  type
  [%atom @tas]
  [%cell type type]
  [%gate type type]
  ...
+$  basic-hoon                              :: output of desugarer
  [%tsls basic-hoon basic-hoon]
  [%wtcl basic-hoon basic-hoon basic-hoon]
  ...
```

one has a single unified core syntax (desugarer output) for types and terms:

```
+$  base
  [%tsls base base]
  [%wtcl base base base]
  ...
  [%atom @tas]
  [%cell base base]
  [%gate base base]
  ...
  [%type ~]          :: type of type
```

The second interesting feature of dependent type theory is that types are
allowed to "depend on" values. For example, the tail of a cell type (or "Sigma
type," in the literature) is allowed to be an expression that computes the type
of the tail based on the *runtime value* of the head. (The type checker, of
course, does not have access to the runtime value, so it reasons symbolically
about the type-producing expression describing the tail type.) For example,
consider the type

```
$%  [%foo @]
    [%bar ^]
```

In dependent hoon, this can be represented by the dependent pair type

```
$:  p/?(%foo %bar)
    ?-  p
      %foo @
      %bar ^
    ==
```

In fact, the former is defined to desugar to the latter. In English, the type
says that it is a cell whose head is `%foo` or `%bar`, and whose tail is either
`@` or `^`, *depending on the specific choice of head value*.

The third thing to understand about dependent types is the nature of the
environment. In a simply typed language, the environment is an ordered list of
`name: type` pairs. For example, right before the elipsis in the following ML
program

```
let x = 1 in
let y = "hello" in
...
```

the environment is `x: int, y: str`. In a dependent language, the environment is
much the same, except that types later in the list *are allowed to mention
variables from earlier*. Consider the go-to toy example used in the dependent
type literature: the `vect` type. A vector is a list whose length is statically
known. The type may be defined, along with a utility:

```
++  vect
  |=  {n/@ t/$$}
  ^-  $$                                                :: type of type
  ?:  =(n 0)
    $~                                                  :: type of ~
  {hd/t tl/(vect (dec n) t)}
::
++  reap                                                ::  repeat
  |=  {t/$$ n/@ a/t}
  ^-  (vect n t)
  ?:  =(n 0)
    ~
  [hd=a tl=(reap t (dec n) a)]
```

(Because types and terms are unified, we cannot reuse the same syntax for both.
Accordingly, we revert to the familiar "middle kingdom" syntax for types.
Furthermore, although I used `$$` for the type of type above, I would prefer to
use `$` for it, and instead use `%` for the empty arm. This far better fits with
the pattern of runic prefixes: $ for types and % for calls.

Also, I give the example of vectors because it is immediately understandable,
not because I think it will be valuable to prove things about the lengths of our
lists all over the place in today's system.

Finally, note that `reap` is a dependent *gate*; its return type depends on the
value of two of its arguments.)

Then in the following program:

```
|=  n/@
=/  lis  (reap @t n 'hi')
...
```

at `...` the context is `n: @, lis: (vect n @t)`. *Notice how the type of `lis`
symbolically refers to `n`.* An astonishing observation is that this has the
same structure as a dependent cell ("Sigma") type: a pair where the type of the
tail depends on the value of the head. In other words, the type of the subject
at the elpisis is the cell type `{n/@ lis/(vect n @t)}`. It is precisely this
observation that enables us to fuse dependently typed programming with subject-
oriented programming!

Unfortunately, as you may have noticed, it also requires us to reverse the order
of the subject, with newer items to the right. This is hopefully the most naive-
user-visible aspect of this proposal, and hopefully it is not that visible
because users are using faces rather than lark syntax. The alternative is making
our `$%`s tail-tagged, which would be far more visibile, and also ridiculous.

One of the exciting parts of the dependent hoon idea is that much of the work
has already been done. In an old prototype called "Deppy," I demonstrated that
dependent type theory can be made to work with subtyping, and, among other
things demonstrated the above encoding of `$%`. (The prototype was then set
aside because Ted wanted a dependent hoon that had an elaborator on day 1.)

(The Deppy prototype also, compellingly, encoded cores as dependent functions,
e.g. `|%  ++  foo  1  ++  bar  'hi'` desugared to `|=  tag/?(%foo %bar)  ?-  tag
%foo  1  %bar  'hi'` of type `$-(tag/?(%foo %bar) ?-(tag %foo @ %bar @t)`. I'm
setting this aside for the moment out of conservatism, but I should point out
that I expect the naive linear scan implementation of pattern matching to be
faster than pointer-chasing battery traversal for many cores in the current
runtime.)

Three substantive theoretical work items remain:

1. Figuring out how to make `find` work across a dependent cell type.

2. Typical implementations of dependent types also track the rhs expressions of
lets in the context, to aid symbolic execution. I have found that this is
definitely required for elaboration, but I am not sure if it is required for
mere type checking. This work item asks if it is, and if so, involves making it
work in the subject as some sort of singleton type.

3. It is possible that I discover other issues in the course of prototyping.

Then there are a bunch of practical considerations, like (for example) deciding
on syntax, testing, etc.

It's worth noting that getting parametric polymorphsim working is not the only
reason to be moved by this proposal. There are a number of other benefits that I
expect to acrue.

For one thing, the new type system will rule out evil vases. Vases are merely
a special case of dependent cell type: `{p/$$ q/p}`, and the same mechanism that
would prevent you from putting wrong tails in your `$%`s happily prevents you
from putting wrong tails in your vases. So that's pretty cool!

Furthermore, I expect that this work will unblock those aspects of system design
that are currently blocked on type system issues. For example, first class types
will be a substantial boon for certain very-typed versions of subscription
reform.


----

Goals:

1. Fix wet gates, on a rational, systematic foundation
2. Rule out evil vases, on a rational, systematic foundation
3. Enable new patterns, e.g. for subscription reform, that would rely on first
class types

Thinking through mull on the tl of sigmas: what operations can allow faces to
vary (is this also the only thing that mull checks in 140?)? Is it just pattern
matching? Think through the cases one by one. What about calling a function that
returns a type, e.g. `list`? (If it's fully saturated, we should evaluate it,
right?)

Can tail-with-pattern match give rise to ?^ in a naturalistic way?


