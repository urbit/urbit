Nock Reference
==============

##What is Nock?

Nock is a homoiconic combinator algebra, not much fancier than SKI combinators.
The spec fits on a T-shirt and gzips to 340 bytes. 

Think of Nock as a kind of functional assembly language. It's not like assembly
language in that it's directly executed by the hardware. It is like assembly
language in that (a) everything in Urbit executes as Nock; (b) you wouldn't want
to program directly in Nock; and (c) learning to program directly in Nock is a
great way to start understanding Urbit from the ground up.

Just as Unix runs C programs by compiling them to assembler, Urbit runs Hoon
programs by compiling them to Nock. You could try to learn Hoon without learning
Nock. But just as C is a thin wrapper over the physical CPU, Hoon is a thin
wrapper over the Nock virtual machine. It's a tall stack made of thin layers,
which is much easier to learn a layer at a time.

And unlike most fundamental theories of computing, there's really nothing smart
or interesting about Nock. Of course, in a strictly formal sense, all of
computing is math. But that doesn't mean it needs to feel like math. Nock is a
simple mechanical device and it's meant to feel that way.

##Nouns

    1  ::    A noun is an atom or a cell.
    2  ::    An atom is a natural number.
    3  ::    A cell is an ordered pair of nouns.


An **atom** is a natural number - ie, an unsigned integer. Nock does not limit
the size of **atoms**, or know what an **atom** means.

For instance, the **atom** `97` might mean the number `97`, or it might mean the
letter `a` (ASCII 97). A very large **atom** might be the number of grains of
sand on the beach - or it might be a GIF of your children playing on the beach.
Typically when we represent strings or files as **atoms**, the first byte is the
low byte. But even this is just a convention. An **atom** is an **atom**.

A **cell** is an ordered pair of any two **nouns** - **cell** or **atom**. We
group cells with square brackets:

    [1 1] 
    [34 45] 
    [[3 42] 12] 
    [[1 0] [0 [1 99]]]

*Nouns* are the dumbest data model ever. **Nouns** make JSON look like XML and
XML look like ASN.1. It may also remind you of Lisp's S-expressions - you can
think of nouns as "S-expressions without the S."

To be exact, a *noun* is an S-expression, except that classic S-expressions have
multiple **atom** types ("S" is for "symbol").  Since Nock is designed to be
used with a higher-level type system (such as Hoon's), it does not need
low-level types. An **atom** is just an unsigned integer of any size.

For instance, it's common to represent strings (or even whole text files) as
atoms, arranging them LSB first - so "foo" becomes `0x6f6f66`. How do we know to
print this as "foo", not `0x6f6f66`? We need external information - such as a
Hoon type.  Similarly, other common atomic types - signed integers, floating
point, etc
- are all straightforward to map into **atoms**.

It's also important to note that, unlike Lisp, Nock cannot create cyclical data
structures. It is normal and common for **nouns** in a Nock runtime system to
have acyclic structure - shared subtrees.  But there is no Nock computation that
can make a child point to its parent. One consequence: Nock has no garbage
collector. (Nor can dag structure be detected, as with Lisp eq.)

There is also no single syntax for **nouns**. If you have **nouns** you have
Nock; if you have Nock you have Hoon; if you have Hoon, you can write whatever
parser you like.

##The Nock Function

    5  ::    nock(a)           *a

Nock is a pure (stateless) function from **noun** to **noun**. In our pseudocode
(and only in our pseudocode) we express this with the prefix operator `*`.

A Nock program is given meaning by a process of reduction. To compute `nock(x)`,
where `x` is any **noun**, we step through the rules from the top down, find the
first left-hand side that matches `x`, and reduce it to the right-hand side (in
more mathematical notation, we might write line 5 as `nock(a) -> *a`, a style we
do use in documentation)

When we use variable names, like `a`, in the pseudocode spec, we simply mean
that the rule fits for any **noun** `a`.

So `nock(x)` is `*x`, for any **noun** `x`. And how do we reduce `*x`?  Looking
up, we see that lines 23 through 39 reduce `*x` - for different patterns of `x`.

Normally `a` in `nock(a)` is a **cell** `[s f]`, or

    [subject formula] 
    
Intuitively, the formula is your function and the subject is its argument.
Hoon, or any other high-level language built on Nock, will build its own
function calling convention which does not map directly to `*[subject formula]`.

##Bracket Grouping

    6  ::    [a b c]           [a [b c]]

Brackets associate to the right. 
    
So instead of writing

    [2 [6 7]]
    [2 [6 [14 15]]]
    [2 [6 [[28 29] [30 31]]]]
    [2 [6 [[28 29] [30 [62 63]]]]]

we can write

    [2 6 7]
    [2 6 14 15]
    [2 6 [28 29] 30 31]
    [2 6 [28 29] 30 62 63]

While this notational convenience is hardly rocket science, it's surprising how
confusing it can be, especially if you have a Lisp background. Lisp's
"S-expressions" are very similar to **nouns**, except that Lisp has multiple
types of **atom**, and Lisp's syntax automatically adds list terminators to
groups. 

For those with Lisp experience, it's important to note that Nock and Hoon use
tuples or "improper lists" much more heavily than Lisp. The list terminator,
normally 0, is never automatic. So the Lisp list

    (a b c)

becomes the Nock **noun**

    [a b c 0]

which is equivalent to

    [a [b [c 0]]]

Note that we can and do use unnecessary brackets anyway, for emphasis.

##Axiomatic Functions

    8  ::    ?[a b]           0
    9  ::    ?a               1
    10 ::    +[a b]           +[a b]
    11 ::    +a               1 + a
    12 ::    =[a a]           0
    13 ::    =[a b]           1

Here we define three of Nock's four axiomatic functions: **Cell-test**,
**Increment** and **Equals** (the fourth axiomatic function, called **Address**,
is defined in lines 16 through 21). These functions are just pseudocode, not
actual Nock syntax, and are only used to define the behaviour of certain Nock
operators. 

We should note that in Nock and Hoon, `0` (pronounced "yes") is true, and `1`
("no") is false. This convention is the opposite of old-fashioned booleans, so
we try hard to say "yes" and "no" instead of "true" and "false."

###Cell-test: `?`

`?` (pronounced "wut") tests whether is a **noun** is a **cell**.  Again, `0`
means "yes", `1` means "no":

    8  ::    ?[a b]           0
    9  ::    ?a               1

---

###Increment: `+`

`+` (pronounced "lus") adds `1` to an **atom**:

    10 ::    +[a b]           +[a b]
    11 ::    +a               1 + a

---

###Equals: `=`

`=` (pronounced "tis") tests a cell for
equality. `0` means "yes", `1` means "no":

    12 ::    =[a a]           0
    13 ::    =[a b]           1
    14 ::    =a               =a

Testing an **atom** for equality makes no sense and logically fails to
terminate.

---

Because `+` works only for **atoms**, whereas `=` works only for **cells**, the
error rules match first for `+` and last for `=`.



##Noun Address

    16 ::    /[1 a]           a
    17 ::    /[2 a b]         a
    18 ::    /[3 a b]         b
    19 ::    /[(a + a) b]     /[2 /[a b]]
    20 ::    /[(a + a + 1) b] /[3 /[a b]]
    21 ::    /a               /a

We define a **noun** as a binary tree - where each node branches to a left and
right child - and assign an address, or **axis**, to every element in the tree.
The root of the tree is `/1`. The left child (**head**) of every node at `/a` is
`/2a`; the right child (**tail**) is `/2a+1`. (Writing `(a + a)` is just a
clever way to write `2a`, while minimizing the set of pseudocode forms.)

`1` is the root. The **head** of every **axis** `n` is `2n`; the **tail** is
`2n+1`.  Thus a simple tree:

         1
      2      3
    4   5  6   7
             14 15

If the value of every leaf is its tree address, this tree is

    [[4 5] [6 14 15]]

Let's use the example `[[97 2] [1 42 0]]`. So

    /[1 [97 2] [1 42 0]] -> [[97 2] [1 42 0]]

because `/1` is the root of the tree, ie, the whole **noun**.

Its left child (**head**) is `/2` (i.e. `(1 + 1)`):

    /[2 [97 2] [1 42 0]] -> [97 2]

And its right child (**tail**) is `/3` (i.e. `(1 + 1 + 1)`):

    /[3 [97 2] [1 42 0]] -> [1 42 0]

And delving into `/3`, we see `/(3 + 3)` and `/(3 + 3 + 1)`:

    /[6 [97 2] [1 42 0]] -> 1
    /[7 [97 2] [1 42 0]] -> [42 0]

It's also fun to build nouns in which every atom is its own axis:

    1
    [2 3]
    [2 6 7]
    [[4 5] 6 7]
    [[4 5] 6 14 15]
    [[4 5] [12 13] 14 15]
    [[4 [10 11]] [12 13] 14 15]
    [[[8 9] [10 11]] [12 13] 14 30 31]

##Distribution

    23 ::    *[a [b c] d]      [*[a b c] *[a d]]

The practical domain of the Nock function is always a **cell**.  When `a` is an
**atom**, `*a`, or `nock(a)`, is always an error.  Conventionally, Nock proper
is always a function of a **cell**.  The **head** of this **cell** is the
**subject**, the **tail** is the **formula**

    [subject formula]

and the result of passing a **noun** through the Nock function is called the
**product**.

    nock([subject formula]) => product

or 

    *[subject formula] => product

The **subject** is your data and the **formula** is your code. And the
**product** is your code's output.

Notice that `a` in the Nock rules is always the **subject**, except line 39,
which is a crash default for malformed **nouns** that do not evaluate.

A **formula** is always a **cell**. But when we look inside that **cell**, we
see two basic kinds of **formulas**:

    [operator operands]
    [formula-x formula-y]

An **operator** is always an **atom** (`0` through `10`). A **formula** is
always a **cell**.  Line 23 distinguishes these forms:

    23 ::    *[a [b c] d]     [*[a b c] *[a d]]

If your code contains multiple **formulas**, the **subject** will distribute
over those **formulas**.

In other words, if you have two Nock **formulas** `x` and `y`, a **formula**
that computes the pair of them is just the **cell** `[x y]`. 
    
    *[subject [x y]]  -> [*[subject x] *[subject y]]

No **atom** is a valid **formula**, and every **formula** that does not use line
23 has an atomic **head**.

Suppose you have two **formulas** `f` and `g`, each of which computes some
function of the **subject** `s`. You can then construct the **formula** `h` as
`[f g]`; and `h(s) = [f(s) g(s)]`.

For example:

    *[[19 42] [0 3] 0 2]

The **subject** `s` is `[19 42]`. The **formula** `h` is `*[[0 3] 0 2]`.

    *[s h]

The **head** of `h` is `f`, which is `[0 3]`. The **tail** of `h` is `g`, which
is `[0 2]`.

    *[s [f g]]

by the distribution rule, `*[s [f g]]` is

    [*[s f] *[s g]]

or

    [*[[19 42] [0 3]] *[[19 42] 0 2]]

`*[s f]` is `f(s)` and produces `42`. `*[s g]` is `g(s)` and produces 19.

Since `h(s)` is `[f(s) g(s)]`, `h(s)` is `[42 19]`:

    *[[19 42] [0 3] 0 2] -> [42 19]

##Operator 0: Axis

    25 ::    *[a 0 b]         /[b a]

Operator 0 is Nock's tree address or **axis** operator, using the tree
addressing structure defined in lines 16 through 20.  `*[a 0 b]` simply returns
the value of the part of `a` at **axis** `b`.  For any subject `a`, the formula
`[0 b]` produces `/[b a]`. 

For example,

    *[[19 42] 0 3] -> /[3 19 42] -> 42.

##Operator 1: Just

    26 ::    *[a 1 b]          b

`1` is the constant, or **Just operator**. It produces its operand `b` without
reference to the **subject**.

For example,

    *[42 1 57] -> 57

**Operator 1** is named **Just** because it produces "just" its operand. 

##Operator 2: Fire

    27 ::    *[a 2 b c]       *[*[a b] *[a c]]

**Operator 2** is the **Fire operator**, which brings us the essential magic of
recursion. Given the **formula** `[2 b c]`, `b` is a **formula** for generating
a new **subject**; `c` is a **formula** for generating a new **formula**. To
compute `*[a 2 b c]`, we evaluate both `b` and `c` against the current
**subject** `a`.

A common use of **Fire** is to evaluate data inside the **subject** as code. 

For example:

    *[[[40 43] [4 0 1]] [2 [0 4] [0 3]]] -> 41

    *[[[40 43] [4 0 1]] [2 [0 5] [0 3]]] -> 44
    
**Operator 2** is called **Fire** because it "fires" Nock **formulas** at its
(possibly modified) **subject**.

##Operator 3: Depth

    28 ::    *[a 3 b]         ?*[a b]

**Operator 3** applies the **Cell-test** function defined in lines 8 and 9 to
the product of `*[a b]`.

###Cell-test: `?`

`?` (pronounced "wut") tests whether is a noun is a cell.  Again, 0 means yes, 1
means no:

    8  ::    ?[a b]           0
    9  ::    ?a               1

---

**Operator 3** is called **Depth** because it tests the "depth" of a noun.
**Cell-test** properly refers to the pseudocode function `?`.

##Operator 4: Bump

    29 ::    *[a 4 b]         +*[a b]

**Operator 4** applies the **Increment** function defined in lines 10 and
11 to the product of `*[a b]`. 

###Increment: +

`+` (pronounced "lus) adds 1 to an **atom**:

    10 ::    +[a b]           +[a b]
    11 ::    +a               1 + a

---

**Operator 4** is called **Bump** because it "bumps" the atomic product `*[a b]`
up by 1. **Increment** properly refers to the pseudocode function `+`.


##Operator 5: Same

    30 ::    *[a 5 b]         =*[a b]

**Operator 5** applies the Equals function defined in lines 12, 13 and 14 to the
product of *[a b]. 

###Equals: =

= (pronounced "tis", or sometimes "is") tests a cell for equality. 0 means yes,
1 means no:

    12 ::    =[a a]           0
    13 ::    =[a b]           1
    14 ::    =a               =a

Testing an atom for equality makes no sense and logically fails to terminate.

---

**Operator 5** is called the **Same** operator, because it tests if the head and
tail of the product of `*[a b]` are the same.  "Equals" properly refers to the
pseudocode function "=".

##Operator 6: If

    32 ::    *[a 6 b c d]     *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

**Operator 6** is a primitive known to every programmer - **If**. Its operands, a
**test formula** `b`, a **then formula** `c` and an **else formula** `d`.

If the **test** `b` applied to the **subject** evaluates to `0` ("yes"),

    *[a b] -> 0
    
then **If** produces the result of `c`, the **then formula**, applied to
the **subject**,

    *[a c]
    
Else, if applying the **test** to the **subject** produces `1` ("no"), 

    *[a b] -> 1
  
**Operator 6** produces the result of `d`, the **else formula**, applied to the
**subject**,
    
    *[a d]
  
If `*[a b]` produces any value other than `0` ("yes") or `1` ("no"), **If**
crashes. 

Let's examine the internals of **Operator 6**:

**If** could have been defined as a built-in pseudocode function, like
**Increment**:

    ::    $[0 b c]         b
    ::    $[1 b c]         c

Then **Operator 6** could have been restated quite compactly:

    ::    *[a 6 b c d]     *[a $[*[a b] c d]]
    
However, this is unnecessary complexity. **If** can be written as a macro using the
primitive operators:

    32 ::    *[a 6 b c d]     *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

Reducing the right-hand side (an excellent exercise for the reader) produces:

    *[a *[[c d] [0 *[[2 3] [0 ++*[a b]]]]]]

Which is the reduced pseudocode form of **Operator 6**.

Additionally, we could simplify the semantics of **If**, at the
expense of breaking the system, by creating a macro that works as
if and only if `*[a b]` produces either `0` or `1`.

This simpler **If** would be:

    ::    *[a 6 b c d]    *[a 2 [0 1] 2 [1 c d] [1 0] [4 4 b]]

which reduces to:

    *[a *[[c d] [0 ++*[a b]]]]

Let's examine the internals of this macro with a test example:

    *[[40 43] [6 [3 0 1] [4 0 2] [4 0 1]]]

Fitting this to the reduced form:

    *[[40 43] *[[4 0 2] [4 0 3]] [0 ++*[[40 43] [3 0 1]]]]]

Our test:

    *[[40 43] [3 0 1]] 
  
produces a 0,

    *[[40 43] *[[[4 0 2] [4 0 3]] [0 ++0]]]]
  
which gets incremented twice

    *[[40 43] *[[[4 0 2] [4 0 3]] [0 2]]]

and is used as an axis to select the head of [[4 0 2] [4 0 3]]

    *[[40 43] [4 0 2]]

which increments `40` to produce `41`. Had the **test** produced a "no" instead of a
"yes", **If** would have incremented the **tail** of the subject instead of the
**head**.

The real **If** is only slightly more complicated:

    ::    *[a 6 b c d]     *[a *[[c d] [0 *[[2 3] [0 ++*[a b]]]]]]

There is an extra step in the real **If** to prevent unexpected behaviour if the
test produces a value other than 0 ("yes") or 1 ("no"). The real **If** will
crash if this happens and the naive **If** may not (the reader will find it a
useful exercise to figure out why).

It's worth noting that practical, compiler-generated Nock never does anything as
funky as these **Operator 6** macro internals. 

##Operator 7: Compose

    33 ::    *[a 7 b c]       *[a 2 b 1 c]

**Operator 7** implements function composition. To use a bit of math
notation, if we define the formula `[7 b c]` as a function `d(x)`:

    d(a) == c(b(a))

This is apparent from the reduced pseudocode form of **Operator 7**:

    *[*[a b] c]

 As an example,

    *[[42 44] [7 [4 0 3] [3 0 1]]] -> 1

The above sequentially applies the **formulas** `[4 0 3]` and `[3 0 1]`
to our subject `[42 44]`, first incrementing the **head**, then testing
the **depth**.

##Operator 8: Push

    34 ::    *[a 8 b c]       *[a 7 [[7 [0 1] b] 0 1] c]

**Operator 8** pushes a new **noun** onto our **subject**, not at all unlike
pushing a new variable onto the stack. 

The internals of **Operator 8** are similar to **Operator 7**, except
that the **subject** for `c` is not simply the **product** of `b`, but the
ordered pair of the **product** of `b` and the original **subject**.

This is apparent from the reduced pseudocode form of **Operator 8**:

    *[[*[a b] a] c]

**Operator 8** evaluates the **formula** `c` with the **cell** of `*[a b]` and
the original **subject** `a`. In math notation, if we define `[8 b c]` as
`d(x)`:

    d(a) == c([b(a) a])

Suppose, for the purposes of the **formula** `c`, we need not just the
**subject** `a`, but some intermediate **noun** computed from the **subject**
that will be useful in the calculation of `c`. **Operator 8** applies
**formula** `c` to a new **subject** that is the pair of the intermediate value
and the old **subject**. 

In higher-level languages that compile to Nock, variable declarations are likely
to generate an **Operator 8**, because the variable is computed against the
present **subject**, and used in a calculation which depends both on the
original **subject** and the new variable. 


##Op 9: Call

    35 ::    *[a 9 b c]       *[a 7 c 2 [0 1] 0 b]

**Operator 9** takes a **subject** and produces a new **subject** containing both code and data, also known as a **core**. A formula is then both extracted from, and applied to this new **core** **subject**. As its breakdown demonstrates, **Operator 9** is a macro that can compose recursive functions, as it encapsulates the functionality of both **Operator 7**(function composition) and **Operator 2** (recursion).

The reduced pseudo code demonstrates this clearly:

    *[*[a c] *[*[a c] 0 b]]

Here, `c` is some formula that produces a **core** when applied to **subject** `a`. This new **core** is then paired with a formula extracted from **axis** `b` within an identical copy of the new **core**. In higher-level languages that compile to Nock, functions that loop recursively often generate **Operator 9**, as it is the easiest way for a function (or **gate**, to use proper Hoon technology) to recall itself with changes made to its data.  

 
##Op 10: Hint

    36 ::    *[a 10 [b c] d]  *[a 8 c 7 [0 3] d]
    37 ::    *[a 10 b c]      *[a c]

**Operator 10** serves as a hint to the interpreter.

As shown above, there are two cases of **Operator 10**. The latter has the formula `[10 b c]`, which then simply reduces to `c`. Although `[10 b c]` has to be semantically equivalent to `c`, it doesn't have to be practically equivalent. Since whatever information in `b` is discarded, a practical interpreter is free to ignore it, or to use it as a hint, as long as it does not affect the results of the computation.

The former case is slightly more complicated. While it may appear that its reduction `*[*[[*[a c] a] 0 3] d]` could be reduced further to simply `[a d]`, as `*[[*[a c] a] 0 3]` could seem to return just `a`. However, there is a possibility that `c` does not terminate, in which case `[a d]` would be incorrect. Therefore, `*[*[[*[a c] a] 0 3] d]` is the most this case can be reduced. This is because **Operator 10** in either case is a hint. If `x` in `[10 x y]` is an atom, we reduce line 37 and `x` is simply discarded. Otherwise, `x` is a cell `[b c]`; b is discarded, but c is computed as a formula and its result is discarded.

Effectively, this mechanism lets us feed both static and dynamic information into the interpreter's hint mechanism.  






##Crash default

    39 ::    *a               *a
