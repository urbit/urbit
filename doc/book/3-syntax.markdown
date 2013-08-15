3: Hoon - Syntax
=========

Now, let's actually look at Hoon.  Really, try not to recoil
in horror.  It's actually not anything like line noise.

Open the Hoon kernel - `urb/les/arvo/hoon.hoon`.  Let's look at
the full, official decrement function (line 549):

	++  dec
          ~/  %dec
          |=  a=@
          ^-  @
          ?<  =(0 a)
          =+  b=@
          |-
          ?:  =(a +(b))
            b
          $(b +(b))

Whaa?

Any attempt to understand this in terms of any language you
already know would clearly be mistaken.  In both syntax and
semantics, learning to program in Hoon is learning to program all
over again.  When we say from scratch, we mean from scratch!

It's actually worse than that - learning Hoon is learning to
_read_ all over again.  Hoon is a keyword-free language - any
alphanumeric text in the program is part of the program.  Where
other languages have reserved words, Hoon has squiggles.

We use so many of these ASCII glyphs that we like to be able
to read them out loud.  A language is meant to be _said_.  The
squiggles have conventional names, sort of, some of them, some of
them easy to say, others not so much.  So we've renamed them:
 
    ace  space      gal  <          per  )
    bar  |          gar  >          sel  [
    bas  \          hax  #          sem  ;
    buc  $          hep  -          ser  ]
    cab  _          kel  {          sig  ~
    cen  %          ker  }          soq  '
    col  :          ket  ^          tar  *
    com  ,          lus  +          tec  `
    doq  "          pam  &          tis  =
    dot  .          pat  @          wut  ?
    fas  /          pel  (          zap  !

You just have to memorize these names.  Sorry.

But is this at least enough symbols?  Alas, nowhere near.
ASCII's glyph supply is not the greatest, but we can make all the
squiggles we need by forming digraphs, or _runes_.  

To pronounce a rune, concatenate the glyph names, stressing the
first syllable and softening the second vowel into a "schwa."
Hence, to say `~.`, say "sigdot."  To say `|=`, say "bartis."
Which has an inevitable tendency to turn into "barts" - a sin
to be encouraged.  In any language actually spoken by actual
humans, laziness soon rounds off any rough edges.

So if we had to read the above decrement, omitting the spaces
(which only a real purist would pronounce), we'd say: "luslus dec
sigfas cen dec bartis a tis pat sigbar soq dec soq ketcab pat wutgal
tis pel zero a per tislus b tis pat barhep wutcol tis pel a lus pel
b per per b buc pel b lus pel b per per." The authorities would
then arrive, and drag us out in a big net.  Definitely don't do
this at the airport.

Geeks being solitary by nature, opportunities for reading code
aloud are limited.  But studies by actual scientists have shown
that even when we read silently, we activate the motor cortex
that controls our vocal cords.  Even if we never speak these
squiggles, they're easier to _think_ if bound to simple sounds.  

(And don't worry if you can't get yourself to say "lus" instead
of "plus" for `+`, or "tar" instead of "star" for `*` - I have
this problem myself.  It's much easier to replace "underscore"
with "cab" or "ampersand" with "pam.")

Hoon has almost 90 digraphic runes.  They are easier to organize
in your head, though, because the choice of glyph is not random.
The second glyph in a rune means little or nothing, but the first
defines a rough semantic category.  These categories are:

    |  bar    gates (ie, functions) (ie, one-method cores)
    ?  wut    conditionals, booleans, tests
    :  col    tuples
    .  dot    nock operators
    $  buc    factory macros (ie, type definitions) 
    ^  ket    type conversions
    =  tis    compositions
    %  cen    invocations
    &  pam    gears (ie, objects) (ie, multi-method cores)
    ~  sig    hints
    ;  sem    miscellaneous macros
    !  zap    special operations

Each rune has _[not]_ its own doc file in the `rune/190` directory.  The
name of the file is the name of the rune, minus the vowels.
Thus, `|=` or "bartis" is _[not]_ defined in `rune/190/brts.txt`.

Opening this file, we see:

    %brts  |=  "bartis"

      define:
        [%brts p=gene q=gene]

      expand:
        [%brts *]  [%brcb p.gen (~(put by *(map term foot)) %% [%ash q.gen])]

There should be some actual discussion, but there isn't.  Still,
`brts.txt` is quite complete as a definition of `|=`.  How?  We
need to step back a little.

When the Hoon parser parses a source file, it generates a noun
called a `gene`.  If you know what an AST is, a gene is an AST node.
If you don't, don't worry about it.

Search the current kernel for `++  gene` - note double space.
This code is both the type declaration for type `gene`, and
a function that maps an untyped noun to a typed gene.  In it
you'll see the above definition,

    [%brts p=gene q=gene]

Ie, one kind of gene is a triple whose head is the constant
`%brts`, and whose tail is a pair of genes, `p` and `q`.  

We also see the semantics of this rune: it expands to

    [%brcb p.gen (~(put by *(map term foot)) %% [%ash q.gen])]

ie, `|=` is a built-in macro.  But back to syntax.

What is `%brts`?  The atom also known as `1937011298` or
`0x73747262`.  Simply a string mapped to an unsigned integer, LSB
first.  It's easy to see why the vowels got lost - `%bartis` is
`126896762413410` or `0x736974726162`.  On a 32-bit CPU with
31-bit direct atoms, `%brts` is direct and `%bartis` indirect
(not that the programmer can tell the difference).  But you still
say "bartis."

For instance, in the decrement above, we have

    |=  a=@
    ~|  'dec'
    ^-  @
    ?<  =(0 a)
    =+  b=@
    |-
    ?:  =(a +(b))
      b
    $(b +(b))

In this `%brts`, `p` is 

    a=@

and `q` is

    ~|  'dec'
    ^-  @
    ?<  =(0 a)
    =+  b=@
    |-
    ?:  =(a +(b))
      b
    $(b +(b))

We are starting to see the principles of Hoon syntax.  Let's make
them clear.

First, for any rune, the Hoon parser has two kinds of syntax:
normal and custom.  Most runes, such as `|=`, have only normal
syntax without custom syntax.  Almost all runes with custom
syntax also have normal.  Custom can mean anything; normal 
is rigid and uniform.

All programming languages, but especially functional ones, face
two difficult syntactic problems.  One is controlling the large
numbers of terminators that appear in any deeply nested tree
structure - Lisp is infamous for its piles of right parens.
These are not a serious usability problem, except inasmuch as you
consider ugly a usability problem (which I do).  Two, a more
serious concern, is keeping complex routines from flowing off the
right margin as tab depth increases.

A glance at the more complex organs of the Hoon kernel reveals
that Hoon is relatively untroubled by either of these woes.  But
why?  One dubious panacea for the terminator problem is the use
of significant whitespace.  Whitespace in Hoon is not
significant.  (To be exact, the presence or absence of whitespace
matters, but the quantity never does.)

The answer is that the normal syntax for every rune has two
forms: "wide" and "tall."  As a functional language, Hoon does
not distinguish between statements and expressions, but normal
wide syntax is expression-like and tall is statement-like.

For instance, in our example above, 

	?:  =(a +(b))
	  b
  	$(b +(b))

is a tall normal form.  The equivalent wide form is

  	?:(=(a +(b)) b $(b +(b)))

It's usually best to use the wide form if your gene fits on the
line, but this is obviously an aesthetic choice.  If your gene
does not fit your margin (which should always be 80 columns), 
you have no choice but to go tall.  For reasons that should be
obvious, a tall gene can contain wide subgenes, but a wide gene
cannot contain tall subgenes - just as, in procedural languages,
a statement can contain expressions but not vice versa.

In the wide normal form, the rune is followed immediately (no
whitespace) by a left paren ("pel"), then the subgenes with a
single space between them, then a right paren ("per") as
terminator.  If the rune was inside the parens rather than a
prefix, this would be the Lisp syntax.

In the tall normal form, any quantity of whitespace follows the
rune, and separates the subgenes from each other.  Where is the
terminator?  There is no terminator - in most cases.

Consider the `?:` rune, "wutcol," `%wtcl`.  This is

  	[%wtcl p=gene q=gene r=gene]

Why should we need a terminator?  We know `%wtcl`, whose
semantics are if-then-else, has three subgenes.  When the parser
sees `?:` followed by space, it simply parses the next three
genes and fills the rune with them.

This only works in runes with fixed tuple structure, which
fortunately is most of them.  A counterexample is `:*`, ie, 

  	[%cltr p=(list gene)]

which is of variable length and needs a terminator.  But we 
have no dangling parens, but an attractive tall closure:

	:*  %foo
            %bar 
            %baz
            %moo
        ==

whose equivalent wide normal is

	:*(%foo %bar %baz %moo)

which no one would ever write, preferring the custom

	[%foo %bar %baz %moo]

This leaves only one question: indentation.  Since space is not
significant (even linebreaks are irrelevant - the newline is just
another space), the use of whitespace in tall forms is purely a
matter of style.  Style is very important, however!

The first law of Hoon indentation style is that all tall
indentation is in two-space increments.  (Tabs are illegal.  If
you pollute a Hoon file with ASCII 9, not only will it not parse,
but thugs in ski masks will kick down your door and shoot you.
You laugh!  Try it!)  Single spaces are for wide only.

The second law of Hoon indentation is that everything in the
kernel is good indentation style.  Or at least if it's not, it
needs changed.  The kernel shall be lapidary, noble, ideal and
above all suspicion - a Doric column, a Tlingit totem pole,
an Egyptian obelisk.

Tallness matters.  The third law of Hoon indentation is that
large genes should flow _down_ and not _across_ - like the
decrement example above.  The right margin is a precious resource
not to be wasted.  It's this law, when properly applied, that
makes casual readers wonder if Hoon is a functional language at
all.  It doesn't have a program counter, but it looks like it
does - at least when written right.

In list-structured runes, like the `:*` above, there is no choice
but to lose right margin.  Fortunately, most runes are tuples,
and most have limited "fanout" - 1, 2, 3 or at most 4.

Both of our above examples - `|=` and `?:` - use "backstep"
indentation which takes advantage of this tuple structure.  For
instance, `|=` has two subgenes, `p` and `q.`  We put `p` on the
same line as `|=`, set off by two spaces, losing 4 characters of
margin.  We put `q` _directly below_, losing no margin at all.

It so happens that in almost every actual case of `|=`, `p` (the
function's argument) is relatively light, whereas `q` (the
function's body) will be much heavier.  Thus, with this pattern of
indentation, we lose no margin and our code flows _down_.

We see this even more in `?:`, where the conditional test (which
is much less likely to be heavy) is first and farthest right,
followed by the "then" case indented two spaces, followed by the
"else" case at the same indent as the rune.

Suppose your "else" is relatively light, and your "then" is
heavy?  You may prefer the `?.` rune, Hoon's "unless," which puts
the else before the then.  Or not.  And in both `?:` and `?.`,
the test (which of course can be arbitrarily heavy) is first.
It is not necessary for your code to _always_ flow down and not
across - just _mostly_.

The conventional principle which backstep indentation sacrifices,
of course, is the idea that absolute indentation depth should
correspond to tree depth, loop depth, or some other metric.  Hoon
is so deeply nested that if tab depth matched tree depth, your
margins on anything interesting would be in the next cube to your
right.  There is perhaps a case for indenting loops, but we don't
find we miss this cue at all.

The paucity of terminators also eliminates a lot of redundancy in
the parser, which can result in relatively exciting syntax
errors.  Our experience is that this is seldom a big problem,
because there are terminated tall forms and the cascade stops
with them.  It is often a small problem, however.
