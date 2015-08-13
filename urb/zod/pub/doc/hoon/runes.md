<div class="short">

Runes
=====

In Hoon there are no reserved words. Instead, [`++twigs`]() (abstract
syntax trees), are formed using a diagraph of two ASCII symbols, which
is called a rune. 

Runes are loosely divided into categories by their first character. To 
find documentation on each individual category, follow these links:

<list></list>

<search/>

</div>

------------------------------------------------------------------------

Introduction
------------

For example, the rune `?:`, pronounced "wutcol", is a rune that accepts
three `++twig` expressions to form an "if-then-else statement," where
`p` is the predicate, `q` is the "then" statement, and `r` is the "else"
statement:

        [%wtcl p=twig q=twig r=twig] 

In a program, it is used like so:

    ++  add
        |=  [a=@ b=@]
        ^-  @
        ?:  =(0 a)  b
        $(a (dec a), b +(b))          

Here the `=(0 a)` is `p`, the `b` is `q`, and the bottom line is the
`r`.

There are several benefits to using runes in lieu of reserved words.
First, it prevents the programmer from accidentally misusing a reserved
word as a variable name, which also allows her to be sure that any word
in her progam is an identifier. Next, as the first ASCII symbol of the
rune digraphs bears semantic significance, the programmer can look at
any rune and immediately have a basic, intuitive understanding as to
what it does.

Next, rune syntax addresses several issues that arise in other
languages. First, it produces cleaner, less verbose code that
conveniently flows downward. To highlight the verbosity Hoon avoids by
implementing runes, here is the C equivalent of the `++add` source code
printed above:

    attribute add {
         function(left-operand: atom, right-operand: atom)
         produce atom
         if equals(0, left-operand) {
          right-operand
         } else {
            recurse(left-operand (decrement left-operand)), 
                    right-operand (increment right-operand))
         }
       } 

What is more, Hoon solves two problems that arise in functional
programming languages with very deep expression trees: first, the
collection of huge piles of closing parens at the end of large blocks;
and second, the fact that the depth of expression trees are thus bounded
by the width of the text box, as subexpressions tend to be indented.
Some languages solve the bracket-terminator problem by parsing
whitespace in order to use indentation to express tree depth:

        ?:
            &
            47
            52

While this is actually valid Hoon, it does not solve the width problem.

To address this problem, most Hoon `++twigs` have a short, fixed
fan-out. Thus, the parser does not need significant whitespace nor a
terminator to determine how many twigs follow a `?:`--it already knows
that the answer is three. A smart parser allows the Hoon programmer to
format her code using a [backstep pattern]() that allows her to descend
into a deep tree without losing right margin.

Lastly, but perhaps most significantly, code is meant to be seen, and
not read. Anyone who has even slight experience coding Hoon will tell
you that they can understand and connect with properly formatted Hoon
code on a deeper, more intuitive level that cannot be explained but must
be experienced. One doesn't read `++add`, she sees it.

Names and Categories
--------------------

While the second glyph in a rune means little or nothing, the first
defines a rough semantic category:

    |  bar    Core construction
    $  buc    Tile construction
    %  cen    Invocations
    :  col    Tuple construction
    .  dot    Nock operations
    #  hax    Pretty printing
    ^  ket    Type conversions
    ;  sem    Composers (and XML generators)
    ~  sig    Hints
    =  tis    Subject modifiers
    ?  wut    Conditionals, booleans, and tests
    !  zap    Special operations

As shown above, each glyph has its own monosyllabic name, designed to be
pronounced quickly in combination with another glyph to form a rune
name. As languages are often read-aloud, this saves the programmer from
having to say "dollar sign, question mark"--"bucwut" is much more
compact.

Irregular pronuncations
-----------------------

To avoid a few tongue-twisters, some runes have irregular pronunciations
that should be noted:

        --    hephep    phep    
        +-    lushep    slep
        ++    luslus    slus
        ==    tistis    stet

Note: these runes are not members of any of the categories above, but
are mostly used to....
