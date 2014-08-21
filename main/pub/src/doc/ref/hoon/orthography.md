Orthography: Consensus Aesthetic
==========

The Hoon compiler enforces the syntactical correctness of the language, it does
not, with some exceptions, enforce aesthetic standards. Many different styles
of Hoon are possible. However, given Hoon's runic syntax, it is remarkably easy
for the novice programmer to generate idiosyncratic illegible code. Many other
languages that make heavy use of ASCII have a similar problem. Furthermore,
collaborative programming is made vastly easier by using a standard style
convention.

The Urbit source is written in a style of Hoon called the Consensus Aesthetic. 
No patches to the Urbit source will be accepted unless they follow the ConsensusAesthetic.

The general rules of the Consensus Aesthetic are the following:

Character Restriction
---------------------

The horizontal tab character, \ht, ASCII 0x9, must never occur. This is
enforced by the compiler.

Line and Comments
-----------------

Lines must not exceed 80 columns in width and should not exceed 55 columns.

Blank lines (lines consisting entirely of whitespace) should not occur. For
visual separation of code, use empty comments.

Comments may appear on column 0, column 57 or inline at the same level of
indentation as the code.

Indentation
-----------

Aesthetically, the act of programming is the act of formatting a big wall of
text. This canvas has a curious but essential property - it is indefinitely
tall, but finitely wide. The programmer's task as a visual designer is to
persuade code to flow down, not across. 

The first law of Hoon indentation style is that all tall indentation is in
two-space increments. Single spaces are for wide only.

The second law of Hoon indentation is that everything in the kernel is good
indentation style. Or at least if it's not, it needs changed. 

The third and most important law of Hoon indentation is that large twigs should
flow down and not across. Longer twigs should occur below shorter ones.  Hoon
has several runes designed specifically to aid this task

The right margin is a precious resource not to be wasted. It's this law, when
properly applied, that makes casual readers wonder if Hoon is a functional
language at all. It doesn't have a program counter, but it looks like it does -
at least when written right.

Naming Convention
-----------------

Names must follow one of the following naming conventions: Austere, Lapidary,
or Freehand. 

In Austere Hoon, variables and arguments are named alphabetically with one
letter, a, b, c etc, in strict order of appearance in the text. This scheme is
only useful in the case of extremely regular and straightforward namespaces:
very short functions, for instance.

Austere arms must be gates or trays. Gate arms are three letters and try to
carry some mnemonic significance - for instance, ++dec. Tray arms are two
letters and try to resemble pronouns - for instance, ++by.

Austere structures must be short tuples, no wider than 5. The legs are named p,
q, r, s and/or t.

Conventional recursive structures use other standard names. The head of a list
is always i, the tail is always t. In a binary tree of nodes, the node is n,
the children l and r.

When in doubt, do not use Austere Hoon. In an ordinary context - not least
because Austere gates are easily mistaken for Lapidary variables - there should
be as few Austere arms as possible. And always remind yourself that Austere
Hoon makes it as hard as possible to refactor your code.

Lapidary Hoon is the ordinary style of most of Hoon and Arvo. In lapidary mode,
variables, arguments, attributes, etc, are three-letter strings, usually
consonant-vowel-consonant, generally meaningless. If the same string is used
more than once in the same file, it should be used for the same concept in some
sense, as often happens spontaneously in cutting and pasting. It would be nice
to have an editor with a macro that generated random unique TLV strings
automatically.

Lapidary arms are always four letters. They may or may not be English words,
which may or may not mean anything relevant.

In Freehand Hoon, do whatever you want. Note that while uppercase is not
permitted in a symbol, - is, suggesting a generally Lisp-like state of gross
hyphenated disorder. F-mode is best used for top-layer software which nothing
else is based on; prototyping and casual coding; etc. Freehand Hoon is not an acceptable style for any code in the Urbit source proper, and is discouraged for production applications.


