Syntax
======


Syntax: Twigs
------------

A twig is an abstract syntax tree (or AST). Everything the Hoon programmer types is parsed into a twig.

##Noun

All constant data in Hoon is 
%dtzy
%dtzz


##Runes





Tall forms

When all is said and done, the programmer is formatting a big wall of text. This canvas has a curious but essential property - it is indefinitely tall, but finitely wide. We strongly encourage an 80-column standard.

So the programmer's task as a visual designer is to persuade her code to flow down, not across. The usual way to lay out a tree which does not fit on one line is to indent the subtrees and enclose them in parens, brackets or braces. Which might look like this (not Hoon syntax):

?:  {
  &
  47
  52
}
Hoon, like other functional languages, has very deep expression trees. In this simple, classic syntax model, a functional language develops huge piles of closing parens at the end of large blocks, which is manageable but ugly. Less manageably, as each subtree is indented to the right, the width of the text window bounds the depth of the expression tree.

Other languages skip the braces and parse whitespace, using indentation to express tree depth. This actually is valid (but ugly) Hoon:

?:
  &
  47
  52
This gets rid of the terminator problem, but keeps the width problem. And parsing whitespace is horrible. Whitespace in Hoon is not significant, though its presence or absence is. (Note also that hard TAB characters are zutiefst verboten).

Hoon notices a couple of things about this problem. First, most Hoon twigs have small constant fanout. A parser shouldn't need either significant whitespace or a terminator to figure out how many twigs follow ?: - the answer is always 3.

Second, our goal is to descend into a deep tree without losing right margin. With the backstep pattern

?:  &
  47
52
or

=+  a=3
b
we step two spaces backward at each subtwig, till the last one is at the same indentation as its parent.

This preserves your right margin in one and only one case - where the bottom twig is the heaviest. For example, if we write

?:  &
  47
?:  |
  52
?:  &
  97
=+  35
b
we see a tree that flows neatly down the screen. It's obviously much nicer than, say (not Hoon syntax):

?:  {
  &
  47
  ?:  {
    |
    52
    ?:  {
      &
      97
      =+  {
        35
        b
      }
    }
  }
}
or any similar abortion. But its downward flow depends on the coincidence of the bottom twig being the heavy one:

?:  &
  ?:  |
    52
  ?:  &
    97
  =+  35
  b
47
To handle this, Hoon has a reasonable selection of reverse hoons, which have the same semantics but inverse order. For instance, if ?: is "if," ?. (wutdot) is "unless":

?.  &
  47
?:  |
  52
?:  &
  97
=+  35
b
Wide forms

Observe that in the tall syntax, there are always at least two spaces (or one newline) between tokens. Other than this, nothing requires anything to be tall. For instance, it is normal and only slightly aggressive to write:

?.  &  47
?:  |  52
?:  &  97
=+  35
b
But we could even go so far as:

?.  &  47  ?:  |  52  ?:  &  97  =+  35  b
Few would find this readable, which is why Hoon also has a wide syntax:

?.(& 47 ?:(| 52 ?:(& 97 =+(35 b))))
On a single line, the parentheses - while a parser could get away with skipping them - are needed to actually read the expression. The hoon attaches directly to the left paren (pel), and a double space or a newline is a syntax error.

The semantics of tall and wide syntax are identical, of course. The choice is entirely up to the programmer. Some languages can be formatted automatically - turning an abstract syntax tree into a tall, handsome Hoon file is an art form. We won't say a program could never do it - but it'd be work.

Wide forms are also nice because our immature and incomplete command-line shell can't process multi-line input.

Irregular forms

For a very large set of primitives, neither tall nor wide form is tight enough. If you go to ++scat in hoon.hoon, you can see them all, organized by initial character.

This isn't the place to go over the irregular forms directly - we'll introduce them when we talk about individual runes, or when we run into them and we can't go around.
