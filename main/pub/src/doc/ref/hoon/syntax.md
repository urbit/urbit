Syntax
======

Runes
-----

Everything in hoon is parsed into an Abstract Syntax tree, called a `twig`. 
A `twig` is a (frond)[lexicon/#fronds] composed of a (cubic)[lexicon/#cubes]
rune and one or more children:

```
~zod/try=> :-(1 2)
[1 2]
~zod/try=> (ream ':-(1 2)')
[%clhp p=[%dtzy p=%ud q=1] q=[%dtzy p=%ud q=2]]
```

As the expression above begins with `:-`, it is parsed into the twig 
`[%clhp p=twig q=twig]`, which accepts two twigs with which it produces a
tuple. In this case, both elements are atom literals and are parsed into the twig
`[%dtzy p=term q=@]`, which produces atomic constants.

There are several valid twig syntaxes in hoon, which are covered below. 

###Tall Form

For all multiline expressions, Tall form is used. To avoid the visual problems
that deep expression trees cause in other functional languages (namely excessive
parentheses and the tendencey for code to drift horizontally off the right side
of the screen), hoon employs neither multiline braces nor significant
indentation. Instead, when a twig, such as the `[%clhp p=twig q=twig]` from
above is parsed, the parser then knows that exactly two twigs are to follow. In
Tall Form, twigs are separated by at least two spaces (a newline is equivalent
to two spaces).

These rules do not prevent the aforementioned problem horizontal drift:

```
?:  |
  47
  ?:  |
    52
    ?:  |
      7
      20
```

**Note:** `?:` initiaties an _if-then_else_ twig. `|` is the loobean false.

To avoid this problem, the official convention prescribes a backstep pattern, which
preserves indentation level in the common case:


```
?:  |
  47
?:  |
  52
?:  |
  7
20
```


Notice that the first child is placed the furthest horizontal distance away
from its parent rune. Each of the subsequent children is outdented, such
that the last child (a similarly formatted twig) is vertically aligned with its parent.

However, this convention does not always prevent horizontal drift. For example,
there are many cases when the last twig is not the heaviest:

```
?:  &
  ?:  |
    52
  ?:  |
    7
  20
47
```

To handle cases like this, there exist several (synthetic runes)[lexicon/#rune_types]
that invert the argument order of common runes. For example, where `?:` is _if-then_, `?.` is _unless_:

```
?.  &
  47
?:  |
  52
?:  |
  7
20
```

####Closing Unbounded Twigs####

Some runes accept a list of an arbitrary number of children,
which is closed by a `==` (pronounced "stet"). By convention, the children
appear in an indented block, and for most runes, the `==` is aligned vertically with
its parent.

```
:*
  1
  2
  3
==
::  [1 2 3]
```

Some runes, such as `;~`, [%smsg p=twig q=tusk]`, have both `twig` and `tusk` 
(list of twigs) children:

```
;~  plug
  ace
  ace
==
```

####Kingside and Queenside####

Runes such as %= accept associative lists. There exist two conventions:
Kingside, where the key-value pairs are (or at least start) on the same line:

```
%=  res
  key1  val
  key2  val2
==
```

And queenside, where the keys lie indented on the line above their respective
values:

```
%=    res
    key
  twig-producing-val
    key2
  =+  a=2
  [a 3]
==
```

The latter is useful when the keys and/or values are heavy.

####Reference[#tall-reference]

For specific rune syntaxes, see the [lexicon]().
The regular form parser, `++norm:vast`, can also be helpful: for example, %dtls
(increment) is parsed by

```
++  norm                                              ::  rune regular form
    |=  tol=?
    =<  %-  stew
        ^.  stet  ^.  limo
        :~  :-  '|'
  ::  ......code skipped.......
```

```
            :-  '.'
              ;~  pfix  dot
                %-  stew
                ^.  stet  ^.  limo
                :~  ['+' (rune lus %dtls expa)]
```

```
    ++  expa  |.(loaf)                                  ::  one twig
```

```
    ++  loaf  ?:(tol tall wide)                         ::  hoon, current width
```

###Wide

Wide form is a more compact and visually structured twig syntax, where an
expression can be no longer than one line. Notably, children are enclosed by
parentheses, and twigs are separated by single spaces: 

```
?:((gth 1 2) %abort-abort %correct)
```

While Tall Form expressions can contain wide form expressions, the converse is
invalid hoon:

```
?:  .=(a b)
  0
1 
::  succeeds
?:(.=  a
     b 0 1)
::  fails  
```

There are two other notable differences from Tall Form: `==` is no longer used,
having been made redundant by closing parentheses; and, the syntax for
associative lists gains a `','` between pairs:

```
?-(a 0 %even, 1 %odd)
::  vs.
?-  a
  0  %even
  1  %odd
==
```

####Reference

For specific rune syntaxes, see the [lexicon]().
The regular form parser, `++norm:vast`, can also be [helpful.](#tall-reference).

###Irregular

For many common runes, there exist special wide forms. For example,
the rune `:*`, which was used as an example in the tall section[linkToExample](),
is more frequently employed in its irregular form, a list of twigs surrounded 
by brackets:

```
~zod/try=> [1 2]
[1 2]
~zod/try=> (ream '[1 2]')
[%cltr p=~[[%dtzy p=%ud q=1] [%dtzy p=%ud q=2]]]
```

A comprehensive list of all of these irregular forms can be found in the
[lexicon](lexicon/irregular-section)i, and the irregular form parser
`++scat:vast`.

###Tile

Some runes accept [tiles](lexicon) instead of/in addition to twigs as children.

These runes switch us into tile syntax:

  Tiles have a similar syntax to twigs when:
    
    Examples


  And different

    1.The irregular forms for `^=` and `:*`, infix `=` and surrounding `[]` 
    repsectively, now denote `$=` and `$:`

    2.Primitive types `'@'`, `'^'`, and `'*'` represent their tiles.

    3.Number literals represent their cubes, and are along with actual cubes
    interpreted as their associated clams.


Tile syntax defaults to parsing a twig, and using the result as a
  [clam](lexicon).



```
~zod/try=> *a=@ud
a=0
```


`a=@ud` is a `tile`, representing a face _a_ wrapped around an atom of [odor]()
_unsigned-decimal_.

Tile syntax is similar to that of twigs, except that `'@'`, `'^'`, and `'*'`
represent the [clams](lexicon) for the `atom`, `cell`, and `noun` primitive types,
and numeric literals become cubed. When a twig is encountered, its product is
used as a clam directly:

```
|=  a=(list char)
[' ' a]
```

See [lexicon tiles[, parser `++noil:vast`

Wings
-----

Items in the subject are accesed with wings

```
>  =<(a.b.+.c c=[4 [b=[a=1 2] b=[a=6 7]])
6
```

Limbs and axes, separated by dots, inner to outer.

Limbs are terms, axes (have a few syntaces)[lexicon axis syntax]

See:**

Nouns
-----

Most common rune is the virtual %dtzy, atomic nock 1

```
>(ream '42')
[%dtzy p=%ud q=42]
```

Its counterpart %dtzz does cubes

```
>(ream '%42')
[%dtzz p=%ud q=42]
```

See [lexicon atoms]

Tapes

###Interpolation

```
"This is an {(weld "inter" (trip %polate))}d string"


Dependencies (%ford)
--------------------

While !^ provides rudimentary %clay includes...


See: ford.hoon

Templating (++sail)
-------------------

Start with `;term`

```
;html
  ;head:title:"Hello"
  ;body
    ;p
     ; Text with some ;{b "bolded"}
     ; and some -{(weld "inter" "polate"}d
    ==
  ==
==
```

See:

Generates `manx`, xml nouns.
[lexicon template lang], parser `++sail:vast`.



































































.=  ~[1 2 3 4 5 6]
:~  1
    2
    3
    4
    5
    6  
==

:*  1  3  4  ==
:-  1  :-  3  4
1^3^4
[1 3 4]



':-' is constructing a tuple of two elements, constants...

```
~zod/try=>(mul 4 (add 2 3))
20
~zod/try=> (ream '(mul 4 (add 2 3))')
[ %cnhp
  p=[%cnzz p=~[%mul]]
    q
  ~[
    [%dtzy p=%ud q=4]
    [%cnhp p=[%cnzz p=~[%add]] q=~[[%dtzy p=%ud q=2] [%dtzy p=%ud q=3]]]
  ]
]
```

A twig is a (frond)[lexicon frond] composed of a rune and one or 
more children; here, we see `%cnhp %cnzz %dtzy`, which are defined 
in `++twig` as:

```
++  twig  $&  [p=twig q=twig]                           ::
          $%  
            [%cnhp p=twig q=tusk]                       ::  slam p w/ sample q
            [%cnzz p=wing]                              ::  pulls p
          ==  

++  tusk  (list twig)                                   ::
```

Skipping the particulars, '%-' (slams gates)[lexicon gates], %cnzz is a virtual
rune that (pulls arms)[lexicon cores], and %dtzy is a virtual rune that 
<makes constants>; the resulting twig representing fetching `add` and `mul`,
and applying them to 2, 3, and 4.

```
++  twig  $%                                            ::
            [%$ p=axis]                                 ::  simple leg
          ::                                            ::
            [%bccb p=tile]                              ::  bunt a tile
            ::  etc.
          ==
```
