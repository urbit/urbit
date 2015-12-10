---
next: true
sort: 5
title: Marks
---

We've used predefined marks already, but we haven't yet created
our own marks.  Let's write a sample mark of our own, then chain
it together with some preexisting ones to have fun with type
conversions.

Let's make a small "cord" mark.  "Cord" is a name we use for `@t`
text, but there's no predefined mark for it.  This is
`/mar/cord.hoon`:

```
/?  314
|_  cod=@t
++  grab
  |%
  ++  atom  |=(arg=@ `@t`(scot %ud arg))
  --
++  grow
  |%
  ++  md  cod
  --
--
```

Let's go through this line by line.  `/?  314` is the required
version number, just like in apps.  After that everything's in a
`|_`, which is a `|%` core, but with input arguments.  In our
case, the argument is the marked data.

There are three possible top-level arms in the `|_` core,
`++grab`, `++grow`, and `++grad`.  `++grad` is used for revision
control, and it's covered elsewhere.  `++grab` specifies
functions to convert from another mark to the current mark.
`++grow` specifies how to convert from the current mark to
another one.

In our case, we only define one arm in `++grab`, namely `++atom`.
This allows us to convert any atom to a cord.  `++scot` is a
standard library function in `hoon.hoon` which pretty-prints
atoms of various types.  We want tot treat the atom as an
unsigned decimal number, so we give `%ud` and the given argument
to `++scot`.  Thus, the form of an arm in `++grab` is
`|=(other-mark-type this-mark-value)`.

In `++grow`, we just convert to the `md` mark, which is just a
`@t` internally, but it has fancy conversion functions to things
like html.  `++md` isn't a function because the "argument" is the
`cod=@t` on the `|_` line.  We just produce the new value
directly.

Let's play around a bit with this mark.  First, let's take a
marked atom and convert it to our new mark.

```
~fintud-macrep:dojo> &atom 9
9
~fintud-macrep:dojo> &cord &atom 9
'9'
~fintud-macrep:dojo> &cord &atom &cord &atom 9
'57'
```

ASCII 9 is 57.  There's no requirement, implicit or otherwise,
that conversions back and forth between marks be inverses of each
other.  These are semantic conversions, in that they refer to the
same concept.  They're not isomorphisms.

Let's play around a little more:

```
~fintud-macrep:dojo> &md &cord &atom 17
'17'
~fintud-macrep:dojo> &hymn &md &cord &atom 17
[ [%html ~]
  [[%head ~] [[%title ~] [[%~. [%~. "Untitled"] ~] ~] ~] ~]
  [ [%body ~]
    [ i=[g=[n=%meta a=~[[n=%value v="{}"] [n=%name v="frontmatter"] [n=[%urb %front] v=""]]] c=~]
      t=[i=[g=[n=%p a=~] c=~[[g=[n=%$ a=~[[n=%$ v="17"]]] c=~]]] t=~]
    ]
  ]
  ~
]
```

Ooh, that was exciting.  If you squint at that, but it looks an
awful lot like html.  Our value `17` is still in there, in the
body.  `urb` is the mark we render most web pages to.  It makes
sure you have a complete skeleton of a web page.  Of course, by
the time this gets to the web page, it's plain html.  Let's do
the final step in the conversion.

```
~fintud-macrep:dojo> &mime &hymn &md &cord &atom 17
[ [%text %html ~]
  p=121
  q='<html><head><title>Untitled</title></head><body><meta value="{}" name="frontmatter" urb:front="" /><p>17</p></body></html>'
]
```

This is a mime-typed octet stream with type `/text/html`, length
121 bytes, and our lowly number `17` rendered to a web page.
`cord` was just one step in the chain.

Of course, arvo can infer some parts of this chain:

```
~fintud-macrep:dojo> &cord 17
'17'
```

Likewise, `md` is the only way to get from `cord` to `hymn`, so
that can be omitted.  If we omit the `hymn`, though, we get:

```
~fintud-macrep:dojo> &mime &cord 17
[[%text %x-markdown ~] p=1 q='7']
```

Here, we converted straight from `cord` to `md` to `mime`.  Arvo
decided that was the most straightforward conversion, but it
gives a different result than passing it through the `hymn`
mark.

The minimal correct input, then, is `&mime &hymn &cord &atom 17`.
