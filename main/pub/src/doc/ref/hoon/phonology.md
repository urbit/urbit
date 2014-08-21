Phonology
=========

Glyphs
------

Hoon is a keyword-free language - any alphanumeric text in the program is part
of the program. Where other languages have reserved words, Hoon syntax uses
ASCII symbols, or glyphs. In normal English, many of these glyphs have
cumbersome multisyllabic names. As Hoon uses these glyphs heavily, it has its
own, more concise, naming scheme for them:

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


A language is meant to be spoken. Even a programming language. Studies have
shown that even when we read silently, we activate the motor cortex that
controls our vocal cords.  Even if we never speak these symbols, they're easier
to think if bound to simple sounds. 

Mnemonic aids for memorizing the above glyphs can be found in the comments of section 2eF of the Urbit Source, which is reprinted here:

```
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eF, parsing (ascii)          ::
::
++  ace  (just ' ')                                     ::  spACE
++  bar  (just '|')                                     ::  vertical BAR
++  bas  (just '\\')                                    ::  Back Slash (escaped)
++  buc  (just '$')                                     ::  dollars BUCks
++  cab  (just '_')                                     ::  CABoose
++  cen  (just '%')                                     ::  perCENt
++  col  (just ':')                                     ::  COLon
++  com  (just ',')                                     ::  COMma
++  doq  (just '"')                                     ::  Double Quote
++  dot  (just '.')                                     ::  dot dot dot ...
++  fas  (just '/')                                     ::  Forward Slash
++  gal  (just '<')                                     ::  Greater Left
++  gar  (just '>')                                     ::  Greater Right
++  hax  (just '#')                                     ::  Hash
++  kel  (just '{')                                     ::  Curly Left
++  ker  (just '}')                                     ::  Curly Right
++  ket  (just '^')                                     ::  CareT
++  lus  (just '+')                                     ::  pLUS
++  hep  (just '-')                                     ::  HyPhen
++  pel  (just '(')                                     ::  Paren Left
++  pam  (just '&')                                     ::  AMPersand pampersand
++  per  (just ')')                                     ::  Paren Right
++  pat  (just '@')                                     ::  AT pat
++  sel  (just '[')                                     ::  Square Left
++  sem  (just ';')                                     ::  SEMicolon
++  ser  (just ']')                                     ::  Square Right
++  sig  (just '~')                                     ::  SIGnature squiggle
++  soq  (just '\'')                                    ::  Single Quote
++  tar  (just '*')                                     ::  sTAR
++  tec  (just '`')                                     ::  backTiCk
++  tis  (just '=')                                     ::  'tis tis, it is
++  wut  (just '?')                                     ::  wut, what?
++  zap  (just '!')                                     ::  zap! bang! crash!!
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
```

Digraph Glyphs: Runes
--------------------

The fundamental building block of Hoon is the digraph glyph or rune. TThe choice of glyph is not random. The first defines a semantic category. That is, all runes whose first glyph is `|` or `bar` are conceptually related. See Morphology for details.

To pronounce a rune, concatenate the glyph names, stressing the first syllable
and softening the second vowel into a "schwa." Hence, to say `~&`, say
"sigpam." To say `|=`, say "bartis."  

Punctuation Runes
----------------

The following runes are used as punctuation in Tall Form Hoon (See Syntax for details) and have mandatory special pronunciation:

    --    hephep    phep    
    +-    lushep    slep
    ++    luslus    slus
    ==    tistis    stet

Wing Runes
---------

The following runes are used to access specific axes or wings in a noun. See Morphology. They have optional alternate phonology.

    +<    lusgal    glus
    +>    lusgar    gras
    -<    hepgal    gelp
    ->    hepgar    garp   

Tile Runes
---------

The following runes comprise the set of "Tile Runes" and are used to generate
complex types (See Morphology for details). They have an optional alternate
phonology, which describes the tile they generate:

    $%    buccen    kelp
    $^    bucket    herb
    $:    buccol    tile
    $=    buctis    bark
    $&    bucpam    bush 
    $?    bucwut    fern
    $|    bucbar    reed

The following glyphs are not runes, but are commonly used with tile runes to specify basic types. (See Morphology for details). In context, they have an optional alternate phonology:

    @     "atom"
    ^     "cell"
    *     "noun"
    ?     "bean"
    ~     "null"

Irregular Runes
--------------

The following glyphs have optional special pronunciation when they appear as
the irregular form as certain digraph runes. It is perfectly acceptable to
pronounce the characters, but some may find the alternate phonology useful,
especially in cases where multiple irregular forms occur in sequence.

Irregular      Regular         Pronunciation

    ,p           $,(p)          "clam p"
    _p           $_(p)          "bunt p"
    p@q          $@(p q)        "whip p into q"
    !p           ?!(p)          "NOT p"
    &(p q)       ?&(p q)        "AND p q
    |(p q)       ?|(p q)        "OR p q"
    ?(p q)       $?(p q)        "fern p q"
    `p`q         ^-(p q)        "cast p q"
    p=q          ^=(p q)        "p is q"
    ~[p q]       :~(a b)        "list p q"
    `[p]         [~ p]          "unit p"
    p^q          [p q]          "cell p q"
    [p q]        :*(p)          "cell p q"
    +(p)         .+(p)          "bump p"
    =(p q)       .=(p q)        "equals p q"
    p:q          =<(p q)        "p of q"
    p(q r)       %=(p q r)      "toss p q r"
    (p q)        %-(p q)        "slam p q"
    ~(p q r)     %~(p q r)      "slug p q r"

Nouns
-----

Some nouns also have an alternate phonology:


    &       "yes"
    %&      "yes"
    %.y     "yes"

    |       "no"
    %|      "no"
    %.n     "no"

    42      "forty-two"      
    0i42    "dec four two"
    0x2e    "hex two e"
    0b10    "bin one zero"
    0v3t    "base thirty two three t"
    0wA4    "base sixty-four big a four"

    'foo'   "cord foo"
    "foo"   "tape foo"


Example
-------

Take the following snippet of Hoon:

    ++  dec                                                 ::  decrement
      ~/  %dec
      |=  a=@
      ~|  %decrement-underflow
      ?<  =(0 a)
      =+  b=0
      |-  ^-  @
      ?:  =(a +(b))
        b
      $(b +(b))

Omitting the spaces and comments (which only a real purist would include), the
above is pronounced:

  slus  dec
    sigfas  cen-dec
    bartis  A tis pat
    sigbar  cen-decrement-underflow
    wutgal  tis zero A
    tislus  B tis zero
    barhep  kethep  pat
    wutcol  tis A lus B
      B
    buc B lus B

Or using the alternate phonology:

  slus  dec
    sigfas  cen-dec
    bartis  A is atom
    sigbar  cen-decrement-underflow
    wutgal  equals zero A
    tislus  B is zero
    barhep  kethep atom
    wutcol  equals A lus B
      B
    buc B lus B

Which is very similar. The alternate phonology exists as a result of common
speech patterns observed amongst Hoon programmers in the wild. In any language
actually spoken by actual humans, laziness soon rounds off any rough edges.

