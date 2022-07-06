:>    Hoon doccords sample
:>
:>  this is a sample file designed to explain syntax and conventions
:>  for doccords
:>
:>  all lines must be under 80 characters.  no blank lines.
:>  any line longer than 60 characters is probably too long.
:>  uppercase or non-ascii letters are strongly discouraged.
:>
:>  informal comments (lines with {:>}) should be used only for
:>  meta-discussion *about* the code.
:>
:>  whenever possible, use formal decorations. {:>} decorates
:>  the next expression; {:<} decorates the previous one.
:>
:>  there are two places to put decorations: in line with the
:>  code, and on the right margin.
:>
:>  the file below is a well-commented library, built around
:>  a typical two-core structure.  the cores are labeled {%arch}
:>  (structures) and {%work} (productions).
:>
:>  this code is written to display the variety of formatting
:>  options the parser allows.  a specific convention should pick
:>  one of these styles and stick to it.
:>
:>  there are three ways to mark the beginning of a formal comment:
:>  1- {:>  $foo:}
:>  2- {:>    +bar:}
:>  3- {:>    }
:>
:>  style 1 is for single-line comments at a given lexical location
:>  (see below), which may be written above any arm in a core and
:>  the parser will move the comment to the specified arm.
:>
:>  style 2 is much like style 1, but may be followed by a series
:>  of paragraphs:
:>  {:>}
:>  {:>  more text}
:>  {:>  even more text}
:>  {:>}
:>  {:>      |=(code=hoon !!)}
:>
:>  a paragraph is a series of lines, not indented for text,
:>  but indented by four extra spaces, {:>      }, for code.
:>
:>  style 3 is used to annotate the hoon or spec that immediately follows
:>  the comment. this may be either a single line comment as in style 1,
:>  or multi-line comment as in style 2.
:>
:>  the $foo and +bar above are examples of *lexical locations* for
:>  style and batch-commenting purposes. this tells the parser to attempt
:>  to attach the comment to the specified location. these locations
:>  may be written as follows:
:>  - `|foo` means a chapter
:>  - `%foo` means a constant
:>  - `.foo` means a face
:>  - `+foo` means an arm
:>  - `$foo` means a spec
:>  - `^foo` means a core
:>  - `_foo` means a door
:>  - `=foo` means a gate
:>  - `/foo` means a file path segment
:>
:>  thus /lib/foo^widget|initial=open means the =open gate in the |initial
:>  chapter of the ^widget core in the /foo library
:>
:>  at present, doccords does not support lexical locations in full.
:>  only single-element locations of the form `$foo` and `+foo` are supported,
:>  and must be written above an arm in the core to which they are to be
:>  attached. you may still write doccords for these locations, and they
:>  will be parsed, but are thrown away before they make it to the compiler.
:>
:>  a postfix formal comment will either attach to hoon or spec on the
:>  current line, or the arm name if there is no hoon or spec on the
:>  line
:>
:>  |% and |@ cores can be given @tas names with e.g. `|%  %foo`. for
:>  now, the only purpose of this is to make cores not built by arms
:>  legible to doccords. in order to name any other core type, you will
:>  need to desugar it for now.
:>
:>  to inspect doccords in this file from dojo, try the following:
:>
:>  > =deco -build-file %/lib/deco/hoon
:>  > # deco
:>  > # deco/arch
:>  > # deco/arch/molds
:>  > # deco/arch/molds/goof
:>
:>  > ?? *goof:deco
:>
=>  ::
    :>    structures for our imaginary hello, world generator.
    :>
    :>  nothing forces us to put structures in a separate core.
    :>  but compile-time evaluation doesnt work in the current
    :>  core; we often want to statically evaluate structures.
    :>
    :>  there are three kinds of structures: moldss (normalizing
    :>  functions), mold builders (functions that build molds), and
    :>  constants (static data).
    :>
    :>  most code will not need its own patterns.  but put them
    :>  in a separate chapter (separated by {+|}).
    |%  %arch
    :>    molds are functions that normalize nouns.
    :>
    :>  arms producing molds are introduced with {+$}.  the
    :>  compiler will copy the arm decoration onto its product
    +|  %molds
    :>  $jam: some delicious jam
    :>  $jelly: different from jam?
    +$  spot  [p=@ q=@]                                 :<  a coordinate
    +$  tops                                            :<  mold for coordinate
      [p=@ q=@]                                         :<  another coordinate
    +$  goof                                            :<  a simple tuple mold
      $:  foo=@                                         :<  something mysterious
          bar=@                                         :<  go here for drink
          moo=(binary-tree juice)                       :<  cows do this
      ==
    +$  juice                                           :<  fruity beverage mold
      $%  [%plum p=@]                                   :<  fresh prune
          [%pear p=@ q=@]                               :<  good for cider
          [%acai p=@]                                   :<  aztec superfood
      ==
    +$  jam  @tas
    +$  jelly  @tas
    :>    mold builders are functions that build molds from other molds
    :>
    :>  other languages might call these "type constructors"
    :>  or "higher-kinded types".
    +|  %mold-builders
    ++  binary-tree                                     :<  tree mold builder
      |*  a=$-(* *)
      $@(~ [n=a l=(binary-tree a) r=(binary-tree a)])
    ::
    :>    if you have constants, put them in their own chapter.
    +|  %constant
    ++  answer                                          :<  answer to everything
      42
    --
:>    engines for our imaginary hello, world app.
:>
|%  %work
:>  +default-jam: bunts $jam
:>  +default-juice: bunts $juice
++  say-hello                                           :<  say hi to someone
  :>    friendly welcome message
  :>
  |=  :>  .txt: friend to say hi to
      :>
      txt=term
  ^-  tape
  "hello, {(rip 3 txt)}"
:>    +say-goodbye: say a really proper goodbye
:>
:>  some paragraphs about the goodbye algorithm, possibly
:>  including code indented by four extra spaces:
:>
:>      ?:  =(%hello %world)
:>        %hello
:>      %world
:>
++  say-goodbye                                         :<  say bye to someone
  :>    describe product of function
  :>
  |=  :>  .txt: departing friend
      :>  .num: number of friends
      $:  txt=term
          num=@
      ==
  ^-  tape
  :>  .foo: four
  :>  .bar: forty-two
  =/  foo  (add 2 2)
  =/  bar  (add (mul num foo) 2)
  =/  moo  (mul num bar)                                :<  for all the cows
  "goodbye and {(scow %ud moo)}, {(rip 3 txt)}"
::
++  say-minimum                                         :<  minimal decoration
  |=  txt=term
  "nothing to say to {(rip 3 txt)}"
::
++  default-jam  *jam
++  default-juice  *juice
--
