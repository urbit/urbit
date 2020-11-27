::  Hoon style sample
::
::  this is a sample file designed to set conventions for
::  high-quality conventional hoon.
::
::  all lines must be under 80 characters.  no blank lines.
::  any line longer than 60 characters is probably too long.
::  uppercase or non-ascii letters are strongly discouraged.
::
::  informal comments (lines with {::}) should be used only for
::  meta-discussion *about* the code.
::
::  whenever possible, use formal decorations. {:>} decorates
::  the next expression; {:<} decorates the previous one.
::
::  there are two places to put decorations: in line with the
::  code, and on the right margin.
::
::  in comments and decorations, use *phrase* for emphasis
::  and {braces} to surround code literals.  (documentation will
::  eventually be automatically generated from formal comments.)
::  %literal, ++literal, ~ship need no braces.  for a valid
::  hoon expression, `exp.
::
::  there are three conventions for naming: *ultralapidary*,
::  *lapidary*, and *natural*.  this file is mostly natural.
::
::  when in doubt, use the *natural* naming convention.  for
::  both arms and faces, natural naming means long, legible,
::  english-language phrases, in hyphen-separated {kebab-case}.
::
::  lapidary conventions should be used only for small, simple,
::  self-contained systems.  lapidary mode means three-letter
::  faces ("variable names") and four-letter arms ("methods").
::
::  ultralapidary conventions use single-letter names starting
::  with {a}.  use this convention only for one-liners, etc.
::
::  the file below is a medium-sized generator, built around
::  a typical two-core structure.  the cores are labeled {%arch}
::  (structures) and {%work} (productions).  this is canonical.
::
::  this code is written to display the variety of formatting
::  options the parser allows.  a specific convention should pick
::  one of these styles and stick to it.
::
::  a forward decoration block {:>} is either a *document block* or
::  a *definition block*.

::  a document block has two parts, each of which is optional:
::  the *title* and the *body*,
::
::  the title is a ++term preceded by {::  #  %}.  only cores
::  and core chapters (preceded by {+|}) can use titles.  titles
::  are optionally surrounded by blank or semi-blank decorations,
::  {:>} or {::  #}.
::
::  the body is either short or long.  a short body is a *single line*
::  preceded by {::  } - ie, not indented.  a long body starts with
::  a *single line* indented by two extra spaces, {::    }, then a
::  blank line, then a series of paragraphs.
::
::  a definition block is a list of name definitions.  the twig below
::  the block is traversed for bindings on these names.
::
::  a name definition can be short or long.  a short definition is
::  a *single line* of the form {::  name: value}.
::
::  a long definition is a short definition, followed by a blank
::  decoration {:>}, followed by a series of paragraphs each
::  indented by an extra two spaces.
::
::  a paragraph is a series of lines, not indented for text,
::  indented by four extra spaces, {::      }, for code.
::
::  a backward decoration {:<} is only one line, always parsed
::  as a short body.
::
:-  %say
|=  *
=<  [%noun (say-hello %world)]
=>  ::  #  %arch
    ::
    ::    structures for our imaginary hello, world generator.
    ::
    ::  nothing forces us to put structures in a separate core.
    ::  but compile-time evaluation doesn't work in the current
    ::  core; we often want to statically evaluate structures.
    ::
    ::  there are three kinds of structures: models (normalizing
    ::  functions), patterns (functions that build models), and
    ::  constants (static data).
    ::
    ::  most code will not need its own patterns.  but put them
    ::  in a separate chapter (separated by {+|}).
    |%
    ::  #  %model
    ::
    ::    models (molds) are functions that normalize nouns.
    ::
    ::  arms producing molds are introduced with {+$}.  the
    ::  compiler will copy the arm decoration onto its product
    :: +|
    +$  spot  [p=@ q=@]                                 ::  a coordinate
    +$  tops                                            ::  also a coordinate
      [p=@ q=@]
    +$  goof                                            ::  a simple tuple
      $:  foo=@                                         ::  something mysterious
          bar=@                                         ::  go here for drink
          moo=(binary-tree juice)                       ::  cows do this
      ==
    +$  juice                                           ::  fruity beverage
      $%  [%plum p=@]                                   ::  fresh prune
          [%pear p=@ q=@]                               ::  good for cider
          [%acai p=@]                                   ::  aztec superfood
      ==
    ::  #
    ::  #  %pattern
    ::  #
    ::
    ::    patterns are functions that build models.
    ::
    ::  other languages might call these "type constructors"
    ::  or "higher-kinded types".
    :: +|
    ++  binary-tree                                     ::  tree pattern
      |*  a=$-(* *)
      $@(~ [n=a l=(binary-tree a) r=(binary-tree a)])
    ::  #
    ::  #  %constant
    ::  #
    ::    if you have constants, put them in their own chapter.
    :: +|
    ++  answer                                          ::  answer to everything
      42
    --
::  #
::  #  %work
::  #
::    engines for our imaginary hello, world app.
::
|%
++  say-hello                                           ::  say hi to someone
  ::  friendly welcome message
  ::
  |=  ::  txt: friend to say hi to
      ::
      txt=term
  ^-  tape
  "hello, {(rip 3 txt)}"
::    ++say-goodbye: say a really proper goodbye
::
::  some paragraphs about the goodbye algorithm, possibly
::  including code indented by four extra spaces:
::
::      ?:  =(%hello %world)
::        %hello
::      %world
::
++  say-goodbye                                         ::
  ::  describe product of function
  ::
  |=  ::  txt: departing friend
      ::  num: number of friends
      $:  txt=term
          num=@
      ==
  ^-  tape
  ::  foo: four
  ::  bar: forty-two
  =/  foo  (add 2 2)
  =/  bar  (add (mul num foo) 2)
  =/  moo  (mul num bar)                                ::  for all the cows
  "goodbye and {(scow %ud moo)}, {(rip 3 txt)}"
::
++  say-minimum                                         ::  minimal decoration
  |=  txt=term
  "nothing to say to {(rip 3 txt)}"
--
