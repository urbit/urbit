|%
:>  #  %unicode-data
:>    types to represent UnicdoeData.txt.
+|
++  line
  :>    an individual codepoint definition
  :>
  $:  code=@c               :<  codepoint in hexadecimal format
      name=tape             :<  character name
      gen=general           :<  type of character this is
      :>  canonical combining class for ordering algorithms
      can=@ud
      bi=bidi               :<  bidirectional category of this character
      de=decomp             :<  character decomposition mapping
      ::  todo: decimal/digit/numeric need to be parsed.
      decimal=tape          :<  decimal digit value (or ~)
      digit=tape            :<  digit value, covering non decimal radix forms
      numeric=tape          :<  numeric value, including fractions
      mirrored=?            :<  whether char is mirrored in bidirectional text
      old-name=tape         :<  unicode 1.0 compatibility name
      iso=tape              :<  iso 10646 comment field
      up=(unit @c)          :<  uppercase mapping codepoint
      low=(unit @c)         :<  lowercase mapping codepoint
      title=(unit @c)       :<  titlecase mapping codepoint
  ==
::
++  general
  :>    one of the normative or informative unicode general categories
  :>
  :>  these abbreviations are as found in the unicode standard, except
  :>  lowercased as to be valid symbols.
  $?  $lu  :<  letter, uppercase
      $ll  :<  letter, lowercase
      $lt  :<  letter, titlecase
      $mn  :<  mark, non-spacing
      $mc  :<  mark, spacing combining
      $me  :<  mark, enclosing
      $nd  :<  number, decimal digit
      $nl  :<  number, letter
      $no  :<  number, other
      $zs  :<  separator, space
      $zl  :<  separator, line
      $zp  :<  separator, paragraph
      $cc  :<  other, control
      $cf  :<  other, format
      $cs  :<  other, surrogate
      $co  :<  other, private use
      $cn  :<  other, not assigned
      ::
      $lm  :<  letter, modifier
      $lo  :<  letter, other
      $pc  :<  punctuation, connector
      $pd  :<  punctuation, dash
      $ps  :<  punctuation, open
      $pe  :<  punctuation, close
      $pi  :<  punctuation, initial quote
      $pf  :<  punctuation, final quote
      $po  :<  punctuation, other
      $sm  :<  symbol, math
      $sc  :<  symbol, currency
      $sk  :<  symbol, modifier
      $so  :<  symbol, other
  ==
::
++  bidi
  :>  bidirectional category of a unicode character
  $?  $l    :<  left-to-right
      $lre  :<  left-to-right embedding
      $lri  :<  left-to-right isolate
      $lro  :<  left-to-right override
      $fsi  :<  first strong isolate
      $r    :<  right-to-left
      $al   :<  right-to-left arabic
      $rle  :<  right-to-left embedding
      $rli  :<  right-to-left isolate
      $rlo  :<  right-to-left override
      $pdf  :<  pop directional format
      $pdi  :<  pop directional isolate
      $en   :<  european number
      $es   :<  european number separator
      $et   :<  european number terminator
      $an   :<  arabic number
      $cs   :<  common number separator
      $nsm  :<  non-spacing mark
      $bn   :<  boundary neutral
      $b    :<  paragraph separator
      $s    :<  segment separator
      $ws   :<  whitespace
      $on   :<  other neutrals
  ==
::
++  decomp
  :>  character decomposition mapping.
  :>
  :>  tag: type of decomposition.
  :>  c: a list of codepoints this decomposes into.
  (unit {tag/(unit decomp-tag) c/(list @c)})
::
++  decomp-tag
  :>  tag that describes the type of a character decomposition.
  $?  $font      :<  a font variant
      $nobreak   :<  a no-break version of a space or hyphen
      $initial   :<  an initial presentation form (arabic)
      $medial    :<  a medial presentation form (arabic)
      $final     :<  a final presentation form (arabic)
      $isolated  :<  an isolated presentation form (arabic)
      $circle    :<  an encircled form
      $super     :<  a superscript form
      $sub       :<  a subscript form
      $vertical  :<  a vertical layout presentation form
      $wide      :<  a wide (or zenkaku) compatibility character
      $narrow    :<  a narrow (or hankaku) compatibility character
      $small     :<  a small variant form (cns compatibility)
      $square    :<  a cjk squared font variant
      $fraction  :<  a vulgar fraction form
      $compat    :<  otherwise unspecified compatibility character
  ==
::
:>  #
:>  #  %case-map
:>  #
:>    types to represent fast lookups of case data
+|
++  case-offset
  :>  case offsets can be in either direction
  $%  :>  add {a} to get the new character
      [%add a=@u]
      :>  subtract {a} to get the new character
      [%sub s=@u]
      :>  take no action; return self
      [%none $~]
      :>  represents series of alternating uppercase/lowercase characters
      [%uplo $~]
  ==
::
++  case-node
  :>    a node in a case-tree.
  :>
  :>  represents a range of
  $:  start=@ux
      end=@ux
      upper=case-offset
      lower=case-offset
      title=case-offset
  ==
::
++  case-tree
  :>  a binary search tree of ++case-node items, sorted on span.
  (tree case-node)
--
