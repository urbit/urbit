/-  unicode-data
=,  eyre
=,  format
::
|_  all/(list line:unicode-data)
++  grab
  :>  converts from mark to unicode-data.
  |%
  ++  mime  |=([* a=octs] (txt (to-wain q.a)))     ::  XX mark translation
  ++  txt
    |^  |=  a=wain
        ^+  all
        %+  murn  a
        |=  b=cord
        ^-  (unit line:unicode-data)
        ?~  b  ~
        `(rash b line)
    ::
    :>  parses a single character information line of the unicode data file.
    ++  line
      ;~  (glue sem)
        hex                       :: code/@c        codepoint in hex format
        name-string               :: name/tape      character name
        general-category          :: gen/general    type of character
        (bass 10 (plus dit))      :: can/@ud        canonical combining class
        bidi-category             :: bi/bidi        bidirectional category
        decomposition-mapping     :: de/decomp      decomposition mapping
      ::
      :: todo: decimal/digit/numeric need to be parsed.
      ::
        string-number    :: decimal/tape      decimal digit value (or ~)
        string-number    :: digit/tape        digit value, even if non-decimal
        string-number    :: numeric/tape      numeric value, including fractions
      ::
        (flag 'Y' 'N')   :: mirrored/?        is char mirrored in bidi text?
        name-string      :: old-name/tape     unicode 1.0 compatibility name
        name-string      :: iso/tape          iso 10646 comment field
        (punt hex)       :: up/(unit @c)      uppercase mapping codepoint
        (punt hex)       :: low/(unit @c)     lowercase mapping codepoint
        (punt hex)       :: title/(unit @c)   titlecase mapping codepoint
      ==
    ::
    :>  parses a single name or comment string.
    ++  name-string
      %+  cook
        |=(a=tape a)
      (star ;~(less sem prn))
    ::
    :>  parses a unicode general category abbreviation to symbol
    ++  general-category
      %+  sear  (soft general:unicode-data)
      :(cook crip cass ;~(plug hig low (easy ~)))
    ::
    :>  parses a bidirectional category abbreviation to symbol.
    ++  bidi-category
      %+  sear  (soft bidi:unicode-data)
      :(cook crip cass (star hig))
    ::
    ++  decomposition-mapping
      %-  punt  :: optional
      :: a tag and a list of characters to decompose to
      ;~  plug
        (punt (ifix [gal ;~(plug gar ace)] decomp-tag))
        (cook |=(a=(list @c) a) (most ace hex))
      ==
    ::
    ++  decomp-tag
      %+  sear  (soft decomp-tag:unicode-data)
      :(cook crip cass (star alf))
    ::
    ++  string-number
      %+  cook
        |=(a=tape a)
      (star ;~(pose nud fas hep))
    ::
    --
  --
++  grad  %txt
--
