/-  unicode-data
=,  eyre
=,  format
::
|_  all/(list line:unicode-data)
++  grab
  :>  converts from mark to unicode-data.
  |%
  ++  mime  |=({* a/octs} (txt (to-wain q.a)))     ::  XX mark translation
  ++  txt
    |^  |=  a/wain
        ^+  all
        %+  turn  a
        |=  b/cord
        ^-  line:unicode-data
        (rash b line)
    ::
    :>  parses a single line of the unicode data file.
    ++  line
      ;~  (glue sem)
        hex
        name-string
        general-category
        (bass 10 (plus sid:ab))
        bidi-category
        decomposition-mapping
        string-number
        string-number
        string-number
        yes-or-no
        name-string
        name-string
        optional-hex
        optional-hex
        optional-hex
      ==
    ::
    :>  parses a single name or comment string.
    ++  name-string
      %+  cook
        |=(a/tape a)
      (star ;~(less sem prn))
    ::
    :>  parses a unicode general category abbreviation to symbol
    ++  general-category
      ;~  pose
        (cold %lu (jest 'Lu'))
        (cold %ll (jest 'Ll'))
        (cold %lt (jest 'Lt'))
        (cold %mn (jest 'Mn'))
        (cold %mc (jest 'Mc'))
        (cold %me (jest 'Me'))
        (cold %nd (jest 'Nd'))
        (cold %nl (jest 'Nl'))
        (cold %no (jest 'No'))
        (cold %zs (jest 'Zs'))
        (cold %zl (jest 'Zl'))
        (cold %zp (jest 'Zp'))
        (cold %cc (jest 'Cc'))
        (cold %cf (jest 'Cf'))
        (cold %cs (jest 'Cs'))
        (cold %co (jest 'Co'))
        (cold %cn (jest 'Cn'))
        (cold %lm (jest 'Lm'))
        (cold %lo (jest 'Lo'))
        (cold %pc (jest 'Pc'))
        (cold %pd (jest 'Pd'))
        (cold %ps (jest 'Ps'))
        (cold %pe (jest 'Pe'))
        (cold %pi (jest 'Pi'))
        (cold %pf (jest 'Pf'))
        (cold %po (jest 'Po'))
        (cold %sm (jest 'Sm'))
        (cold %sc (jest 'Sc'))
        (cold %sk (jest 'Sk'))
        (cold %so (jest 'So'))
      ==
    ::
    :>  parses a bidirectional category abbreviation to symbol.
    ++  bidi-category
      ;~  pose
        (cold %fsi (jest 'FSI'))
        (cold %lre (jest 'LRE'))
        (cold %lri (jest 'LRI'))
        (cold %lro (jest 'LRO'))
        (cold %nsm (jest 'NSM'))
        (cold %pdf (jest 'PDF'))
        (cold %pdi (jest 'PDI'))
        (cold %rle (jest 'RLE'))
        (cold %rli (jest 'RLI'))
        (cold %rlo (jest 'RLO'))
        (cold %al (jest 'AL'))
        (cold %an (jest 'AN'))
        (cold %bn (jest 'BN'))
        (cold %cs (jest 'CS'))
        (cold %en (jest 'EN'))
        (cold %es (jest 'ES'))
        (cold %et (jest 'ET'))
        (cold %on (jest 'ON'))
        (cold %ws (jest 'WS'))
        (cold %b (jest 'B'))
        (cold %l (jest 'L'))
        (cold %r (jest 'R'))
        (cold %s (jest 'S'))
      ==
    ::
    ::  TODO: This seems to be where the nest-fail is. There's an extra @ here?
    ++  decomposition-mapping
      ;~  pose
        :: a tag and a list of characters to decompose to
        %+  stag  ~
          ;~  plug
            ;~  pose
              (stag ~ (ifix [gal ;~(plug gar ace)] decomp-tag))
              (easy ~)
            ==
            (cook |=(a/(list @c) a) (most ace hex))
          ==
        ::  no decomposition information
        (easy ~)
      ==
    ::
    ++  decomp-tag
      ;~  pose
        (cold %font (jest 'font'))
        (cold %no-break (jest 'noBreak'))
        (cold %initial (jest 'initial'))
        (cold %medial (jest 'medial'))
        (cold %final (jest 'final'))
        (cold %isolated (jest 'isolated'))
        (cold %circle (jest 'circle'))
        (cold %super (jest 'super'))
        (cold %sub (jest 'sub'))
        (cold %vertical (jest 'vertical'))
        (cold %wide (jest 'wide'))
        (cold %narrow (jest 'narrow'))
        (cold %small (jest 'small'))
        (cold %square (jest 'square'))
        (cold %fraction (jest 'fraction'))
        (cold %compat (jest 'compat'))
      ==
    ::
    ++  string-number
      %+  cook
        |=(a/tape a)
      (star ;~(pose nud fas hep))
    ::
    ++  yes-or-no
      ;~  pose
        (cold %.y (jest 'Y'))
        (cold %.n (jest 'N'))
      ==
    ::
    ++  optional-hex
      ;~  pose
        (stag ~ hex)
        (easy ~)
      ==
    --
  --
++  grad  %txt
--
