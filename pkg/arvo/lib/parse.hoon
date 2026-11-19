|%
::  combinators
::
++  compose-if           bend  ::  ;~((compose-if GATE) A B ..N)      compose A B ..N with binary GATE that returns unit. Fail if unit null.
++  compose              comp  ::  ;~((compose GATE) A B ..N)         compose A B ..N with binary GATE
++  fail                ^fail  ::  fail                               just fail
++  prefix               pfix  ::  ;~(prefix A B)                     B if preceeded by A (A is discarded)
++  suffix               sfix  ::  ;~(suffix A B)                     A if followed by B (B is discarded)
++  tuple                plug  ::  ;~(tuple A B ..N)                  cell of [A B ..N]
++  or                   pose  ::  ;~(or A B ..N)                     A or B ... or N
++  unless               less  ::  ;~(unless A B)                     B if not A
++  delimit              glue  ::  ;~((delimit A) B C ..N)            cell of [B C ..N] separated by A (A's are discarded)
++  lookahead            simu  ::  ;~(lookahead A B)                  B if A matches
::  rule builders
::
++  succeed              easy  ::  (succeed x)                        produce noun x & consume nothing
++  replace              cold  ::  (replace x A)                      replace A with constant x
++  which                fuss  ::  (which A B)                        %.y if A, %.n if B
++  full                ^full  ::  (full A)                           must parse to end with A
++  prepend              funk  ::  (funk TAPE A)                      prepend with TAPE then A
++  indented             inde  ::  (indented A)                       strip whitespace before A based on column number
++  infix                ifix  ::  (infix [A B] C)                    C if prefixed with A and suffixed with B (A & B discarded)
++  recurse              knee  ::  (recurse BUNT TRAP)                recurse in TRAP that produces type of BUNT
++  unitized             punt  ::  (unitized A)                       parse A or null
++  tag                  stag  ::  (tag x A)                          head-tag A with noun x
++  switch-keys          stet  ::  (switch-keys PAIRS)                add p/q faces to pair-list of ranges and rules
++  switch               stew  ::  (switch PAIRS)                     PAIRS is (list [char-or-range rule]). Match 1st char to item; parse with rule
++  match   ::  match characters
  |%
  ++  any                mask  ::  (any:match "CHARS")                any character in list CHARS
  ++  all                next  ::  all:match                          any character
  ++  char               just  ::  (char:match CHAR)                  match exactly character CHAR
  ++  string             jest  ::  (string:match CORD)                match exactly CORD
  ++  range              shim  ::  (range:match x y)                  match any between x and y, inclusive
  ++  union              perk  ::  (union:match CONSTANTS)            match any in list of %constants
  --
++  base-n  ::  base(n)
  |%
  ++  msb                bass  ::  (msb:base-n n A)                   convert MSB-first list produced by A to base(n) atom
  ++  lsb                boss  ::  (lsb:base-n n A)                   convert LSB-first list produced by A to base(n) atom
  --
++  apply   ::  apply gate
  |%
  ++  that               cook  ::  (that:apply GATE A)                parse A then modify with GATE
  ++  where              here  ::  (where:apply GATE A)               apply GATE to [position A]
  ++  if                 sear  ::  (if:apply GATE A)                  apply GATE which produces a unit to A. If null, fail
  --
++  many    ::  parse repeatedly
  |%
  ++  zero               star  ::  (zero:many A)                      0 or more A
  ++  one                plus  ::  (one:many A)                       1 or more A
  ++  compose            stir  ::  (compose:many BUNT GATE A)         fold over 0 or more A with GATE that produces the type of BUNT
  ++  bounded            stun  ::  (bounded:many [x y] A)             Parse at least x and at most y A's
  ++  delimit
    |%
    ++  zero             more  ::  (zero:delimit:many A B)            0 or more B separated by A
    ++  one              most  ::  (one:delimit:many A B)             1 or more B separated by A
    ++  compose          slug  ::  ((compose:delimit:many GATE) A B)  Compose list of B delimited by A with GATE
    --
  --
::  special characters
::
++  space                ace   ::  ' '
++  bar                 ^bar   ::  '|'
++  backslash            bas   ::  '\'
++  dollar               buc   ::  '$'
++  underscore           cab   ::  '_'
++  percent              cen   ::  '%'
++  colon                col   ::  ':'
++  comma                com   ::  ','
++  double-quote         doq   ::  '"'
++  period               dot   ::  '.'
++  forward-slash        fas   ::  '/'
++  left-chevron         gal   ::  '<'
++  right-chevron        gar   ::  '>'
++  hash                 hax   ::  '#'
++  hyphen               hep   ::  '-'
++  left-brace           kel   ::  '{'
++  right-brace          ker   ::  '}'
++  caret                ket   ::  '^'
++  plus-sign            lus   ::  '+'
++  semicolon            mic   ::  ';'
++  left-paren           pal   ::  '('
++  right-paren          par   ::  ')'
++  ampersand            pam   ::  '&'
++  at-sign              pat   ::  '@'
++  left-bracket         sel   ::  '['
++  right-bracket        ser   ::  ']'
++  tilde                sig   ::  '~'
++  single-quote         soq   ::  '''
++  asterisk             tar   ::  '*'
++  backtick             tic   ::  '`'
++  equals               tis   ::  '='
++  question-mark        wut   ::  '?'
++  exclamation-mark     zap   ::  '!'
::  useful idioms
::
++  alphabetical         alf   ::  [a-zA-Z]
++  alphanumeric         aln   ::  [a-zA-Z0-9]
++  binary               bin   ::  [01]+ to atom
++  binary-digit         but   ::  [01] to atom
++  hexadecimal          hex   ::  [a-fA-F0-9]+ to atom
++  hexadecimal-digit    hit   ::  [a-fA-F0-9] to atom
++  octal-digit          cit   ::  [0-7] to atom
++  decimal              dem   ::  [0-9]+  to atom
++  decimal-digit        dit   ::  [0-9] to atom
++  linebreak-or-space   gah   ::  ' ' or linebreak
++  uppercase-letter     hig   ::  [A-Z]
++  lowercase-letter     low   ::  [a-z]
++  hexbyte              mes   ::  two hex characters to atom
++  digit                nud   ::  [0-9]
++  printable            prn   ::  ASCII decimals 32-256 sans delete
++  triple-backtick      soz   ::  [''']
++  term-strict          sym   ::  'foo-42' to %foo-42
++  term-mixed-case      mixed-case-symbol  ::  'fOo-42' to %fOo-42
++  lark                 ven   ::  '->+' to axis
++  base64-digit         vit   ::  [a-zA-Z0-9+-] to atom
--
